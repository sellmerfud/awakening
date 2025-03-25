//  _          _                _       _   _
// | |    __ _| |__  _   _ _ __(_)_ __ | |_| |__
// | |   / _` | '_ \| | | | '__| | '_ \| __| '_ \
// | |__| (_| | |_) | |_| | |  | | | | | |_| | | |
// |_____\__,_|_.__/ \__, |_|  |_|_| |_|\__|_| |_|
//                   |___/
//     _                _              _
//    / \__      ____ _| | _____ _ __ (_)_ __   __ _
//   / _ \ \ /\ / / _` | |/ / _ \ '_ \| | '_ \ / _` |
//  / ___ \ V  V / (_| |   <  __/ | | | | | | | (_| |
// /_/   \_\_/\_/ \__,_|_|\_\___|_| |_|_|_| |_|\__, |
//                                             |___/
// An scala implementation of the solo AI for the game 
// Labyrinth: The Awakening, 2010 - ?, designed by Trevor Bender and
// published by GMT Games.
// 
// Copyright (c) 2010-2017 Curt Sellmer
//
// Permission is hereby granted, free of charge, to any person obtaining
// a copy of this software and associated documentation files (the
// "Software"), to deal in the Software without restriction, including
// without limitation the rights to use, copy, modify, merge, publish,
// distribute, sublicense, and/or sell copies of the Software, and to
// permit persons to whom the Software is furnished to do so, subject to
// the following conditions:
//
// The above copyright notice and this permission notice shall be
// included in all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
// EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
// MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
// NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
// LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
// OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
// WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

package awakening.cards

import awakening.LabyrinthAwakening._
import awakening.USBot

// Card Text:
// ------------------------------------------------------------------
// Play if there is a plot in an Ally or Good country. Reveal and remove it.
// Draw a card. Select 1 non-US country's Posture.
// ------------------------------------------------------------------
object Card_021 extends Card(21, "Let’s Roll!", US, 2, NoRemove, NoLapsing, NoAutoTrigger) {

  def getCandidates: List[String] = countryNames(
    game.countries.filter {
      case m: MuslimCountry =>
        !m.truce && m.plots.nonEmpty && (m.isGood || m.isAlly)
      case n: NonMuslimCountry =>
        n.plots.nonEmpty && n.isGood
    }
  )

  // Used by the US Bot to determine if the executing the event would alert a plot
  // in the given country
  override
  def eventAlertsPlot(countryName: String, plot: Plot): Boolean = getCandidates.contains(countryName)

  // Used by the US Bot to determine if the executing the event would remove
  // the last cell on the map resulting in victory.
  override
  def eventRemovesLastCell(): Boolean = false

  // Returns true if the printed conditions of the event are satisfied
  override
  def eventConditionsMet(role: Role) = getCandidates.nonEmpty

  // Returns true if the Bot associated with the given role will execute the event
  // on its turn.  This implements the special Bot instructions for the event.
  // When the event is triggered as part of the Human players turn, this is NOT used.
  override
  def botWillPlayEvent(role: Role): Boolean = true

  // Carry out the event for the given role.
  // forTrigger will be true if the event was triggered during the human player's turn
  // and it associated with the Bot player.
  override
  def executeEvent(role: Role): Unit = {
    val plotCandidates    = getCandidates
    val postureCandidates = countryNames(game.nonMuslims.filter(_.canChangePosture))
    if (isHuman(role)) {
      val name = askCountry("Select country with plots: ", plotCandidates)
      val plot = humanPickPlotToAlert(name)
      addEventTarget(name)
      performAlert(name, plot)
      log(s"\n$US draws one card and adds it to their hand", Color.Event)
      askCardDrawnFromDrawPile(role)
      val postureName = askCountry("Select posture of which country: ", postureCandidates)
      val newPosture = askPosture(postureName)
      addEventTarget(postureName)
      setCountryPosture(postureName, newPosture)
    }
    else {
      val USBot.PlotInCountry(plot, c) = USBot.selectPriorityPlot(plotCandidates)
      addEventTarget(c.name)
      performAlert(c.name, plot)
      log(s"\n$US draws one card.", Color.Event)
      askCardDrawnFromDrawPile(role)
        .map(deck(_).numAndName)
        .foreach { cardDisplay =>
          log(s"Place $cardDisplay on top of the $US Bot hand", Color.Event)
        }
      val postureName = USBot.posturePriority(postureCandidates).get
      addEventTarget(postureName)
      setCountryPosture(postureName, game.usPosture)
    }
  }
}
