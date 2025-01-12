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

import scala.util.Random.shuffle
import awakening.LabyrinthAwakening._
import awakening.JihadistBot

// Card Text:
// ------------------------------------------------------------------
// US side selects Scandinavia's Posture.
// Then jihadists place an available Plot 1 or, if any Islamist Rule,
// any available plot in a non-Islamist Rule Muslim country.
// ------------------------------------------------------------------
object Card_096 extends Card(96, "Danish Cartoons", Unassociated, 1, Remove, NoLapsing, NoAutoTrigger) {
  // Used by the US Bot to determine if the executing the event would alert a plot
  // in the given country
  override
  def eventAlertsPlot(countryName: String, plot: Plot): Boolean = false

  // Used by the US Bot to determine if the executing the event would remove
  // the last cell on the map resulting in victory.
  override
  def eventRemovesLastCell(): Boolean = false

  // Returns true if the printed conditions of the event are satisfied
  override
  def eventConditionsMet(role: Role) = true

  def getPlotCandidates() = if (game.numIslamistRule == 0)
    game.availablePlots.filter(_ == Plot1)
  else
    game.availablePlots

  // Returns true if the Bot associated with the given role will execute the event
  // on its turn.  This implements the special Bot instructions for the event.
  // When the event is triggered as part of the Human players turn, this is NOT used.
  override
  def botWillPlayEvent(role: Role): Boolean =
    (role == Jihadist && getPlotCandidates().nonEmpty) ||
    (role == US && getPlotCandidates().isEmpty)

  // Carry out the event for the given role.
  // forTrigger will be true if the event was triggered during the human player's turn
  // and it associated with the Bot player.
  override
  def executeEvent(role: Role): Unit = {
    val posture = if (isHuman(US))
      askPosture(Scandinavia)
    else
      game.usPosture

    addEventTarget(Scandinavia)
    setCountryPosture(Scandinavia, posture)

    val plotCandidates = getPlotCandidates()
    if (plotCandidates.nonEmpty) {
      log()
      val plot = plotCandidates match {
        case p::Nil => p
        case plots if isHuman(Jihadist) => askPlots(plots, 1).head
        case plots => shuffle(plots).head
      }
      val candidates = countryNames(game.muslims filter (!_.isIslamistRule))
      val name = if (isHuman(Jihadist))
        askCountry(s"Place $plot in which country: ", candidates)
      else
        JihadistBot.plotPriority(candidates).get

      addEventTarget(name)
      addAvailablePlotToCountry(name, plot)
    }
    else if (game.numIslamistRule == 0)
      log("\nThere is no available level 1 plot.", Color.Event)
    else
      log("\nThere are no available Plots.", Color.Event)
  }
}
