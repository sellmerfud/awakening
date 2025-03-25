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
import awakening.JihadistBot

// Card Text:
// ------------------------------------------------------------------
// Place a non-WMD Plot in any country OR Remove a Posture marker
// from a tested non-Muslim country.
// ------------------------------------------------------------------
object Card_298 extends Card(298, "False Flag Attacks", Jihadist, 2, NoRemove, NoLapsing, NoAutoTrigger) {
  // Used by the US Bot to determine if the executing the event would alert a plot
  // in the given country
  override
  def eventAlertsPlot(countryName: String, plot: Plot): Boolean = false

  // Used by the US Bot to determine if the executing the event would remove
  // the last cell on the map resulting in victory.
  override
  def eventRemovesLastCell(): Boolean = false

  def plotsMarkers() = game.availablePlots.filterNot(_ == PlotWMD).sorted

  def removePostureCandidates() = countryNames(game.nonMuslims.filter(_.canRemovePosture))

  def botPreferredRemovePosture() = countryNames(
    game.nonMuslims.filter(n => n.canRemovePosture && n.isOppositeUsPosture)
  )

  // Returns true if the printed conditions of the event are satisfied
  override
  def eventConditionsMet(role: Role) = plotsMarkers().nonEmpty || removePostureCandidates().nonEmpty

  // Returns true if the Bot associated with the given role will execute the event
  // on its turn.  This implements the special Bot instructions for the event.
  // When the event is triggered as part of the Human players turn, this is NOT used.
  override
  def botWillPlayEvent(role: Role): Boolean = plotsMarkers().nonEmpty || botPreferredRemovePosture().nonEmpty

  // Carry out the event for the given role.
  // forTrigger will be true if the event was triggered during the human player's turn
  // and it associated with the Bot player.
  override
  def executeEvent(role: Role): Unit = {
    if (isHuman(role)) {
      val postureCandidates = countryNames(game.nonMuslims.filter(n => n.canRemovePosture))
      sealed trait Choice
      case object Plot extends Choice
      case object Posture extends Choice
      val choices = List(
        choice(plotsMarkers().nonEmpty,            Plot,    "Place non-WMD plot in any country"),
        choice(removePostureCandidates().nonEmpty, Posture, "Remove posture marker from non-Muslim country")
      ).flatten

      askMenu("Choose one:", choices).head match {
        case Plot =>
          val target = askCountry("Place plot in which country: ", countryNames(game.countries.filter(!_.truce)))
          val plot  = askPlots(plotsMarkers(), 1).head
          addEventTarget(target)
          addAvailablePlotToCountry(target, plot)

        case Posture =>
          val target = askCountry("Remove posture marker from which country: ", removePostureCandidates())
          addEventTarget(target)
          setCountryPosture(target, PostureUntested)
      }
    }
    else {
      // Bot
      if (plotsMarkers().nonEmpty) {
        addEventTarget(UnitedStates)
        addAvailablePlotToCountry(UnitedStates, JihadistBot.preparePlots(plotsMarkers()).head)
      }
      else {
        val target = botPreferredRemovePosture() match {
          case Nil => JihadistBot.posturePriority(removePostureCandidates()).get
          case preferred => JihadistBot.posturePriority(preferred).get
        }
        addEventTarget(target)
        setCountryPosture(target, PostureUntested)
      }
    }
  }
}
