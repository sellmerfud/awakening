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
import awakening.{ USBot, JihadistBot }

// Card Text:
// ------------------------------------------------------------------
// Remove 1 Awakening or 1 Reaction marker from any country.
// ------------------------------------------------------------------
object Card_210 extends Card(210, "Sectarian Violence", Unassociated, 1, NoRemove, NoLapsing, NoAutoTrigger) {
  // Used by the US Bot to determine if the executing the event would alert a plot
  // in the given country
  override
  def eventAlertsPlot(countryName: String, plot: Plot): Boolean = false

  // Used by the US Bot to determine if the executing the event would remove
  // the last cell on the map resulting in victory.
  override
  def eventRemovesLastCell(): Boolean = false

  def getAwakeningCandidates() = countryNames(game.muslims.filter(_.awakening > 0))

  def getReactionCandidates() = countryNames(game.muslims.filter(_.reaction > 0))

  // Returns true if the printed conditions of the event are satisfied
  override
  def eventConditionsMet(role: Role) = true

  // Returns true if the Bot associated with the given role will execute the event
  // on its turn.  This implements the special Bot instructions for the event.
  // When the event is triggered as part of the Human players turn, this is NOT used.
  override
  def botWillPlayEvent(role: Role): Boolean = role match {
    case US => getReactionCandidates().nonEmpty
    case Jihadist => getAwakeningCandidates().nonEmpty
  }

  // Carry out the event for the given role.
  // forTrigger will be true if the event was triggered during the human player's turn
  // and it associated with the Bot player.
  override
  def executeEvent(role: Role): Unit = {
    val choices = List(
      "reaction" -> "Remove a reaction marker.",
      "awakening" -> "Remove an awakening marker.",
    )

    val action = role match {
      case _ if isHuman(role) => askMenu("Chosse one:", choices).head
      case US => "reaction"
      case Jihadist => "awakening"
    }

    action match {
      case "reaction" if getReactionCandidates().nonEmpty =>
        val name = if (isHuman(role))
          askCountry("Remove reaction marker from which country: ", getReactionCandidates())
        else
          USBot.markerAlignGovTarget(getReactionCandidates()).get
        addEventTarget(name)
        removeReactionMarker(name)

      case "awakening" if getAwakeningCandidates().nonEmpty =>
        val name = if (isHuman(role))
          askCountry("Remove awakening marker from which country: ", getAwakeningCandidates())
        else
          JihadistBot.markerTarget(getAwakeningCandidates()).get
        addEventTarget(name)
        removeAwakeningMarker(name)

      case "reaction" =>
        log("There are no countries with reaction markers.  The event has no effect.", Color.Event)

      case _ =>
        log("There are no countries with awakening markers.  The event has no effect.", Color.Event)
    }
  }
}
