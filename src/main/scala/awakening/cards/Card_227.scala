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
// Place 1 Awakening or 1 Reaction marker in any country which already
// has one of the same type, and then another in an Adjacent country.
// ------------------------------------------------------------------
object Card_227 extends Card(227, "Popular Support", Unassociated, 2, NoRemove, NoLapsing, NoAutoTrigger) {
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
  def eventConditionsMet(role: Role) =
    lapsingEventNotInPlay(ArabWinter) &&
    (getAwakeningCandidates().nonEmpty || getReactionCandidates().nonEmpty)

  // Returns true if the Bot associated with the given role will execute the event
  // on its turn.  This implements the special Bot instructions for the event.
  // When the event is triggered as part of the Human players turn, this is NOT used.
  override
  def botWillPlayEvent(role: Role): Boolean = role match {
    case US => getAwakeningCandidates().nonEmpty
    case Jihadist => getReactionCandidates().nonEmpty
  }

  def getAdjCandidates(name: String) = countryNames(
    game.adjacentMuslims(name).filter(_.canTakeAwakeningOrReactionMarker)
  )
  // Carry out the event for the given role.
  // forTrigger will be true if the event was triggered during the human player's turn
  // and it associated with the Bot player.
  override
  def executeEvent(role: Role, forTrigger: Boolean): Unit = {
    val choices = List(
      choice(getAwakeningCandidates().nonEmpty, "awakening", "Add awakening markers"),
      choice(getReactionCandidates().nonEmpty, "reaction", "Add reaction markers"),
    ).flatten
    val orderedChoices = if (role == US) choices else choices.reverse
    val (target, adjacent, placmentAction) = role match {
      case _ if isHuman(role) =>
        val markerType = askMenu("Choose one:", orderedChoices).head
        val (candidates, function) = if (markerType == "awakening")
          (getAwakeningCandidates(), addAwakeningMarker _)
        else
          (getReactionCandidates(), addReactionMarker _)
        val name = askCountry(s"Select country with $markerType marker: ", candidates)
        val adjacent = getAdjCandidates(name) match {
          case Nil =>
            displayLine(s"\nNo countries adjacent to $name can take $markerType markers.", Color.Event)
            None
          case candidates =>
            Some(askCountry("Select an adjacent Muslim country: ", candidates))
        }
        (name, adjacent, function)

      case US =>
        val name = USBot.markerAlignGovTarget(getAwakeningCandidates()).get
        val adjacent = USBot.markerAlignGovTarget(getAdjCandidates(name))
        (name, adjacent, addAwakeningMarker _)

      case Jihadist =>
        val name = JihadistBot.markerTarget(getReactionCandidates()).get
        val adjacent = JihadistBot.markerTarget(getAdjCandidates(name))
        (name, adjacent, addReactionMarker _)
    }

    addEventTarget(target)
    placmentAction(target, 1)
    adjacent.foreach { adj =>
      addEventTarget(adj)
      placmentAction(adj, 1)
    }
  }
}
