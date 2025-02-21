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
// has the opposite kind of marker.
// ------------------------------------------------------------------
object Card_206 extends Card(206, "Friday of Anger", Unassociated, 1, NoRemove, NoLapsing, NoAutoTrigger) {
  // Used by the US Bot to determine if the executing the event would alert a plot
  // in the given country
  override
  def eventAlertsPlot(countryName: String, plot: Plot): Boolean = false

  // Used by the US Bot to determine if the executing the event would remove
  // the last cell on the map resulting in victory.
  override
  def eventRemovesLastCell(): Boolean = false

  def getAwakeCandidates = if (lapsingEventInPlay(ArabWinter))
    Nil
  else
    countryNames(game.muslims.filter(_.reaction > 0))

  def getReactCandidates = if (lapsingEventInPlay(ArabWinter))
    Nil
  else
    countryNames(game.muslims.filter(_.awakening > 0))

  def getCandidates = if (lapsingEventInPlay(ArabWinter))
    Nil
  else
    countryNames(game.muslims.filter(_.canTakeAwakeningOrReactionMarker))

  // Returns true if the printed conditions of the event are satisfied
  override
  def eventConditionsMet(role: Role) = getAwakeCandidates.nonEmpty || getReactCandidates.nonEmpty

  // Returns true if the Bot associated with the given role will execute the event
  // on its turn.  This implements the special Bot instructions for the event.
  // When the event is triggered as part of the Human players turn, this is NOT used.
  override
  def botWillPlayEvent(role: Role): Boolean = role match {
    case US => getAwakeCandidates.nonEmpty
    case Jihadist => getReactCandidates.nonEmpty
  }

  // Carry out the event for the given role.
  // forTrigger will be true if the event was triggered during the human player's turn
  // and it associated with the Bot player.
  override
  def executeEvent(role: Role): Unit = {
    sealed trait Choice
    case object Awakening extends Choice
    case object Reaction extends Choice
    val action = role match {
      case _ if isHuman(role) =>
        val choices = List(
          choice(getAwakeCandidates.nonEmpty, Awakening, "Place awakening marker"),
          choice(getReactCandidates.nonEmpty, Reaction, "Place reaction marker"),
        ).flatten
        val orderedChoices = if (role == US)
          choices
        else
          choices.reverse
        askMenu("Choose one:", orderedChoices).head

      case US => Awakening
      case Jihadist => Reaction
    }


    val (target, placmentAction) = action match {
      case Awakening if isHuman(role) =>
        (askCountry("Place awakening marker in which country? ", getAwakeCandidates), addAwakeningMarker _)
      case Reaction if isHuman(role) =>
        (askCountry("Place reaction marker in which country? ", getReactCandidates), addReactionMarker _)
      case Awakening =>
        (USBot.markerAlignGovTarget(getAwakeCandidates).get, addAwakeningMarker _)
      case Reaction =>
        (JihadistBot.markerTarget(getReactCandidates).get, addReactionMarker _)
    }

    addEventTarget(target)
    placmentAction(target, 1)
  }
}
