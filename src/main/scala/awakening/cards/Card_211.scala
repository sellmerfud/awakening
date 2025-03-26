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
// Play if a country was subject to an Event or Operations in this or
// last Action Phase.
// Place 1 Awakening or 1 Reaction marker in one of those countries.
// Allows play of Facebook.
// ------------------------------------------------------------------
object Card_211 extends Card(211, "Smartphones", Unassociated, 1, NoRemove, NoLapsing, NoAutoTrigger) {
  // Used by the US Bot to determine if the executing the event would alert a plot
  // in the given country
  override
  def eventAlertsPlot(countryName: String, plot: Plot): Boolean = false

  // Used by the US Bot to determine if the executing the event would remove
  // the last cell on the map resulting in victory.
  override
  def eventRemovesLastCell(): Boolean = false

  val isPlacementCandidate = (m: MuslimCountry) =>
    !m.truce &&
    m.canTakeAwakeningOrReactionMarker &&
    (game.targetsThisPhase.wasOpsOrEventTarget(m.name) || game.targetsLastPhase.wasOpsOrEventTarget(m.name))

  def getPlacementCandidates = countryNames(game.muslims.filter(isPlacementCandidate))

  // Returns true if the printed conditions of the event are satisfied
  // Always can play to allow facebook (event if smartphones is already in effect)
  override
  def eventConditionsMet(role: Role) =
    game.targetsThisPhase.ops.nonEmpty ||
    game.targetsThisPhase.event.nonEmpty ||
    game.targetsLastPhase.ops.nonEmpty ||
    game.targetsLastPhase.event.nonEmpty

  // Returns true if the Bot associated with the given role will execute the event
  // on its turn.  This implements the special Bot instructions for the event.
  // When the event is triggered as part of the Human players turn, this is NOT used.
  //
  // US Bot will play if it can place a marker or if the Smartphone marker is not yet in play.
  // Jihad Bot will only play if it can place a marker.
  override
  def botWillPlayEvent(role: Role): Boolean = role match {
    case US =>
      globalEventNotInPlay(Smartphones) || lapsingEventNotInPlay(ArabWinter)
    case Jihadist if game.botEnhancements =>
      globalEventInPlay(Smartphones) && lapsingEventNotInPlay(ArabWinter)
    case Jihadist =>
      lapsingEventNotInPlay(ArabWinter)
  }

  // Carry out the event for the given role.
  // forTrigger will be true if the event was triggered during the human player's turn
  // and it associated with the Bot player.
  override
  def executeEvent(role: Role): Unit = {
    if (lapsingEventInPlay(ArabWinter))
      log(s"\nCannot place awakening/reaction markers. [Arab Winter]", Color.Event)
    else if (getPlacementCandidates.isEmpty)
      log(s"\nNone of the candidate countries can take awakening/reactions markers.", Color.Event)
    else {
      val (placementAction, target) = role match {
        case _ if isHuman(role) =>
          val choices = List(
            addAwakeningMarker _ -> "Place awakening marker",
            addReactionMarker _ -> "Place reaction marker")
          val orderedChoices = if (role == US) choices else choices.reverse
          (askMenu("Choose one:", orderedChoices).head, askSimpleMenu("Select country:", getPlacementCandidates))
        case US =>
          (addAwakeningMarker _, USBot.markerAlignGovTarget(getPlacementCandidates).get)
        case Jihadist =>
          (addReactionMarker _, JihadistBot.markerTarget(getPlacementCandidates).get)
      }

      addEventTarget(target)
      if (role == Jihadist)
        placementAction(target, 1)
      else
        placementAction(target, 1)
    }

    removeGlobalEventMarker(Censorship)
    addGlobalEventMarker(Smartphones)
  }
}
