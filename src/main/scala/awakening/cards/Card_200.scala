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
// If US play, in a Fair, 2 or 3 Resource country, place 1 Awakening
// marker or Shift Alignment 1 box towards Ally.
// If Jihadist, in a Poor, 1 or 2 Resource Country, place up to 2 Cells
// (from anywhere) or Shift Alignment 1 box towards Adversary.
// SPECIAL: After playing or discarding place this card face down in
// the approximate middle of the draw pile.
// ------------------------------------------------------------------
object Card_200 extends Card(200, "Critical Middle", Unassociated, 1, NoRemove, NoLapsing, NoAutoTrigger) {
  // Used by the US Bot to determine if the executing the event would alert a plot
  // in the given country
  override
  def eventAlertsPlot(countryName: String, plot: Plot): Boolean = false

  // Used by the US Bot to determine if the executing the event would remove
  // the last cell on the map resulting in victory.
  override
  def eventRemovesLastCell(): Boolean = false

  def isCandidate(role: Role) = role match {
    case US => (m: MuslimCountry) => m.isFair && m.resourceValue > 1
    case Jihadist => (m: MuslimCountry) => m.isPoor && m.resourceValue < 3
  }

  val isUSAlignCandidate = (m: MuslimCountry) => isCandidate(US)(m) && !m.isAlly
  val isUSAwakeningCandidate = (m: MuslimCountry) => isCandidate(US)(m) && m.canTakeAwakeningOrReactionMarker
  val isJihadistAlignCandidate = (m: MuslimCountry) => isCandidate(Jihadist)(m) && !m.isAdversary

  def getCandidates(role: Role) = countryNames(game.muslims.filter(isCandidate(role)))

  def getUSAlignCandidates() = countryNames(game.muslims.filter(isUSAlignCandidate))

  def getUSAwakeningCandidates() = countryNames(game.muslims.filter(isUSAwakeningCandidate))

  def getJihadistAlignCandidates() = countryNames(game.muslims.filter(isJihadistAlignCandidate))

  // Returns true if the printed conditions of the event are satisfied
  override
  def eventConditionsMet(role: Role) = getCandidates(role).nonEmpty

  // Returns true if the Bot associated with the given role will execute the event
  // on its turn.  This implements the special Bot instructions for the event.
  // When the event is triggered as part of the Human players turn, this is NOT used.
  override
  def botWillPlayEvent(role: Role): Boolean = role match {
    case US => getUSAlignCandidates().nonEmpty || getUSAwakeningCandidates().nonEmpty
    case Jihadist => true
  }

  // Carry out the event for the given role.
  // forTrigger will be true if the event was triggered during the human player's turn
  // and it associated with the Bot player.
  override
  def executeEvent(role: Role): Unit = {
    // See Event Instructions table
    val (target, action, from) = role match {
      case US if isHuman(role) =>
        val target = askCountry("Select country: ", getCandidates(US))
        val m = game.getMuslim(target)
        val choices = List(
          choice(m.canTakeAwakeningOrReactionMarker, "awakening", "Place an awakening marker"),
          choice(!m.isAlly,                          "shiftLeft", "Shift alignment towards Ally")
        ).flatten
        val action = if (choices.isEmpty)
          None
        else
          askMenu("Choose one:", choices).headOption
        (target, action, Nil)

      case Jihadist if isHuman(role) =>
        val target = askCountry("Select country: ", getCandidates(Jihadist))
        val m = game.getMuslim(target)
        val choices = List(
          choice(game.cellsAvailable > 0, "cells",      "Place cells"),
          choice(!m.isAdversary,          "shiftRight", "Shift alignment towards Adversary")
        ).flatten
        val action = if (choices.isEmpty)
          None
        else
          askMenu("Choose one:", choices).headOption
        val from = action match {
          case Some("cells") =>
            val sources = countryNames(game.countries.filter(c => c.name != target && c.cells > 0))
            askCellsFromAnywhere(2, trackOK = true, sources, sleeperFocus = false)
          case _ => Nil
        }
        (target, action, from)

      case US => // Bot
        val alignCandidates = USBot.criticalMiddleShiftPossibilities(getUSAlignCandidates())
        val awakeCandidates = getUSAlignCandidates()
        (alignCandidates, awakeCandidates) match {
          case (Nil, Nil) =>
            // Unlikely that there are no spaces to act on, but could happen
            // when event triggered during Jihadist player's turn
            val target = USBot.markerAlignGovTarget(getCandidates(US)).get
            (target, None, Nil)

          case (Nil, candidates) =>
            (USBot.markerAlignGovTarget(candidates).get, Some("awakening"), Nil)

          case (candidates, _)  =>
            (USBot.markerAlignGovTarget(candidates).get, Some("shiftLeft"), Nil)
        }

      case Jihadist => // Bot
        getJihadistAlignCandidates() match {
          case Nil =>
            val target = JihadistBot.travelToTarget(getCandidates(Jihadist)).get
            val isFromCandidate = (c: Country) => c.name != target && JihadistBot.hasCellForTravel(c)
            val fromCandidates = countryNames(game.countries.filter(isFromCandidate))
            val from = JihadistBot.selecCellsToPlace(target, fromCandidates, 2)
            (target, Some("cells"), from)

          case candidates =>
            (JihadistBot.alignGovTarget(candidates).get, Some("shiftRight"), Nil)
        }
    }

    addEventTarget(target)
    val m = game.getMuslim(target)
    action match {
      case None =>
        log(s"\nThe event has no effect in $target.", Color.Event)
      case Some("awakening") =>
        addAwakeningMarker(target)
      case Some("shiftLeft") =>
        shiftAlignmentLeft(target)
      case Some("shiftRight") =>
        shiftAlignmentRight(target)
      case _ =>
        moveCellsToTarget(target, from)
    }
  }
}
