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
// Play if Caliphate Capital on Map.
// If US play: +1 Prestige, -1 Funding  REMOVE
// If Jihadist: Place 2 Cells in a Muslim country (may come from anywhere),
// then conduct a Jihad there with this card's Operations value.
// Ignore failures. Ignore any Shifts to Islamist Rule.
// ------------------------------------------------------------------
object Card_342 extends Card(342, "Gulmurod Khalimov", Unassociated, 2, USRemove, NoLapsing, NoAutoTrigger) {
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
  def eventConditionsMet(role: Role) = game.caliphateDeclared

  def cellSources(target: String) = countryNames(
    game.countries.filter(c => !c.truce && c.name != target && c.pieces.totalCells > 0)
  )

  def jihadistCandidates = countryNames(game.muslims.filter(!_.truce))

  // Cell placement: Priority to Good (priority to highest res*, then without Troops),
  // then Fair with a-r<2 (priority to highest res*, then best r-a),
  // then poor with Aid (priority to highest res*).
  // Else unplayable.
  def isEnhBotCandidate = (m: MuslimCountry) =>
    !m.truce && (
      m.isGood ||
      (m.isFair && m.awakening - m.reaction < 2) ||
      (m.isPoor && m.aidMarkers > 0)
    )

  // Returns true if the Bot associated with the given role will execute the event
  // on its turn.  This implements the special Bot instructions for the event.
  // When the event is triggered as part of the Human players turn, this is NOT used.
  override
  def botWillPlayEvent(role: Role): Boolean = role match {
    case US =>
      game.prestige < MaxPrestige || game.funding > 1
    case Jihadist if game.botEnhancements =>
      game.muslims.exists(isEnhBotCandidate)
    case Jihadist =>
      jihadistCandidates.nonEmpty
  }

  // Carry out the event for the given role.
  // forTrigger will be true if the event was triggered during the human player's turn
  // and it associated with the Bot player.
  override
  def executeEvent(role: Role): Unit = {
    if (role == US) {
      increasePrestige(1)
      decreaseFunding(1)
    }
    else if (game.jihadTargets.isEmpty)
      log("\nThere are no Muslim countries where Jihad may be performed.", Color.Event)
    else { // Jihadist
      val candidates = jihadistCandidates
      val (target, cells) = if (isHuman(role)) {
        val name = askCountry("Which country: ", candidates)
        val cells = askCellsFromAnywhere(2, true, cellSources(name), sleeperFocus = false)
        (name, cells)
      }
      else if (game.botEnhancements) {
        // Enhanced Jihad Bot
        // Cell placement: Priority to Good (priority to highest res*, then without Troops),
        // then Fair with a-r<2 (priority to highest res*, then best r-a),
        // then poor with Aid (priority to highest res*).
        // Else unplayable.
        val goodCandidates = game.muslims.filter(m => m.isGood && isEnhBotCandidate(m))
        val fairCandidates = game.muslims.filter(m => m.isFair && isEnhBotCandidate(m))
        val poorCandidates = game.muslims.filter(m => m.isPoor && isEnhBotCandidate(m))
        val goodPriorities = List(JihadistBot.HighestPrintedResourcePriority, JihadistBot.NoTroopsFilter)
        val fairPriorities = List(JihadistBot.HighestPrintedResourcePriority, JihadistBot.HighestReactionMinusAwakeningPriority)
        val poorPriorities = List(JihadistBot.HighestPrintedResourcePriority)
        val target = if (goodCandidates.nonEmpty)
          JihadistBot.topPriority(goodCandidates, goodPriorities).map(_.name).get
        else if (fairCandidates.nonEmpty)
          JihadistBot.topPriority(fairCandidates, fairPriorities).map(_.name).get
        else
          JihadistBot.topPriority(poorCandidates, poorPriorities).map(_.name).get
          
        (target, JihadistBot.selecCellsToPlace(target, cellSources(target), 2))
      }
      else {
        // Standard Jihad Bot
        val target = JihadistBot.cellPlacementPriority(false)(candidates).get
        (target, JihadistBot.selecCellsToPlace(target, cellSources(target), 2))
      }

      addEventTarget(target)
      moveCellsToTarget(target, cells)

      val m = game.getMuslim(target)
      if (m.jihadOK)
        performJihads(JihadTarget(target, false, Pieces(), false, phantoms = 2, ignoreFailures = true)::Nil)
    }
  }
}
