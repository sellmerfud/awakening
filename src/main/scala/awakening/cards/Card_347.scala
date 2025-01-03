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
// Play in a Civil War country with both Cells and Militia where you
// have more Troops + Militia or Cells than your opponent.
// Replace a Cell with a Militia or vice versa.
// ------------------------------------------------------------------
object Card_347 extends Card(347, "Switching Jerseys", Unassociated, 2, NoRemove, NoLapsing, NoAutoTrigger) {
  // Used by the US Bot to determine if the executing the event would alert a plot
  // in the given country
  override
  def eventAlertsPlot(countryName: String, plot: Plot): Boolean = false

  val isCandidate = (m: MuslimCountry) =>
    m.civilWar && m.totalCells > 0 && m.militia > 0

  val isUSCandidate = (m: MuslimCountry) =>
    isCandidate(m) && m.totalTroopsAndMilitia > m.totalCells

  val isJihadistCandidate = (m: MuslimCountry) =>
    isCandidate(m) && m.totalCells > m.totalTroopsAndMilitia

  def getUSCandidates = countryNames(game.muslims.filter(isUSCandidate))

  def getJihadistCandidates = countryNames(game.muslims.filter(isJihadistCandidate))

  // Used by the US Bot to determine if the executing the event would remove
  // the last cell on the map resulting in victory.
  override
  def eventRemovesLastCell(): Boolean =
    getUSCandidates.exists(name => USBot.wouldRemoveLastCell(name, 1))

  // Returns true if the printed conditions of the event are satisfied
  override
  def eventConditionsMet(role: Role) = role match {
    case US => getUSCandidates.nonEmpty && game.militiaAvailable > 0
    case Jihadist => getJihadistCandidates.nonEmpty && game.cellsAvailable > 0
  }

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
    if (role == US) {
      val (target, (actives, sleepers, sadr)) = if (isHuman(role)) {
        val name = askCountry("Which country: ", getUSCandidates)
        (name, askCells(name, 1, true))
      }
      else {
        val name = USBot.deployToPriority(getUSCandidates).get
        (name, USBot.chooseCellsToRemove(name, 1))
      }

      addEventTarget(target)
      removeCellsFromCountry(target, actives, sleepers, sadr, addCadre = true)
      addMilitiaToCountry(target, 1)
    }
    else { // Jihadist
      val target = if (isHuman(role))
        askCountry("Which country: ", getJihadistCandidates)
      else
        JihadistBot.cellPlacementPriority(false)(getJihadistCandidates).get

      addEventTarget(target)
      removeMilitiaFromCountry(target, 1)
      addSleeperCellsToCountry(target, 1)
    }
  }
}
