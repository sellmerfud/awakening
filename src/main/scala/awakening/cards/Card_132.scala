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
import awakening.USBot

// Card Text:
// ------------------------------------------------------------------
// Remove 1 Cell and place up to 2 Militia in any country in Civil War.
// ------------------------------------------------------------------
object Card_132 extends Card(132, "Battle of Sirte", US, 2, NoRemove, NoLapsing, NoAutoTrigger) {
  // Used by the US Bot to determine if the executing the event would alert a plot
  // in the given country
  override
  def eventAlertsPlot(countryName: String, plot: Plot): Boolean = false

  def getCandidates = countryNames(game.muslims.filter(m => !m.truce && m.civilWar))
  def getCandidatesWithCells = countryNames(game.muslims.filter(m => !m.truce && m.civilWar && m.totalCells > 0))

  // Used by the US Bot to determine if the executing the event would remove
  // the last cell on the map resulting in victory.
  override
  def eventRemovesLastCell(): Boolean =
    getCandidatesWithCells.exists(name => USBot.wouldRemoveLastCell(name, 1))

  // Returns true if the printed conditions of the event are satisfied
  override
  def eventConditionsMet(role: Role) = getCandidates.nonEmpty

  // Returns true if the Bot associated with the given role will execute the event
  // on its turn.  This implements the special Bot instructions for the event.
  // When the event is triggered as part of the Human players turn, this is NOT used.
  override
  def botWillPlayEvent(role: Role): Boolean =
    game.militiaAvailable > 0 ||
    getCandidatesWithCells.nonEmpty

  // Carry out the event for the given role.
  // forTrigger will be true if the event was triggered during the human player's turn
  // and it associated with the Bot player.
  override
  def executeEvent(role: Role): Unit = {
    val (target, (cell, sadr)) = if (isHuman(role)) {
      val target = askCountry("Select civil war country: ", getCandidates)
      (target, askCells(target, 1, sleeperFocus = true))
    }
    else {
      // Bot chooses the candidate with the where (totalCells - TandM) is highest.
      val target = if (getCandidatesWithCells.nonEmpty && game.totalCellsOnMap == 1)
        getCandidatesWithCells.head
      else {
        val best = USBot.highestCellsMinusTandM(getCandidates)
        shuffle(best).head
      }
      (target, USBot.chooseCellsToRemove(target, 1))
    }

    println()
    addEventTarget(target)
    removeCellsFromCountry(target, cell, sadr, addCadre = true)
    addMilitiaToCountry(target, game.militiaAvailable min 2)
  }
}
