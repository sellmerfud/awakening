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
import awakening.USBot

// Card Text:
// ------------------------------------------------------------------
// Play if Iran is not a Special Case country.
// Remove a total of up to 3 Cells from any Sunni or Shia Mix countries
// that are adjacent to the other type.
// REMOVE
// ------------------------------------------------------------------
object Card_280 extends Card(280, "Sunni-Shia Rift", US, 3, Remove, NoLapsing, NoAutoTrigger) {
  // Used by the US Bot to determine if the executing the event would alert a plot
  // in the given country
  override
  def eventAlertsPlot(countryName: String, plot: Plot): Boolean = false

  //  Candidates are any Muslim country with cells that is adjacent
  //  to another Muslim country of the opposite type (Sunni/Shia Mix)
  def getCandidates = {
    val adjOther = (m: MuslimCountry) =>
      (m.isSunni && game.adjacentToShiaMix(m.name)) || (m.isShiaMix && game.adjacentToSunni(m.name))
    countryNames(game.muslims.filter(m => !m.truce && m.totalCells > 0 && adjOther(m)))
  }

  def maxCellsToRemove = game.getMuslims(getCandidates).map(_.totalCells).sum min 3
  // Used by the US Bot to determine if the executing the event would remove
  // the last cell on the map resulting in victory.
  override
  def eventRemovesLastCell(): Boolean = maxCellsToRemove == game.totalCellsOnMap


  // Returns true if the printed conditions of the event are satisfied
  override
  def eventConditionsMet(role: Role) = !isIranSpecialCase
  // Returns true if the Bot associated with the given role will execute the event
  // on its turn.  This implements the special Bot instructions for the event.
  // When the event is triggered as part of the Human players turn, this is NOT used.
  override
  def botWillPlayEvent(role: Role): Boolean = getCandidates.nonEmpty

  // Carry out the event for the given role.
  // forTrigger will be true if the event was triggered during the human player's turn
  // and it associated with the Bot player.
  override
  def executeEvent(role: Role): Unit = {
    if (isHuman(role)) {
      println()
      val removed = askToRemoveCells(maxCellsToRemove, true, getCandidates, sleeperFocus = true)
      for (CellsToRemove(name, cells, sadr) <- removed) {
        addEventTarget(name)
        removeCellsFromCountry(name, cells, sadr, addCadre = true)
      }
    }
    else {
      // Bot
      // We will select the cells one at a time, because
      // removal of a cell could change the Bot's priorities
      def nextRemoval(remaining: Int): Unit = {
        val withCells = getCandidates.filter(name => game.getMuslim(name).totalCells > 0)
        if (remaining > 0 && withCells.nonEmpty) {
          val target = USBot.disruptPriority(withCells).get
          val (cell, sadr) = USBot.chooseCellsToRemove(target, 1)

          addEventTarget(target)
          removeCellsFromCountry(target, cell, sadr, addCadre = true)
          nextRemoval(remaining - 1)
        }
      }

      nextRemoval(maxCellsToRemove)
    }
  }
}
