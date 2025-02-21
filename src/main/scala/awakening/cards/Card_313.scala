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
// Play if Syria in Civil War.
// Place 3 Cells there.
// May come from adjacent countries if there are insufficient Cells on the Track.
// REMOVE
// ------------------------------------------------------------------
object Card_313 extends Card(313, "Hayat Tahir al-Sham", Jihadist, 3, Remove, NoLapsing, NoAutoTrigger) {
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
  def eventConditionsMet(role: Role) = game.getMuslim(Syria).civilWar

  def numAdjacentCells = game.adjacentCountriesWithCells(Syria)
    .map(_.cells)
    .sum

  // Returns true if the Bot associated with the given role will execute the event
  // on its turn.  This implements the special Bot instructions for the event.
  // When the event is triggered as part of the Human players turn, this is NOT used.
  //
  // The Enhanced Bot will only play the event if it can declare a Caliphate
  // Capital.  This is because if Syria is already in Civil War then it is an
  // auto recruit country so simply placing cells there is not a priority.
  override
  def botWillPlayEvent(role: Role): Boolean = if (game.botEnhancements)
    !game.caliphateDeclared && game.cellsAvailable + numAdjacentCells >= 3
  else
    game.cellsAvailable + numAdjacentCells >= 3

  // Carry out the event for the given role.
  // forTrigger will be true if the event was triggered during the human player's turn
  // and it associated with the Bot player.
  override
  def executeEvent(role: Role): Unit = {
    val trackCells   = 3 min game.cellsAvailable
    val adjWithCells = game.adjacentCountriesWithCells(Syria).map(_.name)
    val mapCells     = ((3 - trackCells) min numAdjacentCells) max 0

    addEventTarget(Syria)
    addSleeperCellsToCountry(Syria, trackCells)

    // If there were not enough cells on the track
    // then we must make up the difference from adjacent
    // countries as much as possible.
    if (mapCells > 0) {
      println()
      val cellItems = if (isHuman(role))
        askCellsFromAnywhere(mapCells, false, adjWithCells, sleeperFocus = false)
      else {
        def nextAdjacent(cellsLeft: Int, candidates: List[String]): List[CellsItem] = {
          if (cellsLeft == 0 || candidates.isEmpty)
            Nil
          else {
            val target = JihadistBot.hayatTahirTarget(candidates).get
            val m = game.getMuslim(target)
            val actives  = cellsLeft min m.activeCells
            val sleepers = (cellsLeft - actives) min m.sleeperCells
            val remain   = cellsLeft - actives - sleepers
            CellsItem(target, actives, sleepers) :: nextAdjacent(remain, candidates.filterNot(_ == target))
          }
        }

        nextAdjacent(mapCells, adjWithCells)
      }

      moveCellsToTarget(Syria, cellItems)
    }

    // Finally see if the caliphate will be declared
    if (jihadistChoosesToDeclareCaliphate(Syria, trackCells + mapCells))
      declareCaliphate(Syria)
  }
}
