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
// Remove up to 3 Cells total from any countries in Civil War or Regime Change,
// OR Remove 4 Cells total from Caliphate Countries.
// ------------------------------------------------------------------
object Card_269 extends Card(269, "Air America", US, 3, NoRemove, NoLapsing, NoAutoTrigger) {
  // Used by the US Bot to determine if the executing the event would alert a plot
  // in the given country
  override
  def eventAlertsPlot(countryName: String, plot: Plot): Boolean = false

  def getNonCaliphateCandidates = countryNames(
    game.muslims.filter(m => !m.truce && m.totalCells > 0 && (m.civilWar || m.inRegimeChange))
  )

  def getCaliphateCandidates = countryNames(
    game.muslims.filter(m => !m.truce && m.totalCells > 0 && game.isCaliphateMember(m.name))
  )

  // Used by the US Bot to determine if the executing the event would remove
  // the last cell on the map resulting in victory.
  override
  def eventRemovesLastCell(): Boolean = {
    val nonCalNum = getNonCaliphateCandidates.map(name => game.getCountry(name).totalCells).sum min 3
    val calNum    = getCaliphateCandidates.map(name => game.getCountry(name).totalCells).sum min 4

    nonCalNum == game.totalCellsOnMap || calNum == game.totalCellsOnMap
  }

  // Returns true if the printed conditions of the event are satisfied
  override
  def eventConditionsMet(role: Role) =
    getNonCaliphateCandidates.nonEmpty ||
    getCaliphateCandidates.nonEmpty

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
    //  Allows US player to remove up to 4 cells from any caliphate members
    //  or up to 3 cells from any Civil War/Regime Change countries
    if (isHuman(role)) {
      sealed trait Choice
      case object Caliphate extends Choice
      case object NonCaliphate extends Choice
      val choices = List(
        choice(
          getNonCaliphateCandidates.nonEmpty,
          NonCaliphate,
          "Remove up to 3 cells in Civil War/Regime Change countries"),
        choice(
          getCaliphateCandidates.nonEmpty,
          Caliphate,
          "Remove 4 cells total in Caliphate countries")
      ).flatten

      val (candidates, maxCells, upto) = askMenu("Choose one:", choices).head match {
        case NonCaliphate => (getNonCaliphateCandidates, 3, true)
        case Caliphate    => (getCaliphateCandidates, 4, false)
      }

      println()
      val removed = askToRemoveCells(maxCells, upto, candidates, sleeperFocus = true)
      for (CellsToRemove(name, cells, sadr) <- removed) {
        addEventTarget(name)
        removeCellsFromCountry(name, cells, sadr, addCadre = true)
      }
    }
    else {
      // Bot will remove cells from caliphate countries only if it can remove
      // four cells (or there are no Civil War/Regime change countries)
      // Otherwise it will remove the max it can from Civil War/Regime change countries
      val nonCalNum = getNonCaliphateCandidates.map(name => game.getCountry(name).totalCells).sum min 3
      val calNum    = getCaliphateCandidates.map(name => game.getCountry(name).totalCells).sum min 4


      val (candidates, maxCells) = if (calNum == 4 || calNum == game.totalCellsOnMap || nonCalNum == 0)
        (getCaliphateCandidates, calNum)
      else
        (getNonCaliphateCandidates, nonCalNum)

      // We will select the cells one at a time, because
      // removal of a cell could change the Bots priorities
      def nextRemoval(remaining: Int): Unit = {
        val withCells = candidates.filter(name => game.getMuslim(name).totalCells > 0)
        if (remaining > 0 && withCells.nonEmpty) {
          val target = USBot.disruptPriority(withCells).get
          val (cell, sadr) = USBot.chooseCellsToRemove(target, 1)

          addEventTarget(target)
          removeCellsFromCountry(target, cell, sadr, addCadre = true)
          nextRemoval(remaining - 1)
        }
      }

      nextRemoval(maxCells)
    }
  }
}
