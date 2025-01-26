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
// Play if either Iraq or Syria in Civil War. Blocked by Early Exit.
// Place 1 Militia and 1 Advisors in either Iraq or Syria.
// Then, remove up to a total of 3 Cells from these two countries.
// ------------------------------------------------------------------
object Card_275 extends Card(275, "Operation Inherent Resolve", US, 3, NoRemove, NoLapsing, NoAutoTrigger) {
  // Used by the US Bot to determine if the executing the event would alert a plot
  // in the given country
  override
  def eventAlertsPlot(countryName: String, plot: Plot): Boolean = false

  val IraqSyria = List(Iraq, Syria)

  // Used by the US Bot to determine if the executing the event would remove
  // the last cell on the map resulting in victory.
  override
  def eventRemovesLastCell(): Boolean =
    ((game.getMuslims(IraqSyria).map(_.totalCells)).sum min 3) match {
      case n => (n == game.totalCellsOnMap )
    }


  override
  def eventConditionsMet(role: Role) =
    game.getMuslims(IraqSyria).exists(_.civilWar) &&
    globalEventNotInPlay(EarlyExit)

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
    val target = if (isHuman(role))
      askCountry("Place militia and Advisors in which country: ", IraqSyria)
    else
      USBot.deployToPriority(IraqSyria).get

    addEventTarget(target)
    if (game.militiaAvailable > 0)
      addMilitiaToCountry(target, 1)
    else
      log(s"\nThere are no available militia to add to $target.", Color.Event)

    if (game.advisorsAvailable > 0)
      addAdvisorsToCountry(target)
    else
      log(s"\nAll three Advisors are already on the map.", Color.Event)

    val removeCandidates = countryNames(game.getMuslims(IraqSyria).filter(_.totalCells > 0))

    if (removeCandidates.isEmpty)
      log("\nThere are no cells to remove in either Iraq or Syria.", Color.Event)
    else if (isHuman(role)) {
      val removed = askToRemoveCells(3, true, removeCandidates, sleeperFocus = true)
      for (CellsToRemove(name, (actives, sleepers, sadr)) <- removed) {
        addEventTarget(name)
        removeCellsFromCountry(name, actives, sleepers, sadr, addCadre = true)
      }
    }
    else {
      // Bot
      // We will select the cells one at a time, because
      // removal of a cell could change the Bots priorities
     def nextRemoval(remaining: Int): Unit = {
        val withCells = removeCandidates.filter(name => game.getMuslim(name).totalCells > 0)
        if (remaining > 0 && withCells.nonEmpty) {
          val target = USBot.disruptPriority(withCells).get
          val (actives, sleepers, sadr) = USBot.chooseCellsToRemove(target, 1)

          addEventTarget(target)
          removeCellsFromCountry(target, actives, sleepers, sadr, addCadre = true)
          nextRemoval(remaining - 1)
        }
      }

      nextRemoval(3)
    }
  }
}
