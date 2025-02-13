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
// Play if you can select 2 Unmarked Muslim countries (non Iran).
// Place a Cell in each, or 2 in each if any Islamist Rule.
// ------------------------------------------------------------------
object Card_193 extends Card(193, "Regional al-Qaeda", Jihadist, 3, NoRemove, NoLapsing, NoAutoTrigger) {
  // Used by the US Bot to determine if the executing the event would alert a plot
  // in the given country
  override
  def eventAlertsPlot(countryName: String, plot: Plot): Boolean = false

  // Used by the US Bot to determine if the executing the event would remove
  // the last cell on the map resulting in victory.
  override
  def eventRemovesLastCell(): Boolean = false

  def getCandidates = countryNames(game.muslims.filter(m => m.name != Iran && m.isUntested))

  // Returns true if the printed conditions of the event are satisfied
  override
  def eventConditionsMet(role: Role) = getCandidates.nonEmpty

  // Returns true if the Bot associated with the given role will execute the event
  // on its turn.  This implements the special Bot instructions for the event.
  // When the event is triggered as part of the Human players turn, this is NOT used.
  override
  def botWillPlayEvent(role: Role): Boolean = game.cellsAvailable > 0

  // Carry out the event for the given role.
  // forTrigger will be true if the event was triggered during the human player's turn
  // and it associated with the Bot player.
  override
  def executeEvent(role: Role): Unit = {
    val maxPerTarget = if (game.numIslamistRule > 0) 2 else 1
    val maxTargets   = 2 min getCandidates.size
    case class Target(name: String, cells: Int)

    val targets = if (isHuman(role)) {
      def nextTarget(
        available: Int,
        candidates: List[String],
        existingTargets: Vector[Target]): Vector[Target] = {
        if (existingTargets.size == maxTargets)
          existingTargets
        else {
          val num = existingTargets.size + 1
          val name = askCountry(s"\nSelect ${ordinal(num)} unmarked Muslim country: ", candidates)
          val target = if (available == 0)
            Target(name, 0)
          else if (num == maxTargets)
            Target(name, maxPerTarget min available)
          else if (available >= maxTargets * maxPerTarget)
            Target(name, maxPerTarget)
          else {
              displayLine(s"\nThere are ${amountOf(available, "available cell")}", Color.Info)
              Target(name, askInt(s"Place how many cells in $name", 1, 2))
          }

          nextTarget(
            available - target.cells,
            candidates.filterNot(_ == name),
            existingTargets :+ target)
        }
      }

      nextTarget(game.cellsAvailable, getCandidates, Vector.empty)
    }
    else { // Bot
      def nextTarget(
        available: Int,
        candidates: List[String],
        existingTargets: Vector[Target]): Vector[Target] = {
        if (existingTargets.size == maxTargets)
          existingTargets
        else {
          var name = JihadistBot.cellPlacementPriority(false)(candidates).get
          val numCells = maxPerTarget min available

          nextTarget(
            available - numCells,
            candidates.filterNot(_ == name),
            existingTargets :+ Target(name, numCells))
        }
      }

      nextTarget(game.cellsAvailable, getCandidates, Vector.empty)
    }

    for (Target(name, num) <- targets) {
      addEventTarget(name)
      if (num > 0)
        addSleeperCellsToCountry(name, num)
      else
        log(s"\nNo cells available to place in $name.", Color.Event)
    }
  }
}
