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
// Activate 1 Cell and remove up to 2 other Cells in a single Muslim,
// non-Islamist Rule country.
// -1 Funding.
// ------------------------------------------------------------------
object Card_136 extends Card(136, "Factional Infighting", US, 2, NoRemove, NoLapsing, NoAutoTrigger) {
  // Used by the US Bot to determine if the executing the event would alert a plot
  // in the given country
  override
  def eventAlertsPlot(countryName: String, plot: Plot): Boolean = false

  def getCandidates = countryNames(
    game.muslims.filter(m => !m.truce && !m.isIslamistRule && m.totalCells > 0)
  )

  // Used by the US Bot to determine if the executing the event would remove
  // the last cell on the map resulting in victory.
  override
  def eventRemovesLastCell(): Boolean =
    // Must activate a sleeper if present, then remove any two others
    // so only successful if there is no sleeper to flip
    getCandidates match {
      case name::Nil =>
        game.getMuslim(name).sleeperCells == 0 && USBot.wouldRemoveLastCell(name, 2)
      case _ => false
    }

  // Returns true if the printed conditions of the event are satisfied
  override
  def eventConditionsMet(role: Role) = true

  // Returns true if the Bot associated with the given role will execute the event
  // on its turn.  This implements the special Bot instructions for the event.
  // When the event is triggered as part of the Human players turn, this is NOT used.
  override
  def botWillPlayEvent(role: Role): Boolean =
    getCandidates.nonEmpty || game.funding > 1

  // Carry out the event for the given role.
  // forTrigger will be true if the event was triggered during the human player's turn
  // and it associated with the Bot player.
  override
  def executeEvent(role: Role): Unit = {
    val candidates = getCandidates
    if (candidates.isEmpty && game.funding == 1)
      log("\nThe event has no effect.", Color.Event)
    else {
      if (candidates.nonEmpty) {
        val target = if (isHuman(role))
          askCountry("Select a country: ", candidates)
        else
          USBot.disruptPriority(USBot.highestCellsMinusTandM(candidates)).get

        val m = game.getMuslim(target)
        val hasSleeper = m.sleeperCells > 0
        val otherSleepers = (m.sleeperCells - 1) max 0
        val (actives, sleepers, sadr) = (m.activeCells, otherSleepers, m.hasSadr) match {
          case (0, 0, true)  => (0, 0, true)
          case (0, s, true)  => (0, 1, true)
          case (a, _, true)  => (1, 0, true)
          case (a, s, false) =>
            val sleepers = s min 2
            val actives  = a min (2 - sleepers)
            (actives, sleepers, false)
        }
        println()
        addEventTarget(target)
        if (hasSleeper)
          flipSleeperCells(target, 1)
        removeCellsFromCountry(target, actives, sleepers, sadr, addCadre = true)
      }

      // Also, decrease funding by 1 even if no cells affected.
      decreaseFunding(1)
    }
  }
}
