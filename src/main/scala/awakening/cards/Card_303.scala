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
// Play if US Hard and Iran has an unavailable WMD and a Cell.
// Roll a die.
// Success (1-3): Add a WMD to Available Plots. If Iran is a Special
// Case country, flip Iran country mat to Shia-Mix.
// Set to Fair Adversary. REMOVE
// Failure (4-6): Remove the Cell.
// ------------------------------------------------------------------
object Card_303 extends Card(303, "Iranian Withdrawal", Jihadist, 2, NoRemove, NoLapsing, NoAutoTrigger) {
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
  def eventConditionsMet(role: Role) = {
    val iran = game.getCountry(Iran)
    !iran.truce && game.usPosture == Hard && iran.wmdCache > 0 && iran.totalCells > 0
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
    addEventTarget(Iran)
    val die = getDieRoll(s"Enter event die roll: ")
    if (die < 4) {
      log(s"\nDie roll: $die - Add WMD to available plots", Color.Event)
      moveWMDCacheToAvailable(Iran, 1)
      if (isIranSpecialCase) {
        log("\nFlip Iran country mat to its Shia-Mix Muslim side and set it to Fair Adversary.", Color.Event)
        val iran = game.getNonMuslim(Iran)
        game = game.updateCountry(DefaultMuslimIran.copy(
          pieces       = iran.pieces,
          cadres       = iran.cadres,
          plots        = iran.plots,
          markers      = iran.markers,
          wmdCache     = iran.wmdCache
        ))
      }
      // Card only removed if die roll was successful
      removeCardFromGame(this.number)
    }
    else {
      log(s"\nDie roll: $die - Remove a cell", Color.Event)
      val (cells, sadr) = if (isHuman(role))
        askCells(Iran, 1, sleeperFocus = false)
      else
          JihadistBot.chooseCellsToRemove(Iran, 1)

      removeCellsFromCountry(Iran, cells, sadr, addCadre = true)
    }
  }
}
