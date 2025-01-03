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
// Select 3 countries without Cells.
// Test them. Recruit once in each, ignoring Funding.
// Place Cadre in each country that does not receive a Cell.
// ------------------------------------------------------------------
object Card_189 extends Card(189, "Jihadist Videos", Jihadist, 3, NoRemove, NoLapsing, NoAutoTrigger) {
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
  def eventConditionsMet(role: Role) = true

  // Returns true if the Bot associated with the given role will execute the event
  // on its turn.  This implements the special Bot instructions for the event.
  // When the event is triggered as part of the Human players turn, this is NOT used.
  override
  def botWillPlayEvent(role: Role): Boolean = game.cellsAvailable > 0 // Ignores funding so all cells can be used

  // Carry out the event for the given role.
  // forTrigger will be true if the event was triggered during the human player's turn
  // and it associated with the Bot player.
  override
  def executeEvent(role: Role, forTrigger: Boolean): Unit = {
    var targets = if (isHuman(role))
      askCountries(3, countryNames(game.countries.filter(_.totalCells == 0)))
    else {
      // See Event Instructions table
      val candidates = countryNames(game.countries.filter(c => c.totalCells == 0 && !c.isIslamistRule)) match {
        case Nil => countryNames(game.countries.filter(c => c.totalCells == 0))
        case nonIR => nonIR
      }

      // Always US first if it is eligible.
      if (candidates.contains(UnitedStates))
        UnitedStates :: JihadistBot.multipleTargets(2, candidates.filterNot(_ == UnitedStates))(JihadistBot.cellPlacementPriority(false))
      else
        JihadistBot.multipleTargets(3, candidates)(JihadistBot.cellPlacementPriority(false))
    }

    val numCells = if (isBot(role) && game.jihadistIdeology(Potent)) {
      log(s"\n$Jihadist Bot with Potent Ideology places two cells for each recruit success", Color.Event)
      2
    }
    else
      1

    // Process all of the targets
    for (target <- targets) {
      addEventTarget(target)
      log(s"\nRecruit attempt in $target")
      log(separator())
      testCountry(target)
      val c = game.getCountry(target)  // Get country after testing!

      if (game.cellsAvailable > 0) {
        val cells = numCells min game.cellsAvailable
        if (c.autoRecruit) {
          log(s"Recruit in $target succeeds automatically")
          addSleeperCellsToCountry(target, cells)
        }
        else {
          val die = getDieRoll(s"Enter recruit die roll for $target: ", Some(role))
          log(s"Die roll: $die")
          if (c.recruitSucceeds(die)) {
            log(s"Recruit in $target succeeds with a die roll of $die")
            addSleeperCellsToCountry(target, cells)
          }
          else {
            log(s"Recruit in $target fails with a die roll of $die")
            addCadreToCountry(target)
          }
        }
      }
      else {
        log("No available cells.")
        addCadreToCountry(target)
      }
    }
  }
}
