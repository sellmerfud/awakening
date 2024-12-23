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
// Automatically travel 2 cells to or within any Schengen countries.
// ------------------------------------------------------------------
object Card_074 extends Card2(74, "Schengen Visas", Jihadist, 2, NoRemove, NoLapsing, NoAutoTrigger) {
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
  def botWillPlayEvent(role: Role): Boolean = {
    val sources = game.countries.filter(JihadistBot.hasCellForTravel)
    sources.size > 1 || (sources.size == 1 && !Schengen.contains(sources.head.name))
  }

  // Carry out the event for the given role.
  // forTrigger will be true if the event was triggered during the human player's turn
  // and it associated with the Bot player.
  override
  def executeEvent(role: Role, forTrigger: Boolean): Unit = if (isHuman(role)) {
    val num = 2 min game.cellsOnMap
    val travellers = if (num == 1) {
      for (c <- game.countries; if c.cells > 0)
        yield CellsItem(c.name, c.activeCells, c.sleeperCells)
    }
    else {
      println(s"Select 2 cells to travel to Schengen countries: ")
      askCellsFromAnywhere(num, trackOK = false, countryNames(game.countries), sleeperFocus = false)
    }
    var i = 1
    for (CellsItem(from, actives, sleepers) <- travellers) {
      for (a <- 1 to actives) {
        val to = askCountry(s"Select destination country for active cell in $from: ", Schengen)
        addEventTarget(to)
        testCountry(to)
        if (from == to)
          hideActiveCells(to, 1)
        else
          moveCellsBetweenCountries(from, to, 1, true, forTravel = true)
        i += 1
      }

      for (a <- 1 to sleepers) {
        val to = askCountry(s"Select destination country for sleeper cell in $from: ", Schengen.filterNot(_ == from))
        addEventTarget(to)
        testCountry(to)
        moveCellsBetweenCountries(from, to, 1, false, forTravel = true)
        i += 1
      }
    }
  }
  else {
    // Bot
    def nextTravel(numDestinations: Int, alreadyTried: Set[String]): Int = {
      val canTravelFrom = (c: Country) => JihadistBot.hasCellForTravel(c)
      // If the event was triggered during US turn then the Bot may be forced
      // to travel a cell that it normally would not use
      val preferredTravellers = countryNames(game.countries.filter(canTravelFrom))
      val allTravellers = countryNames(game.countries.filter(c => JihadistBot.unusedCells(c) > 0))
      if (numDestinations < 2) {
        val candidates = Schengen.filterNot(alreadyTried.contains)
        val to   = JihadistBot.posturePriority(candidates).get
        val from = JihadistBot.travelFromTarget(to, preferredTravellers.filterNot(_ == to)) orElse {
          if (forTrigger) 
            JihadistBot.travelFromTarget(to, allTravellers.filterNot(_ == to))
          else
            None
        }
        from match {
          case Some(from) =>
            val fromCountry = game.getCountry(from)
            val active = JihadistBot.activeCells(fromCountry) > 0
            addEventTarget(to)
            testCountry(to)
            moveCellsBetweenCountries(from, to, 1, active, forTravel = true)
            JihadistBot.usedCells(to).addSleepers(1)
            nextTravel(numDestinations + 1, alreadyTried + to)
          case None =>
            nextTravel(numDestinations, alreadyTried + to)
        }
      }
      else
        numDestinations
    }

    if (nextTravel(0, Set.empty) == 0)
      log("\nThe event has no effect.", Color.Event)
  }
}
