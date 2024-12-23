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
// Automatically travel 2 cells anywhere.
// ------------------------------------------------------------------
object Card_079 extends Card2(79, "Clean Operatives", Jihadist, 3, NoRemove, NoLapsing, NoAutoTrigger) {
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
  def eventConditionsMet(role: Role) = game.cellsOnMap > 0


  // Returns true if the Bot associated with the given role will execute the event
  // on its turn.  This implements the special Bot instructions for the event.
  // When the event is triggered as part of the Human players turn, this is NOT used.
  override
  def botWillPlayEvent(role: Role): Boolean =
    game.countries
      .exists(c => c.name != UnitedStates && JihadistBot.hasCellForTravel(c))


  // Carry out the event for the given role.
  // forTrigger will be true if the event was triggered during the human player's turn
  // and it associated with the Bot player.
  override
  def executeEvent(role: Role, forTrigger: Boolean): Unit = if (isHuman(role)) {
    val allCountries = countryNames(game.countries)
    val num = 2 min game.cellsOnMap
    val travellers = if (num == 1) {
      for (c <- game.countries; if c.cells > 0)
        yield CellsItem(c.name, c.activeCells, c.sleeperCells)
    }
    else {
      println(s"Select 2 cells to travel anywhere: ")
      askCellsFromAnywhere(num, trackOK = false, allCountries, sleeperFocus = false)
    }
    var i = 1
    for (CellsItem(from, actives, sleepers) <- travellers) {
      for (a <- 1 to actives) {
        val to = askCountry(s"Select ${ordinal(i)} destination country: ", allCountries)
        addEventTarget(to)
        testCountry(to)
        moveCellsBetweenCountries(from, to, 1, true, forTravel = true)
        i += 1
      }
      for (a <- 1 to sleepers) {
        val to = askCountry(s"Select ${ordinal(i)} destination country: ", allCountries)
        addEventTarget(to)
        testCountry(to)
        moveCellsBetweenCountries(from, to, 1, false, forTravel = true)
        i += 1
      }
    }
  }
  else {
    def nextTravel(numTravels: Int): Int = {
      // If the event was triggered during US turn then the Bot may be forced
      // to travel a cell that it normally would not use
      val preferredTravellers = countryNames(
        game.countries.filter(c => c.name != UnitedStates && JihadistBot.hasCellForTravel(c))
      )
      val allTravellers = countryNames(
        game.countries.filter(c => JihadistBot.unusedCells(c) > 0)
      )

      addEventTarget(UnitedStates)
      if (numTravels < 2) {
        val from = JihadistBot.travelFromTarget(UnitedStates, preferredTravellers) orElse {
          if (forTrigger)
            JihadistBot.travelFromTarget(UnitedStates, allTravellers.filterNot(_ == UnitedStates)) orElse
            allTravellers.find(_ == UnitedStates) // Last option travel within US
          else
            None
        }
        from match {
          case Some(from) =>
            val fromCountry = game.getCountry(from)
            val active = JihadistBot.activeCells(fromCountry) > 0
            addEventTarget(UnitedStates)
            if (from == UnitedStates) {
              if (active) {
                log(s"\nJihadist travels an active cell within the United States")
                hideActiveCells(UnitedStates, 1)
              }
              else
                log(s"\nJihadist travels a sleeper cell within the United States (no effect)")
            }
            else
              moveCellsBetweenCountries(from, UnitedStates, 1, active, forTravel = true)

            JihadistBot.usedCells(UnitedStates).addSleepers(1)

            nextTravel(numTravels + 1)
          case None =>
            numTravels
        }
     }
      else
        numTravels
    }
    nextTravel(0)
  }
}
