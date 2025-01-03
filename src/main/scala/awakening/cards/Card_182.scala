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
// Roll a Tan & Black die and place the following in that result if Hard
// (if unplayable, use highest # playable result).
// Cells are placed Active side up.
// Tan                    Black
// -------------------    --------------------
// 1. United States       1. 1 & 2 Plot
// 2. Canada              2. 3 Plot & 2 Cells
// 3. United Kingdom      3. 2 Plot & 1 Cell
// 4. Benelux             4. 2 Plot
// 5. France              5. 1 Plot & 1 Cell
// 6. Any Hd. Schengen    6. 1 Plot
// ------------------------------------------------------------------
object Card_182 extends Card(182, "Paris Attacks", Jihadist, 2, NoRemove, NoLapsing, NoAutoTrigger) {
  // Used by the US Bot to determine if the executing the event would alert a plot
  // in the given country
  override
  def eventAlertsPlot(countryName: String, plot: Plot): Boolean = false

  // Used by the US Bot to determine if the executing the event would remove
  // the last cell on the map resulting in victory.
  override
  def eventRemovesLastCell(): Boolean = false

  val possibleCountries = UnitedStates :: Canada :: UnitedKingdom :: Benelux :: France :: Schengen

  // Returns true if the printed conditions of the event are satisfied
  override
  def eventConditionsMet(role: Role) = {
    (game.cellsAvailable > 0 || game.availablePlots.nonEmpty) &&
    possibleCountries.exists(name => game.getNonMuslim(name).isHard)
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
    // Note: There is a slight chance that the Bot could execute this event,
    // and get a black die roll for only plots or only cells and there are
    // plot/cells available.
    val isHard = (name: String) => game.getNonMuslim(name).isHard

    val countries = List(UnitedStates, Canada, UnitedKingdom, Benelux, France)
    val validSchengen = Schengen.filter(isHard)
    println()
    val tanDie     = getDieRoll("Enter tan die: ", Some(role))
    val blackDie   = getDieRoll("Enter black die: ", Some(role))
    val (plots, activeCells) = blackDie match {
      case 1 => (Plot1::Plot2::Nil, 0)
      case 2 => (Plot3::Nil, 2)
      case 3 => (Plot2::Nil, 1)
      case 4 => (Plot2::Nil, 0)
      case 5 => (Plot1::Nil, 1)
      case _ => (Plot1::Nil, 0)
    }

    def getTarget(list: List[String]): Option[String] = {
      list match {
        case Nil if validSchengen.nonEmpty => Some("Schengen")
        case Nil                           => None
        case x::xs if isHard(x)            => Some(x)
        case x::xs                         => getTarget(xs)
      }
    }

    def placePlots(name: String, plotList: List[Plot]): Unit = plotList match {
      case Nil =>
      case p::ps =>
        if (game.availablePlots.contains(p)) {
          testCountry(name)
          addAvailablePlotToCountry(name, p)
        }
        else
          log(s"\nThere is not an available $p to place", Color.Event)
        placePlots(name, ps)
    }


    log(s"Tan die: $tanDie,  Black die: $blackDie")
    val target = getTarget(countries.drop(tanDie - 1)).map {
      case "Schengen" if isHuman(role) =>
        askCountry("Choose a Hard Schengen country: ", validSchengen)
      case "Schengen" => JihadistBot.plotPriority(validSchengen).get
      case name => name
    }

    target match {
      case None =>
        log("\nNo Hard country was selected for the Paris Attacks event", Color.Event)
      case Some(name) =>
        addEventTarget(name)
        log(s"\n$name is the target of the Paris Attacks event")
        log(separator())
        placePlots(name, plots)
        val numCells = activeCells min game.cellsAvailable
        if (numCells > 0)
          addActiveCellsToCountry(name, activeCells min game.cellsAvailable)
        else
          log("\nThere are no available cells to place", Color.Event)
    }
  }
}
