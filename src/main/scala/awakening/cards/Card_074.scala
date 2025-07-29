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
import scala.annotation.unused

// Card Text:
// ------------------------------------------------------------------
// Automatically travel 2 cells to or within any Schengen countries.
// ------------------------------------------------------------------
object Card_074 extends Card(74, "Schengen Visas", Jihadist, 2, NoRemove, NoLapsing, NoAutoTrigger) {
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


  def numMoveableCellsOutsideSchengen = game.countries
    .filterNot(c => Schengen.contains(c.name))
    .map(c => JihadistBot.numCellsForTravel(c, France))
    .sum
    
  // Returns true if the Bot associated with the given role will execute the event
  // on its turn.  This implements the special Bot instructions for the event.
  // When the event is triggered as part of the Human players turn, this is NOT used.
  override
  def botWillPlayEvent(role: Role): Boolean = if (game.botEnhancements) {
    val schengens = game.getCountries(Schengen)
    // Playable if US is hard and there are no cells in Schengen/US/Canada/UK and
    // there are at least 2 "moveable" cells
    val noCellsNearSchengen = !(UnitedStates::Canada::UnitedKingdom::Schengen).exists(game.getCountry(_).totalCells > 0)
    game.usPosture == Hard && noCellsNearSchengen && numMoveableCellsOutsideSchengen > 1
  }
  else
    Schengen.exists(name => JihadistBot.canTravelTo(name, autoTravel = true))

  // Carry out the event for the given role.
  // forTrigger will be true if the event was triggered during the human player's turn
  // and it associated with the Bot player.
  override
  def executeEvent(role: Role): Unit = if (isHuman(role)) {
    val num = 2 min game.cellsOnMap
    val travellers = if (num == 1) {
      for (c <- game.countries; if c.pieces.totalCells > 0)
        yield CellsItem(c.name, c.pieces.only(Cells))
    }
    else {
      println(s"Select 2 cells to travel to Schengen countries: ")
      askCellsFromAnywhere(num, trackOK = false, countryNames(game.countries), sleeperFocus = false)
    }
    var i = 1
    for (CellsItem(from, cells) <- travellers) {
      for (a <- 1 to cells.activeCells) {
        val to = askCountry(s"Select destination country for active cell in $from: ", Schengen)
        addEventTarget(to)
        if (from == to)
          hideActiveCells(to, 1)
        else
          moveCellsBetweenCountries(from, to, 1, true, forTravel = true)
        i += 1
      }

      for (a <- 1 to cells.sleeperCells) {
        val to = askCountry(s"Select destination country for sleeper cell in $from: ", Schengen.filterNot(_ == from))
        addEventTarget(to)
        moveCellsBetweenCountries(from, to, 1, false, forTravel = true)
        i += 1
      }
    }
  }
  else if (game.botEnhancements) {    
    // We use France as the target to satisfy the call to hasCellForTravel() but it could be
    // any Schengen country.
    val TargetPrioritiesUSHard = List(
      JihadistBot.WithoutCellsPriority, JihadistBot.UnmarkedPriority,
      JihadistBot.HardPosturePriority, JihadistBot.SoftPosturePriority)
    val TargetPrioritiesUSSoft = List(
      JihadistBot.WithoutCellsPriority, JihadistBot.SoftPosturePriority,
      JihadistBot.HardPosturePriority, JihadistBot.UnmarkedPriority)
  def travelCell(target: String): Boolean = {
      // The Bot will not play this event is there are any cells already in Schengen countries.
      // If the event is triggered during a US card play and there are cells in Schengen countries,
      // then we will travel those first before using AutoRecuitePrioroty and finally the travelFrom priorities.
      val source = if (getActiveRole() == Jihadist) 
      {
        val candidates = game.countries
          .filter (c => !Schengen.contains(c.name) && JihadistBot.numCellsForTravel(c, France) > 0)
        // US will always be hard if event played by Enhanced Jihadist
        JihadistBot.topPriority(candidates, JihadistBot.SoftPosturePriority::Nil)
          .map(_.name)
      }
      else {
        // Event triggered during US turn
        val schengenWithCells = game.getNonMuslims(Schengen).filter(c => !c.truce && c.name != target && JihadistBot.unusedCells(c) > 0)
        val schengenSource = if (game.usPosture == Hard)
          JihadistBot.topPriority(schengenWithCells, JihadistBot.SoftPosturePriority::Nil).map(_.name)
        else
          JihadistBot.topPriority(schengenWithCells, JihadistBot.HardPosturePriority::Nil).map(_.name)
  
        val arpSource = JihadistBot.autoRecruitPriorityCountry
          .filter { name =>
            val c = game.getMuslim(name)
            !c.truce && JihadistBot.numCellsForTravel(c, target, Nil, ignoreARP = true) > 0
          }
  
        val otherSource = {
          // If the event was triggered during US turn then the Bot may be forced
          // to travel a cell that it normally would not use
          val sources = countryNames(game.countries.filter(c => !c.truce && c.name != target && JihadistBot.hasCellForTravel(c, target))) match {
            case Nil => countryNames(game.countries.filter(c => !c.truce && c.name != target && JihadistBot.unusedCells(c) > 0))
            case s => s
          }
          JihadistBot.enhancedTravelFromTarget(target, sources, autoSuccess = true).orElse {
            // If no enhanced preferred travel source then fall back to standard rules
            JihadistBot.standardTravelFromTarget(target, sources, inPlaceOk = false)
          }
        }

        schengenSource.orElse(arpSource).orElse(otherSource)
      }


      source match {
        case Some(source) =>
          val fromCountry = game.getCountry(source)
          val active = JihadistBot.activeCells(fromCountry) > 0
          addEventTarget(target)
          moveCellsBetweenCountries(source, target, 1, active, forTravel = true)
          JihadistBot.usedCells(target).addSleepers(1)
          true

        case None =>
          false
      }
    }

    // Target countries:

    // If only one target, then use it twice
    val targets = {
      //   If US hard, select different targets: Unmarked, then hard, then soft.
      //   If US Soft: select the same target: soft, then hard, then Unmarked
      val candidates = if (game.usPosture == Hard)
        JihadistBot.narrowCandidates(game.getNonMuslims(Schengen), TargetPrioritiesUSHard).take(2)
      else
        JihadistBot.narrowCandidates(game.getNonMuslims(Schengen), TargetPrioritiesUSSoft).take(1)

      // If only one target then use it twice
      candidates match {
        case single :: Nil => single::single::Nil
        case _ => candidates
      }
    }

    val numTraveled = targets
      .map(_.name)
      .count(travelCell)

    if (numTraveled == 0)
      log("\nThe event has no effect.", Color.Event)
  }
  else {
    // Bot
    def nextTravel(destNum: Int, alreadyTried: Set[String]): Int = {
      // If the event was triggered during US turn then the Bot may be forced
      // to travel a cell that it normally would not use
      val candidates = Schengen.filter(name => !alreadyTried(name))

      if (destNum < 2 && candidates.nonEmpty) {
        val to   = JihadistBot.posturePriority(candidates).get
        val sources = countryNames(game.countries.filter(JihadistBot.hasCellForTravel(_, to))) match {
          case Nil => countryNames(game.countries.filter(c => JihadistBot.unusedCells(c) > 0))
          case s => s
        }

        if (sources.nonEmpty) {
          val from = JihadistBot.standardTravelFromTarget(to, sources.filterNot(_ == to), inPlaceOk = false)
          from match {
            case Some(from) =>
              val fromCountry = game.getCountry(from)
              val active = JihadistBot.activeCells(fromCountry) > 0
              addEventTarget(to)
              moveCellsBetweenCountries(from, to, 1, active, forTravel = true)
              JihadistBot.usedCells(to).addSleepers(1)
              nextTravel(destNum + 1, alreadyTried + to)
            case None =>
              nextTravel(destNum, alreadyTried + to)
          }
        }
        else
          destNum
      }
      else
        destNum
    }

    if (nextTravel(0, Set.empty) == 0)
      log("\nThe event has no effect.", Color.Event)
  }
}
