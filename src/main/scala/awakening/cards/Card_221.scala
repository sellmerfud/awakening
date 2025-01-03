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

import scala.util.Random.shuffle
import awakening.LabyrinthAwakening._
import awakening.{ USBot, JihadistBot }

// Card Text:
// ------------------------------------------------------------------
// Play if any countries are Civil War, Regime Change or Caliphate.
// Remove 1 Reaction marker or Cell from up to 3 different countries
// and place the same number as Cells in one Civil War, Regime Change
// or Caliphate country.
// ------------------------------------------------------------------
object Card_221 extends Card(221, "FlyPaper", Unassociated, 2, NoRemove, NoLapsing, NoAutoTrigger) {
  // Used by the US Bot to determine if the executing the event would alert a plot
  // in the given country
  override
  def eventAlertsPlot(countryName: String, plot: Plot): Boolean = false

  // Used by the US Bot to determine if the executing the event would remove
  // the last cell on the map resulting in victory.
  override
  def eventRemovesLastCell(): Boolean = false

  val isCellsSource = (c: Country) => c.totalCells > 0
  val isReactionSource = (c: Country) => c match {
    case m: MuslimCountry => m.reaction > 0
    case n: NonMuslimCountry => false
  }
  val isSource = (c: Country) => isCellsSource(c) || isReactionSource(c)

  def getCellSources() = countryNames(game.countries.filter(isCellsSource))

  def getReactionSources() = countryNames(game.muslims.filter(isReactionSource))

  val isCandidate = (m: MuslimCountry) =>
    m.civilWar || m.inRegimeChange || game.isCaliphateMember(m.name)

  def getCandidates() = countryNames(game.muslims.filter(isCandidate))

  // Returns true if the printed conditions of the event are satisfied
  override
  def eventConditionsMet(role: Role) = getCandidates().nonEmpty

  // US bot will place cells in a candidate country with the lowest
  // resource value
  def getUSBotTarget() = USBot.cachedTarget("flypaper-target") {
    val lowResource = (game.muslims.filter(isCandidate).map(_.resourceValue)).min
    val candidates = game.muslims.filter(m => isCandidate(m) && m.resourceValue == lowResource)
    shuffle(countryNames(candidates)).head
  }

  // Enhanced Bot will choose the Caliphate priority target if it is currently
  // at 5 Islamist Resources in order to win the game.  But only if there are
  // 3 cells that can make the trip.
  def getJihadistBotTarget() = JihadistBot.cachedTarget("flypaper-target") {
    JihadistBot.caliphatePriorityTarget(getCandidates())
      .filter { name =>
        game.botEnhancements &&
        game.caliphateCapital.isEmpty &&
        game.islamistResources == 5 &&
        game.countries.count(c => c.name != name && JihadistBot.hasCellForTravel(c)) > 2
      }
      .getOrElse(JihadistBot.cellPlacementPriority(false)(getCandidates()).get)
  }
  // Returns true if the Bot associated with the given role will execute the event
  // on its turn.  This implements the special Bot instructions for the event.
  // When the event is triggered as part of the Human players turn, this is NOT used.
  override
  def botWillPlayEvent(role: Role): Boolean = role match {
    case US =>
      val target = getUSBotTarget()
      game.hasCountry(c => c.name != target && isSource(c))

    case Jihadist =>
      val target = getJihadistBotTarget()
      val numTravelers = game.countries.count(c => c.name != target && JihadistBot.hasCellForTravel(c))
      // Enhanced bot will only select the event if it can remove 3 cells
      // Normal bot only requires 1
      if (game.botEnhancements)
        numTravelers > 2
      else
        numTravelers > 0
  }


  // Carry out the event for the given role.
  // forTrigger will be true if the event was triggered during the human player's turn
  // and it associated with the Bot player.
  override
  def executeEvent(role: Role): Unit = {
    // Can possibly declare Caliphate, by either player
    // See Event Instructions table
    if (isHuman(role)) {
      val reactionSources = getReactionSources()
      val numReaction = askInt("\nRemove how many reaction markers? ", 0, 3 min reactionSources.size)
      val reactionCountries = if (numReaction == 0)
        Nil
      else if (numReaction == 1)
        askCountry("\nRemove reaction marker from which country? ", reactionSources)::Nil
      else {
        println("\nRemove reaction markers from which countries?")
        askCountries(numReaction, reactionSources)
      }

      for (name <- reactionCountries) {
        addEventTarget(name)
        removeReactionMarker(name)
      }

      val cellSources = getCellSources().filterNot(reactionCountries.contains)
      val numCells = askInt("\nRemove how many cells? ", 0, (3 - numReaction) min cellSources.size)
      val cellCountries = if (numCells == 0)
        Nil
      else if (numCells == 1)
        askCountry("\nRemove a cell from which country? ", cellSources)::Nil
      else {
        println("\nRemove cells from which countries?")
        askCountries(numCells, cellSources)
      }

      case class Cells(name: String, cells: (Int, Int, Boolean))

      val cells = for (name <- cellCountries; c = askCells(name, 1, sleeperFocus = role == US))
        yield Cells(name, c)

      for (Cells(name, (a, s, sadr)) <- cells) {
        addEventTarget(name)
        removeCellsFromCountry(name, a, s, sadr, addCadre = true)
      }

      val numToPlace = (numReaction + numCells) min game.cellsAvailable
      if (numToPlace > 0) {
        val name = askCountry(s"Select country to place ${amountOf(numToPlace, "cell")}: ", getCandidates())
        addEventTarget(name)
        testCountry(name)
        addSleeperCellsToCountry(name, numToPlace)
        if (jihadistChoosesToDeclareCaliphate(name, numToPlace))
          declareCaliphate(name)
      }
      else
        log("\nThere are no available cells to place on the map.", Color.Event)
    }
    else if (role == Jihadist) {
      // Jihadist Bot removes only "cells""
      val target = getJihadistBotTarget()
      val cellSources = countryNames(game.countries.filter (c => c.name != target && JihadistBot.hasCellForTravel(c)))
      val countries = if (cellSources.size <= 3)
        cellSources
      else {
        def nextCountry(num: Int, candidates: List[String]): List[String] =
          if (num <= 3 && candidates.nonEmpty) {
            val country = JihadistBot.travelFromPriorities(target, candidates).get
            country :: nextCountry(num + 1, candidates filterNot (_ == country))
          }
          else
            Nil

        nextCountry(1, cellSources)
      }

      for (name <- countries) {
        val (a, s, sadr) = JihadistBot.chooseCellsToRemove(name, 1)
        removeCellsFromCountry(name, a, s, sadr, addCadre = true)
      }

      addEventTarget(target)
      testCountry(target)
      addSleeperCellsToCountry(target, countries.size)
      if (jihadistChoosesToDeclareCaliphate(target, countries.size))
        declareCaliphate(target)
    }
    else {
      // US Bot will remove reaction markers before cells, 2 max
      val target = getUSBotTarget()

      def nextReaction(remaining: Int, countries: List[String]): List[String] = {
        if (remaining == 0 || countries.isEmpty)
          Nil
        else {
          val name = USBot.disruptPriority(countries).get
          name :: nextReaction(remaining - 1, countries filterNot (_ == name))
        }
      }

      def nextCell(remaining: Int, countries: List[String]): List[(String, (Int, Int, Boolean))] = {
        if (remaining == 0 || countries.isEmpty)
          Nil
        else {
          val name = USBot.disruptPriority(countries).get
          val cell = USBot.chooseCellsToRemove(name, 1)
          (name, cell) :: nextCell(remaining - 1, countries filterNot (_ == name))
        }
      }

      // The US Bot does not want to allow a caliphate to be declared
      // so it will limit removals to 2 unless there are not available cells
      // or if the caliphate already exists.
      val numReactions = if (game.caliphateCapital.nonEmpty || (getReactionSources().size >= 3 && game.cellsAvailable < 3))
        3
      else
        2
      val reactionCountries = nextReaction(numReactions, getReactionSources())

      for (name <- reactionCountries)
        removeReactionMarker(name)

      val cellCountries = nextCell(2 - reactionCountries.size max 0, getCellSources().filterNot(_ == target))

      for ((name, (active, sleeper, sadr)) <- cellCountries)
        removeCellsFromCountry(name, active, sleeper, sadr, addCadre = true)

      val toPlace = (reactionCountries.size + cellCountries.size) min game.cellsAvailable
      if (toPlace > 0) {
        addEventTarget(target)
        testCountry(target)
        addSleeperCellsToCountry(target, toPlace)
      }
      else
        log("\nThere are no available cells to place on the map.", Color.Event)
    }
  }
}
