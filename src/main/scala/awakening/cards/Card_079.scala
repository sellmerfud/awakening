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
object Card_079 extends Card(79, "Clean Operatives", Jihadist, 3, NoRemove, NoLapsing, NoAutoTrigger) {
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

  // Enhanced Bot instructions:
  // If WMD available and no cells in or adjacent to US: to US
  // If any [Good Muslim countries without troops]: to Good Muslim without troops (max. 1 cell per country, priority to highest Resource)
  // If [US hard, no GWOT penalty and no cells in Schengen/US/Canada/UK]: to Schengen. Select different targets: Unmarked, then hard, then soft
  // If [US hard and no GWOT penalty]: to Unmarked non-Muslim countries, priority to highest Gov
  // If any [3 resource* Fair Muslim countries without troops: to 3 resource* Fair Muslim without troops], priority to AID
  // Else unplayable

  def enhUSCandidates = {
    val conditionMet = game.availablePlots.contains(PlotWMD) &&
      !(UnitedStates :: getAdjacent(UnitedStates)).exists(name => game.getCountry(name).cells > 0)
    if (conditionMet)
      List(UnitedStates)
    else
      Nil
  }

  def enhGoodMuslimCadidates = game.muslims
    .filter(m => !m.truce && m.isGood && m.totalTroops == 0)
    .sortBy(-JihadistBot.enhBotResourceValue(_))  // Highest resources values first
    .map(_.name)

  def enhSchengenCandidates = {
    val cellsCloseToSchengen = (UnitedStates::UnitedKingdom::Canada::Schengen)
      .exists(name => game.getCountry(name).cells > 0)

    if (game.usPosture == Hard && game.gwotPenalty == 0 && !cellsCloseToSchengen)
      game.getNonMuslims(Schengen)
        .filter(_.cells == 0)
        .sortWith { case (l, r) => (l.isUntested && !r.isUntested) || (l.isHard && r.isSoft) }
        .map(_.name)
    else
      Nil
  }

  def enhUnmarkedNonMuslimCandidates = {
    if (game.usPosture == Hard && game.gwotPenalty == 0)
      game.nonMuslims
        .filter(_.isUntested)
        .sortBy(-_.governance) // Highest governance first
        .map(_.name)
    else
      Nil
  }

  def enhFairMuslimCandidates = game.muslims
    .filter(m => !m.truce && m.isFair && m.totalTroops == 0 && JihadistBot.enhBotResourceValue(m) == 3)
    .sortWith { case (l, r) => l.aidMarkers > 0 && r.aidMarkers == 0}
    .map(_.name)

  def enhBotHasCandidate = (
    enhUSCandidates:::
    enhGoodMuslimCadidates:::
    enhSchengenCandidates:::
    enhUnmarkedNonMuslimCandidates:::
    enhFairMuslimCandidates
  ).exists(JihadistBot.canTravelTo)

  // Returns true if the Bot associated with the given role will execute the event
  // on its turn.  This implements the special Bot instructions for the event.
  // When the event is triggered as part of the Human players turn, this is NOT used.
  override
  def botWillPlayEvent(role: Role): Boolean = if (game.botEnhancements)
    enhBotHasCandidate
  else
    JihadistBot.canTravelTo(UnitedStates)

  // Carry out the event for the given role.
  // forTrigger will be true if the event was triggered during the human player's turn
  // and it associated with the Bot player.
  override
  def executeEvent(role: Role): Unit = if (isHuman(role)) {
    val allCountries = countryNames(game.countries)
    val destCountries = countryNames(game.countries.filter(!_.truce))
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
        val to = askCountry(s"Select ${ordinal(i)} destination country: ", destCountries)
        addEventTarget(to)
        moveCellsBetweenCountries(from, to, 1, true, forTravel = true)
        i += 1
      }
      for (a <- 1 to sleepers) {
        val to = askCountry(s"Select ${ordinal(i)} destination country: ", destCountries)
        addEventTarget(to)
        moveCellsBetweenCountries(from, to, 1, false, forTravel = true)
        i += 1
      }
    }
  }
  else if (game.botEnhancements && enhBotHasCandidate) {

    def attemptTravel(target: String): Boolean = {
      val preferredTravellers = countryNames(
        game.countries.filter(c => c.name != target && JihadistBot.hasCellForTravel(c, target))
      )
      val allTravellers = countryNames(
        game.countries.filter(c => c.name != target && JihadistBot.unusedCells(c) > 0)
      )
      val from = JihadistBot.enhancedTravelFromTarget(target, preferredTravellers, autoSuccess = true) orElse {
        // If event triggered during US turn then there may not be a preferred travel source,
        // so fall back to the standard Bot selection
        JihadistBot.standardTravelFromTarget(target, allTravellers, inPlaceOk = false)
      }

      from match {
        case None => false
        case Some(from) =>
          addEventTarget(target)
          val active = JihadistBot.activeCells(game.getCountry(from)) > 0
          moveCellsBetweenCountries(from, target, 1, active, forTravel = true)
          JihadistBot.usedCells(target).addSleepers(1)
          true
      }
    }

    def travelOnePer(remaining: Int, targets: List[String]): Int = targets match {
      case _ if remaining == 0 => 0
      case Nil => 0
      case target::others =>
        if (attemptTravel(target))
          1 + travelOnePer(remaining - 1, others)
        else
          travelOnePer(remaining, others)
    }

    var numTravelled = 0
    if (enhUSCandidates.nonEmpty)
      while (numTravelled < 2 && attemptTravel(UnitedStates))
        numTravelled += 1

    numTravelled += travelOnePer((2 - numTravelled) max 0, enhGoodMuslimCadidates)
    numTravelled += travelOnePer((2 - numTravelled) max 0, enhSchengenCandidates)
    numTravelled += travelOnePer((2 - numTravelled) max 0, enhUnmarkedNonMuslimCandidates)
    numTravelled += travelOnePer((2 - numTravelled) max 0, enhFairMuslimCandidates)
    // In case we did not use up both travels then just attempt to
    // travel to US by default.
    while (numTravelled < 2 && attemptTravel(UnitedStates))
      numTravelled += 1
  }
  else {
    def nextTravel(numTravels: Int): Int = {
      // If the event was triggered during US turn then the Bot may be forced
      // to travel a cell that it normally would not use
      val preferredTravellers = countryNames(
        game.countries.filter(c => c.name != UnitedStates && JihadistBot.hasCellForTravel(c, UnitedStates))
      )
      val allTravellers = countryNames(
        game.countries.filter(c => JihadistBot.unusedCells(c) > 0)
      )

      addEventTarget(UnitedStates)
      if (numTravels < 2) {
        val from = if (game.botEnhancements) {
          JihadistBot.enhancedTravelFromTarget(UnitedStates, preferredTravellers, autoSuccess = true) orElse {
            JihadistBot.standardTravelFromTarget(UnitedStates, allTravellers.filterNot(_ == UnitedStates), inPlaceOk = false)
          }
        }
        else
          JihadistBot.standardTravelFromTarget(UnitedStates, allTravellers.filterNot(_ == UnitedStates), inPlaceOk = false)
          
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
