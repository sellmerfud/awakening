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
  // Playable if two cells "moveable" cells can be used and can arrive in two different
  // destinations as selected by these priorities:
  // 1. If WMD available and no cells in or adjacent to US: to US
  // 2. To Good Muslim countries without troops, priority to highest Resource
  // 3. If [US hard, no GWOT penalty and no cells in Schengen/US/Canada/UK]: to Schengen
  //    Priorities: Unmarked, Hard, Soft
  // 4. If [US hard and no GWOT penalty]: to Unmarked non-Muslim countries, priority to highest Gov
  // 5. To 3 resource* Fair Muslim country without troops, priority to AID
  // 6. If [triggered during US card play] travel to MJP (1 or 2 cells)
  // 7. If [triggered during US card play] travel cells in place, priority to active cells, then non-Muslim, then highest gov.

  // We use a custom function to build travel attempts because we want to ignore preserving 3 cells in
  // the Auto-Recruit-Priority country.
  def travelAttempt(destination: String, prev: Option[TravelAttempt]): Option[TravelAttempt] = {
    val sources = game.countries
      .filter(source => JihadistBot.numCellsForTravel(source, destination, prev.toList, ignoreARP = true) > 0)
      .map(_.name)

      // If we find a "moveable" cell source, then create a Travel attempt
      JihadistBot.enhancedTravelFromTarget(destination, sources, prev.toList, autoSuccess = true, ignoreARP = true)
        .map { source =>
          val prevActive = prev
            .filter(_.from == source)
            .map(p => if (p.active) 1 else 0)
            .getOrElse(0)
          val active = JihadistBot.activeCells(game.getCountry(source)) - prevActive > 0
          TravelAttempt(source, destination, active)
        }
  }

  def notPrevTarget(name: String, prev: Option[TravelAttempt]) = !prev.exists(_.to == name)

  def enhBotUSTarget(prev: Option[TravelAttempt]): Option[TravelAttempt] = {
    val cellInOrAdjtoUS = (UnitedStates :: getAdjacent(UnitedStates))
      .exists(name => game.getCountry(name).cells > 0)
    
    if (!underTruce(UnitedStates) && game.availablePlots.contains(PlotWMD) && !cellInOrAdjtoUS && notPrevTarget(UnitedStates, prev))
      travelAttempt(UnitedStates, prev)
    else
      None
  }

  def enhBotGoodMuslimTarget(prev: Option[TravelAttempt]): Option[TravelAttempt] = {
    val candidates = game.muslims
      .filter { m =>
        !m.truce &&
        m.isGood &&
        m.totalTroops == 0 &&
        !(m.name == Pakistan && m.hasMarker(BenazirBhutto)) &&
        notPrevTarget(m.name, prev)
      }

    JihadistBot.botLog("Select: Good Muslims without troops")
    JihadistBot.topPriority(candidates, JihadistBot.HighestPrintedResourcePriority::Nil)
      .flatMap(dest => travelAttempt(dest.name, prev))
  }

  def enhBotSchengenTarget(prev: Option[TravelAttempt]): Option[TravelAttempt] = {
    val priorities = List(
      JihadistBot.UnmarkedPriority, JihadistBot.HardPosturePriority, JihadistBot.SoftPosturePriority)
    val cellsCloseToSchengen = (UnitedStates::UnitedKingdom::Canada::Schengen)
      .exists(name => game.getCountry(name).cells > 0)

    if (game.usPosture == Hard && game.gwotPenalty == 0 && !cellsCloseToSchengen) {
      val candidates = game.getNonMuslims(Schengen)
        .filter { n =>
          !n.truce &&
          notPrevTarget(n.name, prev)
        }
      JihadistBot.botLog("Select: Schengen")
      JihadistBot.topPriority(candidates, priorities)
        .flatMap(dest => travelAttempt(dest.name, prev))
    }
    else
      None
  }

  def enhBotUnmarkedNoMuslimTarget(prev: Option[TravelAttempt]): Option[TravelAttempt] = {
    if (game.usPosture == Hard && game.gwotPenalty == 0) {
      val candidates = game.nonMuslims
        .filter { n =>
          !n.truce &&
          n.isUntested &&
          notPrevTarget(n.name, prev)
        }
      JihadistBot.botLog("Select: Unmarked NonMuslims")
      JihadistBot.topPriority(candidates, JihadistBot.HighestPrintedResourcePriority::Nil)
        .flatMap(dest => travelAttempt(dest.name, prev))
    }
    else
      None
  }

  def enhBot3ResFairMuslimTarget(prev: Option[TravelAttempt]): Option[TravelAttempt] = {
    val candidates = game.muslims
      .filter { m =>
        !m.truce &&
        m.isFair &&
        m.totalTroops == 0 &&
        JihadistBot.enhBotResourceValue(m) == 3 &&
        notPrevTarget(m.name, prev)
      }
    JihadistBot.botLog("Select: 3 resource* Muslims without troops")
    JihadistBot.topPriority(candidates, JihadistBot.WithAidPriority::Nil)
      .flatMap(dest => travelAttempt(dest.name, prev))
  }

  // This will only produce a target when the event was triggered
  // during a US card play.
  def enhBotMJPTarget(prev: Option[TravelAttempt]): Option[TravelAttempt] =
    (JihadistBot.majorJihadPriorityCountry.map(game.getMuslim), getActiveRole()) match {
      case (Some(mjp), US)  if !mjp.truce =>
        JihadistBot.botLog(s"Select Major Jihad Priority: ${mjp.name}")
        travelAttempt(mjp.name, prev)
      case _ =>
        None
    }

  // This will only produce a target when the event was triggered
  // during a US card play.
  // 7. If [triggered during US card play] travel cells in place, priority to active cells, then non-Muslim, then highest gov.
  def enhBotInplaceTarget(prev: Option[TravelAttempt]): Option[TravelAttempt] = if (getActiveRole() == US) {
    val priorities = List(
      new JihadistBot.CriteriaFilter("With active cells", c => JihadistBot.activeCells(c) > 0),
      new JihadistBot.CriteriaFilter("Non-Muslim", c => c.isNonMuslim),
      JihadistBot.HighestGovernance
    )
    val candidates = game.countries
      .filter { c =>
        val unusedCells = JihadistBot.unusedCells(c) - prev.count(_.from == c.name)
        !c.truce && unusedCells > 0
      }

    JihadistBot.botLog("Select: In place travel target")
    JihadistBot.topPriority(candidates, priorities)
      .map { target =>
        val prevActive = prev
          .filter(_.from == target.name)
          .map(p => if (p.active) 1 else 0)
          .getOrElse(0)
        val active = JihadistBot.activeCells(target) - prevActive > 0
        TravelAttempt(target.name, target.name, active)
      }
  }
  else
    None

  def enhBotTargets: List[TravelAttempt] = JihadistBot.cachedTarget("clean-ops-target") {

    def nextTarget(prev: Option[TravelAttempt]): Option[TravelAttempt] = {
      enhBotUSTarget(prev) orElse
      enhBotGoodMuslimTarget(prev) orElse
      enhBotSchengenTarget(prev) orElse
      enhBotUnmarkedNoMuslimTarget(prev) orElse
      enhBot3ResFairMuslimTarget(prev) orElse
      enhBotMJPTarget(prev) orElse
      enhBotInplaceTarget(prev)
    }

    nextTarget(None) match {
      case None => Nil
      case first => first.toList ::: nextTarget(first).toList
    }
  }


  // Returns true if the Bot associated with the given role will execute the event
  // on its turn.  This implements the special Bot instructions for the event.
  // When the event is triggered as part of the Human players turn, this is NOT used.
  override
  def botWillPlayEvent(role: Role): Boolean = if (game.botEnhancements)
    enhBotTargets.size == 2
  else
    JihadistBot.canTravelTo(UnitedStates, autoTravel = false)

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
  else if (game.botEnhancements) {
    for (TravelAttempt(from, target, active) <- enhBotTargets) {
      addEventTarget(target)
      if (from == target) {
        if (active) {
          log(s"\nJihadist travels an active cell in place in $target")
          hideActiveCells(target, 1)
        }
        else
          log(s"\nJihadist travels a sleeper cell in place in $target (no effect)")
      }
      else {
        moveCellsBetweenCountries(from, target, 1, active, forTravel = true)
        JihadistBot.usedCells(target).addSleepers(1)
      }
    }
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
        val from = JihadistBot.standardTravelFromTarget(
          UnitedStates, allTravellers.filterNot(_ == UnitedStates), inPlaceOk = false)

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
