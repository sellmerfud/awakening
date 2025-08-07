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
// Select 3 countries without cells. Test them.
// Recruit once in each, ignoring Funding.
// Place cadre in each country that does not receive a cell.
// ------------------------------------------------------------------
object Card_082 extends Card(82, "Jihadist Videos", Jihadist, 3, NoRemove, NoLapsing, NoAutoTrigger) {
  // Used by the US Bot to determine if the executing the event would alert a plot
  // in the given country
  override
  def eventAlertsPlot(countryName: String, plot: Plot): Boolean = false

  // Used by the US Bot to determine if the executing the event would remove
  // the last cell on the map resulting in victory.
  override
  def eventRemovesLastCell(): Boolean = false

  val isCandidate = (c: Country) => !c.truce && c.totalCells == 0

  def getCandidates = countryNames(game.countries.filter(isCandidate))

  // Returns true if the printed conditions of the event are satisfied
  override
  def eventConditionsMet(role: Role) = getCandidates.nonEmpty

  def enhBotUnmarkedNonMuslimCandidates = if (game.usPosture == Hard && game.hardSoftDelta < 2 && game.gwotPenalty == 0)
    game.nonMuslims.filter(n => isCandidate(n) && n.isUntested)
  else
    Nil

  // The Enhance Bot will select targets using the following rules:
  // - First if the US is Hard and hard - soft countries < 2 and there is not GWOT penalty,
  //   select ONE Unmarked non-Muslim country.
  //   Priorities: UK --> Philippines —> non-Muslim Nigeria —> Caucasus —> Russia —> Thailand —>
  //               China —> Kenya —> France —> Spain —> Schengen —> random
  // - Select Muslim countries: Priority MJP -> Poor adjacent to MJP -> Unmarked adjacent to MJP
  //   Priorites: If multiple in a category, use Recruit Priorities
  // -
  def enhBotTargets: List[String] = JihadistBot.cachedTarget("jihadist-videos-targets") {
    import JihadistBot.{ CriteriaFilter, SchengenPriority, topPriority, recruitAndTravelToPriorities }
    def namedPriority(name: String) = new CriteriaFilter(name, _.name == name)

    // If US hard and hard/soft delta < 2 and no GWOT penalty: then place one cell in an Unmarked non-Muslim country.
    // priority: UK, Philippines, non-Muslim Nigeria, Caucasus, Russia, Thailand, China, Kenya, France, Spain, Schengen, random
    // Then use Recruit priorites to select between muslim countries.
    val UnmarkedPriorities = List(
      namedPriority(UnitedKingdom), namedPriority(Philippines), namedPriority(Nigeria), namedPriority(Caucasus),
      namedPriority(Russia), namedPriority(Thailand), namedPriority(China), namedPriority(KenyaTanzania),
      namedPriority(France), namedPriority(Spain), SchengenPriority
    )

    def nextAdjToMjpTarget(candidates: List[MuslimCountry]): List[String] = {
      topPriority(candidates, recruitAndTravelToPriorities).map(_.name) match {
        case None =>
          Nil
        case Some(name) =>
          name :: nextAdjToMjpTarget(candidates.filterNot(_.name == name))
      }
    }
  
    val nonMuslimTarget = JihadistBot.topPriority(enhBotUnmarkedNonMuslimCandidates, UnmarkedPriorities)
        .map(_.name)
        .toList
    val muslimTargets = JihadistBot.PriorityCountries.majorJihadPriority match {
      case None =>
        Nil
      case Some(mjpName) =>
        val mjpTarget = if (isCandidate(game.getMuslim(mjpName)))
          mjpName :: Nil
        else
          Nil
        val adjPoorTargets = game.adjacentMuslims(mjpName)
          .filter(m => m.isPoor && isCandidate(m))
        val adjUnmarkedTargets = game.adjacentMuslims(mjpName)
          .filter(m => m.isUntested && isCandidate(m))
        mjpTarget ::: nextAdjToMjpTarget(adjPoorTargets) ::: nextAdjToMjpTarget(adjUnmarkedTargets)
    }

    (nonMuslimTarget ::: muslimTargets).take(3)
  }

  // Returns true if the Bot associated with the given role will execute the event
  // on its turn.  This implements the special Bot instructions for the event.
  // When the event is triggered as part of the Human players turn, this is NOT used.
  override
  def botWillPlayEvent(role: Role): Boolean = if (game.botEnhancements) {
    // Playable if 3+ cells on track.
    JihadistBot.botLog(s"Jihadist Videos targets: $enhBotTargets")
    game.cellsAvailable > 2 && enhBotTargets.size == 3
  }
  else
    game.cellsAvailable > 0

  // Carry out the event for the given role.
  // forTrigger will be true if the event was triggered during the human player's turn
  // and it associated with the Bot player.
  override
  def executeEvent(role: Role): Unit = {
    var targets = if (isHuman(role))
      askCountries(3, getCandidates)
    else if (game.botEnhancements)
      enhBotTargets
    else {
      // See Event Instructions table
      val nonIR = countryNames(game.countries.filter(m => m.totalCells == 0 && !m.isIslamistRule))
      val candidates = if (nonIR.size >= 3)
        nonIR
      else
        getCandidates

      // US is always first choice if possible
      if (candidates.contains(UnitedStates))
        UnitedStates :: JihadistBot.multipleTargets(2, candidates.filterNot(_ == UnitedStates))(JihadistBot.cellPlacementPriority(false))
      else
        JihadistBot.multipleTargets(3, candidates)(JihadistBot.cellPlacementPriority(false))
    }

    val numCellsPerRecruit = if (isBot(role) && game.jihadistIdeology(Potent)) {
      log(s"\n$Jihadist Bot with Potent Ideology places two cells for each recruit success", Color.Event)
      2
    }
    else
      1

    log(s"\n$Jihadist selects: ${andList(targets)}", Color.Event)
    for (target <- targets) {
      addEventTarget(target)
      testCountry(target)
    }

    // Process all of the targets
    for (target <- targets) {
      log(s"\nRecruit in $target")
      log(separator())
      val c = game.getCountry(target)
      val numCells = numCellsPerRecruit min game.cellsAvailable
      if (numCells == 0) {
        log(s"No cells on the track.", Color.Event)
        addCadreToCountry(target)
      }
      else if (c.autoRecruit) {
          log(s"Recruit succeeds automatically", Color.Event)
          addSleeperCellsToCountry(target, numCells)
      }
      else {
        val die = getDieRoll("Enter event die roll: ", Some(role))
        if (c.recruitSucceeds(die)) {
          log(s"Recruit succeeds with a die roll of $die", Color.Event)
          addSleeperCellsToCountry(target, numCells)
        }
        else {
          log(s"Recruit fails with a die roll of $die", Color.Event)
          addCadreToCountry(target)
        }
      }
    }
  }
}
