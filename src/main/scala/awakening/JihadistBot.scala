
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
// Copyright (c) 2017 Curt Sellmer
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

package awakening

import scala.util.Random.shuffle
import scala.annotation.tailrec
import LabyrinthAwakening._
import awakening.scenarios.LetsRoll
import awakening.scenarios.YouCanCallMeAl
import awakening.LabyrinthAwakening.Color.all

object JihadistBot extends BotHelpers {

  // This object keeps track of which on map cells have been used during the
  // current Bot operation.  A given cell cannot be used more than once during
  // the play of single card including during radicaization.
  object usedCells {
    // Record the number of active and sleeper cells in a country
    // that have been used.  [Country, (actives, sleepers)]
    var sadrUsed = false
    var cellMap = Map.empty[String, (Int, Int)].withDefaultValue((0, 0))
    def clear(): Unit = {
      cellMap = Map.empty.withDefaultValue((0, 0))
      sadrUsed = false
    }
    def apply(name: String) = Helper(name)

    private object Helper {
      def apply(name: String) = new Helper(name)
    }
    class Helper private (name: String) {
      def actives  = cellMap(name) match { case (a, _) => a }
      def sleepers = cellMap(name) match { case (_, s) => s }
      def total    = cellMap(name) match { case (a, s) => a + s }
      def addActives(num: Int) = {
        val (a, s) = cellMap(name)
        cellMap += name -> (a + num, s)
      }
      def addSleepers(num: Int) = {
        val (a, s) = cellMap(name)
        cellMap += name -> (a, s + num)
      }
    }
  }

  object PriorityCountries {
    var autoRecruitPriority: Option[String] = None
    var majorJihadPriority: Option[String] = None
    var autoRecruitPrioritySet: Boolean = false
    var majorJihadPrioritySet: Boolean = false

    def clear(): Unit = {
      autoRecruitPriority = None
      majorJihadPriority  = None
      autoRecruitPrioritySet     = false
      majorJihadPrioritySet      = false
    }
  }

  def sadrValue(s: Boolean) = if (s) 1 else 0
  def activeCells(c: Country)   = (c.activeCells  - usedCells(c.name).actives) max 0
  def sleeperCells(c: Country)  = (c.sleeperCells - usedCells(c.name).sleepers) max 0
  def unusedCells(c: Country)   = (c.cells - usedCells(c.name).total) max 0
  def sadrAvailable(c: Country) = c.hasSadr && !usedCells.sadrUsed

  def totalUnused(c: Country, includeSadr: Boolean) =
    if (includeSadr)
      unusedCells(c) + sadrValue(sadrAvailable(c))
    else
      unusedCells(c)

  def unusedCellsOnMap = game.countries.map(c => totalUnused(c, includeSadr = true)).sum
  // Sadr cannot travel
  def unusedCellsOnMapForTravel = game.countries.map(unusedCells).sum

  // This is called when the current Caliphate capital has been displaced
  // and the Bot must choose the best candidate among those that are adjacent
  // to the previous capital.
  def pickNewCaliphateCapital(previousCapital: String): String = {
    case class CaliphateCapitalCandidate(m: MuslimCountry) {
      val size = game.caliphateDaisyChain(m.name).size
    }
    // Sort first by daisy chain size large to small
    // then by non Ally before Ally
    // then by worse governance before better governance
    // then by worst WoI drm before better WoI drm
    val CapitalOrdering = new Ordering[CaliphateCapitalCandidate] {
      def compare(x: CaliphateCapitalCandidate, y: CaliphateCapitalCandidate) = {
        if (x.size != y.size)
          y.size compare x.size // y first: to sort large to small.
        else if (x.m.isAlly != y.m.isAlly)
          x.m.isAlly compare y.m.isAlly // x first: nonAlly before Ally
        else if (x.m.governance != y.m.governance)
          y.m.governance compare x.m.governance // y first: because Good < Islamist Rule
        else {
          val (xMod, yMod) = (modifyWoiRoll(1, x.m, false, true), modifyWoiRoll(1, y.m, false, true))
          xMod compare yMod
        }
      }
    }

    val candidates = game.adjacentMuslims(previousCapital).filter(_.caliphateCandidate)
    assert(candidates.nonEmpty, s"pickNewCaliphateCapital() - no caliphate canidates adjacent to $previousCapital")

    // The Bot pick the best candidate for the new capital based on
    // the set of conditions outlined by compareCapitalCandidates().
    // We sort the best to worst.  If more than one has the best score, then
    // we choose randomly among them.
    val sortedCandidates = candidates
      .map(CaliphateCapitalCandidate)
      .sorted(CapitalOrdering)

    val best = sortedCandidates
      .takeWhile(CapitalOrdering.compare(_, sortedCandidates.head) == 0)
      .map(_.m.name)
    shuffle(best).head
  }


  def poorMuslimWhereMajorJihadPossible(m: MuslimCountry): Boolean =
    m.isPoor &&
    majorJihadSuccessPossible(m)

  def poorOrUnmarkedMuslimWhereMajorJihadPossible(m: MuslimCountry): Boolean =
    (m.isPoor || m.isUntested) &&
    majorJihadSuccessPossible(m)

  // Poor country with 1 to 4 more cells than Troops and Militia and Jihad success possible
  def poorMuslimNeedsCellsForMajorJihad(m: MuslimCountry): Boolean =
    poorMuslimWhereMajorJihadPossible(m) &&
    (m.totalCells - m.totalTroopsAndMilitia) > 0 &&
    (m.totalCells - m.totalTroopsAndMilitia) < 5

  def poorOrUnmarkedMuslimWhereMajorJSPAndNoTandM(m: MuslimCountry): Boolean =
    poorOrUnmarkedMuslimWhereMajorJihadPossible(m) &&
    m.totalTroopsAndMilitia == 0

  def fairMuslimWhereMajorJihadPossible(m: MuslimCountry): Boolean =
    m.isFair &&
    majorJihadSuccessPossible(m)

  def poorMuslimWithCadreAnNoTroopsOrMilitia(m: MuslimCountry): Boolean =
    m.isPoor &&
    m.hasCadre &&
    m.totalTroopsAndMilitia == 0

  def fairMuslimWithCadreAnNotTroopsOrMilitia(m: MuslimCountry): Boolean =
    m.isFair &&
    m.hasCadre &&
    m.totalTroopsAndMilitia == 0

  // Candidate destinations for enhanced bot travel operation
  // Travel to Poor/Unmarked countries where Major Jihad roll can succeed
  // and there are no Troops/Militia present.
  // If Biometrics is in effect only adjacent travel is allowed.
  def enhancedTravelCandidates = countryNames(
    game.muslims
      .filter(m => !m.truce && poorOrUnmarkedMuslimWhereMajorJSPAndNoTandM(m) && canTravelTo(m))
  )

  // The Enhanced bot will pick a top priority auto-recruit country.
  // It must be a non-Fair country that allows auto-recruit.
  // (Note: by definition a Good country can never be an auto-recruit country)
  def autoRecruitPriorityCountry: Option[String] = {
    if (PriorityCountries.autoRecruitPrioritySet == false) {
      val priorities = List(
        new HighestScoreNode(
          "Poor Regime change w/ highest printed resource*",
          muslimTest(m => m.isPoor && m.inRegimeChange),
          muslimScore(enhBotResourceValue)),
        new CriteriaFilter("Poor Caliphate Capital",
          muslimTest(m => m.isPoor && game.isCaliphateCapital(m.name))),
        new HighestScoreNode(
          "Poor Caliphate country w/ highest printed resource*",
          muslimTest(m => m.isPoor && game.isCaliphateMember(m.name)),
          muslimScore(enhBotResourceValue)),
        new CriteriaFilter("Poor country w/ Training Camps",
          muslimTest(m => m.isPoor && m.hasMarker(TrainingCamps))),
        new HighestScoreNode(
          "Poor Civil War country w/ highest printed resource*",
          muslimTest(m => m.isPoor && m.civilWar),
          muslimScore(enhBotResourceValue)),
        new CriteriaFilter("Islamist Rule Caliphate Capital",
          muslimTest(m => m.isIslamistRule && game.isCaliphateCapital(m.name))),
        new HighestScoreNode(
          "Islamist Rule country w/ highest printed resource*",
          muslimTest(m => m.isIslamistRule),
          muslimScore(enhBotResourceValue)),
        BestJihadDRMPriority(false),
        HighestCellsMinusTandM,
        LowestTandM,
        NoTandMGreatestCellsPlusAdjacent,
        MostMoveableAdjacentCells,
        BesiegedRegimePriority,
        AdversaryPriority,
        NeutralPriority,
        AllyPriority,
        UnmarkedPriority,
        NoAwakeningOrReactionMarkersPriority,
        OilExporterPriority,
        new CriteriaFilter("All have same priority",
          _ => true),
      )

      botLog("Selecting Auto Recruit Priority...", Color.Debug)
      botLog("Start with Auto-recruit Muslim countries worse than Fair")
      val target = game.muslims.filter(m => m.autoRecruit && !m.isFair) match {
        case Nil =>
          botLog("None found")
          None
        case candidates =>
          topPriority(candidates, priorities, allowBotLog = true).map(_.name)
      }

      PriorityCountries.autoRecruitPriority = target
      PriorityCountries.autoRecruitPrioritySet = true
      botLog(s"Auto Recruit Priority: ${target.getOrElse("None")}", Color.Debug)
    }

    PriorityCountries.autoRecruitPriority
  }

  def isAutoRecruitPriority(name: String) = (autoRecruitPriorityCountry == Some(name))

  val SoftPosturePriority = new CriteriaFilter("Posture is Soft", nonMuslimTest(_.isSoft))
  val HardPosturePriority = new CriteriaFilter("Posture is Hard", nonMuslimTest(_.isHard))
  val SchengenPriority = new CriteriaFilter("Schengen", nonMuslimTest(_.isSchengen))
  val HighestGovernance = new HighestScorePriority("Highest Governance Value", _.governance)

  // Return the best candidate for replacing a cell with a plot.
  // This is used by the Martyrdom Operations and KSM events.
  // Priorities
  // 1. US
  // 2. Good Muslim, priority to highest Resource, then with troops
  // 3. Fair Auto-Recruit Muslim, priority to RC
  // 4. If [Abu Sayyaf marker present] and [Prestige>low and/or Funding<7]: Phillipines
  // 5. If [Prestige>low]: Poor Muslim with troops, priority to RC, then AID
  // 6. If [US hard and Funding <7]: non-Muslim, priority to Schengen, then hard, then highest Gov
  // 7. If [US hard and no GWOT penalty]: hard non-Muslim, priority to Schengen, then highest Gov
  // 8. If [US soft and Funding <7]: non-Muslim, priority to soft, then highest Gov
  // 9. Fair 3 Resource Muslim
  // 10. Only Martyrdom Operation: If [Funding <8]: Poor Muslim, priority to Troops, then AID, then Adversary, then Neutral.
  // 11. Else unplayable

  def enhMartyrdomKSMTarget(candidates: List[String], martyrdom: Boolean): Option[String] = {
    val candidatesWithCells = candidates.filter(name => game.getCountry(name).cells > 0)
    lazy val muslims = candidatesWithCells
      .collect { case name if game.isMuslim(name) => game.getMuslim(name) }

    lazy val goodMuslims = muslims.filter(_.isGood)
    lazy val goodMuslimPriorities = List(HighestResourcePriority, WithTroopsPriority)

    lazy val fairAutoRecruits = muslims.filter(m => m.isFair && m.autoRecruit)

    lazy val phillipinesOK =
      candidatesWithCells.contains(Philippines) &&
      game.getCountry(Philippines).hasMarker(AbuSayyaf) &&
      (game.prestige > 3 || game.funding < 7)

    lazy val poorMuslimsWithTroops = muslims.filter(m => m.isPoor && m.totalTroops > 0)

    lazy val nonMuslims = candidatesWithCells
      .collect { case name if game.isNonMuslim(name) => game.getNonMuslim(name) }

    lazy val hardNonMuslims = nonMuslims.filter(_.isHard)

    lazy val fair3ResourceMuslims = muslims.filter(m => m.isFair && enhBotResourceValue(m) >= 3)

    lazy val fairPakistan = muslims.exists(m => m.name == Pakistan && m.isFair)

    lazy val poorMuslims = muslims.filter(_.isPoor)

    if (candidatesWithCells.isEmpty)
      None
    else if (candidatesWithCells.contains(UnitedStates) && game.availablePlots.contains(PlotWMD))
      Some(UnitedStates)
    else if (goodMuslims.nonEmpty && game.availablePlots.contains(Plot3))
      topPriority(goodMuslims, goodMuslimPriorities).map(_.name)
    else if (candidatesWithCells.contains(UnitedStates))
      Some(UnitedStates)
    else if (goodMuslims.nonEmpty)
      topPriority(goodMuslims, goodMuslimPriorities).map(_.name)
    else if (fairAutoRecruits.nonEmpty) {
      val priorities = List(RegimeChangePriority)
      topPriority(fairAutoRecruits, priorities).map(_.name)
    }
    else if (phillipinesOK)
      Some(Philippines)
    else if (game.prestige > 3 && poorMuslimsWithTroops.nonEmpty) {
      val priorities = List(RegimeChangePriority, WithAidPriority)
      topPriority(poorMuslimsWithTroops, priorities).map(_.name)
    }
    else if (game.usPosture == Hard && game.funding < 7 && nonMuslims.nonEmpty) {
      val priorities = List(
        SchengenPriority,
        HardPosturePriority,
        HighestGovernance)
      topPriority(nonMuslims, priorities).map(_.name)
    }
    else if (game.usPosture == Hard && game.gwotPenalty == 0 && hardNonMuslims.nonEmpty) {
      val priorities = List(
        SchengenPriority,
        HighestGovernance)
      topPriority(hardNonMuslims, priorities).map(_.name)
    }
    else if (game.usPosture == Soft && game.funding < 7 && nonMuslims.nonEmpty) {
      val priorities = List(
        SoftPosturePriority,
        HighestGovernance)
      topPriority(nonMuslims, priorities).map(_.name)
    }
    else if (fair3ResourceMuslims.nonEmpty)
      topPriority(fair3ResourceMuslims, plotPriorities).map(_.name)
    else if (fairPakistan)
      Some(Pakistan)
    else if (martyrdom && game.funding < 8 && poorMuslims.nonEmpty) {
      val priorities = List(
        WithTroopsPriority,
        WithAidPriority,
        AdversaryPriority,
        NeutralPriority)
      topPriority(poorMuslims, priorities).map(_.name)
    }
    else
      None
  }

  // The Enhanced Bot uses its current IR resources score
  // to make some decisions about certain priorities.
  //
  // In this case the Bot ignores temporary increaese due to
  // Oil Price Spike, but does honor the +1 bump for Tehran-Beirut Land Corridor
  // in Iran and the +1 bump for the Caliphate capital being declared.

  val enhBotResourceValue = (m: MuslimCountry) =>
    if (m.name == Iran && m.hasMarker(TehranBeirutLandCorridor))
      m.printedResources + 1
    else
      m.printedResources

  def currentIRResources = {
    val muslimResources = game.muslims
      .filter(_.isIslamistRule)
      .foldLeft(0) { (sum, muslim) => sum + enhBotResourceValue(muslim) }
    val caliphateResources = if (game.caliphateDeclared) 1 else 0

    muslimResources + caliphateResources
  }


  // Enhanced Bot will always peform minor Jihad in a good country
  // But in Fair country there are some further condtions to consider.
  // Don't allow using the last cell in a Fair country with the Training Camps
  // marker.  Don't want to risk losing the marker!
  val minorJihadGovTest = (m: MuslimCountry) =>
    if (game.botEnhancements)
      m.isGood ||
      (m.isFair && !(game.isTrainingCamp(m.name) && m.totalCells < 2) && (
        m.name == Pakistan || m.totalTroops > 0 || m.isAlly ||
        enhBotResourceValue(m) == 3 || m.autoRecruit || m.aidMarkers > 0 ||
        m.cells > 2
      ))
    else
      (m.isGood || m.isFair)

  // Find the priority Major Jihad priority target.
  // We start with all Muslim countries where a Major Jihad is possible (including umarked countries).
  // The country must have less than seven Troops and Militia.
  // We then use the Travel OpP flowchart to pick the favorites and finally we use the
  // Recruit/Travel to priorities table to choose among those that remain.
  def majorJihadPriorityCountry: Option[String] = {
    import awakening.scenarios.{ LetsRoll, YouCanCallMeAl }


    // We only select the priority country once per play
    if (PriorityCountries.majorJihadPrioritySet == false) {
      val pakistanAndCentralAsiaSpecial = game.scenarioName == LetsRoll.name || game.scenarioName == YouCanCallMeAl.name
      val specialResVal = (m: MuslimCountry) =>
        if (m.name == Pakistan || m.name == CentralAsia)
          3
        else
          enhBotResourceValue(m)

      // When the scenario is Let's Roll or You can call me Al, then
      // we treat Pakistan and Central Asia has having 3 resources for the
      // sake of determining priority.
      val HighestPrintedResourcePrioritySpecial = new HighestScorePriority(
        "Highest printed resource*, Pakistan/Central Asia = 3",
        muslimScore(specialResVal)
      )

      val firstPriority = currentIRResources match {
        case 1 if pakistanAndCentralAsiaSpecial => HighestPrintedResourcePrioritySpecial::Nil
        case n if n < 4 => HighestPrintedResourcePriority::Nil
        case 4 => PoorOrUntested2PlusResources::Nil
        case _ => Nil
      }
      // These priorities are a slight variation on the travelTo priorities
      val priorities = firstPriority :::
      List(
        BestJihadDRMPriority(false),
        AutoRecruitNoTandMFilter,
        HighestCellsMinusTandM,
        LowestTandM,
        NoTandMGreatestCellsPlusAdjacent,
        MostMoveableAdjacentCells,
        BesiegedRegimePriority,
        AdversaryPriority,
        NeutralPriority,
        AllyPriority,
        UnmarkedPriority,
        NoAwakeningOrReactionMarkersPriority,
        AdjacentToAutoRecruitPriority,
        OilExporterPriority
      )
      val possibilities = game.muslims.filter(m => majorJihadSuccessPossible(m) && m.totalTroopsAndMilitia < 7)
      botLog(s"Selecting Major Jihad Priority...", Color.Debug)
      botLog("Start with Muslim countries with < 7 TandM and Major jihad possible.")
      val target = selectCandidates(possibilities, TravelToFlowchart, allowBotLog = true) match {
        case Nil =>
          botLog("None found")
          None
        case candidates =>
          topPriority(candidates, priorities, allowBotLog = true).map(_.name)
      }
      PriorityCountries.majorJihadPriority = target
      PriorityCountries.majorJihadPrioritySet = true
      botLog(s"Major Jihad Priority: ${target.getOrElse("None")}", Color.Debug)
    }

    PriorityCountries.majorJihadPriority
  }


  // Pick all candidates that are not the same as the target unless there
  // is only the target to choose from.
  class NotDestinationPriority(target: String) extends CountryFilter {
    val desc = s"Not destination"
    def filter(countries: List[Country]) = (countries partition (_.name == target)) match {
      case (Nil, Nil)  => logResult(Nil);  Nil
      case (same, Nil) => logResult(same); same
      case (_, diff)   => logResult(diff); diff
    }
    private def logResult(results: List[Country]) = results match {
      case Nil => botLog(s"Not destination ($target): no candidates")
      case _   => botLog(s"Not destination ($target): found [${results.map(_.name).mkString( ", ")}]")
    }
  }


  // Jihadist Priorities Table entries

  // 1. Best Jihad DRM
  case class BestJihadDRMPriority(major: Boolean) extends CountryFilter {
    val desc = s"Best ${if (major) "Major" else "Minor"} Jihad DRM"
    val lowest = new LowestScorePriority(desc,
                     muslimScore(m => jihadDRM(m, major), nonMuslimScore = 100)
    )
    def filter(countries: List[Country]) = lowest.filter(countries)
  }

  // 2. US
  val USPriority = new CriteriaFilter("US", c => c.name == UnitedStates)

  // 3. With troops unless prestige 1
  val WithPrestigeTroopsPriority = new CriteriaFilter("With troops & Prestige > 1",
                  muslimTest(_.totalTroops > 0 && game.prestige > 1))

  // 4. Not Islamist Rule
  val NotIslamistRulePriority = new CriteriaFilter("Not Islamist Rule",
                  muslimTest(_.isIslamistRule == false, nonMuslim = true))

  // 5. Pakistan with Arsenal
  val PakistanPriority = new CriteriaFilter("Pakistan arsenal",
                  c => c.name == Pakistan && c.wmdCache > 0)

  // 6. Philippines if would prestige - 1
  val PhilippinesPriority = new CriteriaFilter("Philippines",
                  muslimTest(m => m.name == Philippines &&
                                  m.totalTroops > 0 &&
                                  game.prestige > 1))

  // 7. Besieged Regime
  val BesiegedRegimePriority = new CriteriaFilter("Besieged regime", muslimTest(_.besiegedRegime))

  // 8. Most active cells
  val MostActveCellsPriority = new HighestScorePriority("Most active cells", activeCells)

  // 9. Syrai with Arsenal
  val SyriaPriority = new CriteriaFilter("Syria Arsenal", c => c.name == Syria && c.wmdCache > 0)

  // 10. With Aid
  val WithAidPriority = new CriteriaFilter("With aid", muslimTest(_.aidMarkers > 0))

  // 11. Regime Change with troops
  val RegimeChangeTroopsPriority = new CriteriaFilter("Regime change with troops",
                  muslimTest(m => m.inRegimeChange && m.totalTroops > 0))

  // 12. Highest Resource
  val HighestResourcePriority = new HighestScorePriority("Highest resource", muslimScore(_.resourceValue))

  // 13. With Troops
  val WithTroopsPriority = new CriteriaFilter("With troops", muslimTest(_.totalTroops > 0))

  val MostTroopsPriority = new HighestScorePriority("Most troops", _.totalTroops)

  // 14. Iran with Arsenal
  val IranPriority = new CriteriaFilter("Iran arsenal", c => c.name == Iran && c.wmdCache > 0)

  // 15. US  (Already define at #2)

  // 16. Not a travel destination
  //  create an instance of the NotDestinationPriority() class.

  // 17. Islamist Rule
  val IslamistRulePriority = new CriteriaFilter("Islamist Rule", muslimTest(_.isIslamistRule))

  // 18. Poor
  val PoorPriority = new CriteriaFilter("Poor", _.isPoor)

  // 19. Fair
  val FairPriority = new CriteriaFilter("Fair", _.isFair)

  // 20. Good
  val GoodPriority = new CriteriaFilter("Good", _.isGood)

  // 21. Highest Resource (Already defined at #12)

  // 22. Russia
  val RussiaPriority = new CriteriaFilter("Russia", _.name == Russia)

  // 23. No Disrupt prestige gain
  val NoDisruptPretigePriority = new CriteriaFilter("No Disrupt prestige gain",
                  muslimTest(_.totalTroopsThatAffectPrestige == 0, nonMuslim = true))

  // 24. Highest REC#
  val HighestRECPriority = new HighestScorePriority("Highest REC#", nonMuslimScore(_.recruitNumber))

  // 25. Best Jihad DRM (Already defined at #1)

  // 26. Not US
  val NotUSPriority = new CriteriaFilter("Not US", c => c.name != UnitedStates)

  // 27. Most active cells  (Already defined at #8)

  // 28. Not Regime change
  val NotRegimeChangePriority = new CriteriaFilter("Not Regime change",
                  muslimTest(m => !m.inRegimeChange, nonMuslim = true))

  // 29. Worst Jihad DRM
  val WorstJihadDRMPriority = new HighestScorePriority("Worst Jihad DRM",
                  muslimScore(m => jihadDRM(m, false)))

  // 30. Disrupt prestige gain
  val DisruptPrestigePriority = new CriteriaFilter("Disrupt prestige gain",
                  muslimTest(m => m.disruptAffectsPrestige && game.prestige < 12))

  // 31. Civil War
  val CivilWarPriority = new CriteriaFilter("Civil War", muslimTest(_.civilWar))

  // 32. Neutral
  val NeutralPriority = new CriteriaFilter("Muslim Neutral", muslimTest(_.isNeutral))

  // 33. Besieged Regime (Already defined at #7)

  // 34. Adjacent Good Ally
  val AdjacentGoodAllyPriority = new CriteriaFilter("Adjacent Good Ally",
                  muslimTest(m => game.adjacentToGoodAlly(m.name)))

  // 35. Fair Non-Muslim
  val FairNonMuslimPriority = new CriteriaFilter("Fair Non-Muslim", nonMuslimTest(_.isFair))

  // 36. Same posture as US
  val SamePostureAsUSPriority = new CriteriaFilter("Same posture as US",
                  nonMuslimTest(_.posture == game.usPosture))

  // 37. Lowest REC#
  val LowestRECPriority = new LowestScorePriority("Lowest REC#", nonMuslimScore(_.recruitNumber, muslimScore = 100))

  // 38. Most cells
  val MostCellsPriority = new HighestScorePriority("Most cells", unusedCells)

  val WithoutCellsPriority = new CriteriaFilter("Without cells", _.totalCells == 0)
  val WithCellsPriority = new CriteriaFilter("Without cells", _.totalCells > 0)

  // 39. Adjacent to Islamist Rule
  val AdjacentIslamistRulePriority = new CriteriaFilter("Adjacent to Islamist Rule",
                  c => game.adjacentToIslamistRule(c.name))

  // 40. Oil Exporter
  val OilExporterPriority = new CriteriaFilter("Oil exporter", muslimTest(_.oilExporter))

  val RegimeChangePriority = new CriteriaFilter("Regime Change", muslimTest(_.inRegimeChange))

  // Used by the Enh Bot.  Uses unmodified printed resource value
  // exception: Iran with Tehran-Beirut Land Corridor marker counts a Res=3
  val HighestPrintedResourcePriority = new HighestScorePriority(
    "Highest printed resource*",
    muslimScore(enhBotResourceValue)
  )

  // Used by the Enh Bot.
  // Poor or Unmarked Muslim with 2+
  val PoorOrUntested2PlusResources = new CriteriaFilter(
    "Poor/Untested Muslim with 2+ resources*",
    muslimTest(m => (m.isPoor || m.isUntested) && enhBotResourceValue(m) >= 2)
  )

  val MostMilitiaPriority = new HighestScorePriority(
    "Most Militia",
    muslimScore(m => m.militia)
  )

  val IsMajorJihadPriority = new CriteriaFilter(
    "Is Major Jihad Priority",
    c => Some(c.name) == JihadistBot.majorJihadPriorityCountry
  )

  val AdjacentToMajorJihadPriority = new CriteriaFilter(
    "Adjacent to Major Jihad Priority",
    c => JihadistBot.majorJihadPriorityCountry
      .map(mjp => areAdjacent(mjp, c.name))
      .getOrElse(false)
  )

  val AdjacentToMoveableCell = new CriteriaFilter(
    "Adjacent to moveable cell",
    c => game.adjacentCountries(c.name).exists(adj => hasCellForTravel(adj, c.name))
  )

  val HighestCellsMinusTandM = new HighestScorePriority(
    "Highest cells - TandM",
    muslimScore(m => m.totalCells - m.totalTroopsAndMilitia)
  )

  val LowestTandM = new LowestScorePriority(
    "Lowest TandM",
    muslimScore(_.totalTroopsAndMilitia)
  )

  val NoTandMGreatestCellsPlusAdjacent = new HighestScoreNode(
        "No TandM present, Most cells present + adjacent moveable cells",
        muslimTest(m => m.totalTroopsAndMilitia == 0),
        muslimScore(m => m.totalCells + numAdjacentTravelCells(m)))

  val MostMoveableAdjacentCells = new HighestScorePriority(
        "Most moveable adjacent cells",
        c => numAdjacentTravelCells(c))

  val AdversaryPriority = new CriteriaFilter("Muslim Adversary", muslimTest(_.isAdversary))
  val AllyPriority = new CriteriaFilter("Muslim Ally", muslimTest(_.isAlly))
  val UnmarkedPriority = new CriteriaFilter("Unmarked", _.isUntested)

  val NoAwakeningOrReactionMarkersPriority = new CriteriaFilter("No Awakening/Reaction markers",
    muslimTest(m => m.awakening == 0 && m.reaction == 0))

  val AdjacentToAutoRecruitPriority = new CriteriaFilter("Adjacent to Auto-Recruit country",
    c => game.getCountries(getAdjacent(c.name)).exists(_.autoRecruit)
  )

  // Jihadist OpP Flowchart filters

  val NonMuslimFilter     = new CriteriaFilter("Non-Muslim", nonMuslimTest(_  => true))
  val PoorNonMuslimFilter = new CriteriaFilter("Poor Non-Muslim", nonMuslimTest(_.isPoor))
  val FairNonMuslimFilter = new CriteriaFilter("Fair Non-Muslim", nonMuslimTest(_.isFair))
  val GoodNonMuslimFilter = new CriteriaFilter("Good Non-Muslim", nonMuslimTest(_.isGood))

  val PoorMuslimFilter  = new CriteriaFilter("Poor Muslim", muslimTest(_.isPoor))
  val FairMuslimFilter  = new CriteriaFilter("Fair Muslim", muslimTest(_.isFair))
  val GoodMuslimFilter  = new CriteriaFilter("Good Muslim", muslimTest(_.isGood))

  // To try to make the Bot AI smarter, we don't prioritise Good Muslim countries
  // unless there is a cell in an adjacent country so that the travel will succeed
  // without a die roll
  val GoodMuslimWithAdjacentCellsFilter =
    new CriteriaFilter(
      "Good Muslim w/ adjacent cells",
      muslimTest(m => m.isGood && m.hasAdjacent(c => hasCellForTravel(c, m.name)) && m.reactionDelta > 1))
  val AutoRecruitFilter = new CriteriaFilter(
    "Auto recruit",
    muslimTest(_.autoRecruit)
  )

  val NoTandMFilter = new CriteriaFilter(
    "No TandM",
    muslimTest(m => m.totalTroopsAndMilitia == 0)
  )

  val NoTroopsFilter = new CriteriaFilter(
    "No Troops",
    muslimTest(m => m.totalTroops == 0)
  )

  val AutoRecruitNoTandMFilter = new CriteriaFilter(
    "Auto recruit with no TandM",
    muslimTest(m => m.autoRecruit && m.totalTroopsAndMilitia == 0)
  )

  val WorstReactionMinusAwakening = new LowestScorePriority(
    "Worst reaction - awakening delta",
    muslimScore(m => m.reaction - m.awakening)
  )

  // Used with botEnhancements
  val PoorTroopsCellsFilter =
    new CriteriaFilter(
      "Poor with troops and cells",
      c => c.isPoor && c.troops > 0 && unusedCells(c) > 0)

  val PoorTroopsActiveCellsFilter = new CriteriaFilter(
    "Poor with troops and active cells",
    muslimTest(m => m.isPoor && m.troops > 0 && activeCells(m) > 0)
  )
  val PoorNeedCellsforMajorJihad = new CriteriaFilter(
    "Poor, 1-4 more cells than TandM and JSP",
    muslimTest(m => poorMuslimNeedsCellsForMajorJihad(m))
  )
  val PoorCellsOutnumberTroopsMilitiaByAtLeast3 = new CriteriaFilter(
    "Poor Muslim country w/ (cells - TandM) > 2",
    muslimTest(m => m.isPoor && (m.totalCells - m.totalTroopsAndMilitia) > 2)
  )
  val PoorAutoRecruit = new CriteriaFilter(
    "Poor, Auto-recruit Muslim country",
    muslimTest(m => m.isPoor && m.autoRecruit)
  )
  val PoorAutoRecruitNoTandM = new CriteriaFilter(
    "Poor, Auto-recruit Muslim country without TandM",
    muslimTest(m => m.isPoor && m.autoRecruit && m.totalTroopsAndMilitia == 0)
  )

  // val PoorMuslimNoAutoRecruitIfTamdM = new CriteriaFilter(
  //   "Poor Muslim country (not Auto-Recruit if TandM)",
  //   muslimTest(m => m.isPoor && !(m.autoRecruit && m.totalTroopsAndMilitia > 0))
  // )

  // val UnmarkedOrPoorMuslimNoAutoRecruitIfTamdM = new CriteriaFilter(
  //   "Unmarked or Poor Muslim country (not Auto-Recruit if TandM)",
  //   muslimTest( m =>
  //     m.isUntested ||
  //     (m.isPoor && !(m.autoRecruit && m.totalTroopsAndMilitia > 0))
  //   )
  // )

  val UnmarkedOrPoorLessThan3TamdM = new CriteriaFilter(
    "Unmarked OR Poor Muslim with < 3 TandM",
    muslimTest( m =>
      m.isUntested ||
      (m.isPoor && m.totalTroopsAndMilitia < 3)
    )
  )

  val PoorLessThan3TamdM = new CriteriaFilter(
    "Poor Muslim with < 3 TandM",
    muslimTest( m => m.isPoor && m.totalTroopsAndMilitia < 3)
  )

  val AutoRecruitPriorityCountry = new CriteriaFilter(
    "Auto-recruit priority country",
    // We must make sure the ARP has been set to avoid infinitely looping!
    c => PriorityCountries.autoRecruitPrioritySet && Some(c.name) == autoRecruitPriorityCountry
  )

  val EnhRecruitPoorCadreNoTandMMajorJihadPossible = new CriteriaFilter(
    "Poor Muslim, w/ cadre and no TandM and (reaction - awakening > 2) and JSP",
    muslimTest(m =>
      (poorMuslimWhereMajorJihadPossible(m) && poorMuslimWithCadreAnNoTroopsOrMilitia(m))
    )
  )

  val EnhTravelPoorNeedCellsforMajorJihad = new CriteriaFilter(
    "Poor, 1-4 more cells than TandM and (reaction - awakening > 2) and JSP",
    muslimTest(m =>
      poorMuslimNeedsCellsForMajorJihad(m)
    )
  )

  val MostReactionMarkersPriority = new HighestScoreNode(
    "Most reaction markers present",
    _.isMuslim,
    muslimScore(m => m.reaction)
  )

  val HighestAwakeningMinusReactionPriority = new HighestScoreNode(
    "Highest awakening - reaction",
    _.isMuslim,
    muslimScore(m => m.awakening - m.reaction)
  )

  val HighestReactionMinusAwakeningPriority = new HighestScoreNode(
    "Highest reaction - awakening",
    _.isMuslim,
    muslimScore(m => m.reaction - m.awakening)
  )


  // Best DRM but Islamist Rule last.
  // I'm assuming that if there are any Civil War or Regime change countries (even with negative DRMs)
  // then they would be selected over Islamist Rule countries.
  val AutoRecruitBestJihadDRM = new LowestScoreNode("Auto recruit w/ best Jihad DRM",
                  muslimTest(_.autoRecruit),
                  muslimScore(m => if (m.isIslamistRule) 50 else jihadDRM(m, false), nonMuslimScore = 100))
  val FairMuslimBestJihadDRM = new LowestScoreNode("Fair Muslim w/ best Jihad DRM",
                  muslimTest(_.isFair),
                  muslimScore(m => jihadDRM(m, false), nonMuslimScore = 100))
  // To try to make the Bot AI smarter, we don't prioritise Fair Muslim countries
  // where Jihad is possible unless there is a cell in an adjacent country so that
  // the travel will succeed  without a die roll
  val EnhTravelFairMuslimBestJihadDRMWithAdjacentCells = new LowestScoreNode(
    "Fair Muslim and (reaction - awakening > 1) w/ best Jihad DRM w/ Adjacent cells",
    muslimTest(m => m.isFair && m.hasAdjacent(c => hasCellForTravel(c, m.name)) && m.reactionDelta > 1),
    muslimScore(m => jihadDRM(m, false), nonMuslimScore = 100))
  val EnhRecruitFairMuslimBestJihadDRMWithAdjacentCells = new LowestScoreNode(
    "Fair Muslim and (reaction - awakening > 1) w/ best Jihad DRM",
    muslimTest(m => m.isFair && m.reactionDelta > 1),
    muslimScore(m => jihadDRM(m, false), nonMuslimScore = 100))
  val PoorMuslimWhereMajorJSPBestJihadDRM = new LowestScoreNode(
    "Poor Muslim w/ best Jihad DRM",
    muslimTest(m => m.isPoor && majorJihadSuccessPossible(m)),
    muslimScore(m => jihadDRM(m, true), nonMuslimScore = 100))
  val PoorMuslimBestJihadDRM = new LowestScoreNode(
    "Poor Muslim w/ best Jihad DRM",
    muslimTest(_.isPoor),
    muslimScore(m => jihadDRM(m, true), nonMuslimScore = 100))
  val FewestCellsFilter = new LowestScoreNode("Fewest cells", _ => true, _.totalCells)

  def minorJihadPriorities(): List[CountryFilter] = {
    if (game.botEnhancements)
      List(
        WithTroopsPriority, GoodPriority, BestJihadDRMPriority(major = false), WithAidPriority, RegimeChangeTroopsPriority,
        HighestPrintedResourcePriority, MostCellsPriority, AdjacentIslamistRulePriority
      )
    else
      List(
        BestJihadDRMPriority(major = false), PakistanPriority, BesiegedRegimePriority, SyriaPriority, WithAidPriority,
        RegimeChangeTroopsPriority, HighestResourcePriority, WithTroopsPriority, IranPriority, MostCellsPriority,
        AdjacentIslamistRulePriority, OilExporterPriority
      )
  }

  def majorJihadPriorities(): List[CountryFilter] = {
    if (game.botEnhancements)
      List(
        BestJihadDRMPriority(major = true), WithAidPriority, RegimeChangeTroopsPriority,
        HighestPrintedResourcePriority, WithTroopsPriority, MostCellsPriority, AdjacentIslamistRulePriority
      )
    else
      List(
        BestJihadDRMPriority(major = true), PakistanPriority, BesiegedRegimePriority, SyriaPriority,
        WithAidPriority, RegimeChangeTroopsPriority, HighestResourcePriority, WithTroopsPriority,
        IranPriority, MostCellsPriority, AdjacentIslamistRulePriority, OilExporterPriority
      )
  }

  def markerAlignGovPriorities(): List[CountryFilter] = {
    if (game.botEnhancements)
      List(
        WithAidPriority, RegimeChangeTroopsPriority, HighestResourcePriority, WithTroopsPriority,
        MostCellsPriority, AdjacentIslamistRulePriority
      )
    else
      List(
        PakistanPriority, BesiegedRegimePriority, SyriaPriority, WithAidPriority, RegimeChangeTroopsPriority,
        HighestResourcePriority, WithTroopsPriority, IranPriority, MostCellsPriority, AdjacentIslamistRulePriority,
        OilExporterPriority
      )
  }

  // Bot will not try minor Jihad in Poor countries
  def minorJihadTarget(names: List[String]): Option[String] = {
    botLog("Find \"Minor Jihad\" target", Color.Debug)
    topPriority(game getMuslims names, minorJihadPriorities()).map(_.name)
  }

  // Bot will only try major Jihad in Poor countries
  def majorJihadTarget(names: List[String]): Option[String] = {
    botLog("Find \"Major Jihad\" target", Color.Debug)
    topPriority(game getMuslims names, majorJihadPriorities()).map(_.name)
  }

  def alignGovTarget(names: List[String]): Option[String] = {
    botLog("Find \"Align/Gov\" target", Color.Debug)
    if (game.botEnhancements)
      topPriority(game.getCountries(names), recruitAndTravelToPriorities).map(_.name)
    else
      topPriority(game.getCountries(names), markerAlignGovPriorities()).map(_.name)
  }

  def markerTarget(names: List[String]): Option[String] = {
    botLog("Find \"Marker\" target", Color.Debug)
    if (names.isEmpty)
      None
    else if (game.botEnhancements) {
      // If any of the targets have less than 7 troops/mililta
      // then only consider those that have less than 7 troops/militia
      // Then with the resulting candidates,  se the following priorites:
      //   HighestPrintedResource -> Poor -> Unmarked -> Fair -> Travel Priorities
      val preferred = muslimTest(m => m.totalTroopsAndMilitia < 7, nonMuslim = true) _
      val priorities = List(
        HighestPrintedResourcePriority,
        PoorMuslimFilter,
        UnmarkedPriority,
        FairMuslimFilter) ::: recruitAndTravelToPriorities
      val candidates = game.getCountries(names) match {
        case ns if ns.exists(preferred) =>
          if (game.botLogging) {
            val ignored = ns.filterNot(preferred)
            botLog(s"Ignoring candidates with 6+ TandM: [${ignored.map(_.name).mkString(", ")}]")
          }
          ns.filter(preferred)
        case ns =>
          ns
      }
      val narrowed = narrowCandidates(candidates, priorities)
      shuffle(narrowed).map(_.name).headOption
    }
    else
      topPriority(game.getCountries(names), markerAlignGovPriorities()).map(_.name)
  }

  def troopsMilitiaTarget(names: List[String]): Option[String] = {
    botLog("Find \"Troops/Militia\" target", Color.Debug)
    topPriority(game.getCountries(names), markerAlignGovPriorities()).map(_.name)
  }

  def selectPlotMarkers(target: String, numPlots: Int, plotCandidates: List[Plot]): List[Plot] =
    if (game.botEnhancements) {
      // The Enhanced Bot uses the following rules:
      //
      // target       condition                       instructions
      // -------      --------------                  ----------------------
      // US           WMD available                   while 2+ WMD use WMD, once only 1 WMD then select randomly
      // US           WMD NOT available               lowest plot marker
      // Non-Muslim   Funding > 7                     lowest plot marker
      // Non-Muslim   Funding <= 7                    highest plot marker (not WMD)
      // Muslim       WMD available & Troops present  select randomly
      // Muslim                                       highest plot marker (Not WMD)

      target match {
        case UnitedStates if plotCandidates.contains(PlotWMD) =>
          val (wmd, others) = plotCandidates.sorted.partition(_ == PlotWMD)
          // Take all but one WMD first, then shuffle the last WMD with others and take them randomly.
          (wmd.tail ::: shuffle(wmd.head::others))
            .take(numPlots)

        case UnitedStates =>
          // Lowest Plot number
          plotCandidates
            .sorted
            .reverse
            .take(numPlots)

        case _ if game.isNonMuslim(target) && game.funding > 7 =>
          // Lowest Plot number
          plotCandidates
            .sorted
            .reverse
            .take(numPlots)

        case _ if game.isNonMuslim(target) =>
          // Highest Plot number (not WMD unless that is all that is left)
          val (wmd, others) = plotCandidates.partition(_ == PlotWMD)
          (others.sorted ::: wmd)
            .take(numPlots)

        case _ if game.isMuslim(target) && game.getCountry(target).totalTroops > 0 && plotCandidates.contains(PlotWMD) =>
          // Random
          shuffle(plotCandidates)
            .take(numPlots)

        // If we get here we have either:
        // - non-muslime with funding <= 7
        // - muslim without troops
        // - muslime with troops but no WMD available
        case _ =>
          // Highest Plot number (not WMD unless that is all that is left)
          val (wmd, others) = plotCandidates.partition(_ == PlotWMD)
          (others.sorted ::: wmd)
            .take(numPlots)
      }
    }
    else {
      // The Standard Bot simply selects plots at random
      shuffle(plotCandidates).take(numPlots)
    }

  def plotPriorities: List[CountryFilter] =
    game.currentMode match {
      case LabyrinthMode if game.botEnhancements =>
        List(USPriority, WithPrestigeTroopsPriority, PhilippinesPriority,
             MostActveCellsPriority, WithAidPriority, RegimeChangeTroopsPriority,
             HighestPrintedResourcePriority, NeutralPriority,
             AdjacentGoodAllyPriority, FairNonMuslimPriority, SamePostureAsUSPriority,
             LowestRECPriority, AdjacentIslamistRulePriority)

      case LabyrinthMode =>
        List(USPriority, WithPrestigeTroopsPriority, PakistanPriority, PhilippinesPriority,
             MostActveCellsPriority, WithAidPriority, RegimeChangeTroopsPriority,
             HighestResourcePriority, NeutralPriority,
             AdjacentGoodAllyPriority, FairNonMuslimPriority, SamePostureAsUSPriority,
             LowestRECPriority, AdjacentIslamistRulePriority, OilExporterPriority)

      case AwakeningMode if game.botEnhancements =>
        List(USPriority, WithPrestigeTroopsPriority,
             MostActveCellsPriority, WithAidPriority, RegimeChangeTroopsPriority,
             HighestPrintedResourcePriority, CivilWarPriority, NeutralPriority,
             AdjacentGoodAllyPriority, FairNonMuslimPriority, SamePostureAsUSPriority,
             LowestRECPriority, AdjacentIslamistRulePriority)

      case AwakeningMode =>
        List(USPriority, WithPrestigeTroopsPriority, PakistanPriority,
             MostActveCellsPriority, SyriaPriority, WithAidPriority, RegimeChangeTroopsPriority,
             HighestResourcePriority, IranPriority, CivilWarPriority, NeutralPriority,
             AdjacentGoodAllyPriority, FairNonMuslimPriority, SamePostureAsUSPriority,
             LowestRECPriority, AdjacentIslamistRulePriority, OilExporterPriority)

      case ForeverWarMode if game.botEnhancements =>
        List(USPriority, WithPrestigeTroopsPriority,
             MostActveCellsPriority, WithAidPriority, RegimeChangeTroopsPriority,
             HighestPrintedResourcePriority, CivilWarPriority, NeutralPriority,
             AdjacentGoodAllyPriority, FairNonMuslimPriority, SamePostureAsUSPriority,
             LowestRECPriority, AdjacentIslamistRulePriority)

      case ForeverWarMode =>
        List(USPriority, WithPrestigeTroopsPriority, PakistanPriority,
             MostActveCellsPriority, IranPriority, WithAidPriority, RegimeChangeTroopsPriority,
             HighestResourcePriority, SyriaPriority, CivilWarPriority, NeutralPriority,
             AdjacentGoodAllyPriority, FairNonMuslimPriority, SamePostureAsUSPriority,
             LowestRECPriority, AdjacentIslamistRulePriority)
    }

  def plotTarget(names: List[String], prestigeFocus: Boolean): Option[String] = {
    val flowchart = if (game.botEnhancements) {
      // The enhanced Bot does not care about the funding tight decision
      // And if the focus is on hammering US prestige then the final two priorities
      // for Poor Muslim and Non-Muslim are ignored
      if (prestigeFocus)
        List(PoorMuslimFilter, FairMuslimFilter)
      else
        List(PoorTroopsCellsFilter, PoorNonMuslimFilter, PoorMuslimFilter, FairMuslimFilter, GoodMuslimFilter, NonMuslimFilter)
    }
    else {
      if (game.fundingLevel == Tight)
        List(PoorNonMuslimFilter, FairNonMuslimFilter, GoodNonMuslimFilter,
             PoorTroopsActiveCellsFilter, FairMuslimFilter, GoodMuslimFilter, NonMuslimFilter, PoorMuslimFilter)
      else
        List(PoorTroopsActiveCellsFilter, FairMuslimFilter, GoodMuslimFilter, NonMuslimFilter, PoorMuslimFilter)
    }

    botLog("Find \"Plot\" target", Color.Debug)
    val candidates = selectCandidates(game.getCountries(names), flowchart)
    topPriority(candidates, plotPriorities).map(_.name)
  }

  def plotPriority(names: List[String]): Option[String] = {
    topPriority(game.getCountries(names), plotPriorities).map(_.name)
  }

  def RecruitFlowchart = if (game.botEnhancements)
    List(
      PoorCellsOutnumberTroopsMilitiaByAtLeast3,
      PoorLessThan3TamdM,
      AutoRecruitPriorityCountry,
      IslamistRulePriority)
  else
    List(
      PoorNeedCellsforMajorJihad,
      AutoRecruitBestJihadDRM,
      GoodMuslimFilter,
      FairMuslimBestJihadDRM,
      NonMuslimFilter,
      PoorMuslimBestJihadDRM)

  def TravelToFlowchart = if (game.botEnhancements)
    List(
      PoorCellsOutnumberTroopsMilitiaByAtLeast3,
      UnmarkedOrPoorLessThan3TamdM)
  else
    List(PoorNeedCellsforMajorJihad,
         GoodMuslimFilter,
         FairMuslimBestJihadDRM,
         NonMuslimFilter,
         PoorMuslimBestJihadDRM)

  def recruitAndTravelToPriorities: List[CountryFilter] = if (game.botEnhancements) {
    // The first priority for the Ennanced Bot is determined by how close to the Bot
    // is to victory.
    val firstPriority = currentIRResources match {
      case n if n < 4 => HighestPrintedResourcePriority::Nil
      case 4 => PoorOrUntested2PlusResources::Nil
      case _ => Nil
    }
    firstPriority :::
    List(
      BestJihadDRMPriority(false),
      AutoRecruitFilter,
      HighestCellsMinusTandM,
      LowestTandM,
      NoTandMGreatestCellsPlusAdjacent,
      MostMoveableAdjacentCells,
      BesiegedRegimePriority,
      AdversaryPriority,
      NeutralPriority,
      AllyPriority,
      UnmarkedPriority,
      NoAwakeningOrReactionMarkersPriority,
      AdjacentToAutoRecruitPriority,
      OilExporterPriority
    )
  }
  else {
    game.currentMode match {
      case LabyrinthMode =>
        List(NotIslamistRulePriority, PakistanPriority, BesiegedRegimePriority,
          USPriority, PoorPriority, FairPriority, GoodPriority, HighestResourcePriority,
          RussiaPriority, NoDisruptPretigePriority, HighestRECPriority, SamePostureAsUSPriority,
          MostCellsPriority, AdjacentIslamistRulePriority, OilExporterPriority)

      case AwakeningMode =>
        List(NotIslamistRulePriority, PakistanPriority, BesiegedRegimePriority,
            SyriaPriority, IranPriority, USPriority, PoorPriority, FairPriority,
            GoodPriority, HighestResourcePriority, NoDisruptPretigePriority,
            HighestRECPriority, BestJihadDRMPriority(false), SamePostureAsUSPriority,
            MostCellsPriority, AdjacentIslamistRulePriority, OilExporterPriority)

      case ForeverWarMode =>
        List(NotIslamistRulePriority, PakistanPriority, BesiegedRegimePriority,
            IranPriority, SyriaPriority, USPriority, PoorPriority, FairPriority,
            GoodPriority, HighestResourcePriority, NoDisruptPretigePriority,
            HighestRECPriority, BestJihadDRMPriority(false), SamePostureAsUSPriority,
            MostCellsPriority, AdjacentIslamistRulePriority, OilExporterPriority)
    }
  }


  def recruitTravelToPriority(names: List[String]): Option[String] = {
    topPriority(game.getCountries(names), recruitAndTravelToPriorities).map(_.name)
  }

  def fewestCellsPriority(names: List[String]): Option[String] = {
    val priorities = List(new LowestScorePriority("Least cells", (_.totalCells)))
    topPriority(game.getCountries(names), priorities).map(_.name)
  }

  def recruitTarget(names: List[String]): Option[String] = {
    botLog("Find \"Recruit\" target", Color.Debug)
    val candidates = selectCandidates(game.getCountries(names), RecruitFlowchart)
    topPriority(candidates, recruitAndTravelToPriorities).map(_.name)
  }


  def travelToTarget(names: List[String]): Option[String] = {
    botLog("Find \"Travel To\" target", Color.Debug)
    val candidates = selectCandidates(game.getCountries(names), TravelToFlowchart)
    topPriority(candidates, recruitAndTravelToPriorities).map(_.name)
  }

  // Regular (non-enhanced) rules
  // The bot will never travel a SLEEPER cell within the same country
  def standardTravelFromTarget(toCountry: String, names: List[String], inPlaceOk: Boolean): Option[String] = {
    def wouldMoveOrTravelWithinToSleep(c: Country) =
      inPlaceOk && (
        c.name != toCountry ||
        (activeCells(c) > 0 && !game.isCaliphateMember(c.name))
      )

    botLog(s"Find \"Travel From\" target for $toCountry", Color.Debug)
    val flowchart = List(
      new AdjacentCountriesNode(toCountry),
      AutoRecruitFilter,
      FewestCellsFilter)

    val priorities = List(new NotDestinationPriority(toCountry), IslamistRulePriority,
                          PoorPriority, FairPriority, GoodPriority, NotUSPriority,
                          MostActveCellsPriority, NotRegimeChangePriority, WorstJihadDRMPriority,
                          DisruptPrestigePriority, LowestRECPriority)

    val withCells  =
      game.getCountries(names)
        .filter(c => hasCellForTravel(c, toCountry))
        .filter(wouldMoveOrTravelWithinToSleep)
    val candidates = selectCandidates(withCells, flowchart)
    topPriority(candidates, priorities).map(_.name)

  }

  // Enhanced Bot rules
  // For normal travel:
  //   We select a cell from an adjacent country first if possible.
  // For auto success travel (per events):
  //   We select a cell from a non-adjacent country first if possible.
  //
  // If the target destination is GOOD/FAIR, then only allow travel from
  // adjacent countries
  def enhancedTravelFromTarget(toCountry: String, names: List[String], prevAttempts: List[TravelAttempt] = Nil, autoSuccess: Boolean, ignoreARP: Boolean = false): Option[String] = {
    val fromAdjacentOnly =
      game.getCountry(toCountry) match {
        case m: MuslimCountry => (m.isGood || m.isFair) && autoSuccess == false
        case _ => false
      }

    val getTotalCells = (c: Country) =>
      c.totalCells - prevAttempts.count(_.from == c.name)

    val withCells  =
      game.getCountries(names)
        .filter(_.name != toCountry)   // Never travel within same country
        .filter(c => !fromAdjacentOnly || areAdjacent(c.name, toCountry))
        .filter(c => numCellsForTravel(c, toCountry, prevAttempts, autoSuccess, ignoreARP) > 0)


    type FromTest = (Country => Boolean, String)
    type TravelFromCriteria = (FromTest, List[CountryFilter])

    val adjacentAutoRecruit = ((c: Country) => c.autoRecruit && areAdjacent(c.name, toCountry), "Adjacent Auto-Recruit")
    val adjacentMuslim      = ((c: Country) => c.isMuslim && areAdjacent(c.name, toCountry), "Adjacent Muslim")
    val adjacentNonMuslim   = ((c: Country) => c.isNonMuslim && areAdjacent(c.name, toCountry), "Adjacent Non-Muslim")
    val autoRecruit         = ((c: Country) => c.autoRecruit, "Auto-Recruit")
    val muslim              = ((c: Country) => c.isMuslim, "Muslim")
    val nonMuslim           = ((c: Country) => c.isNonMuslim, "Non-Muslim")

    val autoRecruitPriorities = List(
      new CriteriaFilter("Training Camps", c => game.isTrainingCamp(c.name)),
      IslamistRulePriority,
      CivilWarPriority,
      new CriteriaFilter("Regime Change", muslimTest(_.inRegimeChange)),
      new CriteriaFilter("Poor", _.isPoor),
      new HighestScorePriority("Most cells", _.totalCells),
      new LowestScorePriority("Lowest Resource Value", muslimScore(enhBotResourceValue, nonMuslimScore = 100)),
    )
    val muslimPriorities = List(
      new CriteriaFilter("Poor", _.isPoor),
      new CriteriaFilter("Fair", _.isPoor),
      new CriteriaFilter("Good", _.isPoor),
      new LowestScorePriority("Worst DRM", muslimScore(m => jihadDRM(m, false).abs)), // Favorable DRM is negative, to take the absolute value
      new LowestScorePriority("Lowest Resource Value", muslimScore(enhBotResourceValue, nonMuslimScore = 100)),
      new HighestScorePriority("Most cells", getTotalCells),
    )
    val nonMuslimPriorities = List(
      new HighestScorePriority("Most cells", getTotalCells),
      new CriteriaFilter("Good Non-Schengen", nonMuslimTest(c => c.isGood && !c.isSchengen)),
      new LowestScorePriority("Lowest Governance Value", _.governance),
    )

    // If auto sucess travel then do not give priconsider non-adjacent first
    val TravelFromOptions: List[TravelFromCriteria] = if (autoSuccess)
      List(
        (autoRecruit, autoRecruitPriorities),
        (muslim, muslimPriorities),
        (nonMuslim, nonMuslimPriorities),
      )
    else
      List(
        (adjacentAutoRecruit, autoRecruitPriorities),
        (adjacentMuslim, muslimPriorities),
        (adjacentNonMuslim, nonMuslimPriorities),
        (autoRecruit, autoRecruitPriorities),
        (muslim, muslimPriorities),
        (nonMuslim, nonMuslimPriorities),
      )

    @tailrec
    def nextCategory(criteriaOptions: List[TravelFromCriteria]): Option[String] = criteriaOptions match {
      case Nil => None
      case ((criteria, desc), priorities) :: others =>
        val candidates = withCells.filter(criteria)
        botLog(s"Travel From $desc: ${candidates.map(_.name).mkString("[", ", ", "]")}")
        if (candidates.nonEmpty)
          topPriority(candidates, priorities).map(_.name)
        else
          nextCategory(others)
    }

    botLog(s"Find \"Travel From\" target for $toCountry", Color.Debug)
    nextCategory(TravelFromOptions)
  }

  def travelFromTarget(toCountry: String, names: List[String], forPlacement: Boolean = false): Option[String] = if (game.botEnhancements)
    enhancedTravelFromTarget(toCountry, names, autoSuccess = forPlacement)
  else
    standardTravelFromTarget(toCountry, names, inPlaceOk = !forPlacement)


  // This is used for some events where we want to check the priorities only,
  // and skip the flowchart.
  def travelFromPriorities(toCountry: String, names: List[String]): Option[String] = {
    val priorities = List(
      new NotDestinationPriority(toCountry), IslamistRulePriority,
      PoorPriority, FairPriority, GoodPriority, NotUSPriority,
      MostActveCellsPriority, NotRegimeChangePriority, WorstJihadDRMPriority,
      DisruptPrestigePriority, LowestRECPriority)
    topPriority(game.getCountries(names), priorities).map(_.name)
  }


  def botRecruitTargets(muslimWithCadreOnly: Boolean): List[String] = {
    val autoRecruitPriorityIsIR = autoRecruitPriorityCountry.map(name => game.getCountry(name).isIslamistRule).getOrElse(false)
    // Only recruit in IR if:
    //  1. This is the auto-recruit priority country OR The auto-recruit priority country is not IR
    //  2. The country has less than 6 cells.
    // This is ignored if Biometrics is in play.
    val irRestriction = (m: MuslimCountry) =>
      !m.isIslamistRule ||  // This restriction only applies to IR countries
      lapsingEventInPlay(Biometrics) ||  // Ignored when Biometrics in play
      ((isAutoRecruitPriority(m.name) || !autoRecruitPriorityIsIR) && m.totalCells < 6)

    val criteria = if (game.botEnhancements)
      (c: Country) => c match {
        case m: MuslimCountry =>                             // Only recruit in Muslim countries
          (m.autoRecruit || m.isPoor || m.isIslamistRule) && // Only recruit in Good/Fair if auto-recruit
          irRestriction(m) &&                                // Restriction for Islamist Rule countries
          (!muslimWithCadreOnly || m.hasCadre)               // Special radicalization test
        case n: NonMuslimCountry => false
      }
    else
       (c: Country) => (!muslimWithCadreOnly || c.hasCadre) // Special radicalization test

    game.getCountries(game.recruitTargets(madrassas = false))
      .filter(criteria)
      .map(_.name)
  }

  // Get all Poor Muslims where a Major Jihad could be successful
  def majorJihadPoorMuslimTargets(totalOps: Int) = {
    val sufficentOps = (muslim: MuslimCountry) =>
      muslim.majorJihadOK(totalOps) &&
      (muslim.besiegedRegime || (totalOps >= 3) || (totalOps >= 2 && muslim.jihadDRM < 0))

    game.majorJihadTargets(totalOps)
      .map(game.getMuslim)
      .filter { m =>
        !m.truce &&
        m.isPoor &&
        sufficentOps(m) &&
        totalUnused(m, includeSadr = true) - m.totalTroopsAndMilitia >= 5 &&
        majorJihadSuccessPossible(m)
      }
      .map(_.name)
  }


  // To try and make the Bot AI a bit smarter, we don't
  // allow the Bot to recruit into an Islamist Rule country if it
  // already contains 7 or more cells.
  def botRecruitPossible(muslimWithCadreOnly: Boolean): Boolean =
    game.recruitPossible &&
    botRecruitTargets(muslimWithCadreOnly).nonEmpty

  // Jihadist Operations Flowchart definitions.
  sealed trait Operation       extends OpFlowchartNode
  case class  RecruitOp(target: Option[String]) extends Operation
  case class  TravelOp(source: Option[String], target: Option[String], maxAttempts: Option[Int], adjacentOnly: Boolean) extends Operation
  case object PlotOpFunding    extends Operation // plot with focus on raising funding
  case object PlotOpPrestige   extends Operation // plot with focus on hammering US prestige
  case object MinorJihadOp     extends Operation
  case class  MajorJihadOp(target: Option[String]) extends Operation
  case object PlaceRandomCells extends Operation  // Used only by Enhanced Bot
  case object EnhancedTravelOp extends Operation  // Used only by Enhanced Bot
  case object Radicalization   extends Operation
  case object AddToReservesOp  extends Operation  // Used only by Enhanced Bot


  // ------------------------------------------------------
  // EvO table as printed in the Forever War rules
  // ------------------------------------------------------
  object StandardEvoTable {
    // This is the starting point of the Operations Flowchart
    object MajorJihadDecision extends OperationDecision {
      def desc = "Major Jihad Success possible at Poor?"
      def yesPath = MajorJihadOp(None)
      def noPath  = FundingTightDecision
      def condition(ops: Int) = game.majorJihadTargets(ops)
        .map(game.getMuslim)
        .exists { m =>
          m.isPoor &&
          totalUnused(m, includeSadr = true) - m.totalTroopsAndMilitia >= 5 &&
          majorJihadSuccessPossible(m)
        }
    }

    object FundingTightDecision extends OperationDecision {
      def desc = "Funding Tight?"
      def yesPath = CellAvailableOrPlotDecision
      def noPath  = CellInGoodFairWhereJSP
      def condition(ops: Int) = game.fundingLevel == Tight
    }

    object CellAvailableOrPlotDecision extends OperationDecision {
      def desc = "Cells Available and Recruit possible? (1)"
      def yesPath = RecruitOp(None)
      def noPath  = PlotOpFunding
      def condition(ops: Int) = botRecruitPossible(muslimWithCadreOnly = false)
    }

    object CellAvailableOrTravelDecision extends OperationDecision {
      def desc = "Cells Available and Recruit possible? (2)"
      def yesPath = RecruitOp(None)
      def noPath  = TravelOp(source = None, target = None, maxAttempts = None, adjacentOnly = false)
      def condition(ops: Int) = botRecruitPossible(muslimWithCadreOnly = false)
    }

    object CellInGoodFairWhereJSP extends OperationDecision {
      def desc = "Cells in Good or Fair Muslim where Jihad Success Possible?"
      def yesPath = MinorJihadOp
      def noPath  = PoorNeedCellsforMajorJihadDecision
      def condition(ops: Int) = {
        game hasMuslim { m =>
          !m.truce &&
          m.jihadOK &&
          minorJihadGovTest(m) &&
          minorJihadSuccessPossible(m) &&
          totalUnused(m, includeSadr = true) > 0  // Standard Bot includes Sadr
        }
      }
    }

    // This object incorporates two boxes on the Flowchart
    // Finds poor countries in need of cells for major jihad,
    // Finds the highest priority travel destination among them and checks
    // to see if there is a cell in an adjacent country.
    object PoorNeedCellsforMajorJihadDecision extends OperationDecision {
      def desc = "Poor Muslim w/ 1-4 more cells than TandM & Jihad Success Possible?"
      def yesPath = TravelOp(source = None, target = None, maxAttempts = None, adjacentOnly = false)
      def noPath  = FundingModerateDecision
      def condition(ops: Int) = {
        val candidates = game.getMuslims(game.jihadTargets)
          .filter(m => poorMuslimNeedsCellsForMajorJihad(m) && canAdjacentTravelTo(m))
        travelToTarget(countryNames(candidates)).nonEmpty
      }
    }

    object FundingModerateDecision extends OperationDecision {
      def desc = "Funding Moderate?"
      def yesPath = PrestigeOver1AndActiveCellWithTroopsDecision
      def noPath  = CellAvailableOrTravelDecision
      def condition(ops: Int) = game.fundingLevel == Moderate
    }

    object PrestigeOver1AndActiveCellWithTroopsDecision extends OperationDecision {
      def desc = "Prestige > 1 and Active cell with Troops?"
      def yesPath = PlotOpPrestige
      def noPath  = CellAvailableOrCellInNonMuslimDecision
      def condition(ops: Int) =
        game.prestige > 1 &&
        (game.hasMuslim(m => !m.truce && activeCells(m) > 0 && m.totalTroopsThatAffectPrestige > 0))
    }

    object CellAvailableOrCellInNonMuslimDecision extends OperationDecision {
      def desc = "Cells Available and Recruit possible? (3)"
      def yesPath = RecruitOp(None)
      def noPath  = CellInNonMuslim
      def condition(ops: Int) = botRecruitPossible(muslimWithCadreOnly = false)
    }

    object CellInNonMuslim extends OperationDecision {
      def desc = "Cell in Non-Muslim?"
      def yesPath = PlotOpFunding
      def noPath  = TravelOp(source = None, target = None, maxAttempts = None, adjacentOnly = false)
      def condition(ops: Int) = game.hasNonMuslim(n => totalUnused(n, includeSadr = true) > 0)
    }
  }

  val EnhUnmarkedNonMuslimTravelPriorities = List(
    new HighestScorePriority("Highest Governance Value", _.governance),
    new CriteriaFilter("Caucasus", _.name == Caucasus),
    new CriteriaFilter("Russia", _.name == Russia),
    new CriteriaFilter("Philippines", _.name == Philippines),
    new CriteriaFilter("Thailand", _.name == Thailand),
    SchengenPriority,
  )

  // ------------------------------------------------------
  // EvO table for enhanced Bot as designed
  // by Florian Ottich
  // ------------------------------------------------------
  object EnhancedEvoTable {
    // This is the starting point of the Operations Flowchart
    //
    // This is a special action taken by the Bot if all
    // cells are currently on the track.  (The Bot does not lose when this happens)
    // The enhanced Bot will not take a normal action.  Instead it will
    // PLACE a number of cells on the map determined by the number of Ops.
    // It will use the Random Muslim Table to determine the destinations and
    // will place one cell in each.
    object NoCellsOnMapDecision extends OperationDecision {
      def desc = "No cells on the map?"
      def yesPath = PlaceRandomCells
      def noPath  = MajorJihadInPoorDecision
      def condition(ops: Int) = game.totalCellsOnMap == 0
    }

    object MajorJihadInPoorDecision extends OperationDecision {
      def desc = "Major Jihad Success possible at Poor (regardless of Ops)?"
      def yesPath = MajorJihadInPoorDesireableDecision
      def noPath  = TravelToAvoidEasyPrestigeGain
      def condition(ops: Int) = majorJihadPoorMuslimTargets(3).nonEmpty // regardless of Ops available
    }

    object MajorJihadInPoorDesireableDecision extends OperationDecision {
      // To ensure that majorJihadOperation() chooses the same target
      // we pass along the target that we used to determine that the
      // condition was met.
      var designatedTarget: Option[String] = None
      def desc = "Major Jihad Success possible at Poor (besieged regime or sufficient Ops)?"
      def yesPath = MajorJihadOp(designatedTarget)
      def noPath  = AddToReservesOp
      // Note: ops already accounts for any availabe reserves
      def condition(ops: Int) =  {
        designatedTarget = majorJihadTarget(majorJihadPoorMuslimTargets(ops))
        designatedTarget.nonEmpty
      }
    }

    object TravelToAvoidEasyPrestigeGain extends OperationDecision {
      import awakening.scenarios.{ FallOfISIL, TrumpTakesCommand, IslamicStateOfIraq }
      val targetScenarios = Set(FallOfISIL.name, TrumpTakesCommand.name, IslamicStateOfIraq.name)
      def desc = "Travel single cell from Afghanistan to avoid easy Prestige Gain?"
      def yesPath = {
        val die = getDieRoll(s"Enter travel destination die roll (1-2: Central Asia, 3-4: Iran, 5-6: Pakistan): ")
        botLog(s"Travel destination die roll: $die")
        val dest = if (die < 3)
          CentralAsia
        else if (die < 5)
          Iran
        else
          Pakistan

        TravelOp(source = Some(Afghanistan), target = Some(dest), maxAttempts = Some(1), adjacentOnly = true)
      }
      def noPath  = MissionAccomplishedAbuSayyaf
      def condition(ops: Int) = {
        val afghan = game.getMuslim(Afghanistan)
        targetScenarios.contains(game.scenarioName) &&
        afghan.cells == 1 &&
        afghan.totalTroopsThatAffectPrestige > 0 &&
        !afghan.autoRecruit
      }
    }

    // Special case for Mission Acccomplished scenario when
    // Abu Sayyaf is in effect.
    object MissionAccomplishedAbuSayyaf extends OperationDecision {
      def desc = "Abu Sayyaf, 1 cell, 2+ troops in Philippines?"
      def yesPath = {
        val indonesia = game.getMuslim(IndonesiaMalaysia)
        val target = if (countryEventNotInPlay(UnitedStates, PatriotAct) && lapsingEventNotInPlay(Biometrics)) {
          UnitedStates
        }
        else if (indonesia.totalTroops == 0 || majorJihadPriorityCountry == Some(IndonesiaMalaysia))
          IndonesiaMalaysia
        else
          Thailand

        TravelOp(source = Some(Philippines), target = Some(target), maxAttempts = None, adjacentOnly = true)
      }
      def noPath  = CellInGoodFairWhereJSP
      def condition(ops: Int) = {
        import awakening.scenarios.MissionAccomplished
        val philippines = game.getNonMuslim(Philippines)

        game.scenarioName == MissionAccomplished.name &&
        philippines.hasMarker(AbuSayyaf) &&
        philippines.cells == 1 &&
        philippines.totalTroops > 1
      }
    }

    object CellInGoodFairWhereJSP extends OperationDecision {
      def desc = "Cells in Good or Fair* Muslim where Jihad Success Possible?"
      def yesPath = MinorJihadOp
      def noPath  = LabScenarioAndIRResEqualsOne
      def condition(ops: Int) = {
        game hasMuslim { m =>
          !m.truce &&
          m.jihadOK &&
          minorJihadGovTest(m) &&
          minorJihadSuccessPossible(m) &&
          totalUnused(m, includeSadr = false) > 0  // Enhanced Bot does NOT include Sadr
        }
      }
    }

    // We have some special EvO nodes that are used only with the Let's Roll and
    // You Can Call Me Al scenarios:
    object LabScenarioAndIRResEqualsOne extends OperationDecision {
      def desc = "Let's Roll/You can call me Al and IR resources = 1?"
      def yesPath = CanAdjacentTravelToUnmarkedCentralAsia
      def noPath  = TravelToUnmarkedNonMuslim
      def condition(ops: Int) = {
        import awakening.scenarios.{ LetsRoll, YouCanCallMeAl }
        (game.scenarioName == LetsRoll.name || game.scenarioName == YouCanCallMeAl.name) &&
        currentIRResources == 1
      }
    }

    object CanAdjacentTravelToUnmarkedCentralAsia extends OperationDecision {
      def desc = "Central Asia unmarked and moveable adjacent cells?"
      def yesPath = {
        val die = getDieRoll(s"Enter die roll (1-2: consider Pakistan, 3-4: Travel 1 cell to C. Asia, 5-6: GWOT travel): ")
        botLog(s"Central Asia die roll: $die")
        if (die < 3)
          CanAdjacentTravelToFairPakistan
        else if (die < 5)
          TravelOp(source = None, target = Some(CentralAsia), maxAttempts = Some(1), adjacentOnly = true)
        else
          TravelToUnmarkedNonMuslim
      }
      def noPath  = {
        val die = getDieRoll(s"Enter die roll (1-3 = GWOT travel): ")
        botLog(s"Pakistan die roll: $die")
        if (die < 4)
          TravelToUnmarkedNonMuslim
        else
          CanAdjacentTravelToFairPakistan
      }
      def condition(ops: Int) = {
        val centralAsia = game.getMuslim(CentralAsia)
        !centralAsia.truce && centralAsia.isUntested && canAdjacentTravelTo(centralAsia)
      }
    }

    object CanAdjacentTravelToFairPakistan extends OperationDecision {
      def desc = "Pakistan Fair, no troops/FATA/Bhutto and moveable adjacent cells?"
      def yesPath = TravelOp(source = None, target = Some(Pakistan), maxAttempts = None, adjacentOnly = true)
      def noPath  = TravelToUnmarkedNonMuslim
      def condition(ops: Int) = {
        val pakistan = game.getMuslim(Pakistan)
        !pakistan.truce &&
        pakistan.isFair &&
        pakistan.totalTroops == 0 &&
        !pakistan.hasMarker(BenazirBhutto) &&
        !pakistan.hasMarker(FATA) &&
        canAdjacentTravelTo(pakistan)
      }
    }

    object TravelToUnmarkedNonMuslim extends OperationDecision {
      var designatedTarget: Option[String] = None
      var adjacentOnly: Boolean = false
      def desc = "US Hard, prestige>low, GWOT penalty is 0, Hard - Soft countries <2, and can travel to Umarked Non-Muslim?"
      def yesPath = TravelOp(source = None, target = designatedTarget, maxAttempts = Some(1), adjacentOnly = adjacentOnly)
      def noPath  = HaveAutoRecruitPriorityDecision
      def condition(ops: Int) = {
        val biometrics = lapsingEventInPlay(Biometrics)
        val isCandidate = (n: NonMuslimCountry) => n.isUntested && (!biometrics || !n.isGood)
        val adjacentCandidates = game.nonMuslims
          .filter { n: NonMuslimCountry => isCandidate(n) && canAdjacentTravelTo(n) }
        val candidates = game.nonMuslims
          .filter { n: NonMuslimCountry => isCandidate(n) && canTravelTo(n) }

        val condtionMet =
          game.usPosture == Hard &&
          game.prestige > 3 &&
          game.gwotPenalty == 0 &&
          game.hardSoftDelta < 2 &&
          candidates.nonEmpty

        if (condtionMet) {
          adjacentCandidates match {
            case Nil =>
              adjacentOnly = false
              botLog("No candidates can be reached by an adjacent traveling cell")
              designatedTarget = topPriority(candidates, EnhUnmarkedNonMuslimTravelPriorities).map(_.name)
            case _ =>
                adjacentOnly = true
                botLog("Considering only targets that can be reached by an adjacent traveling cell")
              designatedTarget = topPriority(adjacentCandidates, EnhUnmarkedNonMuslimTravelPriorities).map(_.name)
          }
          true
        }
        else
          false
      }
    }

    object HaveAutoRecruitPriorityDecision extends OperationDecision {
      def desc = "Have Auto-Recruit Priority country with < 3 cells?"
      def yesPath = RecruitInAutoRecruitPriorityDecision
      def noPath  = FundingBelow7Decision
      def condition(ops: Int) =
        PriorityCountries.autoRecruitPrioritySet &&  // To avoid infinitely looping
        autoRecruitPriorityCountry.exists(name => !underTruce(name) && game.getCountry(name).totalCells < 3)
    }

    object RecruitInAutoRecruitPriorityDecision extends OperationDecision {
      def desc = s"Can recruit in Auto-Recruit priority (${autoRecruitPriorityCountry.getOrElse("ERROR")})?"
      def yesPath = RecruitOp(autoRecruitPriorityCountry)
      def noPath  = AdjacentTravelToAutoRecruitPriorityDecision
      def condition(ops: Int) = autoRecruitPriorityCountry match {
        case None => false
        case Some(name) =>
          game.recruitPossible &&
          botRecruitTargets(muslimWithCadreOnly = false).contains(name)
      }
    }

    object AdjacentTravelToAutoRecruitPriorityDecision extends OperationDecision {
      def desc = s"Can travel to Auto-Recruit priority (${autoRecruitPriorityCountry.getOrElse("ERROR")} from ajacent?)"
      def yesPath = TravelOp(source = None, target = autoRecruitPriorityCountry, maxAttempts = None, adjacentOnly = true)
      def noPath  = FundingBelow7Decision
      def condition(ops: Int) = autoRecruitPriorityCountry.exists(canAdjacentTravelTo)
    }


    object RecruitOrEnhancedTravelDecision extends OperationDecision {
      def desc = "Cells Available and Recruit possible? (or Travel Poor/Unmarked)"
      def yesPath = RecruitOp(None)
      def noPath  = EnhancedTravelDecision
      def condition(ops: Int) = botRecruitPossible(muslimWithCadreOnly = false)
    }

    object EnhancedTravelDecision extends OperationDecision {
      def desc = "Can travel to Poor/Unmarked where Major JSP and no TandM?"
      def yesPath = EnhancedTravelOp
      def noPath  = Radicalization
      def condition(ops: Int) = enhancedTravelCandidates.nonEmpty
    }

    object FundingBelow7Decision extends OperationDecision {
      def desc = "Funding < 7?"
      def yesPath = PlotOpFunding
      def noPath  = Funding7AndNoCardsInHandDecision
      def condition(ops: Int) = game.funding < 7
    }

    object PrestigeAboveLowAndActiveCellWithTroopsDecision extends OperationDecision {
      def desc = "Prestige > 3 and cell(s) with Troops in Poor country?"
      def yesPath = PlotOpPrestige
      def noPath  = MJP_IsAutoRecruitDecsion
      def condition(ops: Int) =
        game.prestige > 3 &&
        (game.hasCountry(c => !c.truce && c.isPoor && unusedCells(c) > 0 && c.totalTroopsThatAffectPrestige > 0))
    }

    object CellsAvailableOrPlotDecision extends OperationDecision {
      def desc = "Cells Available and Recruit possible? (or Plot)"
      def yesPath = RecruitOp(None)
      def noPath  = PlotOpFunding
      def condition(ops: Int) = botRecruitPossible(muslimWithCadreOnly = false)
    }

    object Funding7AndNoCardsInHandDecision extends OperationDecision {
      def desc = "Funding = 7 and no more Jihadist cards in hand?"
      def yesPath = PlotOpFunding
      def noPath  = PrestigeAboveLowAndActiveCellWithTroopsDecision
      def condition(ops: Int) =
        game.funding == 7 && numCardsInHand(Jihadist) == 0
    }

    // This is starts the last decsion tree for the Enhanced EvO
    // The goal is to get cells to the "Major Jihad Priority" country (MJP)
    // - If MJP is an auto-recruit country then we first try to recruit cells
    //   but it that is not possible then we try to travel ADJACENT cells ONLY to there.
    //   If that fails then we try to travel any cells to there.
    //
    // - If MJP is not an auto-recruit country, then we will first try to
    //   travel ADJACENT cells ONLY to there.
    //   Failing that we will try to recruit first, then travel with non-adjacent cells.
    //
    //   In both cases, if no recruit/travel was possible we then try to recruit in
    //   the auto-recruit priority country and if that is not possible
    //   as a last resort we will do Radicalization.
    object MJP_IsAutoRecruitDecsion extends OperationDecision {
      def desc = s"Major Jihad priority (${majorJihadPriorityCountry.getOrElse("None")}) is auto-recruit?"
      def yesPath = MJP_Recruit_AdjacentTravel_Decision
      def noPath  = MJP_AdjacentTravel_Recruit_Decision
      def condition(ops: Int) = majorJihadPriorityCountry.map(game.getCountry).exists(m => !m.truce && m.autoRecruit)
    }

    // MJP Auto-recruit #1
    object MJP_Recruit_AdjacentTravel_Decision extends OperationDecision {
      def desc = s"Can recruit in Major Jihad priority (${majorJihadPriorityCountry.getOrElse("None")})?"
      def yesPath = RecruitOp(majorJihadPriorityCountry)
      def noPath  = MJP_AdjacentTravel_OtherTravel_Decision
      def condition(ops: Int) = majorJihadPriorityCountry match {
        case None => false
        case Some(name) =>
          game.recruitPossible &&
          botRecruitTargets(muslimWithCadreOnly = false).contains(name)
      }
    }

    // MJP Auto-recruit #2
    object MJP_AdjacentTravel_OtherTravel_Decision extends OperationDecision {
      def desc = s"Adjacent travel to Major Jihad priority (${majorJihadPriorityCountry.getOrElse("None")}) possible?"
      def yesPath = TravelOp(source = None, target = majorJihadPriorityCountry, maxAttempts = None, adjacentOnly = true)
      def noPath  = MJP_OtherTravel_AutoRecruitPriority_Decision
      def condition(ops: Int) = majorJihadPriorityCountry.exists(canAdjacentTravelTo)
    }

    // MJP Not Auto-recruit #1
    object MJP_AdjacentTravel_Recruit_Decision extends OperationDecision {
      def desc = s"Adjacent travel to Major Jihad priority (${majorJihadPriorityCountry.getOrElse("None")}) possible?"
      def yesPath = TravelOp(source = None, target = majorJihadPriorityCountry, maxAttempts = None, adjacentOnly = true)
      def noPath  = MJP_Recruit_OtherTravel_Decision
      def condition(ops: Int) = majorJihadPriorityCountry.exists(canAdjacentTravelTo)
    }

    // MJP Not Auto-recruit #2
    object MJP_Recruit_OtherTravel_Decision extends OperationDecision {
      def desc = s"Can recruit in Major Jihad priority (${majorJihadPriorityCountry.getOrElse("None")})?"
      def yesPath = RecruitOp(majorJihadPriorityCountry)
      def noPath  = MJP_OtherTravel_AutoRecruitPriority_Decision
      def condition(ops: Int) = majorJihadPriorityCountry match {
        case None => false
        case Some(name) =>
          game.recruitPossible &&
          botRecruitTargets(muslimWithCadreOnly = false).contains(name)
      }
    }

    // MJP (Auto-recruit AND Not Auto-recruit ) #3
    object MJP_OtherTravel_AutoRecruitPriority_Decision extends OperationDecision {
      def desc = s"Any travel to Major Jihad priority (${majorJihadPriorityCountry.getOrElse("None")}) possible?"
      def yesPath = TravelOp(source = None, target = majorJihadPriorityCountry, maxAttempts = None, adjacentOnly = false)
      def noPath  = RecruitInAutoRecruitPriority_Decision
      def condition(ops: Int) = majorJihadPriorityCountry.exists(name => canTravelTo(name, autoTravel = false))
    }

    // MJP (Auto-recruit AND Not Auto-recruit ) #4
    object RecruitInAutoRecruitPriority_Decision extends OperationDecision {
      def desc = s"Recruit in Auto-Recruit priority (${autoRecruitPriorityCountry.getOrElse("None")}) possible?"
      def yesPath = RecruitOp(autoRecruitPriorityCountry)
      def noPath  = Radicalization
      def condition(ops: Int) = autoRecruitPriorityCountry match {
        case None => false
        case Some(name) =>
          game.recruitPossible &&
          botRecruitTargets(muslimWithCadreOnly = false).contains(name)
      }
    }
  }

  def yesNo(result: Boolean) = if (result) "YES" else "NO"

  // Follow the operations flowchart (EvO) to pick which operation will be performed.
  def operationsFlowchart(ops: Int): Operation = {
    @tailrec def evaluateNode(node: OpFlowchartNode): Operation = node match {
      case operation: Operation =>
        operation
      case decision: OperationDecision =>
        val decisionSuccess = decision.condition(ops)
        botLog(s"EvO Flowchart: $node ${yesNo(decisionSuccess)}", Color.Debug)
        if (decisionSuccess)
          evaluateNode(decision.yesPath)
        else
          evaluateNode(decision.noPath)
    }
    val startingNode = if (game.botEnhancements)
      EnhancedEvoTable.NoCellsOnMapDecision
    else
      StandardEvoTable.MajorJihadDecision
    botLog(s"Evaluate EvO Flowchart using $ops available Ops (including reserves)", Color.Debug)
    evaluateNode(startingNode)
  }



  // ------------------------------------------------------------------
  def posturePriority(names: List[String]): Option[String] = {
    val priorities = List(
      new CriteriaFilter("Same posture as US",
        nonMuslimTest(n => n.isTested && n.canChangePosture && n.posture == game.usPosture)),
      new CriteriaFilter("Untested Non-Muslim", nonMuslimTest(_.isUntested)),
      HighestGovernance,
      )
    topPriority(game getNonMuslims names, priorities).map(_.name)
  }

  // ------------------------------------------------------------------
  def goodPriority(names: List[String]): Option[String] = {
    val priorities = GoodPriority::Nil
    botLog("Find \"Good Priority\" target", Color.Debug)
    topPriority(game.getCountries(names), priorities).map(_.name)
  }

  def goodThenFairThenPoorPriority(names: List[String]): Option[String] = {
    val priorities = GoodPriority::FairPriority::PoorPriority::Nil
    botLog("Find \"Good/Fair/Poor Priority\" target", Color.Debug)
    topPriority(game.getCountries(names), priorities).map(_.name)
  }

  def criticalMiddleShiftPossibilities(names: List[String]): List[String] = {
    val flowchart = List(
      new CriteriaFilter("Not Adversary", muslimTest(m => !m.isAdversary)),
    )
    botLog("Find \"Critical Middle\" target", Color.Debug)
    countryNames(selectCandidates(game.getCountries(names), flowchart))
  }

  def unCeasefireTarget(names: List[String]): Option[String] = {
    val priorities = List(
      new HighestScorePriority("Most cells - militia", muslimScore(m => m.totalCells - m.militia)),
      new CriteriaFilter("Ally",    muslimTest(m => m.isAlly)),
      new CriteriaFilter("Neutral", muslimTest(m => m.isNeutral)))

    botLog("Find \"UN Ceasefire\" target", Color.Debug)
    topPriority(game.getCountries(names), priorities).map(_.name)
  }

  def revolutionTarget(names: List[String]): Option[String] = {
    val priorities = List(
      new HighestScorePriority("Highest awakening - reaction", muslimScore(_.awakeningDelta)),
      HighestResourcePriority)

    botLog("Find \"Revolution\" target", Color.Debug)
    topPriority(game.getCountries(names), priorities).map(_.name)
  }

  //  To select from which country adjacent to Syria
  //  with cells to move cells from.
  def hayatTahirTarget(names: List[String]): Option[String] = {
    val priorities = List(
      new CriteriaFilter("With troops", muslimTest(m => m.totalTroops > 0)),
      new HighestScorePriority("Largest Cells - TandM", muslimScore(m => m.totalCells - m.totalTroopsAndMilitia)))

    botLog("Best \"Hayat Tahir\" adjacent with cells", Color.Debug)
    topPriority(game.getCountries(names), priorities).map(_.name)
  }

  // Pick actives before sleepers
  // Return (actives, sleepers, sadr)
  def chooseCellsToRemove(name: String, num: Int): (Int, Int, Boolean) = {
    if (num == 0)
      (0, 0, false)
    else {
      val c = game getCountry name
      val actives   = num min c.activeCells
      val sleepers  = (num - actives) min c.sleeperCells
      val sadr      = c.hasSadr && (num - actives - sleepers > 0)
      (actives, sleepers, sadr)
    }
  }

  // Returns "troop-cube", "militia-cube", or the name of a troop marker
  def chooseTroopOrMilitiaToRemove(name: String): TroopOrMilitia = {
    val c = game.getCountry(name)
    if (c.troopsMarkers.nonEmpty)
      TroopMarker(c.troopsMarkers.sorted.reverse.head.name)
    else if (c.troops > 0)
      TroopCube
    else if (game.isMuslim(name) && game.getMuslim(name).militia > 0)
      MilitiaCube
    else
      throw new IllegalStateException(s"JihadistBot.chooseTroopOrMilitiaToRemove($name) not units present")
  }

  // Enhanced Bot
  // The Bot will declare the Caliphate if results in an auto win or
  // if country is not at Fair governance.
  //
  // Normal Bot
  // The Bot will declare the Caliphate if it results in an auto win,
  // or if there is at least one other adjacent country that qualifies to be part
  // of the Caliphate.
  def willDeclareCaliphate(capital: String): Boolean = if (game.botEnhancements)
    canDeclareCaliphate(capital) &&
    (game.islamistResources == 5 || !game.getCountry(capital).isFair)
  else
    canDeclareCaliphate(capital) &&
    (game.islamistResources == 5 || game.caliphateDaisyChain(capital).size > 1)

  def caliphatePriorityTarget(names: List[String]): Option[String] = {
    val priorities = List(
      new HighestScoreNode(
        "IR w/ highest printed resource*",
        _.isIslamistRule,
        muslimScore(enhBotResourceValue)),
      new HighestScoreNode(
        "Poor Civil War w/ highest resource value",
        muslimTest(m => m.isPoor && m.civilWar),
        muslimScore(enhBotResourceValue)),
      new HighestScoreNode(
        "Poor Regime Change w/ highest printed resource*",
        muslimTest(m => m.isPoor && m.inRegimeChange),
        muslimScore(enhBotResourceValue)),
    ) ::: recruitAndTravelToPriorities

    if (game.caliphateDeclared)
      None
    else {
      botLog("Find top priority caliphate target", Color.Debug)
      val candidates = game.getCountries(names.filter(willDeclareCaliphate))
      topPriority(candidates, priorities).map(_.name)
    }
  }

  def possibleToDeclareCaliphate(candidates: List[String]): Boolean =
    !game.caliphateDeclared &&
    game.cellsAvailable >= 3 &&
    candidates.exists(willDeclareCaliphate)

  // Find target for cell placement by event
  def cellPlacementPriority(canDeclareCaliphate: Boolean)(candidates: List[String]): Option[String] = {
    val caliphatePossible =
      canDeclareCaliphate && possibleToDeclareCaliphate(candidates)
    if (caliphatePossible)
      JihadistBot.caliphatePriorityTarget(candidates)
    else
      JihadistBot.recruitTravelToPriority(candidates)
  }

  // Note: this does not test for presence of a cadre, because
  // some events want do know where to NOT place a cadre that will
  // then be removed.
  val isCadreRemovalCandidate = (country: Country) => {
      val WireTapping = Set(UnitedStates, UnitedKingdom, Canada)
      country match {
        case m: MuslimCountry =>
          ((m.isGood || m.isFair) && !m.autoRecruit) ||
          (m.isFair && m.autoRecruit && !game.isTrainingCamp(m.name))

        case n: NonMuslimCountry if game.startingMode == LabyrinthMode =>
          n.name match {
            case Philippines =>
              true
            case name if WireTapping(name) && globalEventNotInPlay(LeakWiretapping) =>
              true
            case _ =>
              false
          }

        case _ =>
          false
      }

  }

  // Enhance bot rule:
  // The Jihadist Bot will voluntarily remove cadres
  // to make it harder for the US player to disrupt for
  // prestige or to execute the Wiretapping Event.
  // Cadres are removed:
  // - In Muslim countries at Good/Fair that are NOT auto-recruit countries.
  // - In Fair Auto-Recruit countries (unless Training Camps is present)
  // - In Philippines (to prevent disrupt if Abu Sayyaf marker is or becomes present).
  // - US, UK, Canada (unless Wiretapping has been blocked by Leak)
  // Note: Abu Sayyaf and Wiretapping are only available in Labyrinth games / campaigns.
  // Returns true if at least one cadres is removed
  def voluntaryCadreRemoval(): Unit = {
    if (game.botEnhancements)
      game.countries.filter(c=> c.hasCadre && isCadreRemovalCandidate(c)) match {
        case Nil =>
        case candidates =>
          log(s"\n$Jihadist voluntary cadre removal", Color.Info)
          log(separator(), Color.Info)
          val total = candidates.map(_.cadres).sum
          for (country <- candidates)
            removeCadresFromCountry(country.name, country.cadres)
          game = game.copy(turnActions = VoluntaryCadreRemoval(total)::game.turnActions)
          saveGameState()
          pause()
      }
  }

  // The enhanced Bot will spend funding to end a Truce
  // if the Truce is in the Major Jihad Priority Country
  def endTruceInMajorJihadPriority(): Unit = {
    if (game.botEnhancements)
      majorJihadPriorityCountry
        .filter(underTruce)
        .foreach { name =>
          // The bot must have enough funding to cover the
          // countries current resource value.
          val resValue = game.getMuslim(name).resourceValue
          if (game.funding >= resValue) {
            log(s"\nThe $Jihadist chooses to end the TRUCE in $name.")
            decreaseFunding(resValue)
            removeTruceMarker(name)
            pause()
          }
        }
  }

  def maxOpsPlusReserves(card: Card): Int = (card.ops + game.reserves.jihadist) min 3

  // Decrement the Bots reserves and log that they were used.
  def expendBotReserves(ops: Int): Unit = {
    if (ops > 0) {
      assert(game.reserves.jihadist >= ops,
         s"expendBotReserves($ops): Only ${opsString(game.reserves.jihadist)} in reserve")
     game = game.copy(reserves = game.reserves.copy(jihadist = game.reserves.jihadist - ops))
     log(s"\n$Jihadist expends ${opsString(ops)} from reserves.  Reserves now ${opsString(game.reserves.jihadist)}")
    }
  }


  def resetStaticData(): Unit = {
    usedCells.clear()
    PriorityCountries.clear()
    clearCachedTargets()
    // Select the MJP and ARP up front
    autoRecruitPriorityCountry
    majorJihadPriorityCountry
  }

  def performTriggeredEvent(card: Card): Unit = {
    resetStaticData()
    performCardEvent(card, Jihadist, triggered = true)
  }

  // Perform a travel operation to the given destination
  // traveling 1 cell from an adjacent country.
  // We will use a "moveable" cell if possible, but will use
  // a non-"moveable" cell if necesary.
  //
  // This function is used by the Enhanced Bot.
  // Returns the number of Ops used: 1 or 0 if there are no
  // adjacent cells.
  def performPriorityAdjacentTravel(destination: String): Int = {
    val sourcesWithMoveableCells = game.adjacentCountries(destination)
      .filter(c => hasCellForTravel(c, destination))
      .map(_.name)
    val sourcesWithCells = game.adjacentCountries(destination)
      .filter(_.cells > 0)
      .map(_.name)

    val sourceName = if (sourcesWithMoveableCells.nonEmpty)
      travelFromPriorities(destination, sourcesWithMoveableCells)
    else if (sourcesWithCells.nonEmpty)
      travelFromPriorities(destination, sourcesWithCells)
    else
      None
  
    sourceName
      .map { source =>
        val attempt = TravelAttempt(source, destination, game.getCountry(source).activeCells > 0)
        log(s"\n$Jihadist performs a Travel operation (from adjacent countries)")
        log(separator())
        val (_, success)::Nil = performTravels(attempt::Nil)
        if (success)
          usedCells(destination).addSleepers(1)
        1 // 1 Op used
      }
      .getOrElse(0)
  }


  // For the Enhanced Bot the cardPlay() function will sometimes receive
  // a parametere dictating how the card should be used.
  sealed trait CardUsage
  // Go through the normal process of Event if possible then EvO...
  case object UseCardNormally extends CardUsage
  // Use the card for its event
  case object UseCardForEvent extends CardUsage
  // Use the card for Ops
  case object UseCardForOps extends CardUsage
  // A special adjacent travel operation where we travel a single
  // cell using a "non-moveable" cell if necessary.
  case class UseCardForPriorityAdjacentTravel(destination: String) extends CardUsage

  case class CardWithUsage(cardNum: Int, usage: CardUsage)
  type UsageList = List[CardWithUsage]

  // This is used when the Enhanced Bot has two cards to play during an action
  // phase.  This function makes some decisions on how to best play the two cards.
  def determineCardUsage(cardNums: List[Int]): UsageList = {
    assert(cardNums.size == 2, "determineCardUsage() requires exactly two card numbers")
    val Fatwa = 97
    val Musharraf = 108
    val JayshAlMahdi = 106
    val Martyrdom = Set(87, 88, 89, 190, 316)
    def otherCardNum(cardNum: Int) = if (cardNums.head == cardNum) cardNums.last else cardNums.head
    val eventsBlocked =
      lapsingEventInPlay(TheDoorOfItjihad) ||  // Blocks all Non-US events
      lapsingEventInPlay(FakeNews)             // Blocks next non-auto event

    def eventPlayable(card: Card) = !eventsBlocked && card.eventIsPlayable(Jihadist)

    // If either of of the events can be played to win the game then
    // play that event first.
    def gameWinner: Option[UsageList] = cardNums
      .map(n => deck(n))
      .find(card => eventPlayable(card) && card.eventWouldResultInVictoryFor(Jihadist))
      .map(card => List(CardWithUsage(card.number, UseCardForEvent), CardWithUsage(otherCardNum(card.number), UseCardNormally)))

    def hasAdjacentCell(name: String) =
      lapsingEventNotInPlay(Biometrics) &&
      game.adjacentCountries(name).exists(c => !c.truce && c.cells > 0)
      
    // If either card is #108 Musharraf and Pakistan is Good and Benazir Bhutto is not present:
    // 1. The play Musharraf first if possible (cells present in Pakistan).
    // 2. If there is a cell adjacent to Pakistan, then use first card to travel the cell
    //    to Pakistan (even if not "moveable"), then play Musharraf event second
    //    Not if Biometrics is active
    // 3. Play other card normally (Ops only if #97 Fatwa), then Musharraf second.
    def musharraf: Option[UsageList] = {
      val pakistan = game.getMuslim(Pakistan)
      cardNums
        .find(card => card == Musharraf && !eventsBlocked && !pakistan.truce && pakistan.isGood && !pakistan.hasMarker(BenazirBhutto))
        .map { card =>
          if (pakistan.cells > 0)
            List(CardWithUsage(card, UseCardForEvent), CardWithUsage(otherCardNum(card), UseCardNormally))
          else if (hasAdjacentCell(Pakistan))
            List(CardWithUsage(otherCardNum(card), UseCardForPriorityAdjacentTravel(Pakistan)), CardWithUsage(card, UseCardForEvent))
          else if (otherCardNum(card) == Fatwa)
            List(CardWithUsage(otherCardNum(card), UseCardForOps), CardWithUsage(card, UseCardNormally))
          else
            List(CardWithUsage(otherCardNum(card), UseCardNormally), CardWithUsage(card, UseCardNormally))
        }
    }

    // If either card is #106 Jaysh al-Mahdi and troops are present in a Good 2+ res* Shia-Mix country:
    // 1. The play Jaysh al-Mahdi first if possible (candidate country has a cell).
    // 2. If there is a cell adjacent to a canididate country, then use first card to travel the cell
    //    to to the country (even if not "moveable"), then play Jaysh al-Mahdi event second
    //    Not if Biometrics is active
    // 3. Play other card normally (Ops only if #97 Fatwa), then Jaysh al-Mahdi second.
    def jayshAlMahdi: Option[UsageList] = {
      val isCandidate = (m: MuslimCountry) =>
        !m.truce && m.isGood && m.isShiaMix && m.totalTroops > 0 && enhBotResourceValue(m) > 1
      val candidates = game.muslims.filter(isCandidate)
      val candidatesWithCells = candidates.filter(_.totalCells > 0)
      val candidatesWithAdjCells = candidates.filter(m => hasAdjacentCell(m.name))
      val priorities = List(HighestPrintedResourcePriority, AdjacentToMoveableCell)
      cardNums
        .find(card => card == JayshAlMahdi && !eventsBlocked && candidates.nonEmpty)
        .map { card =>
          if (candidatesWithCells.nonEmpty)
            List(CardWithUsage(card, UseCardForEvent), CardWithUsage(otherCardNum(card), UseCardNormally))
          else if (candidatesWithAdjCells.nonEmpty) {
            val target = topPriority(candidatesWithAdjCells, priorities).map(_.name).get
            List(CardWithUsage(otherCardNum(card), UseCardForPriorityAdjacentTravel(target)), CardWithUsage(card, UseCardForEvent))
          }
          else if (otherCardNum(card) == Fatwa)
            List(CardWithUsage(otherCardNum(card), UseCardForOps), CardWithUsage(card, UseCardNormally))
          else
            List(CardWithUsage(otherCardNum(card), UseCardNormally), CardWithUsage(card, UseCardNormally))
        }
    }

    // If either card is a Martyrdom event:
    // A: If two plots available: 
    // 1. If a cell in US or a Good 2+ res* country, play the event
    // 2. If a cell adjacent to US or Good 2+ res* country, then use first card to
    //    travel the cell to country (even if not "moveable"), then play Martyrdom event second
    //    Not if Biometrics is active
    // B: If Plot 3 or Plot WMD available
    //    Play other card normally (Ops only if #97 Fatwa), then Martyrdom second.

    def martyrdom: Option[UsageList] = {
      val bigPlot = game.availablePlots.exists(p => p == Plot3 || p == PlotWMD)
      val candidates = if (game.availablePlots.size > 1)
        UnitedStates :: game.muslims.filter(m => m.isGood && enhBotResourceValue(m) > 1).map(_.name)
      else
        Nil
      val candidatesWithCells = game.getCountries(candidates).filter(_.cells > 0)
      val candidatesWithAdjCells = game.getCountries(candidates).filter(c => hasAdjacentCell(c.name))
      val useIt = candidatesWithCells.nonEmpty || candidatesWithAdjCells.nonEmpty || bigPlot
      val USFilter = new CriteriaFilter("United States", _.name == UnitedStates)

      cardNums
        .find(num => Martyrdom(num) && !eventsBlocked && useIt)
        .map { card =>
          if (candidatesWithCells.nonEmpty)
            List(CardWithUsage(card, UseCardForEvent), CardWithUsage(otherCardNum(card), UseCardNormally))
          else if (candidatesWithAdjCells.nonEmpty) {
            val priorities = List(
              if (game.availablePlots.contains(PlotWMD)) Some(USFilter) else None,
              if (game.availablePlots.contains(Plot3)) Some(HighestPrintedResourcePriority) else None,
              Some(AdjacentToMoveableCell),
              Some(USFilter)
            ).flatten
            val target = topPriority(candidatesWithAdjCells, priorities).map(_.name).get
            
            List(CardWithUsage(otherCardNum(card), UseCardForPriorityAdjacentTravel(target)), CardWithUsage(card, UseCardForEvent))
          }
          else if (otherCardNum(card) == Fatwa)
            List(CardWithUsage(otherCardNum(card), UseCardForOps), CardWithUsage(card, UseCardNormally))
          else
            List(CardWithUsage(otherCardNum(card), UseCardNormally), CardWithUsage(card, UseCardNormally))
        }
    }

    gameWinner orElse
    musharraf orElse
    jayshAlMahdi orElse
    martyrdom getOrElse
    cardNums
      .map(num =>CardWithUsage(num, UseCardNormally))
  }

  sealed trait EventTriggerOption
  case object EventTriggerNone extends EventTriggerOption
  case object EventTriggerBefore extends EventTriggerOption
  case object EventTriggerAfter extends EventTriggerOption

  // Starting point for Jihadist bot card play.
  // The playable argument indicates if the
  def cardPlay(card: Card, usage: CardUsage): Unit = {
    // Carry an operation as determined by the operations flowchart
    // possibly followed by radicalization.
    def performOperation(): Unit = {
      val opsUsed = operationsFlowchart(maxOpsPlusReserves(card)) match {
        case RecruitOp(target) =>
          recruitOperation(card, target)
        case TravelOp(source, target, maxAttempts, adjOnly) =>
          travelOperation(card, source, target, maxAttempts, adjOnly)
        case PlotOpFunding =>
          plotOperation(card, prestigeFocus = false)
        case PlotOpPrestige =>
          plotOperation(card, prestigeFocus = true)
        case MinorJihadOp =>
          minorJihadOperation(card)
        case MajorJihadOp(target) =>
          majorJihadOperation(card, target)
        case PlaceRandomCells =>  // Enhanced bot only
          placeRandomCellsOnMap(card)
        case EnhancedTravelOp =>  // Enhanced bot only
          enhancedTravelOperation(card)
        case AddToReservesOp =>  // Enhanced bot only
          addToReservesOperation(card)
        case Radicalization =>
          0 // Enhanced bot only
      }

      if (opsUsed < card.ops)
        radicalization(card, opsUsed)
    }

    // Start of card play
    resetStaticData()
    val eventPlayable =
      lapsingEventNotInPlay(TheDoorOfItjihad) &&  // Blocks all Non-US events
      lapsingEventNotInPlay(FakeNews) && // Blocks next non-auto event
      card.eventIsPlayable(Jihadist)

    // True if a sucessful major jihad would win the game
    val wouldWinGame = (name: String) =>
      game.getMuslim(name).resourceValue + game.islamistResources >= 6

    if (game.botEnhancements) {
      sealed trait CardActivity
      case object PerformEvent extends CardActivity
      case object UseEvO extends CardActivity
      case object MajorJihadForWin extends CardActivity
      case class PriorityAdjacentTravel(target: String)

      // Determine the proper activity to use for this card
      val activity = usage match {
        case UseCardForEvent|UseCardNormally if (eventPlayable && card.eventWouldResultInVictoryFor(Jihadist)) =>
          botLog("Playing event because it can result in auto victory!")
          PerformEvent

        case UseCardForEvent =>
          botLog("Playing event (special event instructions)")
          PerformEvent

        case UseCardForPriorityAdjacentTravel(target) =>
          PriorityAdjacentTravel(target)
        
        case UseCardForOps|UseCardNormally if majorJihadPoorMuslimTargets(3).exists(wouldWinGame) =>
          botLog("Major Jihad could potentially win the game")
          MajorJihadForWin

        case UseCardNormally if eventPlayable && card.botWillPlayEvent(Jihadist) =>
          PerformEvent

        // UseCardNormall or UseCardForOps
        case UseCardNormally|UseCardForOps =>
          UseEvO
      }

      if (activity == PerformEvent)
        performCardEvent(card, Jihadist)
      else {
        val eventTriggerOption = if (card.association == US && (enhBotEasiest() || enhBotEasier())) {
          // If the event conditions are not currently met, then
          // the bot will carry out the event first since it will have
          // no effect.  If difficulty is medium and we will subtract from reserves,
          // then alwasy evaluate the event after conduction operations.
          if (card.eventConditionsMet(US) || (enhBotEasier() && game.reserves.jihadist > 0)) {
            log(s"\nThe $Jihadist Bot will evaluate the $US associated event after performing operations.", Color.Info)
            EventTriggerAfter
          }
          else {
            log(s"\nThe $Jihadist Bot will evaluate the $US associated event before performing operations.", Color.Info)
            EventTriggerBefore
          }
        }
        else
          EventTriggerNone

        // The Bot will execute auto trigger events first.
        if (card.autoTrigger) {
          performCardEvent(card, Jihadist)
          log()
        }

        if (eventTriggerOption == EventTriggerBefore) {
          if (performCardEvent(card, US, triggered = true)) {
            pause()
            log(s"\nOps on the \"${card.cardName}\" card are added to $Jihadist reserves.", Color.Info)
            addToReserves(Jihadist, card.printedOps)
            pause()
          }
        }

        activity match {
          case UseEvO =>
            performOperation()

          case PriorityAdjacentTravel(target) =>
            val opsUsed = performPriorityAdjacentTravel(target)
            if (opsUsed < card.ops)
              radicalization(card, opsUsed)

          case MajorJihadForWin =>
            // It is possible that we do not have enough Ops on the current
            // card + reserves to complete a Major Jihad to win the game.
            val candidates = majorJihadPoorMuslimTargets(maxOpsPlusReserves(card))
              .filter(wouldWinGame)
            majorJihadTarget(candidates) match {
              case None =>
                botLog("Major Jihad would win game, but not enough Ops on current card + reserves")
                addToReservesOperation(card)
              case target =>
                botLog("Performing Major Jihad to attempt winning the game")
                majorJihadOperation(card, target)
            }

          case PerformEvent =>
            throw new IllegalStateException("activity should never be 'PerformEvent' here!")
        }

        if (eventTriggerOption == EventTriggerAfter) {
          pause()
          if (enhBotEasier() && game.reserves.jihadist > 0) {
            // Its possible that the event conditions are no longer satisfied
            if (card.eventConditionsMet(US)) {
              log(s"\n$Jihadist reserves are not empty so the \"${card.cardName}\" event does not trigger.", Color.Info)
              log(s"Ops on the \"${card.cardName}\" card are removed from $Jihadist reserves.", Color.Info)
              subtractFromReserves(Jihadist, card.printedOps)
            }
            else
              log("\n%s event \"%s\" does not trigger. The event conditions are not satisfied. ".format(card.association, card.cardName))
          }
          else if (card.eventConditionsMet(US) && card.eventWouldResultInVictoryFor(US))
            log(s"\nThe \"${card.cardName}\" event will not trigger because it could result in an immediate $US victory.", Color.Info)
          else if (performCardEvent(card, US, triggered = true)) {
            pause()
            log(s"\nOps on the \"${card.cardName}\" card are added to $Jihadist reserves.", Color.Info)
            addToReserves(Jihadist, card.printedOps)
          }
        }
      }
    }
    else { // Standard Bot
      // If the event is playable then the event is always executed
      if (eventPlayable && card.botWillPlayEvent(Jihadist)) {
        performCardEvent(card, Jihadist)
        // If the card event is Unassociated add ops to the Bot's reserves.
        if (card.association == Unassociated)
          addToReserves(Jihadist, card.ops)
      }
      else {
        // US Elections is the only auto trigger event.
        // The Bot will execute the event first.
        if (card.autoTrigger) {
          performCardEvent(card, Jihadist)
          log()
        }

        // There is an unlikely, but possible chance that there are no cells or cadres on
        // the map. (The Bot does not lose when there are no cells on the map).
        // In this case the Bot cannot do anything except add Ops to reserves and wait
        // for an event that places a cell.
        if (!(game.hasCountry(c => c.totalCells > 0 || c.hasCadre))) {
          val opsAdded = card.ops min (2 - game.reserves.jihadist)
          log("There are no cells or cadres on the map.")
          log(s"The $Jihadist Bot cannot execute an operation until an event places a cell.")
          addToReserves(Jihadist, card.ops)
        }
        else
          performOperation()
      }
    }
  }

  def addToReservesOperation(card: Card): Int = {
    log()
    log(s"$Jihadist adds Ops to reserves")
    val opsAdded = card.ops min (2 - game.reserves.jihadist)
    addToReserves(Jihadist, opsAdded)
    opsAdded
  }

  def willCancelUSEventForFerguson(card: Card): Boolean = if (game.botEnhancements) {
    // If card has 3 Ops always block it.
    // If 1 Op or 2 Ops and US has at least two more cards in hand, roll a die: 1-3 = block the event, 4-6 do not block the event.
    // If 1 Ops or  2 Ops and US has one or zero cards in hand, block the event.
    // card.association == US &&
    lazy val die = getDieRoll(s"Enter die roll for Ferguson to cancel event (1-3) to block: ")
    card.ops == 3 || numCardsInHand(US) < 2 || die < 4
  }
  else
    true  // Standard Bot always cancels the first played US associated event

  // Attempt to recruit as many times as possible up to 3
  // If we run out of card ops and there are still cells available, use reserves
  // If we run out of cells and still have card ops (not reserves), then use the
  // excess card ops for radicalization.
  // Returns the number of Ops used.
  def recruitOperation(card: Card, optTarget: Option[String]): Int = {
    if (botRecruitPossible(muslimWithCadreOnly = false)) {
      val recruitOps = game.cellsToRecruit min maxOpsPlusReserves(card)
      log(s"\n$Jihadist performs a Recruit operation")
      log(separator())
      if (recruitOps > card.ops)
        expendBotReserves(recruitOps - card.ops)

      if (recruitOps > 0)
        performRecruit(recruitOps, optTarget)
      recruitOps
    }
    else
      0
  }

  def performRecruit(ops: Int, optTarget: Option[String], ignoreFunding: Boolean = false, madrassas: Boolean = false): Unit = {
    // We should always find a target for recruit operations.
    val target = optTarget orElse recruitTarget(botRecruitTargets(muslimWithCadreOnly = false)) getOrElse {
      throw new IllegalStateException("recruitOperation() should always find a target")
    }
    val c = game.getCountry(target)
    addOpsTarget(target)
    val results = for (i <- 1 to ops) yield {
      if (c.autoRecruit) {
        log(s"${ordinal(i)} Recruit automatically succeeds in $target")
        true
      }
      else {
        val die     = getDieRoll(s"Enter die roll for recruit in $target: ")
        val success = c.recruitSucceeds(die)
        val result  = if (success) "succeeds" else "fails"
        log(s"${ordinal(i)} Recruit $result in $target with a roll of $die")
        success
      }
    }
    val successes = results count (_ == true)
    val available = if (ignoreFunding) game.cellsAvailable else game.cellsToRecruit
    val numCells  = (if (game.jihadistIdeology(Potent)) (successes * 2) else successes) min available

    if (successes > 0 && game.jihadistIdeology(Potent))
      log(s"$Jihadist Bot with Potent Ideology places two cells for each success")

    addSleeperCellsToCountry(target, numCells)
    usedCells(target).addSleepers(numCells)
  }

  // Returns the maximum number of cells that the Bot
  // will allow to leave the country for "travel"
  //
  // Normally, all unused cells are available for travel
  // with the following exceptions:
  // - If the country is an auto-recruit country
  //   will not allow the last cell to travel unless there
  //   are two or more other auto-recruit countries.
  // - If the country contains the "Training Camps" marker
  //   will not allow the last cell to travel.
  // - Nigeria, If Nigeria is Muslim and is a Fair Ally with reaction - awakening < 2
  //   then we will not allow the last cell to travel (as this would
  //   cause Nigeria to revert to Non-Muslim)
  //
  // Since we must determine all travels up front before executing
  // any of them, the travelOperation() code must tell this
  // function how many previous attempts have been made so that
  // the county of auto-recruit countries with cells is accurate
  // `placement` should be true if this is used by and event that can
  // place cell from anywhere on the map.  In this case, Travel Ban
  // is ignored.

  def numCellsForTravel(c: Country, dest: String, prevAttempts: List[TravelAttempt] = Nil, placement: Boolean = false, ignoreARP: Boolean = false): Int = {
    if (c.truce)
      0
    else if (c.name == dest)
      0  // This function is not used for travel within the same country
    else if (!placement && dest == UnitedStates && globalEventInPlay(TravelBan) && isTravelBanCountry(c.name))
      0  // Travel ban prevents travel to the US from certain countries
    else {
      def prevTravellers(name: String) = prevAttempts.count(_.from == name)
      val unusedCellsInCountry = (unusedCells(c) - prevTravellers(c.name)) max 0
      val nigeriaCheck = c match {
        case m: MuslimCountry =>
          m.name == Nigeria && m.isFair && m.isAlly && m.reactionDelta < 2
        case _ =>
          false
      }
      val totalAutoRecruitWithCells =
          game.muslims.count(m => m.autoRecruit && m.totalCells - prevTravellers(m.name) > 0) // Including Sadr

      if (game.botEnhancements) {
        // The Enhanced Bot will never travel any cells out of the Major Jihad Priority country
        // Must check that it is set to avoid infinite looping
        if (PriorityCountries.majorJihadPrioritySet && Some(c.name) == majorJihadPriorityCountry)
          0
        else {

          // Always preseve one cell if the country is:
          // - last cell in United States if there is a WMD plot available
          // - auto-recruit and there is not at least 2 other auto-recruit
          //   counrtries with a cell
          // - The country contains the Training Camps marker
          // - The country is Nigeria while it is a Muslim Ally
          val preserveOne =
            (c.name == UnitedStates && game.plotData.availablePlots.contains(PlotWMD)) ||
            (c.autoRecruit && totalAutoRecruitWithCells < 3) ||
            c.hasMarker(TrainingCamps) ||
            nigeriaCheck
          // The enhanced bot will not move that last three cells (arpLimit unless overridden)
          // out of the Priority Auto Recruit country.
          val numToPreserve = if (!ignoreARP && PriorityCountries.autoRecruitPrioritySet && Some(c.name) == autoRecruitPriorityCountry)
            3
          else if (preserveOne)
            1
          else
            0
          (unusedCellsInCountry - numToPreserve) max 0
        }
      }
      else {
        val isNigeriaMuslimAlly = c match {
          case m: MuslimCountry => m.name == Nigeria && m.isAlly
          case _ => false
        }
        // Standard Bot
        // Always preseve one cell if the country is:
        // - last cell in United States if there is a WMD plot available
        // - auto-recruit and there is not at least 2 other auto-recruit
        //   counrtries with a cell
        // - The country contains the Training Camps marker
        // - The country is Nigeria while it is a Muslim Ally
        val preserveOne =
          (c.name == UnitedStates && game.plotData.availablePlots.contains(PlotWMD)) ||  // Non-standard
          (c.autoRecruit && totalAutoRecruitWithCells < 3) ||
          c.hasMarker(TrainingCamps) ||  // Non-standard
          isNigeriaMuslimAlly            // Non-standard
        val numToPreserve = if (preserveOne)
          1
        else
          0

        (unusedCellsInCountry - numToPreserve) max 0
      }
    }
  }

  // Test the country to see if it has any unused cells that can travel.
  def hasCellForTravelWithPrevious(c: Country, dest: String, prevAttempts: List[TravelAttempt]) =
    numCellsForTravel(c, dest, prevAttempts) > 0

  // This does not account for previous Travel attempts
  // `placement` should be true if this is used by and event that can
  // place cell from anywhere on the map.  In this case, Travel Ban
  // is ignored.
  def hasCellForTravel(c: Country, dest: String, placement: Boolean = false) =
    numCellsForTravel(c, dest: String, Nil, placement) > 0

  def numAdjacentTravelCells(destination: Country, prevAttempts: List[TravelAttempt] = Nil): Int = {
    game.getCountries(getAdjacent(destination.name))
      .map(c => numCellsForTravel(c, destination.name, prevAttempts))
      .sum
  }

  def hasAdjacentTravelCells(destination: Country, prevAttempts: List[TravelAttempt] = Nil) =
    numAdjacentTravelCells(destination, prevAttempts) > 0

  // True if there is at least one cell adjacent to the destinations
  // that is free to travel.
  def canAdjacentTravelTo(dest: Country): Boolean =
    !dest.truce &&
    hasAdjacentTravelCells(dest)

  def canAdjacentTravelTo(destName: String): Boolean =
    canAdjacentTravelTo(game.getCountry(destName))

  // True if there is at least on cell that can attempt to travel
  // to the destination.
  def canTravelTo(dest : Country, autoTravel: Boolean = false): Boolean =
    if (!autoTravel && lapsingEventInPlay(Biometrics))  // Only ajacent travel allowed
      !dest.truce && hasAdjacentTravelCells(dest)
    else
      !dest.truce && game.hasCountry(source => hasCellForTravel(source, dest.name))

  def canTravelTo(destName : String, autoTravel: Boolean): Boolean =
    canTravelTo(game.getCountry(destName), autoTravel)

  def adjacentTravelSources(dest: String, prevAttempts: List[TravelAttempt] = Nil): List[String] =
    getAdjacent(dest).filter(source => hasCellForTravelWithPrevious(game.getCountry(source), dest, prevAttempts))

  def travelSources(dest: String, prevAttempts: List[TravelAttempt] = Nil): List[String] =
    if (lapsingEventInPlay(Biometrics))
      adjacentTravelSources(dest, prevAttempts)
    else
      game.countries
        .filter(source => hasCellForTravelWithPrevious(source, dest, prevAttempts))
        .map(_.name)



  // First select the target to country.
  // Then select one or more countries from which cells may travel.
  // For each source country, make as many attempts as possible, before
  // moving on to the next source (as long as Ops are remaining)
  // - Only travel the last cell out of an Auto Recruit country if
  //   there are two or more other Auto Recruit countries with cells.
  // - Select active cells for travel before sleeper cells
  // - Never travel sleeper cells within the same country
  // Returns the number of Ops used.
  def travelOperation(card: Card, optSource: Option[String], optTarget: Option[String], maxAttempts: Option[Int], adjacentOnly: Boolean): Int = {

    def carryoutTravelOp(toName: String): Int = {
      val maxTravel = maxAttempts.getOrElse(maxOpsPlusReserves(card))

      def nextTravelFrom(alreadyTried: Set[String], attempts: List[TravelAttempt]): List[TravelAttempt] = {
        val remaining = maxTravel - attempts.size
        if (remaining == 0)
          attempts  // We've used all available Ops
        else {
          // When Biometrics is in play, the Standard Bot will consider travelling
          // active cells in place to hide them.
          val inplaceOK =
            !game.botEnhancements &&
            lapsingEventInPlay(Biometrics) &&
            game.getCountry(toName).activeCells > 0
          val inplaceSource = if (inplaceOK) List(toName) else Nil
          // When Biometrics is in play, travelSources() will only return adjacent countries
          val candidates = if (adjacentOnly)
            (adjacentTravelSources(toName, attempts):::inplaceSource).filterNot(alreadyTried)
          else
           (travelSources(toName, attempts):::inplaceSource).filterNot(alreadyTried)

          travelFromTarget(toName, candidates) match {
            case None => attempts  // No more viable countries to travel from
            case Some(fromName) =>
              val from = game.getCountry(fromName)
              // Limit numAttempts to only active cells within same country
              // numCellsForTravel() checks for auto-recruit limit as well as
              // Training Camps and Nigeria limit for Enhanced Bot
              val numAttempts = if (fromName == toName) {
                val prevTravellers = attempts.count(_.from == from.name)
                (activeCells(from) - prevTravellers) max 0 min remaining
              }
              else
                numCellsForTravel(from, toName, attempts) min remaining

              if (numAttempts == 0) {
                botLog(s"No cells in $fromName are allowed to travel")
                nextTravelFrom(alreadyTried + fromName, attempts) // Find the next from target
              }
              else {
                // Determine how many actives/sleepers will attempt travel
                val (actives, sleepers) = if (fromName == toName)
                  (numAttempts, 0) // We've limited the number of attempts to sleepers above
                else {
                  // Actives first
                  val actives = activeCells(from) min numAttempts
                  val sleepers = numAttempts - actives
                  (actives, sleepers)
                }
                val newAttempts = List.fill(actives)(TravelAttempt(fromName, toName, true)) :::
                                  List.fill(sleepers)(TravelAttempt(fromName, toName, false))

                // Find the next country to travel from...
                nextTravelFrom(alreadyTried + fromName, attempts ::: newAttempts)
              }
          }
        }
      }

      val uk = game.getNonMuslim(UnitedKingdom)
      val noUK = if (toName == UnitedKingdom && uk.hasMarker(BREXIT) && uk.isHard)
        Schengen.toSet
      else
        Set.empty[String]

      val banned = if (toName == UnitedStates)
        countryNames(game.countries).filter(isTravelBanCountry).toSet
      else
        Set.empty[String]


      val attempts = nextTravelFrom(noUK ++ banned, Nil)
      val opsUsed = attempts.size
      if (card.ops < opsUsed)
        expendBotReserves(opsUsed - card.ops)

      addOpsTarget(toName)
      for ((name, success) <- performTravels(attempts); if success)
        usedCells(toName).addSleepers(1)

      opsUsed
    }

    // Returns Ops used
    def carryOutPrescibedTravel(fromName: String, toName: String): Int = {
      val from = game.getCountry(fromName)
      val to = game.getCountry(toName)
      val maxTravel = maxAttempts.getOrElse(maxOpsPlusReserves(card))
      val numAttempts = maxTravel min numCellsForTravel(from, toName)
      if (numAttempts > 0) {
        // Actives first
        val actives = activeCells(from) min numAttempts
        val sleepers = numAttempts - actives
        val attempts = List.fill(actives)(TravelAttempt(fromName, toName, true)) :::
                       List.fill(sleepers)(TravelAttempt(fromName, toName, false))

        val opsUsed = attempts.size
        if (card.ops < opsUsed)
          expendBotReserves(opsUsed - card.ops)

        addOpsTarget(toName)
        for ((name, success) <- performTravels(attempts); if success)
          usedCells(toName).addSleepers(1)
        opsUsed
      }
      else
        0
    }

    if (adjacentOnly)
      log(s"\n$Jihadist performs a Travel operation (from adjacent countries)")
    else
      log(s"\n$Jihadist performs a Travel operation")
    log(separator())

    // If Biometrics is in effect only adjacent travel is allowed.
    lazy val toCandidates = if (lapsingEventInPlay(Biometrics)) {
      // Find countries that are adjacent to other countries with cells, or that
      // have active cells (which can travel in place)
      // Note: The Enhanced bot does not travel in place.
      val validCountries = game.countries.filter { c =>
        (c.activeCells > 0 && !game.botEnhancements) || canAdjacentTravelTo(c)
      }
      countryNames(validCountries)
    }
    else if (adjacentOnly) {
      // The Enhanced Bot limits travel to adjacent only when the EvO
      // selected Travel based on an available adjacent cell.
      countryNames(game.countries.filter(canAdjacentTravelTo))
    }
    else
      countryNames(game.countries.filter(canTravelTo(_)))

    // optSource is ignored if no optTarget is present.
    (optTarget, optSource) match {
      case (Some(toName), Some(fromName)) =>
        carryOutPrescibedTravel(fromName, toName)

      case (Some(toName), None) =>
        carryoutTravelOp(toName)

      case (None, _) =>
        travelToTarget(toCandidates)
          .map(toName => carryoutTravelOp(toName))
          .getOrElse(0) // No targets. No Ops used.
    }
  }

  // Select Poor where Best Jihad DRM, the Unmarked
  // Use normal travel priorities among all that are selected
  def enhancedTravelToTarget(names: List[String]): Option[String] = {
    botLog("Find Poor/Unmarked \"Travel To\" target", Color.Debug)
    topPriority(game.getCountries(names), recruitAndTravelToPriorities).map(_.name)
  }

  // Travel to Poor/Unmarked Mulsim country where a Major Jihad roll
  // will succeds and where there are no Troops/Militia.
  def enhancedTravelOperation(card: Card): Int = {

    def carryoutTravelOp(toName: String): Int = {
      val maxTravel = maxOpsPlusReserves(card)

      def nextTravelFrom(alreadyTried: Set[String], attempts: List[TravelAttempt]): List[TravelAttempt] = {
        val remaining = maxTravel - attempts.size
        if (remaining == 0)
          attempts  // We've used all available Ops
        else {

          val candidates = travelSources(toName, attempts).filterNot(alreadyTried)

          travelFromTarget(toName, candidates) match {
            case None => attempts  // No more viable countries to travel from
            case Some(fromName) =>
              val from = game.getCountry(fromName)
              // Limit numAttempts to only active cells within same country
              // numCellsForTravel() checks for auto-recruit limit as well as
              // Training Camps and Nigeria limit for Enhanced Bot
              val numAttempts = if (fromName == toName) {
                val prevTravellers = attempts.count(_.from == from.name)
                (activeCells(from) - prevTravellers) max 0 min remaining
              }
              else
                numCellsForTravel(from, toName, attempts) min remaining

              if (numAttempts == 0) {
                botLog(s"No cells in $fromName are allowed to travel")
                nextTravelFrom(alreadyTried + fromName, attempts) // Find the next from target
              }
              else {
                // Determine how many actives/sleepers will attempt travel
                val (actives, sleepers) = if (fromName == toName)
                  (numAttempts, 0) // We've limited the number of attempts to sleepers above
                else {
                  // Actives first
                  val actives = activeCells(from) min numAttempts
                  val sleepers = numAttempts - actives
                  (actives, sleepers)
                }
                val newAttempts = List.fill(actives)(TravelAttempt(fromName, toName, true)) :::
                                  List.fill(sleepers)(TravelAttempt(fromName, toName, false))

                // Find the next country to travel from...
                nextTravelFrom(alreadyTried + fromName, attempts ::: newAttempts)
              }
          }
        }
      }


      val attempts = nextTravelFrom(Set.empty, Nil)
      val opsUsed = attempts.size
      if (card.ops < opsUsed)
        expendBotReserves(opsUsed - card.ops)

      addOpsTarget(toName)
      for ((name, success) <- performTravels(attempts); if success)
        usedCells(toName).addSleepers(1)

      opsUsed
    }

    log(s"\n$Jihadist performs a Travel operation to Poor/Unmarked Muslim countries")
    log("where Major Jihad success is possible and with no Troops or Militia")
    log(separator())

    enhancedTravelToTarget(enhancedTravelCandidates) match {
      case None => 0 // No targets to no ops used
      case Some(toName) => carryoutTravelOp(toName)
    }
  }


  // Returns the number of Ops used.
  def plotOperation(card: Card, prestigeFocus: Boolean): Int = {
    val maxCells = game.plotTargets.map(game.getCountry).map(unusedCells).sum
    val maxAttempts = maxOpsPlusReserves(card) min maxCells
    log(s"\n$Jihadist performs a Plot operation")
    log(separator())

    // Return the number of attempts made.
    def nextTarget(completed: Int, alreadyTried: Set[String]): Int = {
      val remaining = maxAttempts - completed
      if (remaining == 0 || game.availablePlots.isEmpty) {
        completed
      }
      else {
        val canPlot = (c: Country) => !alreadyTried(c.name) &&
                                      totalUnused(c, includeSadr = true) > 0 && (
                                        (game isNonMuslim c.name) ||
                                        !(game getMuslim c.name).isIslamistRule
                                      )
        val candidates = if (prestigeFocus)
          countryNames(game.countries.filter(c => canPlot(c) && c.totalTroopsThatAffectPrestige > 0))
        else
          countryNames(game.countries.filter(canPlot))
        plotTarget(candidates, prestigeFocus) match {
          case None => completed
          case Some(name) =>
            val c = game getCountry name
            val numAttempts = totalUnused(c, includeSadr = true) min remaining
            val actives     = activeCells(c) min numAttempts
            val sadr        = sadrAvailable(c) && actives < numAttempts
            val sleepers    = numAttempts - actives - sadrValue(sadr)
            val attempts = List.fill(actives + sadrValue(sadr))(PlotAttempt(name, true)) :::
                           List.fill(sleepers)(PlotAttempt(name, false))
            for (a <- attempts)
              addOpsTarget(a.name)
            val numCompleted = performPlots(card.ops, attempts)
            // All sleepers used will have been flipped to active by performPlots()
            if (numCompleted <= actives)
              usedCells(name).addActives(numCompleted)
            else {
              // Mark the originally active as used
              // Mark sadr if present, then finally
              // account for any sleepers that were used.
              usedCells(name).addActives(actives)
              if (sadr)
                usedCells.sadrUsed = true
              usedCells(name).addActives(numCompleted - actives - sadrValue(sadr))
            }
            nextTarget(completed + numCompleted, alreadyTried + name)
        }
      }
    }
    val opsUsed = nextTarget(0, Set.empty)
    if (card.ops < opsUsed)
      expendBotReserves(opsUsed - card.ops)
    opsUsed
  }

  // Get highest priority target country
  // Make as many attempts as possible
  // If ops left over, repeat
  // Returns the number of Ops used.
  def minorJihadOperation(card: Card): Int = {
    log(s"\n$Jihadist performs a Minor Jihad operation")
    log(separator())
    val allowSadr = !game.botEnhancements // Enhanced Bot does not allow Sadr to participate
    val maxJihad = maxOpsPlusReserves(card)
    def nextJihadTarget(completed: Int, alreadyTried: Set[String]): List[JihadTarget] = {
      val remaining = maxJihad - completed
      if (remaining == 0)
        Nil  // We've used all available Ops
      else {
        val canJihad = (m: MuslimCountry) =>
          !alreadyTried(m.name) &&
          m.jihadOK &&
          minorJihadSuccessPossible(m) &&
          totalUnused(m, includeSadr = allowSadr) > 0 &&
          minorJihadGovTest(m)

        val candidates = countryNames(game.jihadTargets.map(game.getMuslim).filter(canJihad))
        minorJihadTarget(candidates) match {
          case None => Nil   // No more candidates
          case Some(name) =>
            val m = game.getMuslim(name)
            // In a Fair country with the Training Camps marker, preserve
            // the last cell unless Sadr is present and we are not allowed to use Sadr
            val limit = if (m.isFair && game.isTrainingCamp(m.name) && (!m.hasSadr || allowSadr))
              remaining min (m.totalCells - 1)
            else
              remaining
            val numAttempts = totalUnused(m, includeSadr = allowSadr) min limit
            val actives  = numAttempts min activeCells(m)
            val sleepers = (numAttempts - actives) min sleeperCells(m)
            val sadr     = numAttempts - actives - sleepers > 0
            val target = JihadTarget(name, false, actives, sleepers, sadr)
            target :: nextJihadTarget(completed + numAttempts, alreadyTried + name)
        }
      }
    }

    val targets = nextJihadTarget(0, Set.empty)
    val opsUsed = (for (JihadTarget(_, _, a, s, sadr, _, _) <- targets)
      yield(a + s + (if (sadr) 1 else 0))).sum
    if (card.ops < opsUsed)
      expendBotReserves(opsUsed - card.ops)
    for (t <- targets)
      addOpsTarget(t.name)
    for ((name, successes, sadr) <- performJihads(targets)) {
      usedCells(name).addActives(successes)
      if (sadr)
        usedCells.sadrUsed = true
    }
    opsUsed
  }

  // Returns the number of Ops used.
  def majorJihadOperation(card: Card, designatedTarget: Option[String]): Int = {
    log(s"\n$Jihadist performs a Major Jihad operation")
    log(separator())
    val opsUsed = maxOpsPlusReserves(card)
    val isCandidate = (m: MuslimCountry) =>
      majorJihadSuccessPossible(m) &&
      m.isPoor &&
      totalUnused(m, includeSadr = true) - m.totalTroopsAndMilitia >= 5
    val candidates = game.majorJihadTargets(opsUsed)
      .map(game.getMuslim)
      .filter(isCandidate)

    (designatedTarget orElse majorJihadTarget(countryNames(candidates)))
      .map { name =>
        if (card.ops < opsUsed)
          expendBotReserves(opsUsed - card.ops)
        addOpsTarget(name)
        val target = JihadTarget(name, true, opsUsed, 0, false)
        for ((_, successes, _) <- performJihads(target::Nil))
              usedCells(name).addActives(successes)
        opsUsed
      }
      .getOrElse(0) // No Ops spent if no target found.  Should not happen!
  }

  // Returns the number of Ops used.
  // This is a special action used by the Enhanced Bot when
  // there are currently no cells anywhere on the map.
  // Cells are placed in random muslim countries.
  def placeRandomCellsOnMap(card: Card): Int = {
    log()
    log(s"$Jihadist Places cells in random Muslim countries")
    log(separator())
    log("When there are no cells on the map, the Enhanced Bot places cells using", Color.Event)
    log("available Ops randomly in Muslim countries using the following priorites:", Color.Event)
    log("Islamist Rule, Poor, Unmarked.  (max 1 cell per country)", Color.Event)

    val maxPlacements = maxOpsPlusReserves(card)

    // We place no more than one cell per Muslim country.
    val priorites = List(
      IslamistRulePriority,
      PoorPriority,
      UnmarkedPriority,
      FairPriority,
    )

    def placeCell(numPlaced: Int, candidates: List[MuslimCountry]): Int =
      if (numPlaced < maxPlacements && candidates.nonEmpty) {
        val target = topPriority(candidates, EnhUnmarkedNonMuslimTravelPriorities)
          .map(_.name)
          .get
        addOpsTarget(target)
        testCountry(target)
        addCellsToCountry(target, active = false, num = 1)
        placeCell(numPlaced + 1, candidates.filterNot(_.name == target))
      }
      else
        numPlaced

    val numPlacements = placeCell(0, game.muslims.filter(!_.truce))
    if (card.ops < numPlacements)
      expendBotReserves(numPlacements - card.ops)
    numPlacements
  }


  sealed trait RadicalizationAction {
    def criteriaMet(onlyReserveOpsRemain: Boolean): Boolean
    def perform(cardOps: Int, reserveOps: Int): Int  // Returns number of Ops used
  }

  object StandardRadicalizationActions {
    // -----------------------------------------------------------
    // Radicalization Action -  Plot WMD In US
    case object PlotWMDInUS extends RadicalizationAction {
      override
      def criteriaMet(onlyReserveOpsRemain: Boolean): Boolean = {
        game.availablePlots.contains(PlotWMD) &&
        totalUnused(game.getCountry(UnitedStates), includeSadr = true) > 0
      }

      // Plot as many time as possible in the US as long as there are
      // unused cells there.
      // cardsOps   - The number of unused Ops remaining from the card
      // reserveOps - The number of unused Ops remaining from reserves
      // Returns the number of ops used
      override
      def perform(cardOps: Int, reserveOps: Int): Int = {
        var headerDisplayed = false
        def displayHeader(): Unit = if (!headerDisplayed) {
          headerDisplayed = true
          log()
          log(s"Radicalization: Plot in the United States")
        }
        val maxOps = cardOps + reserveOps
        def nextAttempt(completed: Int): Int = {
          val us = game getCountry UnitedStates
          if (completed == maxOps || totalUnused(us, includeSadr = true) == 0 || game.availablePlots.isEmpty)
          completed
          else {
            displayHeader()
            if (completed >= cardOps)
              expendBotReserves(1)
            addOpsTarget(UnitedStates)
            performPlots(3, PlotAttempt(UnitedStates, activeCells(us) > 0)::Nil)
            usedCells(UnitedStates).addActives(1)
            nextAttempt(completed + 1)
          }
        }
        nextAttempt(0)
      }
    }

    // -----------------------------------------------------------
    // Radicalization Action -  Travel to untested Muslim country
    case object TravelToUntestedNonMuslim extends RadicalizationAction {
      override
      def criteriaMet(onlyReserveOpsRemain: Boolean): Boolean = {
        game.usPosture == Hard        &&
        game.gwotPenalty == 0         &&
        game.hasNonMuslim(n => n.isUntested && canTravelTo(n))
      }

      // Travel to Untested Non-Muslim countries while the US is Hard to
      // try and increase the GWOT penalty.
      // We give preference to cells in adjacent countries to avoid die rolls
      // when necessary.
      // cardsOps   - The number of unused Ops remaining from the card
      // reserveOps - The number of unused Ops remaining from reserves
      // Returns the number of ops used
      override
      def perform(cardOps: Int, reserveOps: Int): Int = {
        var headerDisplayed = false
        def displayHeader(): Unit = if (!headerDisplayed) {
          headerDisplayed = true
          log()
          log(s"Radicalization: Travel to Untested Non-Muslim countries")
        }

        val maxOps = cardOps + reserveOps
        def createTravelAttempt(from: String, to: String): TravelAttempt = {
          TravelAttempt(from, to, activeCells(game getCountry from) > 0)
        }
        // Create the next highest priority travel attempt.  Max of one per destination.
        def nextTravel(completed: Int, destinations: List[String]): Int = {
          if (completed == maxOps || !destinations.exists(canTravelTo(_, autoTravel = false)))
            completed
          else {
            // First we try to do adjacent travel so that it will automatically succeed.
            val adjDests = destinations.filter(canAdjacentTravelTo)
            if (adjDests.nonEmpty) {
              // Now pick just the destinations that are adjacent to one of the sources
              val to = recruitTravelToPriority(adjDests).get
              travelFromTarget(to, adjacentTravelSources(to)) match {
                case None => nextTravel(completed, destinations.filterNot(_ == to))
                case Some(from) =>
                  displayHeader()
                  if (completed >= cardOps)
                    expendBotReserves(1)
                  performTravels(createTravelAttempt(from, to)::Nil) match {
                    case (_, true)::Nil => usedCells(to).addSleepers(1)
                    case _ =>
                  }
                  nextTravel(completed + 1, destinations.filterNot(_ == to))
              }
            }
            else if (lapsingEventInPlay(Biometrics)) // Non adjacent travel is not allowd
              completed
            else {
              val to = recruitTravelToPriority(destinations).get
              travelFromTarget(to, travelSources(to)) match {
                case None => nextTravel(completed, destinations.filterNot(_ == to))
                case Some(from) =>
                  displayHeader()
                  if (completed >= cardOps)
                  expendBotReserves(1)
                  performTravels(createTravelAttempt(from, to)::Nil) match {
                    case (_, true)::Nil => usedCells(to).addSleepers(1)
                    case _ =>
                  }
                  nextTravel(completed + 1, destinations.filterNot(_ == to))
              }
            }
          }
        }
        nextTravel(0, countryNames(game.nonMuslims.filter(_.isUntested)))
      }
    }

    // -----------------------------------------------------------
    // Radicalization Action -  Plot in Soft NonMuslim
    case object PlotInSoftNonMuslim extends RadicalizationAction {
      override
      def criteriaMet(onlyReserveOpsRemain: Boolean): Boolean = {
        !game.botEnhancements &&
        game.usPosture == Soft &&
        game.gwotPenalty == 0  &&
        game.availablePlots.nonEmpty &&
        game.hasNonMuslim(n => n.isSoft && totalUnused(n, includeSadr = true) > 0)
      }

      // Plot in Soft Non-Muslim countries while the US posture is Soft to
      // try and increase the GWOT penalty.
      // cardsOps   - The number of unused Ops remaining from the card
      // reserveOps - The number of unused Ops remaining from reserves
      // Returns the number of ops used
      override
      def perform(cardOps: Int, reserveOps: Int): Int = {
        var headerDisplayed = false
        def displayHeader(): Unit = if (!headerDisplayed) {
          headerDisplayed = true
          log()
          log(s"Radicalization: Plot in Soft Non-Muslim countries")
        }
        val maxOps = cardOps + reserveOps
        def nextPlotTarget(completed: Int, candidates: List[NonMuslimCountry]): Int = {
          if (completed == maxOps || candidates.isEmpty || game.availablePlots.isEmpty)
            completed
          else {
            // Give preference to countries with the worst governance
            // as they will have the highest chance of a successful plot.
            // Within those we use the plot priorities to select the best one.
            val softs = {
              val ss = candidates sortBy (n => -n.governance) // sort so worst comes first
              ss.takeWhile(_.governance == ss.head.governance)
            }
            val target = topPriority(softs, plotPriorities).get

            def nextAttempt(plotsPerformed: Int): Int = {
              if (completed + plotsPerformed == maxOps || totalUnused(target, includeSadr = true) == 0 || game.availablePlots.isEmpty)
                plotsPerformed
              else {
                displayHeader()
                if (completed + plotsPerformed >= cardOps)
                  expendBotReserves(1)

                addOpsTarget(target.name)
                performPlots(3, PlotAttempt(target.name, activeCells(target) > 0)::Nil)
                usedCells(target.name).addActives(1)
                nextAttempt(plotsPerformed + 1)
              }
            }
            val numPlots = nextAttempt(0)
            nextPlotTarget(completed + numPlots, candidates.filterNot(_.name == target.name))
          }
        }
        nextPlotTarget(0, game.nonMuslims.filter(n => n.isSoft && totalUnused(n, includeSadr = true) > 0))
      }
    }

    // -----------------------------------------------------------
    // Radicalization Action -  Recruit at Muslim country with Cadre
    case object RecruitAtMuslimCadre extends RadicalizationAction {
      override
      def criteriaMet(onlyReserveOpsRemain: Boolean): Boolean = {
        // I'm allowing recruit in IR countries, not sure if that is the intent?
        botRecruitPossible(muslimWithCadreOnly = true)
      }

      // Recruit in the Muslim county with a cadre that has the best
      // Jihad DRM.
      // cardsOps   - The number of unused Ops remaining from the card
      // reserveOps - The number of unused Ops remaining from reserves
      // Returns the number of ops used
      override
      def perform(cardOps: Int, reserveOps: Int): Int = {
        var headerDisplayed = false
        def displayHeader(): Unit = if (!headerDisplayed) {
          headerDisplayed = true
          log()
          log(s"Radicalization: Recruit in a Muslim country with a cadre")
        }
        val maxOps     = cardOps + reserveOps
        val candidates = game.getMuslims(botRecruitTargets(muslimWithCadreOnly = true)).sortBy(m => jihadDRM(m, m.isPoor))
        val target     = recruitTarget(candidates.map(_.name)).get
        addOpsTarget(target)
        val m = game getMuslim target
        def nextAttempt(completed: Int): Int = {
          if (completed == maxOps || game.cellsToRecruit == 0)
            completed
          else {
            displayHeader()
            if (completed >= cardOps)
              expendBotReserves(1)
            log(s"\n$Jihadist attempts to recruit a cell into $target")
            if (m.autoRecruit) {
              log(s"Recruit is automatically successful in $target")
              addSleeperCellsToCountry(target, 1)
              usedCells(target).addSleepers(1)
            }
            else {
              val die = getDieRoll(s"Enter die roll for recruit in $target: ")
              val success = m.recruitSucceeds(die)
              val result  = if (success) "succeeds" else "fails"
              log(s"Recruit $result in $target with a roll of $die")
              if (success) {
                val numCells = if (game.jihadistIdeology(Potent)) {
                  log(s"$Jihadist Bot with Potent Ideology places two cells for each success")
                  2 min game.cellsToRecruit
                }
                else
                  1

                addSleeperCellsToCountry(target, numCells)
                usedCells(target).addSleepers(numCells)
              }
            }
            nextAttempt(completed + 1)
          }
        }
        nextAttempt(0)
      }
    }


    // -----------------------------------------------------------
    // Radicalization Action -  Add surplus card Ops to reserves
    case object AddToReserves extends RadicalizationAction {
      override
      def criteriaMet(onlyReserveOpsRemain: Boolean): Boolean = {
        !onlyReserveOpsRemain &&
        game.reserves.jihadist < 2
      }

      // cardsOps - The number of unused Ops remaining from the card
      // Add any remaining card ops to reserves until reserves are full.
      // Returns the number of ops added
      override
      def perform(cardOps: Int, reserveOps: Int): Int = {
        log()
        log(s"Radicalization: Add to reserves")
        val opsAdded = cardOps min (2 - game.reserves.jihadist)
        addToReserves(Jihadist, opsAdded)
        opsAdded
      }
    }

    // -----------------------------------------------------------
    // Radicalization Action -  Recruit cells (cannot use reserve Ops)
    case object Recruit extends RadicalizationAction {
      override
      def criteriaMet(onlyReserveOpsRemain: Boolean): Boolean = {
        !onlyReserveOpsRemain &&
        botRecruitPossible(muslimWithCadreOnly = false)
      }

      // Perform a recruit operation.  Do not use any reserves.
      // cardsOps - The number of unused Ops remaining from the card
      // Returns the number of ops used
      override
      def perform(cardOps: Int, reserveOps: Int): Int = {
        var headerDisplayed = false
        def displayHeader(): Unit = if (!headerDisplayed) {
          headerDisplayed = true
          log()
          log(s"Radicalization: Recruit")
        }
        val target = recruitTarget(botRecruitTargets(muslimWithCadreOnly = false)).get
        addOpsTarget(target)
        val m = game getMuslim target
        def nextAttempt(completed: Int): Int = {
          if (completed == cardOps || game.cellsToRecruit == 0)
            completed
          else {
            displayHeader()
            log(s"$Jihadist attempts to recruit a cell into $target")
            if (m.autoRecruit) {
              log(s"Recruit is automatically successful in $target")
              addSleeperCellsToCountry(target, 1)
              usedCells(target).addSleepers(1)
            }
            else {
              val die = getDieRoll(s"Enter die roll for recruit in $target: ")
              val success = m.recruitSucceeds(die)
              val result  = if (success) "succeeds" else "fails"
              log(s"Recruit $result in $target with a roll of $die")
              if (success) {
                val numCells = if (game.jihadistIdeology(Potent)) {
                  log(s"$Jihadist Bot with Potent Ideology places two cells for each success")
                  2 min game.cellsToRecruit
                }
                else
                  1
                addSleeperCellsToCountry(target, numCells)
                usedCells(target).addSleepers(numCells)
              }
            }
            nextAttempt(completed + 1)
          }
        }
        nextAttempt(0)
      }
    }

    // -----------------------------------------------------------
    // Radicalization Action -  Travel cells to the US (cannot use reserve Ops)
    case object TravelToUS extends RadicalizationAction {
      override
      def criteriaMet(onlyReserveOpsRemain: Boolean): Boolean = {
        !onlyReserveOpsRemain &&
        unusedCellsOnMapForTravel > 0
      }

      // Travel to the US. (From adjacent if possible). Do not use reserves.
      // Perform a recruit operation.  Do not use any reserves.
      // Returns the number of ops used
      override
      def perform(cardOps: Int, reserveOps: Int): Int = {
        var headerDisplayed = false
        def displayHeader(): Unit = if (!headerDisplayed) {
          headerDisplayed = true
          log()
          log(s"Radicalization: Travel to the United States")
        }

        def createTravelAttempt(from: String): TravelAttempt = {
          TravelAttempt(from, UnitedStates, activeCells(game getCountry from) > 0)
        }

        def nextTravel(completed: Int): Int = {
          if (completed == cardOps || travelSources(UnitedStates).isEmpty)
            completed
          else {
            // First we try to do adjacent travel so that it will automatically succeed.
            if (adjacentTravelSources(UnitedStates).nonEmpty) {
              travelFromTarget(UnitedStates, adjacentTravelSources(UnitedStates)) match {
                case None => completed  // No more source countries
                case Some(from) =>
                  displayHeader()
                  performTravels(createTravelAttempt(from)::Nil) match {
                    case (_, true)::Nil => usedCells(UnitedStates).addSleepers(1)
                    case _ =>
                  }
                  nextTravel(completed + 1)
              }
            }
            else {
            travelFromTarget(UnitedStates, travelSources(UnitedStates)) match {
              case None => completed   // No more source countries
              case Some(from) =>
                displayHeader()
                performTravels(createTravelAttempt(from)::Nil) match {
                  case (_, true)::Nil => usedCells(UnitedStates).addSleepers(1)
                  case _ =>
                }
                nextTravel(completed + 1)
            }
            }
          }
        }
        nextTravel(0)
      }
    }
  }

  object EnhancedRadicalizationActions {
    // -----------------------------------------------------------
    // Radicalization Action -  Plot WMD In US
    // If WMD available and there are unused cells in the US
    case object PlotWMDInUS extends RadicalizationAction {
      override
      def criteriaMet(onlyReserveOpsRemain: Boolean): Boolean = {
        game.availablePlots.contains(PlotWMD) &&
        totalUnused(game.getCountry(UnitedStates), includeSadr = true) > 0
      }

      // Plot as many time as possible in the US as long as there are
      // unused cells there.
      // cardsOps   - The number of unused Ops remaining from the card
      // reserveOps - The number of unused Ops remaining from reserves
      // Returns the number of ops used
      override
      def perform(cardOps: Int, reserveOps: Int): Int = {
        var headerDisplayed = false
        def displayHeader(): Unit = if (!headerDisplayed) {
          headerDisplayed = true
          log()
          log(s"Radicalization: Plot in the United States")
        }
        val maxOps = cardOps + reserveOps
        def nextAttempt(completed: Int): Int = {
          val us = game.getCountry(UnitedStates)
          if (completed == maxOps || totalUnused(us, includeSadr = true) == 0 || game.availablePlots.isEmpty)
            completed
          else {
            displayHeader()
            if (completed >= cardOps)
              expendBotReserves(1)
            addOpsTarget(UnitedStates)
            performPlots(3, PlotAttempt(UnitedStates, activeCells(us) > 0)::Nil)
            usedCells(UnitedStates).addActives(1)
            nextAttempt(completed + 1)
          }
        }
        nextAttempt(0)
      }
    }

    // -----------------------------------------------------------
    // Radicalization Action -  Adjacent Travel to US in order to attempt WMD plot.  MAX 1 cell.
    // If WMD available and there are no cells in the US and there is an unused adjacent cell.
    case object TravelToUSForWMD extends RadicalizationAction {
      override
      def criteriaMet(onlyReserveOpsRemain: Boolean): Boolean = {
        game.availablePlots.contains(PlotWMD) &&
        game.getCountry(UnitedStates).totalCells == 0 &&
        canAdjacentTravelTo(UnitedStates)
      }

      // Travel 1 cell to the US from an adjacent country.
      // cardsOps   - The number of unused Ops remaining from the card
      // reserveOps - The number of unused Ops remaining from reserves
      // Returns the number of ops used
      override
      def perform(cardOps: Int, reserveOps: Int): Int = {
        var headerDisplayed = false
        def displayHeader(): Unit = if (!headerDisplayed) {
          headerDisplayed = true
          log()
          log(s"Radicalization: Travel 1 adjacent cell to the United States")
        }
        val maxOps = cardOps + reserveOps
        // Note: Under some circumstances adjacent travel may not
        // succeed automatically, so we will continue to try as many
        // times as possible until successful.
        def nextAttempt(completed: Int): Int = {
          val us = game.getCountry(UnitedStates)
          val sources = adjacentTravelSources(UnitedStates)
          if (completed == maxOps || us.totalCells > 0 || sources.isEmpty)
            completed
          else {
            displayHeader()
            if (completed >= cardOps)
              expendBotReserves(1)
            addOpsTarget(UnitedStates)
            val source = travelFromTarget(UnitedStates, sources).get
            val activeCell = activeCells(game.getCountry(source)) > 0
            val attempt = TravelAttempt(source, UnitedStates, activeCell)
            performTravels(attempt::Nil) match {
              case (_, true)::Nil => usedCells(UnitedStates).addSleepers(1)
              case _ =>
            }
            nextAttempt(completed + 1)
          }
        }
        nextAttempt(0)
      }
    }

    case object PerformMinorJihadIfPossible extends RadicalizationAction {

      val canJihad = (m: MuslimCountry) =>
        m.jihadOK &&
        minorJihadGovTest(m) &&
        minorJihadSuccessPossible(m) &&
        totalUnused(m, includeSadr = false) > 0

      override
      def criteriaMet(onlyReserveOpsRemain: Boolean): Boolean = game.hasMuslim(canJihad)

      // cardsOps   - The number of unused Ops remaining from the card
      // reserveOps - The number of unused Ops remaining from reserves
      // Returns the number of ops used
      override
      def perform(cardOps: Int, reserveOps: Int): Int = {
        val maxJihad = cardOps + reserveOps

        // Return number of Ops used
        def nextJihad(cardOpsRemaining: Int, resOpsRemaining: Int, alreadyTried: Set[String]): Int = {
          val opsRemaining = cardOpsRemaining + resOpsRemaining
          val candidates = countryNames(
            game.jihadTargets
              .map(game.getMuslim)
              .filter(m => !alreadyTried(m.name) && canJihad(m))
          )
          if (opsRemaining > 0 && candidates.nonEmpty) {
            val name = minorJihadTarget(candidates).get
              val m = game.getMuslim(name)
              // In a Fair country with the Training Camps marker, preserve
              // the last cell unless Sadr is present and we are not allowed to use Sadr
              val limit = if (m.isFair && game.isTrainingCamp(m.name) && !m.hasSadr)
                opsRemaining min (m.totalCells - 1)
              else
                opsRemaining min m.totalCells
              val numAttempts = totalUnused(m, includeSadr = false) min limit
              val actives  = numAttempts min activeCells(m)
              val sleepers = (numAttempts - actives) min sleeperCells(m)
              val target = JihadTarget(name, false, actives, sleepers, false)
              val opsUsed = actives + sleepers
              val cardOpsUsed = cardOpsRemaining min opsUsed
              val resOpsUsed = opsUsed - cardOpsUsed

              if (resOpsUsed > 0)
                expendBotReserves(resOpsUsed)

              addOpsTarget(name)
              for ((name, successes, _) <- performJihads(target::Nil))
                usedCells(name).addActives(successes)

              opsUsed + nextJihad(cardOpsRemaining - cardOpsUsed,  resOpsRemaining - resOpsUsed, alreadyTried + name)
          }
          else
            0
        }

        log(s"\nRadicalization: Minor Jihad")
        log(separator())
        nextJihad(cardOps, reserveOps, Set.empty)
      }
    }


    case object PlotToIncreaseFunding extends RadicalizationAction {
      override
      def criteriaMet(onlyReserveOpsRemain: Boolean): Boolean = game.funding < 6

      // cardsOps   - The number of unused Ops remaining from the card
      // reserveOps - The number of unused Ops remaining from reserves
      // Returns the number of ops used
      override
      def perform(cardOps: Int, reserveOps: Int): Int = {
        var headerDisplayed = false
        def displayHeader(): Unit = if (!headerDisplayed) {
          headerDisplayed = true
          log()
          log(s"Radicalization: Plot")
        }
        val maxOps = cardOps + reserveOps
        val plotCandidate = (c: Country) =>
          (game.isNonMuslim(c.name) || !game.getMuslim(c.name).isIslamistRule) &&
          totalUnused(c, includeSadr = true) > 0

        def nextPlot(completed: Int): Int = {
          val candidates = countryNames(game.countries.filter(plotCandidate))
          if (completed == maxOps || candidates.isEmpty || game.availablePlots.isEmpty)
            completed
          else {
            val remaining = maxOps - completed
            val name = plotTarget(candidates, prestigeFocus = false).get
            val c = game.getCountry(name)
            val numAttempts = totalUnused(c, includeSadr = true) min remaining min game.availablePlots.size
            val reservesUsed = completed + numAttempts - cardOps
            val actives     = activeCells(c) min numAttempts
            val sadr        = sadrAvailable(c) && actives < numAttempts
            val sleepers    = numAttempts - actives - sadrValue(sadr)
            val attempts = List.fill(actives + sadrValue(sadr))(PlotAttempt(name, true)) :::
                           List.fill(sleepers)(PlotAttempt(name, false))

            displayHeader()
            if (reservesUsed > 0)
              expendBotReserves(reservesUsed)
            addOpsTarget(name)
            performPlots(3, attempts)
            // All sleepers used will have been flipped to active by performPlots()
            if (numAttempts <= actives)
              usedCells(name).addActives(numAttempts)
            else {
              // Mark the originally active as used
              // Mark sadr if present, then finally
              // account for any sleepers that were used.
              usedCells(name).addActives(actives)
              if (sadr)
                usedCells.sadrUsed = true
              usedCells(name).addActives(numAttempts - actives - sadrValue(sadr))
            }
            nextPlot(completed + numAttempts)
          }
        }

        nextPlot(0)
      }
    }

    // -----------------------------------------------------------
    // Radicalization Action -  Adjacent Travel to Good Muslim countries with no Troops.
    // Travel as many adjacent cells as possible to one or more Good counties where a WoI
    // roll would be possible if the country were worsened to Fair.

    // The Bot will travel adjacent cells to Good Muslim 2+ resource countries
    // without troops where a WoI roll of 1-5 would fail if the
    // country's governance was worsened to Fair.
    case object AdjacentTravelToGoodMuslims extends RadicalizationAction {
      // We must subtract 1 from the value calculated by modifyWoiRoll()
      // because the country is currently at Good governance, but if it
      // were at Fair then there would be -1 modifier for trying to improve
      // it to Good.  We test a roll of 5.  If that would fail
      // then by definition a  roll of 1-4 would also fail.
      val destCandidate = (m: MuslimCountry) =>
        m.isGood &&
        enhBotResourceValue(m) > 1 &&
        m.totalTroops == 0 &&
        !(m.name == Pakistan && m.hasMarker(BenazirBhutto)) &&
        canAdjacentTravelTo(m) &&
        modifyWoiRoll(5, m, silent = true) - 1 < 5

      override
      def criteriaMet(onlyReserveOpsRemain: Boolean): Boolean =
        // game.prestigeModifier - game.gwotPenalty < 0 &&
        game.muslims.filter(destCandidate).nonEmpty

      // Travel 1 cell to the US from an adjacent country.
      // cardsOps   - The number of unused Ops remaining from the card
      // reserveOps - The number of unused Ops remaining from reserves
      // Returns the number of ops used
      override
      def perform(cardOps: Int, reserveOps: Int): Int = {
        var headerDisplayed = false
        def displayHeader(): Unit = if (!headerDisplayed) {
          headerDisplayed = true
          log()
          log(s"Radicalization: Adjacent Travel to Good Muslim countries with no troops")
        }
        val maxOps = cardOps + reserveOps

        def nextTravel(completed: Int, target: String): Int = {

          val sources = adjacentTravelSources(target)
          if (completed == maxOps || sources.isEmpty)
            completed
          else {
            displayHeader()
            if (completed >= cardOps)
              expendBotReserves(1)
            addOpsTarget(target)
            val source = travelFromTarget(target, sources).get
            val activeCell = activeCells(game.getCountry(source)) > 0
            val attempt = TravelAttempt(source, target, activeCell)
            performTravels(attempt::Nil) match {
              case (_, true)::Nil => usedCells(target).addSleepers(1)
              case _ =>
            }
            nextTravel(completed + 1, target)
          }
        }

        def nextDestination(completed: Int, dests: List[String]): Int = {
          if (completed == maxOps || dests.isEmpty)
            completed
          else {
            botLog("Find Good Muslim travel destination", Color.Debug)
            val target = topPriority(game.getCountries(dests), recruitAndTravelToPriorities).map(_.name).get
            val updatedCompleted = nextTravel(completed, target)

            nextDestination(updatedCompleted, dests.filterNot(_ == target))
          }
        }


        nextDestination(0, countryNames(game.muslims.filter(destCandidate)))
      }
    }

    // -----------------------------------------------------------
    // Radicalization Action -  Travel to untested Muslim country
    case object TravelToUntestedNonMuslim extends RadicalizationAction {
      override
      def criteriaMet(onlyReserveOpsRemain: Boolean): Boolean = {
        game.usPosture == Hard        &&
        game.gwotPenalty == 0         &&
        game.hasNonMuslim(n => n.isUntested && canTravelTo(n))
      }

      // Travel to Untested Non-Muslim countries while the US is Hard to
      // try and increase the GWOT penalty.
      // We give preference to cells in adjacent countries to avoid die rolls
      // when necessary.
      // cardsOps   - The number of unused Ops remaining from the card
      // reserveOps - The number of unused Ops remaining from reserves
      // Returns the number of ops used
      override
      def perform(cardOps: Int, reserveOps: Int): Int = {
        var headingDisplayed = false
        def displayHeader(): Unit = if (!headingDisplayed) {
          headingDisplayed = true
          log()
          log(s"Radicalization: Travel to Untested Non-Muslim countries")
        }

        val maxOps = cardOps + reserveOps
        def createTravelAttempt(from: String, to: String): TravelAttempt = {
          TravelAttempt(from, to, activeCells(game getCountry from) > 0)
        }
        // Create the next highest priority travel attempt.  Max of one per destination.
        def nextTravel(completed: Int, destinations: List[String]): Int = {
          if (completed == maxOps || !destinations.exists(canTravelTo(_, autoTravel = false)))
            completed
          else {
            // First we try to do adjacent travel so that it will automatically succeed.
            val adjDests = destinations.filter(canAdjacentTravelTo)
            if (adjDests.nonEmpty) {
              val to = recruitTravelToPriority(adjDests).get
              travelFromTarget(to, adjacentTravelSources(to)) match {
                case None => nextTravel(completed, destinations.filterNot(_ == to))
                case Some(from) =>
                  displayHeader()
                  if (completed >= cardOps)
                    expendBotReserves(1)
                  performTravels(createTravelAttempt(from, to)::Nil) match {
                    case (_, true)::Nil => usedCells(to).addSleepers(1)
                    case _ =>
                  }
                  nextTravel(completed + 1, destinations.filterNot(_ == to))
              }
            }
            else if (lapsingEventInPlay(Biometrics)) // Non adjacent travel is not allowd
              completed
            else {
              val to = recruitTravelToPriority(destinations).get
              // Don't allow travel within the same country
              travelFromTarget(to, travelSources(to)) match {
                case None => nextTravel(completed, destinations.filterNot(_ == to))
                case Some(from) =>
                  displayHeader()
                  if (completed >= cardOps)
                  expendBotReserves(1)
                  performTravels(createTravelAttempt(from, to)::Nil) match {
                    case (_, true)::Nil => usedCells(to).addSleepers(1)
                    case _ =>
                  }
                  nextTravel(completed + 1, destinations.filterNot(_ == to))
              }
            }
          }
        }
        nextTravel(0, countryNames(game.nonMuslims.filter(_.isUntested)))
      }
    }

    // -----------------------------------------------------------
    // Radicalization Action -  Recruit cells (CAN use reserve Ops)
    case object Recruit extends RadicalizationAction {
      private def canRecruitInMjp: Boolean = majorJihadPriorityCountry
        .filterNot(underTruce)
        .map(name => game.getMuslim(name).recruitOK(madrassas = false))
        .getOrElse(false)

      override
      def criteriaMet(onlyReserveOpsRemain: Boolean): Boolean =
        canRecruitInMjp ||
        botRecruitPossible(muslimWithCadreOnly = false)

      // Perform a recruit operation.
      // cardsOps - The number of unused Ops remaining from the card
      // Returns the number of ops used
      override
      def perform(cardOps: Int, reserveOps: Int): Int = {
        var headerDisplayed = false
        def displayHeader(): Unit = if (!headerDisplayed) {
          headerDisplayed = true
          log()
          log(s"Radicalization: Recruit")
        }
        val maxOps = cardOps + reserveOps
        val target = if (canRecruitInMjp)
          majorJihadPriorityCountry.get
        else {
          val candidates = game.getCountries(botRecruitTargets(muslimWithCadreOnly = false))
          topPriority(candidates, recruitAndTravelToPriorities)
            .map(_.name)
            .get
        }
        addOpsTarget(target)
        val m = game getMuslim target
        def nextAttempt(completed: Int): Int = {
          if (completed == maxOps || game.cellsToRecruit == 0)
            completed
          else {
            displayHeader()
            log(s"$Jihadist attempts to recruit a cell into $target")
            if (completed >= cardOps)
              expendBotReserves(1)
            if (m.autoRecruit) {
              log(s"Recruit is automatically successful in $target")
              addSleeperCellsToCountry(target, 1)
              usedCells(target).addSleepers(1)
            }
            else {
              val die = getDieRoll(s"Enter die roll for recruit in $target: ")
              val success = m.recruitSucceeds(die)
              val result  = if (success) "succeeds" else "fails"
              log(s"Recruit $result in $target with a roll of $die")
              if (success) {
                val numCells = if (game.jihadistIdeology(Potent)) {
                  log(s"$Jihadist Bot with Potent Ideology places two cells for each success")
                  2 min game.cellsToRecruit
                }
                else
                  1
                addSleeperCellsToCountry(target, numCells)
                usedCells(target).addSleepers(numCells)
              }
            }
            nextAttempt(completed + 1)
          }
        }
        nextAttempt(0)
      }
    }

    // -----------------------------------------------------------
    // Radicalization Action -  Travel to the Major Jihad Priority Country
    case object TravelToMajorJihadPriorityCountry extends RadicalizationAction {
      override
      def criteriaMet(onlyReserveOpsRemain: Boolean): Boolean =
        majorJihadPriorityCountry.exists(canTravelTo(_, autoTravel = false))

      // Make as many travel attempts as possible to the MJP
      //
      // cardsOps   - The number of unused Ops remaining from the card
      // reserveOps - The number of unused Ops remaining from reserves
      // Returns the number of ops used
      override
      def perform(cardOps: Int, reserveOps: Int): Int = {
        val mjpName = majorJihadPriorityCountry.get
        var headerDisplayed = false
        def displayHeader(): Unit = if (!headerDisplayed) {
          headerDisplayed = true
          log()
          log(s"Radicalization: Travel to Major Jihad Priority country ($mjpName)")
        }
        val maxOps = cardOps + reserveOps

        def nextAttempt(completed: Int): Int = {
          val optSource = travelFromTarget(mjpName, travelSources(mjpName))
          if (completed == maxOps || optSource.isEmpty)
            completed
          else {
            displayHeader()
            if (completed >= cardOps)
              expendBotReserves(1)
            addOpsTarget(mjpName)
            val source = optSource.get
            val activeCell = activeCells(game.getCountry(source)) > 0
            val attempt = TravelAttempt(source, mjpName, activeCell)
            performTravels(attempt::Nil) match {
              case (_, true)::Nil => usedCells(mjpName).addSleepers(1)
              case _ =>
            }
            nextAttempt(completed + 1)
          }
        }
        nextAttempt(0)
      }
    }

    // -----------------------------------------------------------
    // Radicalization Action -
    // Adjacent Travel to non-Good Muslim countrie without Troops when Biometrics is active
    // When the Biometrics event is lapsing, select a non-Good Muslim Country and travel
    // as many adjacent cells as possible to it.
    // Destination priorites: Adjacent to MJP, then Iran, then random
    // Use TravelFrom Priorites for cell sources.

    case object AdjacentTravelToNonGoodMuslimWithoutTroops extends RadicalizationAction {
      def candidates = game.muslims
        .filter(m=> !m.isGood && m.totalTroops == 0 && hasAdjacentTravelCells(m))

      override
      def criteriaMet(onlyReserveOpsRemain: Boolean): Boolean =
        lapsingEventInPlay(Biometrics) && candidates.nonEmpty
        
      // Make as many travel attempts as possible to the MJP
      //
      // cardsOps   - The number of unused Ops remaining from the card
      // reserveOps - The number of unused Ops remaining from reserves
      // Returns the number of ops used
      override
      def perform(cardOps: Int, reserveOps: Int): Int = {
        val priorities = List(
          new CriteriaFilter("Adjacent to Major Jihad Priority country",
            c => majorJihadPriorityCountry
              .map(mjp => areAdjacent(c.name, mjp))
              .getOrElse(false)
          ),
          new CriteriaFilter("Iran", _.name == Iran)
        )
        val mjpName = majorJihadPriorityCountry.get
        var headerDisplayed = false
        def displayHeader(): Unit = if (!headerDisplayed) {
          headerDisplayed = true
          log()
          log(s"Radicalization: Adjcent Travel to non-Good Muslim country when Biometrics is active")
        }
        val maxOps = cardOps + reserveOps
        val destination = topPriority(candidates, priorities)
          .map(_.name)
          .get

        def nextAttempt(completed: Int): Int = {
          val optSource = travelFromTarget(destination, adjacentTravelSources(destination))
          if (completed == maxOps || optSource.isEmpty)
            completed
          else {
            displayHeader()
            if (completed >= cardOps)
              expendBotReserves(1)
            addOpsTarget(destination)
            val source = optSource.get
            val activeCell = activeCells(game.getCountry(source)) > 0
            val attempt = TravelAttempt(source, mjpName, activeCell)
            performTravels(attempt::Nil) match {
              case (_, true)::Nil => usedCells(destination).addSleepers(1)
              case _ =>
            }
            nextAttempt(completed + 1)
          }
        }

        nextAttempt(0)
      }
    }

    // -----------------------------------------------------------
    // Radicalization Action -  Add surplus card Ops to reserves
    case object AddToReserves extends RadicalizationAction {
      override
      def criteriaMet(onlyReserveOpsRemain: Boolean): Boolean = {
        !onlyReserveOpsRemain &&
        game.reserves.jihadist < 2
      }

      // cardsOps - The number of unused Ops remaining from the card
      // Add any remaining card ops to reserves until reserves are full.
      // Returns the number of ops added
      override
      def perform(cardOps: Int, reserveOps: Int): Int = {
        log()
        log(s"Radicalization: Add to reserves")
        val opsAdded = cardOps min (2 - game.reserves.jihadist)
        addToReserves(Jihadist, opsAdded)
        opsAdded
      }
    }

  }

  // Perform radicalization
  // The opsUsed parameter is the number of Ops used to perform the card operation.
  // If this value is less than the number of Ops on the card, then we will
  // perform radicalization until a maximum of 3 Ops total have been used using
  // any available reserves as necessary.
  // If the opUsed is greater than or equal to the number of Ops on the card,
  // then we do nothing.
  def radicalization(card: Card, opsUsed: Int): Unit = {
    // A maximum of 3 ops can be used on a given card.  This is limited by the
    // number of available reserves.
    // Determine how ops we can use for radicalization.
    val noActionTakern = opsUsed == 0
    val unusedOps   = card.ops - opsUsed
    val maxReserves = (3 - card.ops) min game.reserves.jihadist
    val maxRadOps   = unusedOps + maxReserves
    val biometrics = lapsingEventInPlay(Biometrics)

    def addAction(test: Boolean, action: RadicalizationAction): Option[RadicalizationAction] =
      if (test) Some(action) else None

    val actionsToConsider = if (game.botEnhancements)
      List(
        addAction(true, EnhancedRadicalizationActions.PlotWMDInUS),
        addAction(!biometrics, EnhancedRadicalizationActions.TravelToUSForWMD),
        addAction(true, EnhancedRadicalizationActions.PlotToIncreaseFunding),
        addAction(true, EnhancedRadicalizationActions.PerformMinorJihadIfPossible),
        addAction(!biometrics, EnhancedRadicalizationActions.AdjacentTravelToGoodMuslims),
        addAction(true, EnhancedRadicalizationActions.Recruit),
        addAction(true, EnhancedRadicalizationActions.TravelToMajorJihadPriorityCountry),
        addAction(biometrics, EnhancedRadicalizationActions.AdjacentTravelToNonGoodMuslimWithoutTroops),
        addAction(true, EnhancedRadicalizationActions.AddToReserves),
      ).flatten
    else // Standard Bot
      List(
      StandardRadicalizationActions.PlotWMDInUS,
      StandardRadicalizationActions.TravelToUntestedNonMuslim,
      StandardRadicalizationActions.PlotInSoftNonMuslim,
      StandardRadicalizationActions.RecruitAtMuslimCadre,
      StandardRadicalizationActions.AddToReserves,
      StandardRadicalizationActions.Recruit,
      StandardRadicalizationActions.TravelToUS,
    )


    // Returns the number of actions executed
    def nextAction(actions: List[RadicalizationAction], completed: Int): Int = {
      if (actions.nonEmpty && completed < maxRadOps) {
        val cardOps    = (unusedOps - completed) max 0   // Ops remaining from the card
        val reserveOps = maxRadOps - cardOps - completed // Ops remaining from reserves
        val onlyReserveOpsRemain = cardOps == 0
        val action = actions.head

        val opsUsedByAction = if (action.criteriaMet(onlyReserveOpsRemain))
          action.perform(cardOps, reserveOps)
        else
          0

        nextAction(actions.tail, completed + opsUsedByAction)
      }
      else
        completed
    }

    log()
    log(s"$Jihadist performs Radicalization with ${amountOf(unusedOps, "unused Op")}")
    log(s"(Can add up to ${amountOf(maxReserves,"Op")} from reserve)")
    log(separator())
    if (nextAction(actionsToConsider, 0) == 0)
      log(s"\nThe $Jihadist does not perform any action.", Color.Event)
  }

  // Selects troops that are on the map to take off of the map.
  // This does not take troops from the track.  The caller should
  // only call this if there were not enough troops on the track
  // to satisfy the event.
  def troopsToTakeOffMap(remaining: Int, candidates: List[String]): List[MapItem] = {
    if (remaining == 0 || candidates.isEmpty)
      Nil
    else {
      val target = troopsMilitiaTarget(candidates).get
      val num    = game.getCountry(target).troops min remaining
      MapItem(target, num) :: troopsToTakeOffMap(remaining - num, candidates.filterNot(_ == target))
    }
  }


  // Select cells to place in a target country when the cells must first come from
  // the track, then from any of the given countries.
  def selecCellsToPlace(destination: String, sources: List[String], numCells: Int): List[CellsItem] = {

    def nextFromMap(countries: List[String], remaining: Int): List[CellsItem] =
      if (remaining == 0 || countries.isEmpty)
        Nil
      else
        JihadistBot.travelFromTarget(destination, countries, forPlacement = true) match {
          case Some(name) =>
            val c = game.getCountry(name)
            val n = remaining min JihadistBot.numCellsForTravel(c, destination)
            val a = n min activeCells(c)
            val s = n - a
            CellsItem(name, a, s) :: nextFromMap(countries.filterNot(_ == name), remaining - n)
          case None => Nil
        }

      val numFromTrack = numCells min game.cellsAvailable
      val fromMap = nextFromMap(sources, numCells - numFromTrack)
      if (numFromTrack > 0)
        CellsItem("track", 0, numFromTrack) :: fromMap
      else
        fromMap
  }

}
