
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

object JihadistBot extends BotHelpers {
  
  // This object keeps track of which on map cells have been used during the
  // current Bot operation.  A given cell cannot be used more than once during
  // the play of single card including during radicaization.
  object usedCells {
    // Record the number of active and sleeper cells in a country
    // that have been used.  [Country, (actives, sleepers)]
    var sadrUsed = false
    var cellMap = Map.empty[String, (Int, Int)].withDefaultValue((0, 0))
    def clear = {
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
  
  def sadrValue(s: Boolean) = if (s) 1 else 0
  def activeCells(c: Country)   = (c.activeCells  - usedCells(c.name).actives) max 0
  def sleeperCells(c: Country)  = (c.sleeperCells - usedCells(c.name).sleepers) max 0
  def unusedCells(c: Country)   = (c.totalCells   - usedCells(c.name).total) max 0
  def sadrAvailable(c: Country) = c.hasSadr && !usedCells.sadrUsed
  def totalUnused(c: Country)   = unusedCells(c) + sadrValue(sadrAvailable(c))
  def unusedCellsOnMap = (game.countries map totalUnused).sum
  // Sadr cannot travel
  def unusedCellsOnMapForTravel = (game.countries map unusedCells).sum
  
  // Poor country with 1 to 4 more cells than Troops and Militia and Jihad success possible
  // I added a couple of sanity checks that are not in the Bot Flowchart:
  // - Do not consider Pakistan if Benazir Bhutto is in play
  // - Do not consider countries where the US player has stacked
  //   so many troops/militia that there are not enough cells in
  //   the game to make a major jihad possible
  def poorMuslimNeedsCellsForMajorJihad(m: MuslimCountry): Boolean = 
    m.isPoor &&
    !(m.name == Pakistan && m.hasMarker(BenazirBhutto)) &&
    jihadSuccessPossible(m, true) &&
    game.totalCellCapacity - m.totalTroopsAndMilitia >= 5 &&
    (totalUnused(m) - m.totalTroopsAndMilitia) > 0 &&
    (totalUnused(m) - m.totalTroopsAndMilitia) < 5
  

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
      case _   => botLog(s"Not destination ($target): found [${results map (_.name) mkString ", "}]")
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
  val NeutralPriority = new CriteriaFilter("Neutral", muslimTest(_.isNeutral))
  
  // 33. Besieged Regime (Already defined at #7)
  
  // 34. Adjacent Good Ally
  val AdjacentGoodAllyPriority = new CriteriaFilter("Adjacent Good Ally", 
                  muslimTest(m => game.adjacentToGoodAlly(m.name)))
                  
  // 35. Fair non-Muslim
  val FairNonMuslimPriority = new CriteriaFilter("Fair non-Muslim", nonMuslimTest(_.isFair))
  
  // 36. Same posture as US
  val SamePostureAsUSPriority = new CriteriaFilter("Same posture as US",
                  nonMuslimTest(_.posture == game.usPosture))
                  
  // 37. Lowest REC#
  val LowestRECPriority = new LowestScorePriority("Lowest REC#", nonMuslimScore(_.recruitNumber, muslimScore = 100))
  
  // 38. Most cells  
  val MostCellsPriority = new HighestScorePriority("Most cells", unusedCells)
  
  // 39. Adjacent to Islamist Rule
  val AdjacentIslamistRulePriority = new CriteriaFilter("Adjacent to Islamist Rule", 
                  c => game.adjacentToIslamistRule(c.name))
                  
  // 40. Oil Exporter
  val OilExporterPriority = new CriteriaFilter("Oil exporter", muslimTest(_.oilExporter))


  // Jihadist OpP Flowchart filters
  
  val NonMuslimFilter     = new CriteriaFilter("non-Muslim", nonMuslimTest(_  => true))
  val PoorNonMuslimFilter = new CriteriaFilter("Poor non-Muslim", nonMuslimTest(_.isPoor))
  val FairNonMuslimFilter = new CriteriaFilter("Fair non-Muslim", nonMuslimTest(_.isFair))
  val GoodNonMuslimFilter = new CriteriaFilter("Good non-Muslim", nonMuslimTest(_.isGood))
  
  val PoorMuslimFilter  = new CriteriaFilter("Poor Muslim", muslimTest(_.isPoor))
  val FairMuslimFilter  = new CriteriaFilter("Fair Muslim", muslimTest(_.isFair))
  val GoodMuslimFilter  = new CriteriaFilter("Good Muslim", muslimTest(_.isGood))

  // To try to make the Bot AI smarter, we don't prioritise Good Muslim countries
  // unless there is a cell in an adjacent country so that the travel will succeed 
  // without a die roll
  val GoodMuslimWithAdjacentCellsFilter =
    new CriteriaFilter("Good Muslim w/ adjacent cells", muslimTest(m => m.isGood && m.hasAdjacent(hasCellForTravel)))
  val AutoRecruitFilter = new CriteriaFilter("Auto recruit", muslimTest(_.autoRecruit))
  
  val PoorTroopsActiveCellsFilter = new CriteriaFilter("Poor with troops and active cells",
                  muslimTest(m => m.isPoor && activeCells(m) > 0))
  val PoorNeedCellsforMajorJihad = new CriteriaFilter("Poor, 1-4 more cells than troops/militia and JSP",
                  muslimTest(m => poorMuslimNeedsCellsForMajorJihad(m)))
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
  val FairMuslimBestJihadDRMWithAdjacentCells = new LowestScoreNode("Fair Muslim w/ best Jihad DRM w/ Adjacent cells",
                  muslimTest(m => m.isFair && m.hasAdjacent(hasCellForTravel)),
                  muslimScore(m => jihadDRM(m, false), nonMuslimScore = 100))
  val PoorMuslimBestJihadDRM = new LowestScoreNode("Poor Muslim w/ best Jihad DRM",
                  muslimTest(_.isPoor),
                  muslimScore(m => jihadDRM(m, true), nonMuslimScore = 100))
  val FewestCellsFilter = new LowestScoreNode("Fewest cells", _ => true, _.totalCells)
  
  // jihad == None        - Do not include jihad drm priority
  // jihad == Some(true)  - Include major jihad drm priority
  // jihad == Some(false) - Include minor jihad drm priority
  def jihadMarkerAlignGovPriorities(jihad: Option[Boolean]): List[CountryFilter] = {
    val jihadPriorities = jihad match {
      case None    => List.empty
      case Some(x) => List(BestJihadDRMPriority(x))
    }
    
    game.currentMode match {
      case LabyrinthMode =>
        List(
          PakistanPriority, BesiegedRegimePriority,  WithAidPriority, RegimeChangeTroopsPriority, 
          HighestResourcePriority, WithTroopsPriority, MostCellsPriority, AdjacentIslamistRulePriority,
          OilExporterPriority)
      case AwakeningMode =>
        jihadPriorities ::: List(
          PakistanPriority, BesiegedRegimePriority, SyriaPriority, WithAidPriority, RegimeChangeTroopsPriority, 
          HighestResourcePriority, WithTroopsPriority, IranPriority, MostCellsPriority, AdjacentIslamistRulePriority,
          OilExporterPriority)
      case ForeverWarMode =>
        jihadPriorities ::: List(
          PakistanPriority, BesiegedRegimePriority, IranPriority, WithAidPriority, RegimeChangeTroopsPriority, 
          HighestResourcePriority, WithTroopsPriority, SyriaPriority, MostCellsPriority, AdjacentIslamistRulePriority,
          OilExporterPriority)
    }
  }
  
    
  // Bot will not try minor Jihad in Poor countries
  def minorJihadTarget(names: List[String]): Option[String] = {
    botLog("Find \"Minor Jihad\" target")
    topPriority(game getMuslims names, jihadMarkerAlignGovPriorities(Some(false))) map (_.name)
  }
  
  // Bot will only try major Jihad in Poor countries
  def majorJihadTarget(names: List[String]): Option[String] = {
    botLog("Find \"Major Jihad\" target")
    topPriority(game getMuslims names, jihadMarkerAlignGovPriorities(Some(true))) map (_.name)
  }
  
  def markerAlignGovTarget(names: List[String]): Option[String] = {
    botLog("Find \"Marker/Align/Gov\" target")
    topPriority(game getCountries names, jihadMarkerAlignGovPriorities(None)) map (_.name)
  }
  
  def troopsMilitiaTarget(names: List[String]): Option[String] = {
    botLog("Find \"Troops/Militia\" target")
    topPriority(game getCountries names, jihadMarkerAlignGovPriorities(None)) map (_.name)    
  }

  val TightPlotFlowchart = List(
    PoorNonMuslimFilter, FairNonMuslimFilter, GoodNonMuslimFilter)
    
  val OtherPlotFlowchart = List(
    PoorTroopsActiveCellsFilter, FairMuslimFilter, GoodMuslimFilter, NonMuslimFilter,
    PoorMuslimFilter)
  
  val LabyrinthPlotPriorities = List(
    USPriority, WithPrestigeTroopsPriority, PakistanPriority, PhilippinesPriority,
    MostActveCellsPriority, WithAidPriority, RegimeChangeTroopsPriority,
    HighestResourcePriority, NeutralPriority, BesiegedRegimePriority,
    AdjacentGoodAllyPriority, FairNonMuslimPriority, SamePostureAsUSPriority, 
    LowestRECPriority, AdjacentIslamistRulePriority, OilExporterPriority)

  val AwakeningPlotPriorities = List(
    USPriority, WithPrestigeTroopsPriority, PakistanPriority,
    MostActveCellsPriority, SyriaPriority, WithAidPriority, RegimeChangeTroopsPriority,
    HighestResourcePriority, IranPriority, CivilWarPriority, NeutralPriority, BesiegedRegimePriority,
    AdjacentGoodAllyPriority, FairNonMuslimPriority, SamePostureAsUSPriority, 
    LowestRECPriority, AdjacentIslamistRulePriority, OilExporterPriority)
    
  val ForeverWarPlotPriorities = List(
    USPriority, WithPrestigeTroopsPriority, PakistanPriority,
    MostActveCellsPriority, IranPriority, WithAidPriority, RegimeChangeTroopsPriority,
    HighestResourcePriority, SyriaPriority, CivilWarPriority, NeutralPriority, BesiegedRegimePriority,
    AdjacentGoodAllyPriority, FairNonMuslimPriority, SamePostureAsUSPriority, 
    LowestRECPriority, AdjacentIslamistRulePriority, OilExporterPriority)
    
  def plotPriorities: List[CountryFilter] = game.currentMode match {
    case LabyrinthMode   => LabyrinthPlotPriorities
    case AwakeningMode   => AwakeningPlotPriorities
    case ForeverWarMode  => ForeverWarPlotPriorities
  }
  
  def plotTarget(names: List[String]): Option[String] = {
    val flowchart = if (game.fundingLevel == Tight)
      TightPlotFlowchart ::: OtherPlotFlowchart
    else
      OtherPlotFlowchart
    botLog("Find \"Plot\" target")
    val candidates = selectCandidates(game getCountries names, flowchart)
    topPriority(candidates, plotPriorities) map (_.name)
  }
  
  def plotPriority(names: List[String]): Option[String] = {
    topPriority(game getCountries names, plotPriorities) map (_.name)
  }
  
  def RecruitFlowchart = 
    List(PoorNeedCellsforMajorJihad, AutoRecruitBestJihadDRM, GoodMuslimFilter,
         FairMuslimBestJihadDRM, NonMuslimFilter, PoorMuslimBestJihadDRM)        
    
  def TravelToFlowchart = if (game.botEnhancements)
    List(PoorNeedCellsforMajorJihad, GoodMuslimWithAdjacentCellsFilter,
         FairMuslimBestJihadDRMWithAdjacentCells, PoorMuslimBestJihadDRM, NonMuslimFilter)
  else
    List(PoorNeedCellsforMajorJihad, GoodMuslimFilter,
         FairMuslimBestJihadDRM, NonMuslimFilter, PoorMuslimBestJihadDRM)        
  
  val LabyrinthRecruitAndTravelToPriorities = List(
    NotIslamistRulePriority, PakistanPriority, BesiegedRegimePriority,
    USPriority, PoorPriority, FairPriority, GoodPriority, HighestResourcePriority, 
    RussiaPriority, NoDisruptPretigePriority, HighestRECPriority, SamePostureAsUSPriority,
    MostCellsPriority, AdjacentIslamistRulePriority, OilExporterPriority
  )
  val AwakeningRecruitAndTravelToPriorities = List(
    NotIslamistRulePriority, PakistanPriority, BesiegedRegimePriority,
    SyriaPriority, IranPriority, USPriority, PoorPriority, FairPriority,
    GoodPriority, HighestResourcePriority, NoDisruptPretigePriority,
    HighestRECPriority, BestJihadDRMPriority(false), SamePostureAsUSPriority,
    MostCellsPriority, AdjacentIslamistRulePriority, OilExporterPriority
  )
  val ForeverWarRecruitAndTravelToPriorities = List(
    NotIslamistRulePriority, PakistanPriority, BesiegedRegimePriority,
    IranPriority, SyriaPriority, USPriority, PoorPriority, FairPriority,
    GoodPriority, HighestResourcePriority, NoDisruptPretigePriority,
    HighestRECPriority, BestJihadDRMPriority(false), SamePostureAsUSPriority,
    MostCellsPriority, AdjacentIslamistRulePriority, OilExporterPriority
  )
  
  def recruitAndTravelToPriorities: List[CountryFilter] = game.currentMode match {
    case LabyrinthMode   => LabyrinthRecruitAndTravelToPriorities
    case AwakeningMode   => AwakeningRecruitAndTravelToPriorities
    case ForeverWarMode  => ForeverWarRecruitAndTravelToPriorities
  }
  
  def recruitTravelToPriority(names: List[String]): Option[String] = {
    topPriority(game getCountries names, recruitAndTravelToPriorities) map (_.name)
  }
  
  def fewestCellsPriority(names: List[String]): Option[String] = {
    val priorities = List(new LowestScorePriority("Least cells", (_.totalCells)))
    topPriority(game getCountries names, priorities) map (_.name)
  }

  // For the Early Exit event, Forever War card #297
  def earlyExitPriority(names: List[String]): Option[String] = {
    val withAdvisors = (game getCountries names) filter (_.numAdvisors > 0)
    
    if (withAdvisors.nonEmpty)
      topPriority(withAdvisors, List(new HighestScorePriority("Most Advisors", (_.numAdvisors)))) map (_.name)
    else
      topPriority(game getCountries names, List(new LowestScorePriority("Fewest Troops", (_.totalTroops)))) map (_.name)
  }
  
  
  def recruitTarget(names: List[String]): Option[String] = {
    botLog("Find \"Recruit\" target")
    val candidates = selectCandidates(game getCountries names, RecruitFlowchart)
    topPriority(candidates, recruitAndTravelToPriorities) map (_.name)
  }
  

  def travelToTarget(names: List[String]): Option[String] = {
    botLog("Find \"Travel To\" target")
    val candidates = selectCandidates(game getCountries names, TravelToFlowchart)
    topPriority(candidates, recruitAndTravelToPriorities) map (_.name)
  }
  

  
  def travelFromPriorities: List[CountryFilter] = game.currentMode match {
    case LabyrinthMode   => LabyrinthRecruitAndTravelToPriorities
    case AwakeningMode   => AwakeningRecruitAndTravelToPriorities
    case ForeverWarMode  => ForeverWarRecruitAndTravelToPriorities
  }
  
  def travelFromTarget(toCountry: String, names: List[String]): Option[String] = {
    // The bot will never travel a SLEEPER cell within the same country
    def wouldMoveOrTravelWithinToSleep(c: Country) = c.name != toCountry || (activeCells(c) > 0 && !game.isCaliphateMember(c.name))
    
    botLog("Find \"Travel From\" target")
    val flowchart = List(
      new AdjacentCountriesNode(toCountry),
      AutoRecruitFilter,
      FewestCellsFilter)

    val priorities = List(new NotDestinationPriority(toCountry), IslamistRulePriority,
                          PoorPriority, FairPriority, GoodPriority, NotUSPriority,
                          MostActveCellsPriority, NotRegimeChangePriority, WorstJihadDRMPriority,
                          DisruptPrestigePriority, LowestRECPriority)
    val withCells  = game.getCountries(names) filter hasCellForTravel filter wouldMoveOrTravelWithinToSleep
    val candidates = selectCandidates(withCells, flowchart)
    topPriority(candidates, priorities) map (_.name)
  }
  
  
  // This is used for some events where we want to check the priorities only,
  // and skip the flowchart.
  def travelFromPriorities(toCountry: String, names: List[String]): Option[String] = {
    val priorities = List(
      new NotDestinationPriority(toCountry), IslamistRulePriority,
      PoorPriority, FairPriority, GoodPriority, NotUSPriority,
      MostActveCellsPriority, NotRegimeChangePriority, WorstJihadDRMPriority,
      DisruptPrestigePriority, LowestRECPriority)
    topPriority(game getCountries names, priorities) map (_.name)
  }
  
  
  def botRecruitTargets(muslimWithCadreOnly: Boolean): List[String] = {
    val maxCellsInIR = if (game.botEnhancements) 7 else 1000  // ie.  no limit if botEnancement not in use
    game.getCountries(game.recruitTargets(madrassas = false)).
    filter(c => ((c.isMuslim && c.hasCadre) || !muslimWithCadreOnly) && (!c.isIslamistRule || c.totalCells < maxCellsInIR))
    .map(_.name)
  }
  
  // To try and make the Bot AI a bit smarter, we don't
  // allow the Bot to recruit into an Islamist Rule country if it
  // already contains 7 or more cells.
  def botRecruitPossible(muslimWithCadreOnly: Boolean): Boolean = game.recruitPossible && botRecruitTargets(muslimWithCadreOnly).nonEmpty

  // Jihadist Operations Flowchart definitions.
  sealed trait Operation extends OpFlowchartNode
  case object RecruitOp    extends Operation
  case object TravelOp     extends Operation
  case object PlotOp       extends Operation
  case object MinorJihadOp extends Operation
  case object MajorJihadOp extends Operation
  
  // This is the starting point of the Operations Flowchart
  object MajorJihadDecision extends OperationDecision {
    val desc = "Major Jihad Success possible at Poor?"
    def yesPath = MajorJihadOp
    def noPath  = FundingTightDecision
    def condition(ops: Int) = game.majorJihadTargets(ops) map game.getMuslim exists { m =>
      m.isPoor &&
      totalUnused(m) - m.totalTroopsAndMilitia >= 5 &&
      jihadSuccessPossible(m, true)
    }
  }
  
  object FundingTightDecision extends OperationDecision {
    val desc = "Funding Tight?"
    def yesPath = CellAvailableOrPlotDecision
    def noPath  = CellInGoodFairWhereJSP
    def condition(ops: Int) = game.fundingLevel == Tight
  }
  
  object CellAvailableOrPlotDecision extends OperationDecision {
    val desc = "Cells Available?"
    def yesPath = RecruitOp
    def noPath  = PlotOp
    def condition(ops: Int) = botRecruitPossible(muslimWithCadreOnly = false)
  }
  
  object CellAvailableOrTravelDecision extends OperationDecision {
    val desc = "Cells Available?"
    def yesPath = RecruitOp
    def noPath  = TravelOp
    def condition(ops: Int) = botRecruitPossible(muslimWithCadreOnly = false)
  }
  
  object CellInGoodFairWhereJSP extends OperationDecision {
    val desc = "Cells in Good or Fair Muslim where Jihad Success Possible?"
    def yesPath = MinorJihadOp
    def noPath  = PoorNeedCellsforMajorJihadDecision
    def condition(ops: Int) = {
      game hasMuslim { m =>
        m.jihadOK &&
        (m.isGood || m.isFair) &&
        jihadSuccessPossible(m, false) && 
        totalUnused(m) > 0
      }
    }
  }
  
  // This object incorporates two boxes on the Flowchart
  // Finds poor countries in need of cells for major jihad,
  // Finds the highest priority travel destination among them and checks
  // to see if there is a cell in an adjacent country.
  object PoorNeedCellsforMajorJihadDecision extends OperationDecision {
    val desc = "Poor Muslim w/ 1-4 more cells than TandM & Jihad Success Possible?"
    def yesPath = TravelOp
    def noPath  = FundingModerateDecision
    def condition(ops: Int) = {
      val candidates = 
        countryNames(game.getMuslims(game.jihadTargets) filter poorMuslimNeedsCellsForMajorJihad)
      travelToTarget(candidates) match {
        case None         => false
        case Some(target) => game.adjacentCountries(target) exists hasCellForTravel
      }
    }
  }
  
  object FundingModerateDecision extends OperationDecision {
    val desc = "Funding Moderate?"
    def yesPath = PrestigeOver1AndActiveCellWithTroopsDecision
    def noPath  = CellAvailableOrTravelDecision
    def condition(ops: Int) = game.fundingLevel == Moderate
  }
  
  object PrestigeOver1AndActiveCellWithTroopsDecision extends OperationDecision {
    val desc = "Prestige > 1 and Active cell with Troops?"
    def yesPath = PlotOp
    def noPath  = CellAvailableOrCellInNonMuslimDecision
    def condition(ops: Int) = 
      game.prestige > 1 &&
      (game hasMuslim (m => activeCells(m) > 0 && m.totalTroopsThatAffectPrestige > 0))
  }
  
  object CellAvailableOrCellInNonMuslimDecision extends OperationDecision {
    val desc = "Cells Available?"
    def yesPath = RecruitOp
    def noPath  = CellInNonMuslim
    def condition(ops: Int) = botRecruitPossible(muslimWithCadreOnly = false)
  }
  
  object CellInNonMuslim extends OperationDecision {
    val desc = "Cell in non-Muslim?"
    def yesPath = PlotOp
    def noPath  = TravelOp
    def condition(ops: Int) = game hasNonMuslim (totalUnused(_) > 0)
  }
  
  // Follow the operations flowchart to pick which operation will be performed.
  def operationsFlowchart(ops: Int): Operation = {
    @tailrec def evaluateNode(node: OpFlowchartNode): Operation = node match {
      case operation: Operation        => operation
      case decision: OperationDecision =>
        botLog(s"EvO Flowchart: $node")
        if (decision.condition(ops))
          evaluateNode(decision.yesPath)
        else
          evaluateNode(decision.noPath)
    }
    evaluateNode(MajorJihadDecision)
  }
  
  
  
  // ------------------------------------------------------------------
  def posturePriority(names: List[String]): Option[String] = {
    val priorities = List(
      new CriteriaFilter("Same posture as US",
        nonMuslimTest(n => !n.isUntested && n.canChangePosture && n.posture == game.usPosture)),
      new CriteriaFilter("Untested non-Muslim", nonMuslimTest(_.isUntested)),
      FewestCellsFilter)
    topPriority(game getNonMuslims names, priorities) map (_.name)
  }
  
  // ------------------------------------------------------------------
  def goodPriority(names: List[String]): Option[String] = {
    val priorities = GoodPriority::Nil
    botLog("Find \"Good Priority\" target")
    topPriority(game getCountries names, priorities) map (_.name)
  }

  def goodThenFairThenPoorPriority(names: List[String]): Option[String] = {
    val priorities = GoodPriority::FairPriority::PoorPriority::Nil
    botLog("Find \"Good/Fair/Poor Priority\" target")
    topPriority(game getCountries names, priorities) map (_.name)
  }
  
  // ------------------------------------------------------------------
  // Get target for the Status Quo event
  def changeOfStateTarget(names: List[String]): Option[String] = {
    val flowchart = List(
      new CriteriaFilter("Fair Ally", muslimTest(m => m.isFair && m.isAlly)))
    val priorities = List(
      HighestResourcePriority,
      new CriteriaFilter("No Troops", muslimTest(m => m.totalTroops == 0)))
      
    botLog("Find \"Change of State\" target")
    val candidates = selectCandidates(game getCountries names, flowchart)
    topPriority(candidates, priorities) map (_.name)
  }
  
  // ------------------------------------------------------------------
  // Get target for the Status Quo event
  def talibanResurgentTarget(names: List[String]): Option[String] = {
    val flowchart = List(
      GoodPriority,
      FairPriority,
      new CriteriaFilter("Poor with Troops and US Prestige > 1", 
           muslimTest(m => m.isPoor && game.prestige > 1)))
      
    botLog("Find \"Taliban Resurgent\" target")
    val candidates = countryNames(selectCandidates(game getCountries names, flowchart))
    minorJihadTarget(candidates)
  }
  
  def iranTarget(names: List[String]): Option[String] = {
    val priorities = List(
      GoodPriority,
      FairPriority,
      new CriteriaFilter("Untested Muslim", muslimTest(_.isUntested)))
      botLog("Find \"Iran\" target")
      topPriority(game getMuslims names, priorities) map (_.name)
  }
  
  
  def criticalMiddleShiftPossibilities(names: List[String]): List[String] = {
    val flowchart = List(
      new CriteriaFilter("Ally",    muslimTest(m => m.isAlly)),
      new CriteriaFilter("Neutral", muslimTest(m => m.isNeutral)))
    botLog("Find \"Critical Middle\" target")
    countryNames(selectCandidates(game getCountries names, flowchart)) 
  }
  
  def unCeasefireTarget(names: List[String]): Option[String] = {
    val priorities = List(
      new HighestScorePriority("Most cells - militia", muslimScore(m => m.totalCells - m.militia)),
      new CriteriaFilter("Ally",    muslimTest(m => m.isAlly)),
      new CriteriaFilter("Neutral", muslimTest(m => m.isNeutral)))
      
    botLog("Find \"UN Ceasefire\" target")
    topPriority(game getCountries names, priorities) map (_.name)
  }
  
  def qadhafiTarget(names: List[String]): Option[String] = {
    val priorities = List(
      new CriteriaFilter("Cells > TandM", muslimTest(m => m.totalCells > m.totalTroopsAndMilitia)),
      HighestResourcePriority)
      
    botLog("Find \"Qadhafi\" target")
    topPriority(game getCountries names, priorities) map (_.name)
  }

  def revolutionTarget(names: List[String]): Option[String] = {
    val priorities = List(
      new HighestScorePriority("Highest awakening - reaction", muslimScore(m => m.awakening - m.reaction)),
      HighestResourcePriority)
      
    botLog("Find \"Revolution\" target")
    topPriority(game getCountries names, priorities) map (_.name)
  }
  
  def hamaOffensiveTarget(names: List[String]): Option[String] = {
    val priorities = List(
      new HighestScorePriority("Largest Cells - TandM", muslimScore(m => m.totalCells - m.totalTroopsAndMilitia)),
      HighestResourcePriority)
      
    botLog("Find \"Hama Offensive\" target")
    topPriority(game getCountries names, priorities) map (_.name)
  }
  
  //  To select from which country adjacent to Syria
  //  with cells to move cells from.
  def hayatTahirTarget(names: List[String]): Option[String] = {
    val priorities = List(
      new CriteriaFilter("With troops", muslimTest(m => m.totalTroops > 0)),
      new HighestScorePriority("Largest Cells - TandM", muslimScore(m => m.totalCells - m.totalTroopsAndMilitia)))
      
    botLog("Best \"Hayat Tahir\" adjacent with cells")
    topPriority(game getCountries names, priorities) map (_.name)
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
  def chooseTroopOrMilitiaToRemove(name: String): String = {
    val c = game.getCountry(name)
    if (c.troopsMarkers.nonEmpty)
      c.troopsMarkers.sorted.reverse.head.name
    else if (c.troops > 0)
      "troop-cube"
    else if (game.isMuslim(name) && game.getMuslim(name).militia > 0)
      "militia-cube"
    else
      throw new IllegalStateException(s"JihadistBot.chooseTroopOrMilitiaToRemove($name) not units present")
  }
  
  // The Bot will declare the Caliphate if it results in an auto win,
  // or if there is at least one other adjacent country that qualifies to be part
  // of the Caliphate.
  def willDeclareCaliphate(capital: String): Boolean = {
    canDeclareCaliphate(capital) &&
    (game.islamistResources == 5 || game.caliphateDaisyChain(capital).size > 1)
  }
  
  def maxOpsPlusReserves(card: Card): Int = (card.ops + game.reserves.jihadist) min 3
  
  // Decrement the Bots reserves and log that they were used.
  def expendBotReserves(ops: Int): Unit = {
    if (ops > 0) {
      assert(game.reserves.jihadist >= ops,
         s"expendBotReserves($ops): Only ${opsString(game.reserves.jihadist)} in reserve")
     game = game.copy(reserves = game.reserves.copy(jihadist = game.reserves.jihadist - ops))
     log()
     log(s"$Jihadist expends ${opsString(ops)} from reserves.  Reserves now ${opsString(game.reserves.jihadist)}")
    }
  }
  
  
  def performTriggeredEvent(card: Card): Unit = {
    usedCells.clear
    performCardEvent(card, Jihadist, triggered = true)
  }
  
  // Starting point for Jihadist bot card play.
  def cardPlay(card: Card, playable: Boolean): Unit = {
    usedCells.clear
    
    // If the event is playable then the event is always executed
    if (playable) {
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
      if (!(game hasCountry (c => c.totalCells > 0 || c.hasCadre))) {
        val opsAdded = card.ops min (2 - game.reserves.jihadist)
        log("There are no cells or cadres on the map.")
        log(s"The $Jihadist Bot cannot execute an operation until an event places a cell.")
        addToReserves(Jihadist, card.ops)
      }
      else {
        val opsUsed = operationsFlowchart(maxOpsPlusReserves(card)) match {
          case RecruitOp    => recruitOperation(card)
          case TravelOp     => travelOperation(card)
          case PlotOp       => plotOperation(card)
          case MinorJihadOp => minorJihadOperation(card)
          case MajorJihadOp => majorJihadOperation(card)
        }
        radicalization(card, opsUsed)
      }
    }
  }
  
  // Attempt to recruit as many times as possible up to 3
  // If we run out of card ops and there are still cells available, use reserves
  // If we run out of cells and still have card ops (not reserves), then use the
  // excess card ops for radicalization.
  // Returns the number of Ops used.
  def recruitOperation(card: Card): Int = {
    val recruitOps = game.cellsToRecruit min maxOpsPlusReserves(card)
    log()
    log(s"$Jihadist performs a Recruit operation")
    log(separator())
    if (recruitOps > card.ops)
      expendBotReserves(recruitOps - card.ops)
    
    if (recruitOps > 0) 
      performRecruit(recruitOps)
    recruitOps
  }
  
  def performRecruit(ops: Int, ignoreFunding: Boolean = false, madrassas: Boolean = false): Unit = {
    // We should always find a target for recruit operations.
    val target = recruitTarget(botRecruitTargets(muslimWithCadreOnly = false)) getOrElse {
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
        val die     = dieRoll
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
  
  // Test the country to see if it has an unused cells that 
  // can travel.
  // If the country is an auto-recruit country, then it must
  // have more than one cell, unless there are at least two 
  // other auto-recruit countries with cells.
  def hasCellForTravel(c: Country): Boolean = {
    if (c.autoRecruit) {
      val numAutoRecruit = game.muslims count (m => m.autoRecruit && m.totalCells > 0)
      if (numAutoRecruit > 2)
        unusedCells(c) > 0
      else
        unusedCells(c) > 1
    }
    else
      unusedCells(c) > 0
  }
  
  // First select the target to country.
  // Then select one or more countries from which cells may travel.
  // For each source country, make as many attempts as possible, before
  // moving on to the next source (as long as Ops are remaining)
  // - Only travel the last cell out of an Auto Recruit country if
  //   there are two or more other Auto Recruit countries with cells.
  // - Select active cells for travel before sleeper cells
  // - Never travel sleeper cells within the same country
  // Returns the number of Ops used.
  def travelOperation(card: Card): Int = {
    log()
    log(s"$Jihadist performs a Travel operation")
    log(separator())
    
    // If Biometrics is in effect only adjacent travel is allowed.
    val toCandidates = if (lapsingEventInPlay(Biometrics)) {
      // Find countries that are adjacent to other countries with cells, or that
      // have active cells (which can travel in place)
      val validCountries = game.countries filter { c =>
        c.activeCells > 0 ||
        (game.adjacentCountries(c.name) exists (_.totalCells > 0))
      }
      countryNames(validCountries)
    }
    else
      countryNames(game.countries)
    
    val toName = travelToTarget(toCandidates) getOrElse {
      throw new IllegalStateException("travelOperation() should always find \"travel to\" target")
    }
    
    // Count sadr here, but below when finding cells to move.
    def numAutoRecruit = game.muslims count (m => m.autoRecruit && m.totalCells > 0)

    def numAutoRecruitWithCells(travelAttempts: List[TravelAttempt]) = {
      game.muslims.count { m =>
        val usedCells = travelAttempts.count(x => x.from == m.name)
        val numCells = m.totalCells - usedCells
        (m.autoRecruit && numCells > 0)
      }
    }
    val maxTravel = maxOpsPlusReserves(card)
    
    def nextTravelFrom(alreadyTried: Set[String], attempts: List[TravelAttempt]): List[TravelAttempt] = {
      val remaining = maxTravel - attempts.size
      if (remaining == 0)
        attempts  // We've used all available Ops
      else {
        val canTravelFrom = (c: Country) => !alreadyTried(c.name) && hasCellForTravel(c)
        val canTravelInPlace = !alreadyTried(toName) && game.getCountry(toName).activeCells > 0
        val candidates = if (lapsingEventInPlay(Biometrics)) {
          val toCountry = if (canTravelInPlace) List(game.getCountry(toName)) else Nil
          countryNames((game.adjacentCountries(toName) filter canTravelFrom) ::: toCountry)
        }
        else
          countryNames(game.countries filter canTravelFrom)
        travelFromTarget(toName, candidates) match {
          case None => attempts  // No more viable countries to travel from
          case Some(fromName) =>
            val from = game.getCountry(fromName)
            // Limit numAttempts to only active cells within same country
            // and to not allow last cell out of auto recruit (if only 1 other auto recruit with cells)
            val numAttempts = if (fromName == toName)
              activeCells(from) min remaining
            else if (from.autoRecruit && numAutoRecruitWithCells(attempts) < 3)
              (unusedCells(from) - 1) max 0 min remaining
            else
              unusedCells(from) min remaining
          
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
      (countryNames(game.countries) filter isTravelBanCountry).toSet
    else
      Set.empty[String]

    
    val attempts = nextTravelFrom(noUK ++ banned, Nil)
    val opsUsed = attempts.size
    if (card.ops < opsUsed)
      expendBotReserves(opsUsed - card.ops)
    
    addOpsTarget(toName)
    for ((name, true) <- performTravels(attempts))
      usedCells(toName).addSleepers(1)

    opsUsed
  }
  
  // Returns the number of Ops used.
  def plotOperation(card: Card): Int = {
    val maxCells = (game.plotTargets map game.getCountry map unusedCells).sum
    val maxAttempts = maxOpsPlusReserves(card) min maxCells
    log()
    log(s"$Jihadist performs a Plot operation")
    log(separator())
    
    // Return the number of attempts made.
    def nextTarget(completed: Int, alreadyTried: Set[String]): Int = {
      val remaining = maxAttempts - completed
      if (remaining == 0 || game.availablePlots.isEmpty) {
        completed
      }
      else {
        val canPlot = (c: Country) => !alreadyTried(c.name) &&
                                      totalUnused(c) > 0 && (
                                        (game isNonMuslim c.name) ||
                                        !(game getMuslim c.name).isIslamistRule
                                      )
        val candidates = countryNames(game.countries filter canPlot)
        plotTarget(candidates) match {
          case None => completed
          case Some(name) =>
            val c = game getCountry name
            val numAttempts = totalUnused(c) min remaining
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
    log()
    log(s"$Jihadist performs a Minor Jihad operation")
    log(separator())
    val maxJihad = maxOpsPlusReserves(card)
    def nextJihadTarget(completed: Int, alreadyTried: Set[String]): List[JihadTarget] = {
      val remaining = maxJihad - completed
      if (remaining == 0)
        Nil  // We've used all available Ops
      else {
        // The Bot will never conduct minor Jihad in a country with Poor governance.
        val canJihad = (m: MuslimCountry) => !alreadyTried(m.name)          &&
                                             jihadSuccessPossible(m, false) &&
                                             totalUnused(m) > 0 && !m.isPoor
        val candidates = countryNames(game.jihadTargets map game.getMuslim filter canJihad)
        minorJihadTarget(candidates) match {
          case None => Nil   // No more candidates
          case Some(name) =>
            val m = game.getMuslim(name)
            val numAttempts = totalUnused(m) min remaining
            val actives  = numAttempts min activeCells(m)
            val sleepers = (numAttempts - actives) min sleeperCells(m)
            val sadr     = numAttempts - actives - sleepers > 0
            val target = JihadTarget(name, actives, sleepers, sadr, major = false)
            target :: nextJihadTarget(completed + numAttempts, alreadyTried + name)
        }
      }
    }

    val targets = nextJihadTarget(0, Set.empty)
    val opsUsed = (for (JihadTarget(_, a, s, sadr, _) <- targets) 
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
  def majorJihadOperation(card: Card): Int = {
    log()
    log(s"$Jihadist performs a Major Jihad operation")
    log(separator())
    val maxJihad = maxOpsPlusReserves(card)
    def nextJihadTarget(completed: Int, alreadyTried: Set[String]): List[JihadTarget] = {
      val remaining = maxJihad - completed
      if (remaining == 0)
        Nil  // We've used all available Ops
      else {
        // The Bot will only conduct major Jihad in a countries with Poor governance.
        val canJihad = (m: MuslimCountry) => !alreadyTried(m.name)         &&
                                             jihadSuccessPossible(m, true) &&
                                             m.isPoor                      &&
                                             totalUnused(m) - m.totalTroopsAndMilitia >= 5
        val candidates = countryNames(game.majorJihadTargets(3) map game.getMuslim filter canJihad)
        majorJihadTarget(candidates) match {
          case None => Nil   // No more candidates
          case Some(name) =>
            val m = game.getMuslim(name)
            val numAttempts = remaining
            // Sadr never used to roll a die in Major Jihad
            // There are alway at least 4 other cells in the country.
            val target = JihadTarget(name, numAttempts, 0, false, major = true)
            target :: nextJihadTarget(completed + numAttempts, alreadyTried + name)
        }
      }
    }

    val targets = nextJihadTarget(0, Set.empty)
    val opsUsed = (for (JihadTarget(_, a, s, _, _) <- targets) yield(a + s)).sum
    if (card.ops < opsUsed)
      expendBotReserves(opsUsed - card.ops)
    for (t <- targets)
      addOpsTarget(t.name)
    for ((name, successes, _) <- performJihads(targets))
      usedCells(name).addActives(successes)
    
    opsUsed
  }
  
  
  sealed trait RadicalizationAction
  case object PlotWMDInUS               extends RadicalizationAction
  case object TravelToUntestedNonMuslim extends RadicalizationAction
  case object PlotInSoftMuslim          extends RadicalizationAction
  case object RecruitAtMuslimCadre      extends RadicalizationAction
  case object AddToReserves             extends RadicalizationAction
  case object Recruit                   extends RadicalizationAction
  case object TravelToUS                extends RadicalizationAction

  // The requiresReserves parameter indicates, that all of the Ops on the played
  // card have been used, and any further actions will require the use of reserves.
  // The AddToReserves, Recruit, and TravelToUS action cannot use reserves.
  def getRadicalizationAction(requiresReserves: Boolean): Option[RadicalizationAction] = {
    val canPlotWMDInUs = (game.availablePlots contains PlotWMD) && 
                         totalUnused(game getCountry UnitedStates) > 0
    val canTravelToUntestedNonMuslim = game.usPosture == Hard        &&
                                       game.gwotPenalty == 0         &&
                                       unusedCellsOnMapForTravel > 0 &&
                                       (game hasNonMuslim (_.isUntested))
    val canPlotInSoftMuslim = game.usPosture == Soft &&
                              game.gwotPenalty == 0  &&
                              game.availablePlots.nonEmpty &&
                              (game hasNonMuslim (n => n.isSoft && totalUnused(n) > 0))
    // I'm allowing recruit in IR countries, not sure if that is the intent?
    val canRecruitAtMuslimCadre = botRecruitPossible(muslimWithCadreOnly = true)
    val canAddToReserves = !requiresReserves && game.reserves.jihadist < 2
    val canRecruit = botRecruitPossible(muslimWithCadreOnly = false) && !requiresReserves
    val canTravelToUS = !requiresReserves && unusedCellsOnMapForTravel > 0
    
    if      (canPlotWMDInUs)               Some(PlotWMDInUS)
    else if (canTravelToUntestedNonMuslim) Some(TravelToUntestedNonMuslim)
    else if (canPlotInSoftMuslim)          Some(PlotInSoftMuslim)
    else if (canRecruitAtMuslimCadre)      Some(RecruitAtMuslimCadre)
    else if (canAddToReserves)             Some(AddToReserves)
    else if (canRecruit)                   Some(Recruit)
    else if (canTravelToUS)                Some(TravelToUS)
    else                                   None
  }
  
  // Perform radicalization
  // The opsUsed parameter is the number of Ops used to perform the card operation.
  // If this value is less than the number of Ops on the card, then we will 
  // perform radicalization until a maximum of 3 Ops total have been used using
  // any available reserves as necessary.
  // If the opUsed is greater than or equal to the number of Ops on the card,
  // then we do nothing.
  def radicalization(card: Card, opsUsed: Int): Unit = {
    if (opsUsed < card.ops) {
      // A maximum of 3 ops can be used on a given card.  This is limited by the
      // number of available reserves.
      // Determine how ops we can use for radicalization.
      val unusedOps   = card.ops - opsUsed
      val maxReserves = (3 - card.ops) min game.reserves.jihadist
      val maxRadOps   = unusedOps + maxReserves
      log()
      log(s"$Jihadist performs Radicalization with ${amountOf(unusedOps, "unused Op")} (${amountOf(maxReserves,"reserve")})")
      log(separator())
    
      // Returns the number of actions executed
      def nextAction(completed: Int): Unit = {
        if (completed < maxRadOps) {
          val cardOps    = (unusedOps - completed) max 0   // Ops remaining from the card
          val reserveOps = maxRadOps - cardOps - completed // Ops remaining from reserves
          val ops = getRadicalizationAction(cardOps == 0) match {
            case Some(PlotWMDInUS)               => radPlotWMDInUs(cardOps, reserveOps)
            case Some(TravelToUntestedNonMuslim) => radTravelToUntestedNonMuslim(cardOps, reserveOps)
            case Some(PlotInSoftMuslim)          => radPlotInSoftMuslim(cardOps, reserveOps)
            case Some(RecruitAtMuslimCadre)      => radRecruitAtMuslimCadre(cardOps, reserveOps)
            case Some(AddToReserves)             => radAddToReserves(cardOps)
            case Some(Recruit)                   => radRecruit(cardOps)
            case Some(TravelToUS)                => radTravelToUS(cardOps)
            case None => -1  // Finished with radicalization
          }
          if (ops > 0)  
            nextAction(completed + ops)
        }
      }
      nextAction(0)
    }
  }
  
  // Plot as many time as possible in the US as long as there are
  // unused cells there.
  // cardsOps   - The number of unused Ops remaining from the card
  // reserveOps - The number of unused Ops remaining from reserves
  // Returns the number of ops used
  def radPlotWMDInUs(cardOps: Int, reserveOps: Int): Int = {
    log()
    log(s"Radicalization: Plot in the United States")
    val maxOps = cardOps + reserveOps
    def nextAttempt(completed: Int): Int = {
      val us = game getCountry UnitedStates
      if (completed == maxOps || totalUnused(us) == 0 || game.availablePlots.isEmpty) 
        completed
      else {
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
  
  // Travel to Untested non-Muslim countries while the US is Hard to
  // try and increase the GWOT penalty.
  // We give preference to cells in adjacent countries to avoid die rolls
  // when necessary.
  // cardsOps   - The number of unused Ops remaining from the card
  // reserveOps - The number of unused Ops remaining from reserves
  // Returns the number of ops used
  def radTravelToUntestedNonMuslim(cardOps: Int, reserveOps: Int): Int = {
    log()
    log(s"Radicalization: Travel to Untested non-Muslim countries")
    val maxOps = cardOps + reserveOps
    def createTravelAttempt(from: String, to: String): TravelAttempt = {
      TravelAttempt(from, to, activeCells(game getCountry from) > 0)
    }
    // Create the next highest priority travel attempt.  Max of one per destination.
    def nextTravel(completed: Int, destinations: List[String]): Int = {
      val sources = countryNames(game.countries filter hasCellForTravel)
      if (completed == maxOps || destinations.isEmpty || sources.isEmpty)
        completed
      else {
        // First we try to do adjacent travel so that it will automatically succeed.
        val adjSources = sources filter (s => destinations exists (d => areAdjacent(s, d)))
        if (adjSources.nonEmpty) {
          // Now pick just the destinations that are adjacent to one of the sources
          val adjDests = destinations filter (d => adjSources exists (s => areAdjacent(s, d)))
          val to = recruitTravelToPriority(adjDests).get
          // Don't allow travel within the same country
          travelFromTarget(to, adjSources filterNot (_ == to)) match {
            case None => nextTravel(completed, destinations filterNot (_ == to))
            case Some(from) =>
              if (completed >= cardOps)
                expendBotReserves(1)
              performTravels(createTravelAttempt(from, to)::Nil) match {
                case (_, true)::Nil => usedCells(to).addSleepers(1)
                case _ =>
              }
              nextTravel(completed + 1, destinations filterNot (_ == to))
          }
        }
        else if (lapsingEventInPlay(Biometrics)) // Non adjacent travel is not allowd
          completed
        else {
         val to = recruitTravelToPriority(destinations).get
         // Don't allow travel within the same country
         travelFromTarget(to, sources filterNot (_ == to)) match {
           case None => nextTravel(completed, destinations filterNot (_ == to))
           case Some(from) => 
              if (completed >= cardOps)
               expendBotReserves(1)
              performTravels(createTravelAttempt(from, to)::Nil) match {
                case (_, true)::Nil => usedCells(to).addSleepers(1)
                case _ =>
              }
              nextTravel(completed + 1, destinations filterNot (_ == to))
         }
        }
      }
    }
    nextTravel(0, countryNames(game.nonMuslims filter (_.isUntested)))
  }
  
  // Plot in Soft non-Muslim countries while the US posture is Soft to
  // try and increase the GWOT penalty.
  // cardsOps   - The number of unused Ops remaining from the card
  // reserveOps - The number of unused Ops remaining from reserves
  // Returns the number of ops used
  def radPlotInSoftMuslim(cardOps: Int, reserveOps: Int): Int = {
    log()
    log(s"Radicalization: Plot in Soft non-Muslim countries")
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
          ss takeWhile (_.governance == ss.head.governance)
        }
        val target = topPriority(softs, plotPriorities).get
        
        def nextAttempt(plotsPerformed: Int): Int = {
          if (completed + plotsPerformed == maxOps || totalUnused(target) == 0 || game.availablePlots.isEmpty) 
            plotsPerformed
          else {
            if (completed + plotsPerformed >= cardOps)
              expendBotReserves(1)              
            
            addOpsTarget(target.name)
            performPlots(3, PlotAttempt(target.name, activeCells(target) > 0)::Nil)
            usedCells(target.name).addActives(1)
            nextAttempt(plotsPerformed + 1)
          }
        }
        val numPlots = nextAttempt(0)
        nextPlotTarget(completed + numPlots, candidates filterNot (_.name == target.name))
      }
    }
    nextPlotTarget(0, game.nonMuslims filter (n => n.isSoft && totalUnused(n) > 0))
  }
  
  // Recruit in the Muslim county with a cadre that has the best
  // Jihad DRM.
  // cardsOps   - The number of unused Ops remaining from the card
  // reserveOps - The number of unused Ops remaining from reserves
  // Returns the number of ops used
  def radRecruitAtMuslimCadre(cardOps: Int, reserveOps: Int): Int = {
    log()
    log(s"Radicalization: Recruit in a Muslim country with a cadre")
    val maxOps     = cardOps + reserveOps
    val candidates = game.getMuslims(botRecruitTargets(muslimWithCadreOnly = true)).sortBy(m => jihadDRM(m, m.isPoor))
    val target     = recruitTarget(candidates map (_.name)).get
    addOpsTarget(target)
    val m = game getMuslim target
    def nextAttempt(completed: Int): Int = {
      if (completed == maxOps || game.cellsToRecruit == 0)
        completed
      else {
        if (completed >= cardOps)
          expendBotReserves(1)
        log(s"$Jihadist attempts to recruit a cell into $target")
        if (m.autoRecruit) {
          log(s"Recruit is automatically successful in $target")
          addSleeperCellsToCountry(target, 1)
          usedCells(target).addSleepers(1)
        }
        else {
          val die = dieRoll
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
  
  // cardsOps - The number of unused Ops remaining from the card
  // Add any remaining card ops to reserves until reserves are full.
  // Returns the number of ops added
  def radAddToReserves(cardOps: Int): Int = {
    log()
    log(s"Radicalization: Add to reserves")
    val opsAdded = cardOps min (2 - game.reserves.jihadist)
    addToReserves(Jihadist, opsAdded)
    opsAdded
  }
  
  // Perform a recruit operation.  Do not use any reserves.
  // cardsOps - The number of unused Ops remaining from the card
  // Returns the number of ops used
  def radRecruit(cardOps: Int): Int = {
    log()
    log(s"Radicalization: Recruit")
    val target = recruitTarget(botRecruitTargets(muslimWithCadreOnly = false)).get
    addOpsTarget(target)
    val m = game getMuslim target
    def nextAttempt(completed: Int): Int = {
      if (completed == cardOps || game.cellsToRecruit == 0)
        completed
      else {
        log(s"$Jihadist attempts to recruit a cell into $target")
        if (m.autoRecruit) {
          log(s"Recruit is automatically successful in $target")
          addSleeperCellsToCountry(target, 1)
          usedCells(target).addSleepers(1)
        }
        else {
          val die = dieRoll
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
  
  // Travel to the US. (From adjacent if possible). Do not use reserves.
  // Perform a recruit operation.  Do not use any reserves.
  // Returns the number of ops used
  def radTravelToUS(cardOps: Int): Int = {
    log()
    log(s"Radicalization: Travel to the United States")

    def createTravelAttempt(from: String): TravelAttempt = {
      TravelAttempt(from, UnitedStates, activeCells(game getCountry from) > 0)
    }

    def nextTravel(completed: Int): Int = {
      val sources = countryNames(game.countries filter (c => c.name != UnitedStates && hasCellForTravel(c)))
      if (completed == cardOps || sources.isEmpty)
        completed
      else {
        // First we try to do adjacent travel so that it will automatically succeed.
        val adjSources = sources filter (s => areAdjacent(s, UnitedStates))
        if (adjSources.nonEmpty) {
          travelFromTarget(UnitedStates, adjSources) match {
            case None => completed  // No more source countries
            case Some(from) =>
              performTravels(createTravelAttempt(from)::Nil) match {
                case (_, true)::Nil => usedCells(UnitedStates).addSleepers(1)
                case _ =>
              }
              nextTravel(completed + 1)
          }
        }
        else {
         travelFromTarget(UnitedStates, sources) match {
           case None => completed   // No more source countries
           case Some(from) => 
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
      MapItem(target, num) :: troopsToTakeOffMap(remaining - num, candidates filterNot (_ == target))
    }
  }  
}