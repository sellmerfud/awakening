

// Labyrinth Awakening
//
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


package awakening

import scala.util.Random.shuffle
import scala.annotation.tailrec
import LabyrinthAwakening._

object JihadistBot {
  
  def jihadSuccessPossible(m: MuslimCountry) = m.jihadOK && 1 - m.jihadDRM <= m.governance
  
  def botLog(msg: String) = println(msg)
  
  // Priorities are used to narrow down a list of countries to a single best country
  // per the Priorities table.
  sealed trait Priority {
    val desc: String
    def filter(countries: List[Country]): List[Country]
  }
  
  // If no candidates match the criteria, then return the list unchanged
  // If at least on candidate matches the criteria then return all of the
  // candidates that match the criteria.
  class CriteriaPriority(val desc: String, criteria: (Country) => Boolean) extends Priority {
    def filter(countries: List[Country]) = (countries filter criteria) match {
      case Nil      => botLog(s"Criteria ($desc): match = false"); countries
      case matching => botLog(s"Criteria ($desc): match = true"); matching
    }
  }
  
  // Picks ALL of the candidates with the highest score and discards the rest.
  class HighestScorePriority(val desc: String, score: (Country) => Int) extends Priority {
    def filter(countries: List[Country]): List[Country] = {
      val high = (countries map score).max
      botLog(s"Highest ($desc): score = $high")
      countries filter (c => score(c) == high)
    }
  }
  
  // Picks ALL of the candidates with the lowest score and discards the rest.
  class LowestScorePriority(val desc: String, score: (Country) => Int) extends Priority {
    def filter(countries: List[Country]): List[Country] = {
      val low = (countries map score).min
      botLog(s"Lowest ($desc): score = $low")
      countries filter (c => score(c) == low)
    }
  }

  // Pick all candidates that are not the same as the target unless there
  // is only the target to choose from.
  class NotDestinationPriority(target: String) extends Priority {
    val desc = s"Not destination"
    def filter(countries: List[Country]) = (countries partition (_.name == target)) match {
      case (Nil, Nil)  => botLog(s"Not destination ($target): no candidates");   Nil
      case (same, Nil) => botLog(s"Not destination ($target): found only dest"); same
      case (_, diff)   => botLog(s"Not destination ($target): found non-dests"); diff
    }
  }

  // Flowchart filters implement the logic of the Operations Priorities (OpP) flowchart
  sealed trait FlowchartFilter {
    val desc: String
    def filter(countries: List[Country]): List[Country]
  }
  
  // Filters the given countries and returns the results.
  class CriteriaFilter(val desc: String, criteria: (Country) => Boolean) extends FlowchartFilter {
    def filter(countries: List[Country]) = (countries filter criteria)
  }
  
  // First picks only candidates that satisfy the criteria.
  // Then among those takes only the ones with the highest score.
  class HighestScoreFilter(val desc: String,
                          criteria: (Country) => Boolean, 
                          score:    (Country) => Int) extends FlowchartFilter {
    def filter(countries: List[Country]) = (countries filter criteria) match {
      case Nil        => Nil
      case candidates =>
        val high = (candidates map score).max
        candidates filter (c => score(c) == high)
    }
  }
  
  // First picks only candidates that satisfy the criteria.
  // Then among those takes only the ones with the lowest score.
  class LowestScoreFilter(val desc: String,
                          criteria: (Country) => Boolean, 
                          score:    (Country) => Int) extends FlowchartFilter {
    def filter(countries: List[Country]) = (countries filter criteria) match {
      case Nil        => Nil
      case candidates =>
        val low = (candidates map score).min
        candidates filter (c => score(c) == low)
    }
  }
  
  // Filters out all countries that are not adjacent to the target country.
  class AdjacentFilter(target: String) extends FlowchartFilter {
    val desc = s"Adjacent to $target"
    def filter(countries: List[Country]) = 
      (countries filter (c => c.name == target || areAdjacent(c.name, target)))
  }
  
  // Helper functions used to make it easier to construct Priorities and FlowchartFilters
  
  // Helper function for scores that only apply Muslim countries
  def muslimScore(score: (MuslimCountry) => Int, nonMuslimScore: Int = -100)(c: Country): Int = c match {
    case m: MuslimCountry    => score(m)
    case n: NonMuslimCountry => nonMuslimScore  
  }
  // Helper function for scores that only apply non-Muslim countries
  def nonMuslimScore(score: (NonMuslimCountry) => Int, muslimScore: Int = -100)(c: Country): Int = c match {
    case m: MuslimCountry    => muslimScore
    case n: NonMuslimCountry => score(n)  
  }
  
  // Helper function for criteria tests that only apply Muslim countries
  def muslimTest(test: (MuslimCountry) => Boolean, nonMuslim: Boolean = false)(c: Country): Boolean = c match {
    case m: MuslimCountry    => test(m)
    case n: NonMuslimCountry => nonMuslim  
  }
  
  // Helper function for criteria tests that only apply Muslim countries
  def nonMuslimTest(test: (NonMuslimCountry) => Boolean, muslim: Boolean = false)(c: Country): Boolean = c match {
    case m: MuslimCountry    => muslim
    case n: NonMuslimCountry => test(n)  
  }
  
  
  // Narrow the given countries to the best target using the given list of priorities.
  // If we run out of priorities and we have multiple candidates, the take one at random.
  @tailrec def topPriority(countries: List[Country], priorities: List[Priority]): Option[Country] = {
    botLog(s"topPriority: [${(countries map (_.name)) mkString ", "}]")
    (countries, priorities) match {
      case (Nil, _)    => None
      case (c::Nil, _) => Some(c)                             // We've narrowed it to one
      case (cs, Nil)   => shuffle(cs).headOption              // Take one at random
      case (cs, p::ps) => topPriority(p filter countries, ps) // Filter by next priority
    }
  }
  
  
  // Test the list of countries by each flowchart filter until one of the filters returns
  // a non-empty list of candidates.  If none of the filters finds a match then
  // we return Nil to indicate that the operation being tried cannot be carried out.
  @tailrec def followFlowchart(countries: List[Country], filters: List[FlowchartFilter]): List[Country] = {
    (countries, filters) match {
      case (Nil, _)    => Nil    // No countries to consider
      case (_, Nil)    => Nil    // No filter found any candidates
      case (_, f::fs) =>
        f.filter(countries) match {
          case Nil =>
            botLog(s"Flowchart (${f.desc}): failed")
            followFlowchart(countries, fs)
          case results =>
            botLog(s"Flowchart (${f.desc}): [${(results map (_.name) mkString ", ")}]")
            results
        }
    }
  }
  
  // Priorities Table

  // 1. Best Jihad DRM
  val BestJihadDRMPriority = new HighestScorePriority("Best Jihad DRM", muslimScore(_.jihadDRM))
  // 2. US
  val USPriority = new CriteriaPriority("US", c => c.name == UnitedStates)
  // 3. With troops unless prestige 1
  val WithPrestigeTroopsPriority = new CriteriaPriority("With troops & Prestige > 1",
                  muslimTest(_.totalTroops > 0 && game.prestige > 1))
  // 4. Not Islamist Rule
  val NotIslamistRulePriority = new CriteriaPriority("Not Islamist Rule",
                  muslimTest(_.isIslamistRule == false, nonMuslim = true))
  // 5. Pakistan with Arsenal
  val PakistanPriority = new CriteriaPriority("Pakistan arsenal",
                  c => c.name == Pakistan && c.wmdCache > 0)
  // 6. Philippines if would prestige - 1
  val PhilippinesPriority = new CriteriaPriority("Phillipines", 
                  muslimTest(m => m.name == Philippines &&
                                  m.totalTroops > 0 &&
                                  game.prestige > 1))
  // 7. Besieged Regime
  val BesiegedRegimePriority = new CriteriaPriority("Besieged regime", muslimTest(_.besiegedRegime))
  // 8. Most active cells  
  val MostActveCellsPriority = new HighestScorePriority("Most active cells", _.activeCells)
  // 9. Syrai with Arsenal
  val SyriaPriority = new CriteriaPriority("Syria Arsenal", c => c.name == Syria && c.wmdCache > 0)
  // 10. With Aid
  val WithAidPriority = new CriteriaPriority("With aid", muslimTest(_.aidMarkers > 0))
  // 11. Regime Change with troops
  val RegimeChangeTroopsPriority = new CriteriaPriority("Regime change with troops", 
                  muslimTest(m => m.inRegimeChange && m.totalTroops > 0))
  // 12. Highest Resource
  val HighestResourcePriority = new HighestScorePriority("Highest resource", muslimScore(_.resources))
  // 13. With Troops
  val WithTroopsPriority = new CriteriaPriority("With troops", muslimTest(_.totalTroops > 0))
  // 14. Iran with Arsenal
  val IranPriority = new CriteriaPriority("Iran arsenal", c => c.name == Iran && c.wmdCache > 0)
  // 15. US  (Already define at #2)
  // 16. Not a travel destination  
  //  create an instance of the NotDestinationPriority() class.
  // 17. Islamist Rule
  val IslamistRulePriority = new CriteriaPriority("Islamist Rule", muslimTest(_.isIslamistRule))
  // 18. Poor
  val PoorPriority = new CriteriaPriority("Poor", muslimTest(_.isPoor))
  // 19. Fair
  val FairPriority = new CriteriaPriority("Fair", muslimTest(_.isFair))
  // 20. Good
  val GoodPriority = new CriteriaPriority("Good", muslimTest(_.isGood))
  // 21. Highest Resource (Already defined at #12)
  // 22. Russia
  val RussiaPriority = new CriteriaPriority("Russia", _.name == Russia)
  // 23. No Disrupt pretige gain
  val NoDisruptPretigePriority = new CriteriaPriority("No Disrupt prestige gain",
                  muslimTest(_.totalTroopsThatAffectPrestige == 0, nonMuslim = true))
  // 24. Highest REC#
  val HighestRECPriority = new HighestScorePriority("Highest REC#", nonMuslimScore(_.recruitNumber))
  // 25. Best Jihad DRM (Already defined at #1)
  // 26. Not US
  val NotUSPriority = new CriteriaPriority("Not US", c => c.name != UnitedStates)
  // 27. Most active cells  (Already defined at #8)
  // 28. Not Regime change
  val NotRegimeChangePriority = new CriteriaPriority("Not Regime change", 
                  muslimTest(m => !m.inRegimeChange, nonMuslim = true))
  // 29. Worst Jihad DRM
  val WorstJihadDRMPriority = new LowestScorePriority("Worst Jihad DRM",
                  muslimScore(_.jihadDRM, nonMuslimScore = 100))
  // 30. Disrupt prestige gain
  val DisruptPrestigePriority = new CriteriaPriority("Disrupt prestige gain", 
                  muslimTest(m => m.disruptAffectsPrestige && game.prestige < 12))
  // 31. Cival War
  val CivilWarPriority = new CriteriaPriority("Civil War", muslimTest(_.civilWar))
  // 32. Neutral
  val NeutralPriority = new CriteriaPriority("Neutral", muslimTest(_.isNeutral))
  // 33. Besieged Regmime (Already defined at #7)
  // 34. Adjacent Good Ally
  val AdjacentGoodAllyPriority = new CriteriaPriority("Adjacent Good Ally", 
                  muslimTest(m => game.adjacentToGoodAlly(m.name)))
  // 35. Fair non-Muslim
  val FairNonMuslimPriority = new CriteriaPriority("Fair non-Muslim", nonMuslimTest(_.isFair))
  // 36. Same posture as US
  val SamePostureAsUSPriority = new CriteriaPriority("Same posture as US",
                  nonMuslimTest(_.posture == game.usPosture))
  // 37. Lowest REC#
  val LowestRECPriority = new LowestScorePriority("Lowest REC#", nonMuslimScore(_.recruitNumber, muslimScore = 100))
  // 38. Most cells  
  val MostCellsPriority = new HighestScorePriority("Most cells", _.totalCells)
  // 39. Adjacent to Islamist Rule
  val AdjacentIslamistRulePriority = new CriteriaPriority("Adjacent to Islamist Rule", 
                  c => game.adjacentToIslamistRule(c.name))
  // 40. Oil Exporter
  val OilExporterPriority = new CriteriaPriority("Oil exporter", muslimTest(_.oilProducer))


  // Flowchart filters
  
  val NonMuslimFilter     = new CriteriaFilter("non-Muslim", nonMuslimTest(_  => true))
  val PoorNonMuslimFilter = new CriteriaFilter("Poor non-Muslim", nonMuslimTest(_.isPoor))
  val FairNonMuslimFilter = new CriteriaFilter("Fair non-Muslim", nonMuslimTest(_.isFair))
  val GoodNonMuslimFilter = new CriteriaFilter("Good non-Muslim", nonMuslimTest(_.isGood))
  
  val PoorMuslimFilter  = new CriteriaFilter("Poor Muslim", muslimTest(_.isPoor))
  val FairMuslimFilter  = new CriteriaFilter("Fair Muslim", muslimTest(_.isFair))
  val GoodMuslimFilter  = new CriteriaFilter("Fair Muslim", muslimTest(_.isGood))
  val AutoRecruitFilter = new CriteriaFilter("Auto recruit", muslimTest(_.autoRecruit))
  
  val PoorTroopsActiveCellsFilter = new CriteriaFilter("Poor with troops and active cells",
                  muslimTest(m => m.isPoor && m.activeCells > 0))
  val PoorNeedCellsforMajorJihad = new CriteriaFilter("Poor, 1-4 more cells that troops/militia and JSP",
                  muslimTest(m => m.isPoor &&
                              jihadSuccessPossible(m) &&
                              (m.totalCells - m.totalTroopsAndMilitia) > 0 &&
                              (m.totalCells - m.totalTroopsAndMilitia) < 5))
  // Best DRM but Islamist Rule last.
  // I'm assuming that if there are any Civil War or Regime change countries (even with negative DRMs)
  // then they would be selected over Islamist Rule countries.                            
  val AutoRecruitBestJihadDRM = new HighestScoreFilter("Auto recruit w/ best Jihad DRM",
                  muslimTest(_.autoRecruit),
                  muslimScore(m => if (m.isIslamistRule) -50 else m.jihadDRM))
  val FairMuslimBestJihadDRM = new HighestScoreFilter("Fair Muslim w/ best Jihad DRM",
                  muslimTest(_.isFair),
                  muslimScore(_.jihadDRM))
  val PoorMuslimBestJihadDRM = new HighestScoreFilter("Poor Muslim w/ best Jihad DRM",
                  muslimTest(_.isPoor),
                  muslimScore(_.jihadDRM))
  val FewestCellsFilter = new LowestScoreFilter("Fewest cells", _ => true, muslimScore(_.totalCells))
  
  
  
  // Jihad column of the Priorities table when checking for Jihad/Major Jihad
  val JihadPriorities = List(
    BestJihadDRMPriority, PakistanPriority, BesiegedRegimePriority, SyriaPriority,
    WithAidPriority, BesiegedRegimePriority, HighestResourcePriority, WithTroopsPriority,
    IranPriority, MostCellsPriority, AdjacentIslamistRulePriority, OilExporterPriority)
  
  // Jihad column when checking Markers, Alignment, Governance
  // Jihad column of the Priorities table when checking Markers, Alignment, Governance
  // Does not include the Best Jihad DRM priority
  val MarkerAlignGovPriorities = JihadPriorities drop 1
  
  // Bot will not try minor Jihad in Poor countries
  // val candidates = game.muslims filter (m => !m.isPoor && jihadSuccessPossible(m))
  def minorJihadTarget(names: List[String]): Option[String] = {
    topPriority(game getMuslims names, JihadPriorities) map (_.name)
  }
  
  // Bot will only try major Jihad in Poor countries
  // val candidates = game.muslims filter (m => m.isPoor && m.majorJihadOK(ops))
  def majorJihadTarget(names: List[String]): Option[String] = {
    topPriority(game getMuslims names, JihadPriorities) map (_.name)
  }
  
  def markerAlignGovTarget(names: List[String]): Option[String] = {
    topPriority(game getCountries names, MarkerAlignGovPriorities) map (_.name)
  }
  

  val TightPlotFlowchart = List(
    PoorNonMuslimFilter, FairNonMuslimFilter, GoodNonMuslimFilter)
    
  val OtherPlotFlowchart = List(
    PoorTroopsActiveCellsFilter, FairMuslimFilter, GoodMuslimFilter, NonMuslimFilter,
    PoorMuslimFilter)
  
  val PlotPriorities = List(
    USPriority, WithPrestigeTroopsPriority, PakistanPriority, MostActveCellsPriority,
    SyriaPriority, WithAidPriority, RegimeChangeTroopsPriority, HighestResourcePriority,
    IranPriority, CivilWarPriority, NeutralPriority, AdjacentGoodAllyPriority,
    FairNonMuslimPriority, SamePostureAsUSPriority, LowestRECPriority,
    AdjacentIslamistRulePriority, OilExporterPriority)
  
  // val candidates = followFlowchart(game.countries filter (_.totalCells > 0), flowchart)
  def plotTarget(names: List[String]): Option[String] = {
    val flowchart = if (game.fundingLevel == Tight)
      TightPlotFlowchart ::: OtherPlotFlowchart
    else
      OtherPlotFlowchart
    val candidates = followFlowchart(game getCountries names, flowchart)
    topPriority(candidates, PlotPriorities) map (_.name)
  }
  
  val RecruitTravelToFlowchart = List(
    PoorNeedCellsforMajorJihad, AutoRecruitBestJihadDRM, GoodMuslimFilter,
    FairMuslimBestJihadDRM, NonMuslimFilter, PoorMuslimBestJihadDRM)        
  
  val RecruitPriorities = List(
    NotIslamistRulePriority, PakistanPriority, BesiegedRegimePriority,
    SyriaPriority, IranPriority, USPriority, PoorPriority, FairPriority,
    GoodPriority, HighestResourcePriority, NoDisruptPretigePriority,
    HighestRECPriority, BestJihadDRMPriority, SamePostureAsUSPriority,
    MostCellsPriority, AdjacentIslamistRulePriority, OilExporterPriority)
    
  def recruitTarget(names: List[String]): Option[String] = {
    val candidates = followFlowchart(game getCountries names, RecruitTravelToFlowchart)
    topPriority(candidates, RecruitPriorities) map (_.name)
  }
  
  val TravelToPriorities = List(
    NotIslamistRulePriority, PakistanPriority, BesiegedRegimePriority,
    SyriaPriority, IranPriority, USPriority, PoorPriority, FairPriority, GoodPriority,
    HighestResourcePriority, NoDisruptPretigePriority, BestJihadDRMPriority,
    SamePostureAsUSPriority, MostCellsPriority, AdjacentIslamistRulePriority,
    OilExporterPriority)

  def travelToTarget(names: List[String]): Option[String] = {
    val candidates = followFlowchart(game getCountries names, RecruitTravelToFlowchart)
    topPriority(candidates, TravelToPriorities) map (_.name)
  }
  
  def travelFromTarget(toCountry: String, names: List[String]): Option[String] = {
    val flowchart = List(
      new AdjacentFilter(toCountry), AutoRecruitFilter, FewestCellsFilter)

    val priorities = List(
      new NotDestinationPriority(toCountry), IslamistRulePriority,
      PoorPriority, FairPriority, GoodPriority, NotUSPriority,
      MostActveCellsPriority, NotRegimeChangePriority, WorstJihadDRMPriority,
      DisruptPrestigePriority, LowestRECPriority)
      
    val candidates = followFlowchart(game getCountries names, flowchart)
    topPriority(candidates, priorities) map (_.name)
  }
}