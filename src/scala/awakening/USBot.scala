
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

object USBot extends BotHelpers {


  def woiMuslimTargets(ops: Int): List[MuslimCountry] = game.muslims filter (_.warOfIdeasOK(ops))
  def woiNonMuslimTargets(ops: Int): List[NonMuslimCountry] = game.nonMuslims filter (_.warOfIdeasOK(ops))
  
  val onlyOneActiveCell = (c: Country) => c.activeCells == 1 && c.sleeperCells == 0
  val numPlotDice = (m: MuslimCountry) => (m.plots map { case PlotOnMap(plot, _) => plot.number }).sum

  // ------------------------------------------------------------------
  // Jihadist Priorities Table

  //  1. Lowest # of Plot dice
  val FewestPlotDicePriority = new LowestScorePriority("Lowest # of Plot dice",
                  muslimScore(numPlotDice, nonMuslimScore = 100))
  
  //  2. Regime Change
  val RegimeChangePriority = new CriteriaPriority("Regime Change",
                  muslimTest(_.inRegimeChange))
  
  //  3. Caliphate Captial
  val CaliphateCaptialPriority = new CriteriaPriority("Caliphate Captial",
                  muslimTest(_.caliphateCapital))

  //  4. Pakistan Arsenal
  val PakistanPriority = new CriteriaPriority("Pakistan arsenal",
                  c => c.name == Pakistan && c.wmdCache > 0)
  
  //  5. Syria Arsenal
  val SyriaPriority = new CriteriaPriority("Syria arsenal",
                  c => c.name == Syria && c.wmdCache > 0)
  
  //  6. Iran Arsenal
  val IranPriority = new CriteriaPriority("Iran arsenal",
                  c => c.name == Iran && c.wmdCache > 0)
  
  //  7. Civil War
  val CivilWArPriority = new CriteriaPriority("Civil War",
                  muslimTest(_.civilWar))

  //  8. Adjacent Good Ally
  val AdjacentGoodAllyPriority = new CriteriaPriority("Adjacent Good Ally",
                  muslimTest(m => game.adjacentToGoodAlly(m.name)))
                  
  //  9. Philippines (if Abu Sayyaf)  (Base game only)
  val PhilippinesPriority = new CriteriaPriority("Philippines (if Abu Sayyaf)",
                  c => c.name == Philippines && globalEventInPlay("Abu Sayyaf"))
                  
  // 10. Good
  val GoodPriority = new CriteriaPriority("Good Muslim", _.isGood)
  
  // 11. Fair
  val FairPriority = new CriteriaPriority("Fair Muslime", _.isFair)
  
  // Not in the table on the player aid sheet.  (Used by WoI non-Muslime)
  val PoorPriority = new CriteriaPriority("Poor Muslime", _.isPoor)
  
  // 12. Highest Resource
  val HighestResourcePriority = new HighestScorePriority("Highest resource",
                  muslimScore(_.resources))
  
  // 13. Neutral
  val NeutralPriority = new CriteriaPriority("Neutral Muslime", muslimTest(_.isNeutral))
  
  // 14. Besieged Regime
  val BesiegedRegimePriority = new CriteriaPriority("Besieged Regime",
                  muslimTest(_.besiegedRegime))
  
  // 15. Most Cells
  val MostCellsPriority = new HighestScorePriority("Most cells", _.totalCells)
  
  // 16. Adjacent Islamist Rule
  val AdjacentIslamistRulePriority = new CriteriaPriority("Adjacent Islamist Rule",
                  muslimTest(m => game.adjacentToIslamistRule(m.name)))
  
  // 17. With Aid
  val WithAidPriority = new CriteriaPriority("With Aid", muslimTest(_.aidMarkers > 0))
  
  // 18. Fewest Cells
  val FewestCellsPriority = new LowestScorePriority("Fewest cells", _.totalCells)
  
  // 19. Lowest Resource
  val LowestResourcePriority = new LowestScorePriority("Lowest resource",
                  muslimScore(_.resources, nonMuslimScore = 100))
  
  // 20. Most Plots
  val MostPlotsPriority = new HighestScorePriority("Most Plots", _.plots.size)

  // 21. Most Troops
  val MostTroopsPriority = new HighestScorePriority("Most Troops",
                  muslimScore(_.totalTroops))
  // 22. Oil Exporter
  val OilExporterPriority = new CriteriaPriority("Oil exporter",
                  muslimTest(_.oilProducer))

  // ------------------------------------------------------------------
  // US Operations Flowchart definitions.
  sealed trait Operation extends OpFlowchartItem
  case object WoiMuslimHighestDRM  extends Operation
  case object WoiMuslimMinusOneDRM extends Operation
  case object WoiNonMuslim         extends Operation
  case object Deploy               extends Operation  // Includes Withdraw
  case object Disrupt              extends Operation
  case object RegimeChange         extends Operation
  case object HomelandSecurity     extends Operation
  
  
  // This is the starting point of the PAR Flowchart
  object DisruptMuslim2MoreCellsDecision extends OperationDecision {
    val desc = "Disrupt Muslim at least 2 more cells than TandM and JSP?"
    def yesPath = Disrupt
    def noPath  = IR3AndRegimeChangeDecision
    def condition(ops: Int) = game.disruptMuslimTargets(ops) map game.getMuslim exists { m => 
      m.totalCells - m.totalTroopsAndMilitia >= 2 &&
      jihadSuccessPossible(m, false) 
    }
  }
  
  object IR3AndRegimeChangeDecision extends OperationDecision {
    val desc = "Islamist Rule resources >= 3 and Regime Change possible?"
    def yesPath = RegimeChange
    def noPath  = WoiMuslimNoPenaltyDecision
    def condition(ops: Int) = game.islamistResources >= 3 && game.regimeChangePossible(ops)
  }
  
  object WoiMuslimNoPenaltyDecision extends OperationDecision {
    val desc = "WoI in Muslim with DRM >= 0?"
    def yesPath = WoiMuslimHighestDRM
    def noPath  = PrestigeLowDecision
    def condition(ops: Int) = 
      woiMuslimTargets(ops) exists (m => modifyWoiRoll(0, m, silent = true) >= 0)
  }
  
  object PrestigeLowDecision extends OperationDecision {
    val desc = "Prestige Low?"
    def yesPath = DisruptForPrestigeDecision
    def noPath  = DeployDecision
    def condition(ops: Int) = game.prestigeLevel == Low
  }
  
  object DisruptForPrestigeDecision extends OperationDecision {
    val desc = "Disrupt for Pestige gain?"
    def yesPath = Disrupt
    def noPath  = USSoftDecision
    def condition(ops: Int) = game.disruptMuslimTargets(ops) map game.getMuslim exists { m => 
      m.disruptAffectsPrestige
    }
  }
  
  object DeployDecision extends OperationDecision {
    val desc = "Deploy Possible?"
    def yesPath = Deploy
    def noPath  = RegimeChangeDecision
    def condition(ops: Int) = game.deployTargets(ops) match {
      case Some((fromCandidates, toCandidates)) =>
        val from = deployFromTarget(fromCandidates)
        val to   = deployToTarget(toCandidates)
        (from.nonEmpty && to.nonEmpty && from != to)
      case None => false
    }
  }
  
  object USSoftDecision extends OperationDecision {
    val desc = "US Soft?"
    def yesPath = WoiNonMuslim
    def noPath  = DeployDecision
    def condition(ops: Int) = game.usPosture == Soft
  }
  
  object RegimeChangeDecision extends OperationDecision {
    val desc = "Regime Change Possible?"
    def yesPath = RegimeChange
    def noPath  = DisruptForPrestigeOrPlaceCadreDecision
    def condition(ops: Int) = game.regimeChangePossible(ops)
  }
  
  object DisruptForPrestigeOrPlaceCadreDecision extends OperationDecision {
    val desc = "Disrupt for Prestige gain or to place a Cadre?"
    def yesPath = Disrupt
    def noPath  = WoiMuslimMinus1DrmDecision
    def condition(ops: Int) = game.disruptMuslimTargets(ops) map game.getMuslim exists { m => 
      m.disruptAffectsPrestige || onlyOneActiveCell(m)
    }
  }
  
  object WoiMuslimMinus1DrmDecision extends OperationDecision {
    val desc = "WoI in Muslim with DRM of -1?"
    def yesPath = WoiMuslimMinusOneDRM
    def noPath  = HomelandSecurity
    def condition(ops: Int) = 
      woiMuslimTargets(ops) exists (m => modifyWoiRoll(0, m, silent = true) == -1)
  }

  
  // ------------------------------------------------------------------
  // Follow the operations flowchart to pick which operation will be performed.
  def operationsFlowchart(ops: Int): Operation = {
    @tailrec def evaluateItem(item: OpFlowchartItem): Operation = item match {
      case operation: Operation        => operation
      case decision: OperationDecision =>
        botLog(s"PAR Flowchart: $item")
        if (decision.condition(ops))
          evaluateItem(decision.yesPath)
        else
          evaluateItem(decision.noPath)
    }
    evaluateItem(DisruptMuslim2MoreCellsDecision)
  }


  // ------------------------------------------------------------------
  val DisruptPriorities = List(
    PakistanPriority, SyriaPriority, IranPriority, PhilippinesPriority,  // Base game only if Aby Sayyaf event is in play
    GoodPriority, FairPriority, HighestResourcePriority, BesiegedRegimePriority,
    MostCellsPriority, AdjacentIslamistRulePriority, WithAidPriority, OilExporterPriority)

  val DisruptFlowchart = List(
    new CriteriaNode("Muslim at least 2 more cells than TandM and JSP", 
      muslimTest(m => m.totalCells - m.totalTroopsAndMilitia >= 2 && jihadSuccessPossible(m, false))),
    new CriteriaNode("For Prestige gain", muslimTest(_.disruptAffectsPrestige)),
    new CriteriaNode("To place cadre", onlyOneActiveCell)    
  )
  
  def disruptTarget(names: List[String]): Option[String] = {
    botLog("Find \"Disrupt\" target")
    val candidates = followOpPFlowchart(game getCountries names, DisruptFlowchart)
    topPriority(candidates, DisruptPriorities) map (_.name)
  }
  
  // ------------------------------------------------------------------
  // Not in the Priorities Table, but listed in OpP flowchard.
  val WoiNonMuslimPriorities = List(
    PoorPriority, FairPriority, GoodPriority, FewestCellsPriority)
  
  val WoiNonMuslimFlowchart = List(
    new CriteriaNode("Opposite posture US",
      nonMuslimTest(n => !n.isUntested && n.canChangePosture && n.posture != game.usPosture)),
    new CriteriaNode("Untested non-Muslim", nonMuslimTest(_.isUntested)),
    new CriteriaNode("Same posture as US",
      nonMuslimTest(n => !n.isUntested && n.canChangePosture && n.posture == game.usPosture))
  )
  
  def woiNonMuslimTarget(names: List[String]): Option[String] = {
    botLog("Find \"non-Muslim WoI\" target")
    val candidates = followOpPFlowchart(game getCountries names, WoiNonMuslimFlowchart)
    topPriority(candidates, WoiNonMuslimPriorities) map (_.name)
  }

  // ------------------------------------------------------------------
  val DeployToPriorities = List(
    PakistanPriority, SyriaPriority, IranPriority, FairPriority,
    HighestResourcePriority, BesiegedRegimePriority, MostCellsPriority,
    AdjacentIslamistRulePriority, WithAidPriority, OilExporterPriority)
  
  val DeployToFlowchart = List(
    new CriteriaNode("Philippines if Abu Sayyaf, cell, no troops",  // Base game only
        muslimTest(m => globalEventInPlay("Abu Sayyaf") && m.name == Philippines && 
                         m.totalCells > 0 && m.totalTroops == 0)),
    new CriteriaNode("With cell, but no troops or militia",
      muslimTest(m => m.totalCells > 0 && m.totalTroopsAndMilitia == 0)),
    new CriteriaNode("Regime Change needs troops + militia for WoI",
      muslimTest(m => m.inRegimeChange && m.totalTroopsAndMilitia - m.totalCells < 5))
  )

  def deployToTarget(names: List[String]): Option[String] = {
    botLog("Find \"Deploy To\" target")
    val track        = names find (_ == "track")
    val countryNames = names filterNot (_ == "track")
    followOpPFlowchart(game getCountries countryNames, DeployToFlowchart) match {
      case Nil        => track
      case candidates => topPriority(candidates, DeployToPriorities) map (_.name)
    }
  }
  
  // ------------------------------------------------------------------
  val DeployFromPriorities = List(
    FewestCellsPriority, LowestResourcePriority, MostPlotsPriority, MostTroopsPriority)
  
  val DeployFromFlowchart = List(
    new CriteriaNode("Philippines if Moro Talks",  // Base game only
        muslimTest(m => globalEventInPlay("Moro Talks") && m.name == Philippines)),
    new CriteriaNode("Islamist Rule", muslimTest(_.isIslamistRule)),
    new CriteriaNode("Good Ally without cells OR with troop markers/militia",
        muslimTest(m => m.isGood && m.isAlly && 
             (m.totalCells == 0 || m.militia > 0 || m.markerTroops > 0))),
    new CriteriaNode("Fair Ally without cells OR with troop markers/militia",
        muslimTest(m => m.isFair && m.isAlly && 
             (m.totalCells == 0 || m.militia > 0 || m.markerTroops > 0)))
  )
  
  def deployFromTarget(names: List[String]): Option[String] = {
    botLog("Find \"Deploy From\" target")
    val track        = names find (_ == "track")
    val countryNames = names filterNot (_ == "track")
    followOpPFlowchart(game getCountries countryNames, DeployFromFlowchart) match {
      case Nil        => track
      case candidates => topPriority(candidates, DeployFromPriorities) map (_.name)
    }
  }
  
  // ------------------------------------------------------------------
  val RegimeChangePriorities = List(
    HighestResourcePriority, AdjacentIslamistRulePriority, FewestCellsPriority)
  
  def regimeChangeTarget(names: List[String]): Option[String] = {
    botLog("Find \"Regime Change\" target")
    topPriority(game getCountries names, RegimeChangePriorities) map (_.name)
  }
  
  
  // ------------------------------------------------------------------
  val RegimeChangeFromFlowchart = List(
    new CriteriaNode("Philippines if Moro Talks",  // Base game only
        muslimTest(m => globalEventInPlay("Moro Talks") && m.name == Philippines)),
    new CriteriaNode("Islamist Rule", muslimTest(_.isIslamistRule)),
    new CriteriaNode("Good Ally without cells OR with troop markers/militia",
        muslimTest(m => m.isGood && m.isAlly && 
             (m.totalCells == 0 || m.militia > 0 || m.markerTroops > 0))),
    new CriteriaNode("Fair Ally without cells OR with troop markers/militia",
        muslimTest(m => m.isFair && m.isAlly && 
             (m.totalCells == 0 || m.militia > 0 || m.markerTroops > 0))),
    new CriteriaNode("Good", _.isGood),         
    new CriteriaNode("Fair", _.isFair),         
    new CriteriaNode("Poor", _.isPoor)
  )
  
  // Used to determine from where to get troops to use in a Regime Change Operation.
  def regimeChangeFromTarget(names: List[String]): Option[String] = {
    botLog("Find \"Regime Change From\" target")
    val track        = names find (_ == "track")
    val countryNames = names filterNot (_ == "track")
    followOpPFlowchart(game getCountries countryNames, RegimeChangeFromFlowchart) match {
      case Nil        => track
      case candidates => topPriority(candidates, DeployFromPriorities) map (_.name)
    }
  }
  
  // ------------------------------------------------------------------
  val WoiMuslimPriorities = List(
    FewestPlotDicePriority, RegimeChangePriority, CaliphateCaptialPriority,
    PakistanPriority, SyriaPriority, IranPriority, CivilWArPriority,
    AdjacentGoodAllyPriority, HighestResourcePriority, NeutralPriority,
    BesiegedRegimePriority, AdjacentIslamistRulePriority, FewestCellsPriority,
    MostTroopsPriority, OilExporterPriority)
  
  def markerAlignGovTarget(names: List[String]): Option[String] = {
    botLog("Find \"Marker/Align/Gov\" target")
    topPriority(game getCountries names, WoiMuslimPriorities) map (_.name)
  }
  
  val BestWoiDRMFilter = new HighestScoreNode("Highest WoI DRM",
    muslimTest(_ => true),
    muslimScore(m => modifyWoiRoll(0, m, silent = true)))
  
  def woiBestDRMTarget(names: List[String]): Option[String] = {
    botLog("Find \"Best DRM WoI\" target")
    val flowchart  = BestWoiDRMFilter::Nil
    val candidates = followOpPFlowchart(game getCountries names, flowchart)
    topPriority(candidates, WoiMuslimPriorities) map (_.name)
  }
  
  val WoiDrmMinusOneFilter = new CriteriaNode("WoI DRM -1",
    muslimTest(m => modifyWoiRoll(0, m, silent = true) == -1))
  
  def woiDrmMinusOneTarget(names: List[String]): Option[String] = {
    botLog("Find \"DRM -1 WoI\" target")
    val flowchart = WoiDrmMinusOneFilter::Nil
    val candidates = followOpPFlowchart(game getCountries names, flowchart)
    topPriority(candidates, WoiMuslimPriorities) map (_.name)
  }
  
  // ------------------------------------------------------------------
  def maxOpsPlusReserves(card: Card): Int = (card.ops + game.reserves.us) min 3
  
  // Decrement the Bots reserves and log that they were used.
  def expendBotReserves(ops: Int): Unit = {
    if (ops > 0) {
      assert(game.reserves.us >= ops,
         s"expendBotReserves($ops): Only ${opsString(game.reserves.us)} in reserve")
     game = game.copy(reserves = game.reserves.copy(us = game.reserves.us - ops))
     log()
     log(s"$US expends ${opsString(ops)} from reserves.  Reserves now ${opsString(game.reserves.us)}")
    }
  }
  
  
  // Called when the Human Jihadist player plays a Jihadist Associated card.
  def performTriggeredEvent(card: Card): Unit = {
    performCardEvent(card, US, triggered = true)
  }
  
  
  // Starting point for Jihadist bot card play.
  def cardPlay(card: Card): Unit = {
    
    // If there is at least one plot on the map then
    // we consult the Alert Resolution Flowchart (ARF)
    val consultPAR = if (game.countries exists (_.plots.nonEmpty)) {
      // May have to add a new check to the cards to see if the event would
      // remove the Priority Plot!
      // The ARF may defer to the PAR
      true
    }
    else 
      true
    
    if (consultPAR && !reassessment(card)) {
      // If the event is playable then the event is alwasy executed
      if (card.eventIsPlayable(US)) {
        performCardEvent(card, US)
        // If the card event is Unassociated add ops to the Bot's reserves.
        if (card.association == Unassociated) 
          addToReserves(US, card.ops)
      }
      else {
        // US Elections is the only auto trigger event.
        // The Bot will execute the event first.
        if (card.autoTrigger)
          performCardEvent(card, US)
    
        val opsUsed = operationsFlowchart(maxOpsPlusReserves(card)) match {
          case WoiMuslimHighestDRM  => woiMuslimHighestDRMOperation(card)
          case WoiMuslimMinusOneDRM => woiMuslimDRMMinusOneOperation(card)
          case WoiNonMuslim         => woiNonMuslimOperation(card)
          case Deploy               => deployOperation(card)
          case Disrupt              => disruptOperation(card)
          case RegimeChange         => regimeChangeOperation(card)
          case HomelandSecurity     => 0 // No operation performed
        }
        homelandSecurity(card, opsUsed)
        
      }
    }
  }
  
  // Checks to see if the Bot wants to do a Reassessment and
  // has enough Ops to do so.
  // Returns true if Reassessment performed
  def reassessment(card: Card): Boolean = {
    // Is this the first US card of the action phase?
    // We assume that if the cardsPlayed is empty and a US card is being played,
    // then the Jihadist has already played all her cards.
    val tryReassess = 
       (game.cardsPlayed.isEmpty || game.cardsPlayed.head.role != US) &&
       (card.ops + game.reserves.us >= 3) &&  // Possible if we have at least 3 on hand
       ((game.usPosture == Soft && game.islamistResources >= 2) ||
        (game.usPosture == Hard && game.gwotPenalty == 3 && game.numIslamistRule == 0))
    
    if (tryReassess) {
      // Reassessment is desired. Ask if the next US card has enough Ops
      val opsNeeded = 6 - card.ops - game.reserves.us
      println("The US is planning a Reassessment.")
      val reassess = if (opsNeeded == 1)
        askYorN("Does the US have another card in hand (y/n)? ")
      else
        askYorN(s"Does the US have another card in hand with at least $opsNeeded Ops (y/n)? ")

      if (reassess) {
        val cardNum = askCardNumber("Card # ", initial = None, allowNone = false).get
        val card2 = deck(cardNum)
        if (card2.ops >= opsNeeded) {
          game = game.copy(cardsPlayed = PlayedCard(US, card2) :: game.cardsPlayed)
          logCardPlay(US, card2)
          // Check to see if either of the cards played has the autoTrigger
          // US Elections event.  If so the event happens first.
          // Calculate the new Posture before any change caused by the Elections.
          val newPosture = if (game.usPosture == Hard) Soft else Hard
          if (card.autoTrigger)
            performCardEvent(card, US)
          else if (card2.autoTrigger)
            performCardEvent(card2, US)

          expendBotReserves(6 - card.ops - card2.ops)
          log(s"$US performs a Reassessment operation")
          setUSPosture(newPosture)
          true
        }
        else {
          println(s"$card2 does not have enought Ops")
          println("Place the card back on top of the US hand of cards")
          false
        }
      }
      else
        false
    }
    else
      false
  }


  def woiMuslimHighestDRMOperation(card: Card): Int = {
    val maxOps  = maxOpsPlusReserves(card)
    val target  = woiBestDRMTarget(countryNames(woiMuslimTargets(maxOps))).get
    val opsUsed = (game getMuslim target).governance
    if (opsUsed > card.ops)
      expendBotReserves(opsUsed - card.ops)
    performWarOfIdeas(target, opsUsed)
    opsUsed
  }
  
  def woiMuslimDRMMinusOneOperation(card: Card): Int = {
    val maxOps  = maxOpsPlusReserves(card)
    val target  = woiDrmMinusOneTarget(countryNames(woiMuslimTargets(maxOps))).get
    val opsUsed = (game getMuslim target).governance
    if (opsUsed > card.ops)
      expendBotReserves(opsUsed - card.ops)
    performWarOfIdeas(target, opsUsed)
    opsUsed
  }
  
  def woiNonMuslimOperation(card: Card): Int = {
    val maxOps  = maxOpsPlusReserves(card)
    val target  = woiNonMuslimTarget(countryNames(woiNonMuslimTargets(maxOps))).get
    val opsUsed = (game getMuslim target).governance
    if (opsUsed > card.ops)
      expendBotReserves(opsUsed - card.ops)
    performWarOfIdeas(target, opsUsed)
    opsUsed
  }
  
  def deployOperation(card: Card): Int = {
    val maxOps  = maxOpsPlusReserves(card)
    val target  = woiNonMuslimTarget(countryNames(woiNonMuslimTargets(maxOps))).get
    val (fromCandidates, toCandidates) = game.deployTargets(maxOps).get
    val from = deployFromTarget(fromCandidates).get
    val to   = deployToTarget(toCandidates).get
    
    val opsUsed = to match {
      case "track" => 1
      case name    => (game getMuslim name).governance
    }
    if (opsUsed > card.ops)
      expendBotReserves(opsUsed - card.ops)

    val (withdraw, numTroops) = from match {
      case "track" => (false, 2)  // Always deploy exactly 2 troops from track
      case name    =>
        val m = game getMuslim name
        (m.inRegimeChange, m.maxDeployFrom(opsUsed))
    }
    if (withdraw) {
      log(s"$US performs a Withdraw operation")
      performWithdraw(from, to, numTroops)
    }
    else {
      log(s"$US performs a Deploy operation")
      moveTroops(from, to, numTroops)
    }
    opsUsed
  }
  
  def disruptOperation(card: Card): Int = {
    val maxOps  = maxOpsPlusReserves(card)
    val target  = disruptTarget(game disruptTargets maxOps).get
    val opsUsed = (game getMuslim target).governance
    if (opsUsed > card.ops)
      expendBotReserves(opsUsed - card.ops)
    log(s"$US performs a Disrupt operation in $target")
    performDisrupt(target)
    opsUsed
  }
  
  def regimeChangeOperation(card: Card): Int = {
    val maxOps  = maxOpsPlusReserves(card)
    assert(maxOps == 3, "regimeChangeOperation() called with less than 3 Ops available")
    val opsUsed = 3
    val target  = regimeChangeTarget(game.regimeChangeTargets).get
    val source  = regimeChangeFromTarget(game.regimeChangeSources(3)).get
    
    if (opsUsed > card.ops)
      expendBotReserves(opsUsed - card.ops)
    log(s"$US performs a Regime Change operation in $target")
    performRegimeChange(source, target, 6) // Bot always uses exactly 6 troops
    opsUsed
  }
  

  // Perform Homeland Security
  // The opsUsed parameter is the number of Ops used to perform the card operation.
  // If this value is less than the number of Ops on the card, then we will 
  // perform homeland using the remainder of ops on the card plus reserves as needed.
  // This may result in more than 3 total Ops being used for the current card.
  // If the opUsed is greater than or equal to the number of Ops on the card,
  // then we do nothing.
  def homelandSecurity(card: Card, opsUsed: Int): Unit = {
    if (opsUsed < card.ops) {
      // TODO: flesh out
    }
  }
  
  
}