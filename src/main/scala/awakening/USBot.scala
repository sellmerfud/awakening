
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

object USBot extends BotHelpers {


  // The Bot will not consider WoI in an untested muslim country unless
  // it has 3 Ops to work with.
  def woiMuslimTargets(ops: Int): List[MuslimCountry] = if (ops >= 3)
    game.muslims filter (_.warOfIdeasOK(ops))
  else
    game.muslims filter (m => m.isTested && m.warOfIdeasOK(ops))
  def woiNonMuslimTargets(ops: Int): List[NonMuslimCountry] = game.nonMuslims filter (_.warOfIdeasOK(ops))
  
  // Pick sleepers before actives
  // Return (actives, sleepers, sadr)
  def chooseCellsToRemove(name: String, num: Int): (Int, Int, Boolean) = {
    if (num == 0)
      (0, 0, false)
    else {
      val c = game getCountry name
      val sadr     = c.hasSadr
      val numCells = num - (if (sadr) 1 else 0)
      val sleepers = numCells min c.sleeperCells
      val actives  = (numCells - sleepers) min c.activeCells
      (actives, sleepers, sadr)
    }
  }
  
  // Pick actives before sleepers
  // Return (actives, sleepers)
  def chooseCellsToDisrupt(name: String, num: Int): (Int, Int) = {
    if (num == 0)
      (0, 0)
    else {
      val c = game getCountry name
      val actives    = num min c.activeCells
      val sleepers  = (num - actives) min c.sleeperCells
      (actives, sleepers)
    }
  }
  
  // Returns "troop-cube", "militia-cube", or the name of a troop marker
  def chooseTroopOrMilitiaToRemove(name: String): String = {
    val c = game.getCountry(name)
    if (game.isMuslim(name) && game.getMuslim(name).militia > 0)
      "militia-cube"
    else if (c.troops > 0)
      "troop-cube"
    else if (c.troopsMarkers.nonEmpty)
      c.troopsMarkers.sorted.head.name
    else
      throw new IllegalStateException(s"USBot.chooseTroopOrMilitiaToRemove($name) not units present")
  }
  
  // Bot considers wwithdraw as part of deploy decision
  def botDeployTargets(ops: Int):  Option[(List[String], List[String])] = {
    val withdrawFrom = game.withdrawFromTargets
    val withdrawTo   = game.withdrawToTargets
    if (game.usPosture == Soft && ops == 3 && withdrawFrom.nonEmpty && withdrawTo.nonEmpty) {
      val newTargets = game.deployTargets(ops) map {
        case (from, to) => ((from:::withdrawFrom).distinct, (to:::withdrawTo).distinct)
      }
      newTargets orElse Some(withdrawFrom -> withdrawTo)
    }
    else
      game.deployTargets(ops)  // Normal deploy targets only
  }
  
  val onlyOneActiveCell = (c: Country) => c.activeCells == 1 && c.sleeperCells == 0
  val numPlotDice = (m: MuslimCountry) => (m.plots map { case PlotOnMap(plot, _) => plot.number }).sum

  // This class is used when determining the priority plot to alert.
  // It contains an PlotOnMap and a Country containing that plot.
  case class PlotInCountry(onMap: PlotOnMap, country: Country) {
    val id = PlotInCountry.nextId
    def isWMD = onMap.plot == PlotWMD
    override def toString() = s"$onMap in ${country.name}"
  }
  object PlotInCountry {
    // In order to distinguish between on map plots during the US Bot alert process
    // each PlotInCountry instance has a unique identifier.
    private var _nextId = 0
    private def nextId: Int = {
      _nextId += 1
      _nextId
    }
  }
  
  // PlotFilters are used both when following the Alert Priorities table.
  // Each filter simply takes a list of PlotInCountry instances as input
  // and produces a filtered list as output.
  trait PlotFilter {
    val desc: String
    def filter(plots: List[PlotInCountry]): List[PlotInCountry]
    override def toString() = desc
  }
  
  // Boolean criteria filter used with Alert Priorities Table
  // The input is filtered and if the results are empty, the original input
  // is returned. Otherwise the filtered input is returned.
  class PlotCriteria(val desc: String, criteria: (PlotInCountry) => Boolean) extends PlotFilter {
    def filter(plots: List[PlotInCountry]) = (plots filter criteria) match {
      case Nil      => botLog(s"Criteria ($desc): match = false"); plots
      case matching => botLog(s"Criteria ($desc): match = true"); matching
    }
  }
  
  // Highest integer score filter used with Alert Priorities Table.
  // Applies the given score function to each plot in the input list and
  // takes the highest value.
  // Then returns the list of plots whose score matches that highest value.
  class PlotHighestScore(val desc: String, score: (PlotInCountry) => Int) extends PlotFilter {
    def filter(plots: List[PlotInCountry]): List[PlotInCountry] = {
      val high = (plots map score).max
      botLog(s"Highest ($desc): score = $high")
      plots filter (plot => score(plot) == high)
    }
  }
  
  // Calculate the resulting funding if the plot were to be resolved.
  def fundingResult(plot: PlotInCountry): Int = {
    val funding = game.funding
    plot.country match {
      case m: MuslimCountry                              => game.funding + (if (m.isGood) 2 else 1)
      case n: NonMuslimCountry if n.name == Iran         => game.funding + 1
      case n: NonMuslimCountry if n.name == UnitedStates => 9
      case n: NonMuslimCountry if plot.isWMD             => 9
      case n: NonMuslimCountry if n.isGood               => funding + plot.onMap.plot.number * 2
      case n: NonMuslimCountry                           => funding + plot.onMap.plot.number
    }
  }

  def inMuslimCountry(plot: PlotInCountry) = game isMuslim plot.country.name
  def inNonMuslimCountry(plot: PlotInCountry) = game isNonMuslim plot.country.name
  def getMuslim(plot: PlotInCountry) = game getMuslim plot.country.name
  def getNonMuslim(plot: PlotInCountry) = game getNonMuslim plot.country.name
  
  // ------------------------------------------------------------------
  // Alert Priorities Table
  
  val AlertPriorities = List(
    new PlotCriteria("WMD in US", 
      plot => plot.isWMD && plot.country.name == UnitedStates),
    new PlotCriteria("WMD with troops",
      plot => plot.isWMD && 
              inMuslimCountry(plot) && 
              getMuslim(plot).totalTroopsThatAffectPrestige > 0),
    new PlotCriteria("In US",
      plot => plot.country.name == UnitedStates),
    new PlotCriteria("Good Muslim",
      plot => inMuslimCountry(plot) &&
              getMuslim(plot).isGood),
    new PlotCriteria("non-Muslim if increase funding >= 8 from <= 7",
      plot => inNonMuslimCountry(plot) &&
              !plot.onMap.backlashed &&
              game.funding <= 7 &&
              fundingResult(plot) >= 8),
    new PlotCriteria("Fair Muslim",
      plot => inMuslimCountry(plot) &&
              getMuslim(plot).isFair),
    new PlotCriteria("With troops",
      plot => inMuslimCountry(plot) && 
              getMuslim(plot).totalTroopsThatAffectPrestige > 0),
    new PlotHighestScore("Plot #", 
      plot => plot.onMap.plot.number),
    new PlotCriteria("Poor with Aid",
      plot => inMuslimCountry(plot) && 
              getMuslim(plot).isPoor &&
              getMuslim(plot).aidMarkers > 0),
    new PlotHighestScore("Resource", 
      plot => if (inMuslimCountry(plot)) getMuslim(plot).resourceValue else 0)
  )
  
  // Process the list of PlotInCountry instances by each PlotFilter in the priorities list.
  // In this function each filter is processed in order until we have used all filters
  // in the list to narrow the choices to a single country.  If we go through all of
  // the filters and we still have more than one viable country, then we pick one at
  // random.
  def priorityPlot(plots: List[PlotInCountry]): PlotInCountry = {
    assert(plots.nonEmpty, "priorityPlot() called with empty list")
    @tailrec def topPriority(plots: List[PlotInCountry], priorities: List[PlotFilter]): PlotInCountry = {
      botLog(s"priorityPlot: [${(plots map (_.toString)) mkString ", "}]")
      (plots, priorities) match {
        case (Nil, _)    => throw new IllegalStateException("priorityPlot() found nothing")
        case (p::Nil, _) => p                               // We've narrowed it to one
        case (ps, Nil)   => shuffle(ps).head                // Take one at random
        case (ps, f::fs) => topPriority(f filter plots, fs) // Filter by next priority
      }
    }
    assert(plots.nonEmpty, "priorityPlot() called with empty list of plots")
    topPriority(plots, AlertPriorities)
  }
  
  def roll(plot: Plot, m: MuslimCountry) =
    getDieRoll(s"Enter Alert Plot die roll for ${m.name}: ") - m.aidMarkers <= plot.number  

  def roll(plot: Plot, n: NonMuslimCountry) =
    getDieRoll(s"Enter Alert Plot die roll for ${n.name}: ") <= plot.number  
  
  // ------------------------------------------------------------------
  // US Alert Table
  trait AlertResult {
    def apply(plot: PlotInCountry): Boolean
  }
  
  class MuslimAlertResult(test: (Plot, MuslimCountry) => Boolean) extends AlertResult {
    def apply(plot: PlotInCountry): Boolean = 
      test(plot.onMap.plot, game getMuslim plot.country.name)
  }
  
  class NonMuslimAlertResult(test: (Plot, NonMuslimCountry) => Boolean) extends AlertResult {
    def apply(plot: PlotInCountry): Boolean = 
      test(plot.onMap.plot, game getNonMuslim plot.country.name)
  }
  
  // Muslim Alert Table
  val muslimTable = {
    val R = new MuslimAlertResult((p, m) => roll(p, m))
    val N = new MuslimAlertResult((_, _) => false)
    val Y = new MuslimAlertResult((_, _) => true)
    val G = new MuslimAlertResult((_, m) => m.isGood)
    val L = new MuslimAlertResult((_, m) => m.isGood || m.totalTroopsThatAffectPrestige > 0)
    val T = new MuslimAlertResult((p, m) => m.totalTroopsThatAffectPrestige > 0 || roll(p, m))
    
    Vector(//1   2   3   4   5   6   7   8   9  10  11  12
      Vector(R,  R,  R,  T,  R,  R,  T,  R,  R,  T,  R,  R),  // 1
      Vector(R,  R,  R,  T,  R,  R,  T,  R,  R,  T,  R,  R),  // 2
      Vector(G,  G,  G,  L,  G,  G,  L,  G,  G,  L,  G,  G),  // 3
      Vector(Y,  Y,  Y,  Y,  Y,  Y,  Y,  Y,  Y,  Y,  Y,  Y),  // 4
      Vector(R,  R,  R,  T,  R,  R,  T,  R,  R,  T,  R,  R),  // 5
      Vector(G,  G,  G,  L,  G,  G,  L,  G,  G,  L,  G,  G),  // 6
      Vector(Y,  Y,  Y,  Y,  Y,  Y,  Y,  Y,  Y,  Y,  Y,  Y),  // 7
      Vector(R,  R,  R,  T,  R,  R,  T,  R,  R,  T,  R,  R),  // 8
      Vector(N,  N,  N,  T,  N,  N,  T,  N,  N,  T,  N,  N)   // 9
    )
  }
  
  // non-Muslim Alert Table
  val nonMuslimTable = {
    val R = new NonMuslimAlertResult((p, n) =>  roll(p,n))
    val N = new NonMuslimAlertResult((_, _) => false)
    val Y = new NonMuslimAlertResult((_, _) => true)
    val Z = new NonMuslimAlertResult((_, n) => n.name == UnitedStates)
    val U = new NonMuslimAlertResult((p, n) => n.name == UnitedStates || roll(p, n))
    val S = new NonMuslimAlertResult((p, n) => n.name == UnitedStates && roll(p, n))
    
    case class _2(letter: NonMuslimAlertResult) extends NonMuslimAlertResult((_, _) => true) {
      override def apply(plot: PlotInCountry): Boolean = {
        val p = plot.onMap.plot
        val n = game getNonMuslim plot.country.name
        (p == Plot1 && n.isGood) || p == Plot2 || p == Plot3 || letter(plot)
      }
    }
    case class _3(letter: NonMuslimAlertResult) extends NonMuslimAlertResult((_, _) => true) {
      override def apply(plot: PlotInCountry): Boolean = {
        val p = plot.onMap.plot
        val n = game getNonMuslim plot.country.name
        (p == Plot2 && n.isGood) || p == Plot3 || letter(plot)
      }
    }
    case class _4(letter: NonMuslimAlertResult) extends NonMuslimAlertResult((_, _) => true) {
      override def apply(plot: PlotInCountry): Boolean = {
        val p = plot.onMap.plot
        val n = game getNonMuslim plot.country.name
        ((p == Plot2 || p == Plot3) && n.isGood) || letter(plot)
      }
    }
    
    Vector(//   1      2      3      4      5      6      7      8      9     10     11      12
      Vector(_4(Z), _4(U), _4(U), _4(U), _4(U), _4(U), _4(U), _4(U), _4(U), _4(U), _4(Z), _4(Z)),  // 1
      Vector(_3(Z), _3(U), _3(U), _3(U), _3(U), _3(U), _3(U), _3(U), _3(U), _3(U), _3(Z), _3(Z)),  // 2
      Vector(_2(Z), _2(U), _2(U), _2(U), _2(U), _2(U), _2(U), _2(U), _2(U), _2(U), _2(Z), _2(Z)),  // 3
      Vector(    Y,     Y,     Y,     Y,     Y,     Y,     Y,     Y,     Y,     Y,     Y,     Y),  // 4
      Vector(_3(R), _3(U), _3(U), _3(U), _3(U), _3(U), _3(U), _3(U), _3(U), _3(U), _3(Z), _3(Z)),  // 5
      Vector(_2(R), _2(U), _2(U), _2(U), _2(U), _2(U), _2(U), _2(U), _2(U), _2(U), _2(Z), _2(Z)),  // 6
      Vector(    Y,     Y,     Y,     Y,     Y,     Y,     Y,     Y,     Y,     Y,     Y,     Y),  // 7
      Vector(    R,     Z,     Z,     U,     Z,     Z,     U,     Z,     Z,     U,     Z,     S),  // 8
      Vector(    R,     Z,     Z,     U,     Z,     Z,     U,     Z,     Z,     U,     Z,     S)   // 9
    )
  }
  
  
  // Returns true if the plot is Alerted.
  def alertTable(card: Card, plots: List[PlotInCountry]): Boolean = {
    val plot = priorityPlot(plots)
    val (fundingMod, prestigeMod) = alertTableMods(plots filterNot (_.id == plot.id))
    val funding     = (game.funding  + fundingMod)  max 1 min 9
    val prestige    = (game.prestige + prestigeMod) max 1 min 12
    val alertResult = if (game isMuslim plot.country.name)
      muslimTable(funding - 1)(prestige - 1)
    else
      nonMuslimTable(funding - 1)(prestige - 1)
    if (alertResult(plot)) {
      // The Bot will execute the auto trigger event first.
      if (card.autoTrigger) {
        performCardEvent(card, US)
        log()
      }
      
      alertPlot(card, plot)
      val otherPlotsInCountry = plots filter (p => p.id != plot.id && p.country.name == plot.country.name)
      if (game.usResolve(Competent) && otherPlotsInCountry.nonEmpty) {
        log(s"$US Bot with Competent resolve alerts two plots in the same country")
        val plot2 = priorityPlot(otherPlotsInCountry)
        performAlert(plot2.country.name, plot2.onMap)
      }
      true
    }
    else
      false
  }
  
  // Returns the drms for (funding, prestige)
  def alertTableMods(otherPlots: List[PlotInCountry]): (Int, Int) = {
    var (funding, prestige) = (0, 0)
    for (plot <- otherPlots) {
      plot.country match {
        case m: MuslimCountry =>
          if (m.isGood) funding += 2
          if (m.isGood && m.totalTroopsThatAffectPrestige > 0) prestige -= 1
          if (m.isFair || m.isPoor) funding += 1
          if ((m.isFair || m.isPoor) && m.totalTroopsThatAffectPrestige > 0) prestige -= 1
        case n: NonMuslimCountry =>
          if (n.isGood) funding += (plot.onMap.plot.number * 2)
          if (n.isFair || n.isPoor) funding += plot.onMap.plot.number
      }
    }
    
    // Don't really want to ask everytime if this is the last turn, so
    // we will just assume that it is not.
    val lastTurn = false 
    if (lastTurn) funding += 1
    else {
      game.gwot match {
        case (posture, 3) if posture == game.usPosture => prestige += 1
        case _ =>
      }
      if (game hasMuslim (_.isIslamistRule))
        prestige -= 1
      if (globalEventInPlay(Pirates1) || globalEventInPlay(Pirates2))
        funding += 1
      if (globalEventInPlay(Fracking))
        funding -= 1
    }
    (funding, prestige)
  }
  

  // ------------------------------------------------------------------
  // US Alert Resolution Flowchart definitions.
  
  trait AlertFlowchartNode
  
  trait AlertDecision extends AlertFlowchartNode {
    val desc: String
    def yesPath: AlertFlowchartNode
    def noPath: AlertFlowchartNode
    // ops is the total number of ops available including reserves
    def condition(card: Card, ops: Int, playableEvent: Boolean, plots: List[PlotInCountry]): Boolean
    override def toString() = desc
  }
  
  sealed trait AlertAction    extends AlertFlowchartNode
  case object AlertPlot       extends AlertAction
  case object AlertTable      extends AlertAction
  case object PARFlowchart    extends AlertAction
  case object AddToUSReserves extends AlertAction

  // This is the starting point of the ARF Flowchart
  object WMDPlacedInCountry extends AlertDecision {
    val desc = "WMD place in country?"
    def yesPath = ThreeOpsForWMD
    def noPath  = ThreeOpsAvailable
    def condition(card: Card, ops: Int, playableEvent: Boolean, plots: List[PlotInCountry]) = plots exists (_.isWMD)
  }
  
  object ThreeOpsForWMD extends AlertDecision {
    val desc = "3 Ops available for WMD alert?"
    def yesPath = WMDInUS
    def noPath  = LastCardOrEventAlertsPriorityPlot
    def condition(card: Card, ops: Int, playableEvent: Boolean, plots: List[PlotInCountry]) = ops >= 3
  }
  
  object WMDInUS extends AlertDecision {
    val desc = "WMD in the United States?"
    def yesPath = AlertPlot
    def noPath  = WMDWithTroopsAndPrestigeAbove3
    def condition(card: Card, ops: Int, playableEvent: Boolean, plots: List[PlotInCountry]) = 
      plots exists (p => p.isWMD && p.country.name == UnitedStates)
  }
  
  object LastCardOrEventAlertsPriorityPlot extends AlertDecision {
    val desc = "Last card of phase or Event would alert priority plot?"
    def yesPath = PARFlowchart
    def noPath  = AddToUSReserves
    def condition(card: Card, ops: Int, playableEvent: Boolean, plots: List[PlotInCountry]) = {
      val targetPlot = priorityPlot(plots)
      (playableEvent && card.eventAlertsPlot(targetPlot.country.name, targetPlot.onMap.plot)) ||
      !firstCardOfPhase(US)
    }
  }
  
  object WMDWithTroopsAndPrestigeAbove3 extends AlertDecision {
    val desc = "WMD with troops and Prestige > 3?"
    def yesPath = AlertPlot
    def noPath  = WMDAtNonMuslimAndFundingBelow8
    def condition(card: Card, ops: Int, playableEvent: Boolean, plots: List[PlotInCountry]) =
      game.prestige > 3 &&
      (plots.exists (plot => plot.isWMD && 
                     inMuslimCountry(plot) && 
                     getMuslim(plot).totalTroopsThatAffectPrestige > 0))
  }
  
  object WMDAtNonMuslimAndFundingBelow8 extends AlertDecision {
    val desc = "WMD at non-Muslim and Funding <8?"
    def yesPath = AlertPlot
    def noPath  = AlertTable
    def condition(card: Card, ops: Int, playableEvent: Boolean, plots: List[PlotInCountry]) =
      game.funding < 8 &&
      (plots exists (plot => plot.isWMD && inNonMuslimCountry(plot)))
  }
  
  
  object ThreeOpsAvailable extends AlertDecision {
    val desc = "3 Ops available?"
    def yesPath = LastCardOrOrReservesBelow2
    def noPath  = PlayableNonJihadistEvent
    def condition(card: Card, ops: Int, playableEvent: Boolean, plots: List[PlotInCountry]) = ops >= 3
  }
  
  object LastCardOrOrReservesBelow2 extends AlertDecision {
    val desc = "Last card of phase or US reserves < 2?"
    def yesPath = AlertTable
    def noPath  = MultiplePlots
    def condition(card: Card, ops: Int, playableEvent: Boolean, plots: List[PlotInCountry]) = {
      game.reserves.us < 2 || !firstCardOfPhase(US)
    }
  }
  
  object PlayableNonJihadistEvent extends AlertDecision {
    val desc = "Playable non-Jihadist event?"
    def yesPath = PARFlowchart
    def noPath  = LastCardOrEventAlertsPriorityPlot
    def condition(card: Card, ops: Int, playableEvent: Boolean, plots: List[PlotInCountry]) = playableEvent
  }
  
  object MultiplePlots extends AlertDecision {
    val desc = "Multiple plots?"
    def yesPath = AlertTable
    def noPath  = PARFlowchart
    def condition(card: Card, ops: Int, playableEvent: Boolean, plots: List[PlotInCountry]) = plots.size > 1
  }
  
  // ------------------------------------------------------------------
  // Follow the operations flowchart to pick which operation will be performed.
  def alertResolutionFlowchart(
    card: Card,
    ops: Int,
    playableEvent: Boolean,
    plots: List[PlotInCountry]): AlertAction = {
    assert(plots.nonEmpty, "alertResolutionFlowchart() called with empty plots list")
    @tailrec def evaluateNode(node: AlertFlowchartNode): AlertAction = node match {
      case action:   AlertAction   => action
      case decision: AlertDecision =>
        botLog(s"ARF Flowchart: $node", Color.Debug)
        if (decision.condition(card, ops, playableEvent, plots))
          evaluateNode(decision.yesPath)
        else
          evaluateNode(decision.noPath)
    }
    evaluateNode(WMDPlacedInCountry)
  }
  


  // ------------------------------------------------------------------
  // US Priorities Table entries

  //  1. Lowest # of Plot dice
  val FewestPlotDicePriority = new LowestScorePriority("Lowest # of Plot dice",
                  muslimScore(numPlotDice, nonMuslimScore = 100))
  
  //  2. Regime Change
  val RegimeChangePriority = new CriteriaFilter("Regime Change",
                  muslimTest(_.inRegimeChange))
  
  //  3. Caliphate Capital
  val CaliphateCaptialPriority = new CriteriaFilter("Caliphate Capital",
                  muslimTest(_.caliphateCapital))

  //  4. Pakistan Arsenal
  val PakistanPriority = new CriteriaFilter("Pakistan arsenal",
                  c => c.name == Pakistan && c.wmdCache > 0)
  
  //  5. Syria Arsenal
  val SyriaPriority = new CriteriaFilter("Syria arsenal",
                  c => c.name == Syria && c.wmdCache > 0)
  
  //  6. Iran Arsenal
  val IranPriority = new CriteriaFilter("Iran arsenal",
                  c => c.name == Iran && c.wmdCache > 0)
  
  //  7. Civil War
  val CivilWArPriority = new CriteriaFilter("Civil War",
                  muslimTest(_.civilWar))

  //  8. Adjacent Good Ally
  val AdjacentGoodAllyPriority = new CriteriaFilter("Adjacent Good Ally",
                  muslimTest(m => game.adjacentToGoodAlly(m.name)))
                  
  //  9. Philippines (if Abu Sayyaf)  (Base game only)
  val PhilippinesPriority = new CriteriaFilter("Philippines (if Abu Sayyaf)",
                  c => c.name == Philippines && globalEventInPlay(AbuSayyaf))
                  
  // 10. Good
  val GoodPriority = new CriteriaFilter("Good Muslim", _.isGood)
  
  // 11. Fair
  val FairPriority = new CriteriaFilter("Fair Muslim", _.isFair)
  
  // Not in the table on the player aid sheet.  (Used by WoI non-Muslim)
  val PoorPriority = new CriteriaFilter("Poor Muslim", _.isPoor)
  
  // 12. Highest Resource
  val HighestResourcePriority = new HighestScorePriority("Highest resource",
                  muslimScore(_.resourceValue))
  
  // 13. Neutral
  val NeutralPriority = new CriteriaFilter("Neutral Muslim", muslimTest(_.isNeutral))
  
  // 14. Besieged Regime
  val BesiegedRegimePriority = new CriteriaFilter("Besieged Regime",
                  muslimTest(_.besiegedRegime))
  
  // 15. Most Cells
  val MostCellsPriority = new HighestScorePriority("Most cells", _.totalCells)
  
  // 16. Adjacent Islamist Rule
  val AdjacentIslamistRulePriority = new CriteriaFilter("Adjacent Islamist Rule",
                  muslimTest(m => game.adjacentToIslamistRule(m.name)))
  
  // 17. With Aid
  val WithAidPriority = new CriteriaFilter("With Aid", muslimTest(_.aidMarkers > 0))
  
  // 18. Fewest Cells
  val FewestCellsPriority = new LowestScorePriority("Fewest cells", _.totalCells)
  
  // 19. Lowest Resource
  val LowestResourcePriority = new LowestScorePriority("Lowest resource",
                  muslimScore(_.resourceValue, nonMuslimScore = 100))
  
  // 20. Most Plots
  val MostPlotsPriority = new HighestScorePriority("Most Plots", _.plots.size)

  // 21. Most Troops
  val MostTroopsPriority = new HighestScorePriority("Most Troops",
                  muslimScore(_.totalTroops))
  // 22. Oil Exporter
  val OilExporterPriority = new CriteriaFilter("Oil exporter",
                  muslimTest(_.oilExporter))

  // Other priorities that are not in the priority table.
  val NoCellsPriority = new CriteriaFilter("No cells", _.totalCells == 0)
  val HasCadrePriority = new CriteriaFilter("Has Cadre", _.hasCadre)
  val ClosestToUSPriority = new LowestScorePriority("Closest to US",
    c => distance(c.name, UnitedStates))

  // ------------------------------------------------------------------
  // US Operations Flowchart definitions.
  sealed trait Operation extends OpFlowchartNode
  case object WoiMuslimHighestDRM  extends Operation
  case object WoiMuslimMinusOneDRM extends Operation
  case object WoiNonMuslim         extends Operation
  case object Deploy               extends Operation  // Includes Withdraw
  case object Disrupt              extends Operation
  case object RegimeChange         extends Operation
  case object HomelandSecurity     extends Operation
  
  
  // This is the starting point of the PAR Flowchart
  object DisruptMuslim2MoreCellsDecision extends OperationDecision {
    def desc = "Disrupt Muslim at least 2 more cells than TandM and JSP?"
    def yesPath = Disrupt
    def noPath  = IR3AndRegimeChangeDecision
    def condition(ops: Int) = game.disruptMuslimTargets(ops) map game.getMuslim exists { m => 
      m.totalCells - m.totalTroopsAndMilitia >= 2 &&
      minorJihadSuccessPossible(m) 
    }
  }
  
  object IR3AndRegimeChangeDecision extends OperationDecision {
    def desc = "Islamist Rule resources >= 3 and Regime Change possible?"
    def yesPath = RegimeChange
    def noPath  = WoiMuslimNoPenaltyDecision
    def condition(ops: Int) = game.islamistResources >= 3 && game.regimeChangePossible(ops)
  }
  
  object WoiMuslimNoPenaltyDecision extends OperationDecision {
    def desc = "WoI in Muslim with DRM >= 0?"
    def yesPath = WoiMuslimHighestDRM
    def noPath  = PrestigeLowDecision
    def condition(ops: Int) = 
      woiMuslimTargets(ops) exists (m => modifyWoiRoll(0, m, silent = true) >= 0)
  }
  
  object PrestigeLowDecision extends OperationDecision {
    def desc = "Prestige Low?"
    def yesPath = DisruptForPrestigeDecision
    def noPath  = DeployDecision
    def condition(ops: Int) = game.prestigeLevel == Low
  }
  
  object DisruptForPrestigeDecision extends OperationDecision {
    def desc = "Disrupt for Prestige gain?"
    def yesPath = Disrupt
    def noPath  = USSoftDecision
    def condition(ops: Int) = game.disruptMuslimTargets(ops) map game.getMuslim exists { m => 
      m.disruptAffectsPrestige
    }
  }
    
  object DeployDecision extends OperationDecision {
    def desc = "Deploy Possible?"
    def yesPath = Deploy
    def noPath  = RegimeChangeDecision
    def condition(ops: Int) = botDeployTargets(ops) match {
      case Some((fromCandidates, toCandidates)) =>
        val from = deployFromTarget(fromCandidates)
        val to   = deployToTarget(toCandidates)
        (from.nonEmpty && to.nonEmpty && from != to)
      case None => false
    }
  }
  
  object USSoftDecision extends OperationDecision {
    def desc = "US Soft?"
    def yesPath = WoiNonMuslim
    def noPath  = DeployDecision
    def condition(ops: Int) = game.usPosture == Soft
  }
  
  object RegimeChangeDecision extends OperationDecision {
    def desc = "Regime Change Possible?"
    def yesPath = RegimeChange
    def noPath  = DisruptForPrestigeOrPlaceCadreDecision
    def condition(ops: Int) = game.regimeChangePossible(ops)
  }
  
  object DisruptForPrestigeOrPlaceCadreDecision extends OperationDecision {
    def desc = "Disrupt for Prestige gain or to place a Cadre?"
    def yesPath = Disrupt
    def noPath  = WoiMuslimMinus1DrmDecision
    def condition(ops: Int) = game.disruptMuslimTargets(ops) map game.getMuslim exists { m => 
      m.disruptAffectsPrestige || onlyOneActiveCell(m)
    }
  }
  
  object WoiMuslimMinus1DrmDecision extends OperationDecision {
    def desc = "WoI in Muslim with DRM of -1?"
    def yesPath = WoiMuslimMinusOneDRM
    def noPath  = HomelandSecurity
    def condition(ops: Int) = 
      woiMuslimTargets(ops) exists (m => modifyWoiRoll(0, m, silent = true) == -1)
  }

  
  // ------------------------------------------------------------------
  // Follow the operations flowchart to pick which operation will be performed.
  def operationsFlowchart(ops: Int): Operation = {
    @tailrec def evaluateNode(node: OpFlowchartNode): Operation = node match {
      case operation: Operation =>
        operation
      case decision: OperationDecision =>
        botLog(s"PAR Flowchart: $node", Color.Debug)
        if (decision.condition(ops))
          evaluateNode(decision.yesPath)
        else
          evaluateNode(decision.noPath)
    }
    evaluateNode(DisruptMuslim2MoreCellsDecision)
  }


  // ------------------------------------------------------------------
  // Note: I put PhilippinesPriority in all three lists.
  // It will only trigger if Aby Sayyaf event is in play and that
  // can only happen if a campaign game is being played that started
  // with one of the Labyrinth scenarios.
  val LabyrinthDisruptPriorities = List(
    PakistanPriority, PhilippinesPriority, GoodPriority, FairPriority, HighestResourcePriority, 
    BesiegedRegimePriority, MostCellsPriority, AdjacentIslamistRulePriority, WithAidPriority, OilExporterPriority)
    
  val AwakeningDisruptPriorities = List(
    PakistanPriority, SyriaPriority, IranPriority, PhilippinesPriority, 
    GoodPriority, FairPriority, HighestResourcePriority, BesiegedRegimePriority,
    MostCellsPriority, AdjacentIslamistRulePriority, WithAidPriority, OilExporterPriority)
    
  val ForeverWarDisruptPriorities = List(
    PakistanPriority, IranPriority, SyriaPriority, PhilippinesPriority,
    GoodPriority, FairPriority, HighestResourcePriority, BesiegedRegimePriority,
    MostCellsPriority, AdjacentIslamistRulePriority, WithAidPriority, OilExporterPriority)

  def disruptPriorities: List[CountryFilter] = game.currentMode match {
    case LabyrinthMode   => LabyrinthDisruptPriorities
    case AwakeningMode   => AwakeningDisruptPriorities
    case ForeverWarMode  => ForeverWarDisruptPriorities
  }
  
  val DisruptFlowchart = List(
    new CriteriaFilter("Muslim at least 2 more cells than TandM and JSP", 
      muslimTest(m => m.totalCells - m.totalTroopsAndMilitia >= 2 && minorJihadSuccessPossible(m))),
    new CriteriaFilter("For Prestige gain", muslimTest(_.disruptAffectsPrestige)),
    new CriteriaFilter("To place cadre", onlyOneActiveCell)    
  )
  
  def disruptTarget(names: List[String]): Option[String] = {
    botLog("Find \"Disrupt\" target", Color.Debug)
    val candidates = selectCandidates(game getCountries names, DisruptFlowchart)
    topPriority(candidates, disruptPriorities) map (_.name)
  }
  
  def disruptPriority(names: List[String]): Option[String] = {
   topPriority(names map game.getCountry, disruptPriorities) map (_.name) 
  }
  
  // Narrow a list of Muslim countries to those where the
  // number of cells - (troops + militia) is highest.
  def highestCellsMinusTandM(names: List[String]): List[String] = {
    val muslims = names.map(game.getMuslim)
    val score = (m: MuslimCountry) => m.totalCells - m.totalTroopsAndMilitia
    val high = muslims.map(score).max
    muslims.filter(m => score(m) == high).map(_.name)
  }
  
  // ------------------------------------------------------------------
  def servalTarget(names: List[String]): Option[String] = {
    val candidates = game.getMuslims(highestCellsMinusTandM(names))
    topPriority(candidates, NeutralPriority::Nil) map (_.name)
  }
  
  // ------------------------------------------------------------------
  def posturePriority(names: List[String]): Option[String] = {
    val priorities = List(
      new CriteriaFilter("Opposite posture of US",
        nonMuslimTest(n => n.isTested && n.canChangePosture && n.posture != game.usPosture)),
      new CriteriaFilter("Untested non-Muslim", nonMuslimTest(_.isUntested)))
    topPriority(game getNonMuslims names, priorities) map (_.name)
  }
  
  // ------------------------------------------------------------------
  
  val WoiNonMuslimFlowchart = List(
    new CriteriaFilter("Opposite posture of US",
      nonMuslimTest(n => n.isTested && n.canChangePosture && n.posture != game.usPosture)),
    new CriteriaFilter("Untested non-Muslim", nonMuslimTest(_.isUntested)),
    new CriteriaFilter("Same posture as US",
      nonMuslimTest(n => n.isTested && n.canChangePosture && n.posture == game.usPosture))
  )
  
  def woiNonMuslimTarget(names: List[String]): Option[String] = {
    // Not in the Priorities Table, but listed in OpP flowchard.
    val priorities = List(
      PoorPriority, FairPriority, GoodPriority, FewestCellsPriority)
    botLog("Find \"non-Muslim WoI\" target", Color.Debug)
    val candidates = selectCandidates(game getCountries names, WoiNonMuslimFlowchart)
    topPriority(candidates, priorities) map (_.name)
  }

  def woiNonMuslimPriority(names: List[String]): Option[String] = {
    val priorities = List(
      new CriteriaFilter("Opposite posture of US",
        nonMuslimTest(n => n.isTested && n.canChangePosture && n.posture != game.usPosture)),
      new CriteriaFilter("Same posture as US",
        nonMuslimTest(n => n.isTested && n.canChangePosture && n.posture == game.usPosture)),
        FewestCellsPriority, GoodPriority, FairPriority, PoorPriority)
    
    botLog("Find \"non-Muslim WoI\" priority", Color.Debug)
    topPriority(game getCountries names, priorities) map (_.name)
  }
  // ------------------------------------------------------------------
  val LabyrinthDeployToPriorities = List(
    PakistanPriority, FairPriority,
    HighestResourcePriority, BesiegedRegimePriority, MostCellsPriority,
    AdjacentIslamistRulePriority, WithAidPriority, OilExporterPriority)
    
  val AwakeningDeployToPriorities = List(
    PakistanPriority, SyriaPriority, IranPriority, FairPriority,
    HighestResourcePriority, BesiegedRegimePriority, MostCellsPriority,
    AdjacentIslamistRulePriority, WithAidPriority, OilExporterPriority)
  
  val ForeverWarDeployToPriorities = List(
    PakistanPriority, IranPriority, SyriaPriority, FairPriority,
    HighestResourcePriority, BesiegedRegimePriority, MostCellsPriority,
    AdjacentIslamistRulePriority, WithAidPriority, OilExporterPriority)
  
  def deployToPriorities: List[CountryFilter] = game.currentMode match {
    case LabyrinthMode   => LabyrinthDeployToPriorities
    case AwakeningMode   => AwakeningDeployToPriorities
    case ForeverWarMode  => ForeverWarDeployToPriorities
  }
  
  val DeployToFlowchart = List(
    new CriteriaFilter("Philippines if Abu Sayyaf, cell, no troops",  // Base game only
        nonMuslimTest(n => n.name == Philippines && n.hasMarker(AbuSayyaf) &&  
                         n.totalCells > 0 && n.totalTroops == 0)),
    new CriteriaFilter("With cells, but no troops or militia",
      muslimTest(m => m.totalCells > 0 && m.totalTroopsAndMilitia == 0)),
    new CriteriaFilter("Regime Change needs troops + militia for WoI",
      muslimTest(m => m.inRegimeChange && m.totalTroopsAndMilitia - m.totalCells < 5))
  )

  def deployToTarget(names: List[String]): Option[String] = {
    botLog("Find \"Deploy To\" target", Color.Debug)
    val track        = names find (_ == "track")
    val countryNames = names filterNot (_ == "track")
    val target = selectCandidates(game getCountries countryNames, DeployToFlowchart) match {
      case Nil        => track
      case candidates => topPriority(candidates, deployToPriorities) map (_.name)
    }
    botLog(s"Deploy To result: ${target getOrElse "<none>"}")
    target
  }
  
  def deployToPriority(names: List[String]): Option[String] = {
   topPriority(names map game.getCountry, deployToPriorities) map (_.name) 
  }
  
  // ------------------------------------------------------------------
  val DeployFromPriorities = List(
    FewestCellsPriority, LowestResourcePriority, MostPlotsPriority, MostTroopsPriority)
  
  val DeployFromFlowchart = List(
    new CriteriaFilter("Philippines if Moro Talks",  // Base game only
        nonMuslimTest(n => n.name == Philippines && n.hasMarker(MoroTalks))),
    new CriteriaFilter("Islamist Rule", muslimTest(_.isIslamistRule)),
    new CriteriaFilter("Good Ally without cells OR with troop markers/militia",
        muslimTest(m => m.isGood && m.isAlly && 
             (m.totalCells == 0 || m.militia > 0 || m.markerTroops > 0))),
    new CriteriaFilter("Fair Ally without cells OR with troop markers/militia",
        muslimTest(m => m.isFair && m.isAlly && 
             (m.totalCells == 0 || m.militia > 0 || m.markerTroops > 0)))
  )
  
  def deployFromTarget(names: List[String]): Option[String] = {
    botLog("Find \"Deploy From\" target", Color.Debug)
    // Bot will always deploy exactly two troops from the track so it is of the table
    // if there is only 1 cube left
    val track        = names find (_ == "track" && game.troopsAvailable > 1)
    // The bot will not deploy markers so get rid of any countries without troops cubes
    val countryNames = names filterNot (_ == "track") filterNot (name => (game getCountry name).troops == 0)
    val target = selectCandidates(game getCountries countryNames, DeployFromFlowchart) match {
      case Nil        => track
      case candidates => topPriority(candidates, DeployFromPriorities) map (_.name)
    }
    botLog(s"Deploy From result: ${target getOrElse "<none>"}")
    target
  }
  
  // ------------------------------------------------------------------
  val RegimeChangePriorities = List(
    HighestResourcePriority, AdjacentIslamistRulePriority, FewestCellsPriority)
  
  def regimeChangeTarget(names: List[String]): Option[String] = {
    botLog("Find \"Regime Change\" target", Color.Debug)
    topPriority(game getCountries names, RegimeChangePriorities) map (_.name)
  }
  
  
  // ------------------------------------------------------------------
  val RegimeChangeFromFlowchart = List(
    new CriteriaFilter("Philippines if Moro Talks",  // Base game only
        muslimTest(m => globalEventInPlay(MoroTalks) && m.name == Philippines)),
    new CriteriaFilter("Islamist Rule", muslimTest(_.isIslamistRule)),
    new CriteriaFilter("Good Ally without cells OR with troop markers/militia",
        muslimTest(m => m.isGood && m.isAlly && 
             (m.totalCells == 0 || m.militia > 0 || m.markerTroops > 0))),
    new CriteriaFilter("Fair Ally without cells OR with troop markers/militia",
        muslimTest(m => m.isFair && m.isAlly && 
             (m.totalCells == 0 || m.militia > 0 || m.markerTroops > 0))),
    new CriteriaFilter("Good", _.isGood),         
    new CriteriaFilter("Fair", _.isFair),         
    new CriteriaFilter("Poor", _.isPoor)
  )
  
  // Used to determine from where to get troops to use in a Regime Change Operation.
  def regimeChangeSource(names: List[String]): Option[String] = {
    botLog("Find \"Regime Change From\" target", Color.Debug)
    val track        = names find (_ == "track")
    val countryNames = names filterNot (_ == "track")
    selectCandidates(game getCountries countryNames, RegimeChangeFromFlowchart) match {
      case Nil        => track
      case candidates => topPriority(candidates, DeployFromPriorities) map (_.name)
    }
  }
  
  // ------------------------------------------------------------------
  val LabyrinthWoiMuslimPriorities = List(
    FewestPlotDicePriority, RegimeChangePriority, CaliphateCaptialPriority,
    PakistanPriority, AdjacentGoodAllyPriority, HighestResourcePriority, NeutralPriority,
    BesiegedRegimePriority, AdjacentIslamistRulePriority, FewestCellsPriority,
    MostTroopsPriority, OilExporterPriority)
  
  val AwakeningWoiMuslimPriorities = List(
    FewestPlotDicePriority, RegimeChangePriority, CaliphateCaptialPriority,
    PakistanPriority, SyriaPriority, IranPriority, CivilWArPriority,
    AdjacentGoodAllyPriority, HighestResourcePriority, NeutralPriority,
    BesiegedRegimePriority, AdjacentIslamistRulePriority, FewestCellsPriority,
    MostTroopsPriority, OilExporterPriority)
  
  val ForeverWarWoiMuslimPriorities = List(
    FewestPlotDicePriority, RegimeChangePriority, CaliphateCaptialPriority,
    PakistanPriority, IranPriority, SyriaPriority, CivilWArPriority,
    AdjacentGoodAllyPriority, HighestResourcePriority, NeutralPriority,
    BesiegedRegimePriority, AdjacentIslamistRulePriority, FewestCellsPriority,
    MostTroopsPriority, OilExporterPriority)
  
  def woiMuslimPriorities: List[CountryFilter] = game.currentMode match {
    case LabyrinthMode   => LabyrinthWoiMuslimPriorities
    case AwakeningMode   => AwakeningWoiMuslimPriorities
    case ForeverWarMode  => ForeverWarWoiMuslimPriorities
  }

  // The Trump Tweets event can for the US to remove an aid marker.
  // The rules do not specify how to prioritize this so I will
  // get the list of all countries with aid markers using the woiMuslimPriorities
  // then select the last one in the list.
  def removeAidTarget: Option[String] = {
    val withAid = countryNames(game.muslims filter (_.aidMarkers > 0))
    
    @tailrec def nextPriority(candidates: List[String], inOrder: List[String]): List[String] = candidates match {
      case Nil => inOrder
      case xs  => 
        val winner = markerAlignGovTarget(xs).get
        nextPriority(candidates filterNot (_ == winner), winner :: inOrder)
    }
    
    if (withAid.isEmpty)
      None
    else
      Some(nextPriority(withAid, Nil).head)
  }
  
  def markerAlignGovTarget(names: List[String]): Option[String] = {
    botLog("Find \"Marker/Align/Gov\" target", Color.Debug)
    topPriority(game getCountries names, woiMuslimPriorities) map (_.name)
  }
  
  val BestWoiDRMFilter = new HighestScoreNode("Highest WoI DRM",
    muslimTest(_ => true),
    muslimScore(m => modifyWoiRoll(0, m, silent = true)))
  
  def woiBestDRMTarget(names: List[String]): Option[String] = {
    botLog("Find \"Best DRM WoI\" target", Color.Debug)
    val flowchart  = BestWoiDRMFilter::Nil
    val candidates = selectCandidates(game getCountries names, flowchart)
    topPriority(candidates, woiMuslimPriorities) map (_.name)
  }
  
  val WoiDrmMinusOneFilter = new CriteriaFilter("WoI DRM -1",
    muslimTest(m => modifyWoiRoll(0, m, silent = true) == -1))
  
  def woiDrmMinusOneTarget(names: List[String]): Option[String] = {
    botLog("Find \"DRM -1 WoI\" target", Color.Debug)
    val flowchart = WoiDrmMinusOneFilter::Nil
    val candidates = selectCandidates(game getCountries names, flowchart)
    topPriority(candidates, woiMuslimPriorities) map (_.name)
  }
  
  // ------------------------------------------------------------------
  // Get target for the UNSCR 1973 event
  def unscr1973Target(names: List[String]): Option[String] = {
    val flowchart = List(USBot.HighestResourcePriority)
      
    botLog("Find \"UNSCR 1973\" target", Color.Debug)
    val candidates = selectCandidates(game getCountries names, flowchart)
    topPriority(candidates, disruptPriorities) map (_.name) 
  }
  
  // ------------------------------------------------------------------
  // Get target for the SCAF event
  def scafTarget(names: List[String]): Option[String] = {
    val flowchart = List(
      new CriteriaFilter("Adversary Muslim", muslimTest(_.isAdversary)),
      new CriteriaFilter("Neutral Muslim", muslimTest(_.isNeutral)))
    val priorities = FewestCellsPriority::PoorPriority::woiMuslimPriorities
      
    botLog("Find \"SCAF\" target", Color.Debug)
    val candidates = selectCandidates(game getCountries names, flowchart)
    topPriority(candidates, priorities) map (_.name)
  }
  
  // ------------------------------------------------------------------
  // Get target for the Status Quo event
  def statusQuoTarget(names: List[String]): Option[String] = {
    val priorities = List(
      new CriteriaFilter("Adversary Muslim", muslimTest(_.isAdversary)),
      new CriteriaFilter("Neutral Muslim", muslimTest(_.isNeutral)),
      HighestResourcePriority
    )
      
    botLog("Find \"Status Quo\" target", Color.Debug)
    topPriority(game getMuslims names, priorities) map (_.name)
  }
  
  // ------------------------------------------------------------------
  // Ebola Scare target
  def ebolaScareTarget(names: List[String]): Option[String] = {
    val flowchart = List(
      new CriteriaFilter("Islamist Rule", muslimTest(_.isIslamistRule)),
      new CriteriaFilter("Ally without cells", muslimTest(m => m.isAlly && m.totalCells == 0)))
      
    botLog("Find \"Ebola Scare\" target", Color.Debug)
    selectCandidates(game getCountries names, flowchart) match {
      case Nil => shuffle(names).headOption
      case xs  => shuffle(xs).headOption map (_.name)
    }
  }
  
  def unCeasefireTarget(names: List[String]): Option[String] = {
    val priorities = List(
      new HighestScorePriority("Most militia - cells", muslimScore(m => m.militia - m.totalCells)),
      new CriteriaFilter("Adversary", muslimTest(m => m.isAdversary)),
      new CriteriaFilter("Neutral", muslimTest(m => m.isNeutral)))
      
    botLog("Find \"UN Ceasefire\" target", Color.Debug)
    topPriority(game getMuslims names, priorities) map (_.name)
  }
  
  def qadhafiCandidates: List[String] =
    game.muslims
      .filter(m => m.civilWar && m.totalTroopsAndMilitia > m.totalCells)
      .map(_.name)
    
  def qadhafiTarget(names: List[String]): Option[String] = {
    val priorities = List(
      new CriteriaFilter("TandM > Cells", muslimTest(m => m.totalTroopsAndMilitia > m.totalCells)),
      HighestResourcePriority)
      
    botLog("Find \"Qadhafi\" target", Color.Debug)
    topPriority(game getMuslims names, priorities) map (_.name)
  }
  
  def revolutionTarget(names: List[String]): Option[String] = {
    val priorities = List(
      new HighestScorePriority("Highest reaction - awakening", muslimScore(_.reactionDelta)),
      HighestResourcePriority)
      
    botLog("Find \"Revolution\" target", Color.Debug)
    topPriority(game getMuslims names, priorities) map (_.name)
  }

  
  def criticalMiddleShiftPossibilities(names: List[String]): List[String] = {
    val flowchart = List(
      new CriteriaFilter("Adversary", muslimTest(m => m.isAdversary)),
      new CriteriaFilter("Neutral", muslimTest(m => m.isNeutral)))
    botLog("Find \"Critical Middle\" target", Color.Debug)
    countryNames(selectCandidates(game getCountries names, flowchart)) 
  }
  
  // ------------------------------------------------------------------
  // Pick the top priority plot in the given countries.
  def selectPriorityPlot(names: List[String]): PlotInCountry = {
    val plots = for (name <- names; c = game getCountry name; p <- c.plots)
      yield(PlotInCountry(p, c))
    assert(plots.nonEmpty, s"selectPriorityPlot(): No plots in the given countries")
    val best = priorityPlot(plots)
    best
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
  
  
  def resetStaticData(): Unit = {
    clearCachedTargets()
  }

  // Called when the Human Jihadist player plays a Jihadist Associated card.
  def performTriggeredEvent(card: Card): Unit = {
    resetStaticData()
    performCardEvent(card, US, triggered = true)
  }
  
  def disruptRemovesLastCell(card: Card): Boolean = {
    val maxOps  = maxOpsPlusReserves(card)
    game.disruptTargets(maxOps) exists { name =>
      game.disruptLosses(name) match {
        case Some(Left(numCells)) =>
          //  If we would disrupt the last cells on the map and they are
          //  not sleeper cells, then we would remove last cell from the map
          numCells == game.totalCellsOnMap && game.getCountry(name).sleeperCells == 0
        
        case _ => false  // Would only disrupt a cadre
      }
    }
  }
  
  
  def wouldRemoveLastCell(target: String, numToRemove: Int): Boolean = {
    val targetCells = game.getCountry(target).totalCells

    targetCells > 0 &&
    targetCells == game.totalCellsOnMap &&
    numToRemove >= targetCells
  }
  
  
  // Starting point for Jihadist bot card play.
  def cardPlay(card: Card, ignoreEvent: Boolean): Unit = {
    resetStaticData()
    val eventPlayable =
      !ignoreEvent &&
      card.eventIsPlayable(US) &&
      card.botWillPlayEvent(US)

    if (disruptRemovesLastCell(card))  {
      // The Bot will execute the auto trigger event first.
      if (card.autoTrigger) {
        performCardEvent(card, US)
        log()
      }
      
      disruptOperation(card)
    }
    else if (eventPlayable && card.eventRemovesLastCell())
      performCardEvent(card, US)
    else {
      val maxOps = maxOpsPlusReserves(card)
      val plots = for (country <- game.countries; plot <- country.plots)
        yield PlotInCountry(plot, country)
    
    
      // If there is at least one plot on the map then
      // we consult the Alert Resolution Flowchart (ARF)
      val consultPAR = plots.isEmpty || {
        alertResolutionFlowchart(card, maxOps, eventPlayable, plots) match {
          case AlertPlot       => 
            val plot = priorityPlot(plots)
            val otherPlotsInCountry = plots filter (p => p.id != plot.id && p.country.name == plot.country.name)
            
            // The Bot will execute the auto trigger event first.
            if (card.autoTrigger) {
              performCardEvent(card, US)
              log()
            }
            alertPlot(card, plot)
            
            if (game.usResolve(Competent) && otherPlotsInCountry.nonEmpty) {
              log(s"$US Bot with Competent resolve alerts two plots in the same country")
              val plot2 = priorityPlot(otherPlotsInCountry)
              performAlert(plot2.country.name, plot2.onMap)
            }
            false
          
          case AlertTable      => !alertTable(card, plots)
          case PARFlowchart    => true
          case AddToUSReserves => addToReserves(US, card.ops); false
        }
      }
    
      // If the Alert Resolution Flowchart has indicated that we continue,
      // the first see if Reassessment is possible.  If Reassessment is
      // not performed, then finally we consult the PAR flowchart.
      if (consultPAR && !reassessment(card)) {
        // If the event is playable then the event is always executed
        if (eventPlayable) {
          performCardEvent(card, US)
          // If the card event is Unassociated add ops to the Bot's reserves.
          if (card.association == Unassociated) 
            addToReserves(US, card.ops)
        }
        else {
          // The Bot will execute the auto trigger event first.
          if (card.autoTrigger) {
            performCardEvent(card, US)
            log()
          }
    
          val opsUsed = operationsFlowchart(maxOps) match {
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
  }
  
  // Alert the given plot
  def alertPlot(card: Card, plot: PlotInCountry): Unit = {
    log()
    log(s"$US performs an Alert operation")
    log(separator())
    assert(maxOpsPlusReserves(card) >= 3, "Not enough Ops for Alert")
    if (3 > card.ops)
      expendBotReserves(3 - card.ops)
    performAlert(plot.country.name, plot.onMap)
  }
  
  // Checks to see if the Bot wants to do a Reassessment and
  // has enough Ops to do so.
  // Returns true if Reassessment performed
  def reassessment(card: Card): Boolean = {
    val tryReassess = {
       firstCardOfPhase(US) &&
       hasCardInHand(US) &&
       (card.ops + game.reserves.us >= 3) &&  // Possible if we have at least 3 on hand
       ((game.usPosture == Soft && game.islamistResources >= 2) ||
        (game.usPosture == Hard && game.gwotPenalty == 3 && game.numIslamistRule == 0))
    }
    if (tryReassess) {
      // Reassessment is desired. Ask if the next US card has enough Ops
      val opsNeeded = 6 - card.ops - game.reserves.us

      if (opsNeeded == 1 || askYorN(s"Does the next card in the $US Bot hand have at least $opsNeeded Ops (y/n)? ")) {
        val prompt = "Enter the next card in the US hand. Card # "
        val cardNum = askCardNumber(FromRole(US)::Nil, prompt, allowNone = false).get
        val card2 = deck(cardNum)
        if (card2.ops >= opsNeeded) {
          // Replace the head card play with a reassessment 
          game = game.copy(plays = PlayedReassement(card.number, card2.number) :: game.plays.tail)
          logCardPlay(US, card2, false)
          // Check to see if either of the cards played has an auto trigger event.
          // If so the event happens first.
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
          println(s"$card2 does not have enough Ops")
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
    testCountry(target)
    val opsUsed = (game getMuslim target).governance
    if (opsUsed > card.ops)
      expendBotReserves(opsUsed - card.ops)
    addOpsTarget(target)
    performWarOfIdeas(target, opsUsed)
    opsUsed
  }
  
  def woiMuslimDRMMinusOneOperation(card: Card): Int = {
    val maxOps  = maxOpsPlusReserves(card)
    val target  = woiDrmMinusOneTarget(countryNames(woiMuslimTargets(maxOps))).get
    testCountry(target)
    val opsUsed = (game getMuslim target).governance
    if (opsUsed > card.ops)
      expendBotReserves(opsUsed - card.ops)
    addOpsTarget(target)
    performWarOfIdeas(target, opsUsed)
    opsUsed
  }
  
  def woiNonMuslimOperation(card: Card): Int = {
    val maxOps  = maxOpsPlusReserves(card)
    val target  = woiNonMuslimTarget(countryNames(woiNonMuslimTargets(maxOps))).get
    val opsUsed = (game getNonMuslim target).governance
    if (opsUsed > card.ops)
      expendBotReserves(opsUsed - card.ops)
    addOpsTarget(target)
    performWarOfIdeas(target, opsUsed)
    opsUsed
  }
  
  def deployOperation(card: Card): Int = {
    val maxOps  = maxOpsPlusReserves(card)
    val (fromCandidates, toCandidates) = botDeployTargets(maxOps).get
    val from = deployFromTarget(fromCandidates).get
    val to   = deployToTarget(toCandidates).get
    val withdraw = {
      maxOps >= 3 && 
      game.usPosture == Soft &&
      (game isMuslim from) &&
      (game getMuslim from).inRegimeChange
    }
    
    val numTroops = from match {
      case "track"          => 2 min game.troopsAvailable  // Always deploy exactly 2 troops from track
      case name if withdraw => (game getCountry name).troops  // Withdraw all troop
      case name             => (game getCountry name).maxDeployFrom
    }
    
    val opsUsed = if (withdraw)
      3
    else
      to match {
        case "track" => 1
        case name    => (game getCountry name).governance
      }
      
    if (opsUsed > card.ops)
      expendBotReserves(opsUsed - card.ops)

    if (withdraw) {
      log(s"$US performs a Withdraw operation")
      addOpsTarget(from)
      performWithdraw(from, to, numTroops)
    }
    else {
      log(s"$US performs a Deploy operation")
      addOpsTarget(to)
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
    addOpsTarget(target)
    performDisrupt(target)
    opsUsed
  }
  
  def regimeChangeOperation(card: Card): Int = {
    val maxOps  = maxOpsPlusReserves(card)
    assert(maxOps >= 3, "regimeChangeOperation() called with less than 3 Ops available")
    assert(
      game.regimeChangePossible(maxOps),
      s"regimeChangeOperation() called but regimeChangePossible($maxOps) == false")
    val opsUsed = 3
    // Returns (target, source)
    def getTarget(candidates: List[String]): (String, String) = {
      if (candidates.isEmpty)
        throw new IllegalStateException("regimeChangeOperation() no valid regime change target")
      else {
        // Get the highest priority target.  But we must make sure that it is not also
        // the only space with enough cells to act as the source!
        val target = regimeChangeTarget(candidates).get
        regimeChangeSource(game.regimeChangeSourcesFor(target)) match {
          case Some(source) => (target, source)
          case None => getTarget(candidates filterNot (_ == target))
        }
      }
    }
    val (target, source)  = getTarget(game.regimeChangeTargets)
    if (opsUsed > card.ops)
      expendBotReserves(opsUsed - card.ops)
    log(s"$US performs a Regime Change operation in $target")
    addOpsTarget(target)
    performRegimeChange(source, target, 6) // Bot always uses exactly 6 troops
    opsUsed
  }
  
  sealed trait HomelandSecurityAction
  case object DisruptUS            extends HomelandSecurityAction
  case object WoiSoftNonMuslim     extends HomelandSecurityAction
  case object DisruptNonMuslim     extends HomelandSecurityAction
  case object AddToReserves        extends HomelandSecurityAction
  case object DisruptMuslimToCadre extends HomelandSecurityAction
  case object WoiNonMuslimOpposite extends HomelandSecurityAction
  case object WoiNonMuslimUntested extends HomelandSecurityAction

  // cardOps is the numbe of Ops remaining from the played card. (no reserves)
  // maxOps  is the tota number of Ops available including reserves.
  // The DisruptMuslimToRemoveCadre, WoiNonMuslimOpposite, WoiNonMuslimUntested actions cannot use reserves.
  def getHomelandSecurityAction(cardOps: Int, maxOps: Int): Option[HomelandSecurityAction] = {
    val canDisruptUS =
      (game getNonMuslim UnitedStates).cells > 0 ||
      (game getNonMuslim UnitedStates).hasCadre
    val canWoiSoftNonMuslim =
      game.usPosture == Hard &&
      game.worldPosture == Soft &&
      (game.warOfIdeasNonMuslimTargets(maxOps).map(game.getNonMuslim).filter(_.isSoft)).nonEmpty
    
    val canDisruptNonMuslim = game.disruptNonMuslimTargets(maxOps).nonEmpty
    // The following can only use cardOps (no reserves)
    val canAddToReserves = cardOps > 0 && game.reserves.us < 2
    val canDisruptMuslimCadre = game.disruptMuslimTargets(cardOps) map game.getMuslim exists (_.hasCadre)
    val canWoINonMuslimOpposite = 
      game.warOfIdeasNonMuslimTargets(cardOps) map game.getNonMuslim exists (_.isOppositeUsPosture)
    val canWoINonMuslimUntested = 
      game.warOfIdeasNonMuslimTargets(cardOps) map game.getNonMuslim exists (_.isUntested)
    
    if      (canDisruptUS)            Some(DisruptUS)
    else if (canWoiSoftNonMuslim)     Some(WoiSoftNonMuslim)
    else if (canDisruptNonMuslim)     Some(DisruptNonMuslim)
    else if (canAddToReserves)        Some(AddToReserves)
    else if (canDisruptMuslimCadre)   Some(DisruptMuslimToCadre)
    else if (canWoINonMuslimOpposite) Some(WoiNonMuslimOpposite)
    else if (canWoINonMuslimUntested) Some(WoiNonMuslimUntested)
    else                              None
  }
  
  // Perform Homeland Security
  // The opsUsed parameter is the number of Ops used to perform the card operation.
  // If this value is less than the number of Ops on the card, then we will 
  // perform homeland using the remainder of ops on the card plus reserves as needed.
  // This may result in more than 3 total Ops being used for the current card.
  // But no single operation can be performed using only reserve ops.
  // If the opUsed is greater than or equal to the number of Ops on the card,
  // then we do nothing.
  def homelandSecurity(card: Card, opsUsed: Int): Unit = {
    if (opsUsed < card.ops) {
      val unusedOps  = card.ops - opsUsed
      val maxRadOps  = unusedOps + game.reserves.us
      val unusedDisp = amountOf(unusedOps, "unused Op")
      val resDisp    = amountOf(game.reserves.us,"reserve")
      log()
      log(s"$US performs Homeland Security with ${unusedDisp} (${resDisp})")
      log(separator())
      
      def nextAction(completed: Int): Unit = {
        // Stop once all of the used cards ops have been used
        if (completed < unusedOps) {
          val cardOps    = unusedOps - completed      // Ops remaining from the card
          val reserveOps = maxRadOps - cardOps        // Ops remaining from reserves
          val ops = getHomelandSecurityAction(cardOps, maxRadOps - completed) match {
            case Some(DisruptUS)            => hsDisruptUS(cardOps, reserveOps)
            case Some(WoiSoftNonMuslim)     => hsWoiSoftNonMuslim(cardOps, reserveOps)
            case Some(DisruptNonMuslim)     => hsDisruptNonMuslim(cardOps, reserveOps)
            case Some(AddToReserves)        => hsAddtoReserves(cardOps)
            case Some(DisruptMuslimToCadre) => hsDisruptMuslimCadre(cardOps)
            case Some(WoiNonMuslimOpposite) => hsWoiNonMuslimOpposite(cardOps)
            case Some(WoiNonMuslimUntested) => hsWoiNonMuslimUntested(cardOps)
            case None => -1  // Finished with radicalization
          }
          if (ops > 0)  
            nextAction(completed + ops)
        }
      }
      nextAction(0)
    }
  }
  
  
  // Disrupt in the UnitedStates
  // cardsOps   - The number of unused Ops remaining from the card
  // reserveOps - The number of unused Ops remaining from reserves
  // Returns the number of ops used
  def hsDisruptUS(cardOps: Int, reserveOps: Int): Int = {
    val opsUsed = 1
    if (cardOps < opsUsed)
      expendBotReserves(opsUsed)
    log()
    log(s"Homeland Security: Disrupt in the United States")
    performDisrupt(UnitedStates)
    opsUsed
  }
    
  // War of Ideas in a Soft Muslim country.
  // Priorities: Good then Fair then Poor. 
  // Among these, no Cells then Cadre, priority to closest to the US.
  // cardsOps   - The number of unused Ops remaining from the card
  // reserveOps - The number of unused Ops remaining from reserves
  // Returns the number of ops used
  def hsWoiSoftNonMuslim(cardOps: Int, reserveOps: Int): Int = {
    val priorities = List(
      GoodPriority, FairPriority, PoorPriority,
      NoCellsPriority, HasCadrePriority, ClosestToUSPriority)              
    val maxOps = cardOps + reserveOps
    val candidates = game.warOfIdeasNonMuslimTargets(maxOps) map game.getNonMuslim filter (_.isSoft)
    val target  = topPriority(candidates, priorities).get
    val opsUsed = target.governance
    if (opsUsed > cardOps)
      expendBotReserves(opsUsed - cardOps)
    log()
    log(s"Homeland Security: War of Ideas in ${target.name}")
    performWarOfIdeas(target.name, opsUsed)
    opsUsed
  }
    
  // Disrupt a non-Muslim country
  // Priorities: Closest to the US, then most cells.
  // cardsOps   - The number of unused Ops remaining from the card
  // reserveOps - The number of unused Ops remaining from reserves
  // Returns the number of ops used
  def hsDisruptNonMuslim(cardOps: Int, reserveOps: Int): Int = {
    val priorities = List(ClosestToUSPriority, MostCellsPriority)              
    val maxOps = cardOps + reserveOps
    val candidates = game.disruptNonMuslimTargets(maxOps) map game.getNonMuslim
    val target  = topPriority(candidates, priorities).get
    val opsUsed = target.governance
    if (opsUsed > cardOps)
      expendBotReserves(opsUsed - cardOps)
    log()
    log(s"Homeland Security: Disrupt in ${target.name}")
    performDisrupt(target.name)
    opsUsed
  }
    
  // cardsOps - The number of unused Ops remaining from the card
  // Add any remaining card ops to reserves until reserves are full.
  // Returns the number of ops added
  def hsAddtoReserves(cardOps: Int): Int = {
    log()
    log(s"Homeland Security: Add to reserves")
    val opsAdded = cardOps min (2 - game.reserves.us)
    addToReserves(US, opsAdded)
    opsAdded
  }
  
  // Disrupt a Muslim country to remvoe a Cadre.
  // Priorities: Closest to the US.
  // cardsOps   - The number of unused Ops remaining from the card
  // Returns the number of ops used
  def hsDisruptMuslimCadre(cardOps: Int): Int = {
    val priorities = List(ClosestToUSPriority)              
    val candidates = game.disruptMuslimTargets(cardOps) map game.getMuslim
    val target  = topPriority(candidates, priorities).get
    val opsUsed = target.governance
    log()
    log(s"Homeland Security: Disrupt to remove Cadre in ${target.name}")
    performDisrupt(target.name)
    opsUsed
  }
    
  def hsWoiNonMuslimOpposite(cardOps: Int): Int = {
    val priorities = List(PoorPriority, FairPriority, GoodPriority)
    val candidates = game.warOfIdeasNonMuslimTargets(cardOps) map game.getNonMuslim filter (_.isOppositeUsPosture)
    val target  = topPriority(candidates, priorities).get
    val opsUsed = target.governance
    log()
    log(s"Homeland Security: War of Ideas in ${target.name}")
    performWarOfIdeas(target.name, opsUsed)
    opsUsed
  }
    
  def hsWoiNonMuslimUntested(cardOps: Int): Int = {
    val priorities = List(PoorPriority, FairPriority, GoodPriority)
    val candidates = game.warOfIdeasNonMuslimTargets(cardOps) map game.getNonMuslim filter (_.isUntested)
    val target  = topPriority(candidates, priorities).get
    val opsUsed = target.governance
    log()
    log(s"Homeland Security: War of Ideas in ${target.name}")
    performWarOfIdeas(target.name, opsUsed)
    opsUsed
  }  
}