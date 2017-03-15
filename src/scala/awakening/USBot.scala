
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

  // Priorities Table

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
    val desc = "Disrupt Muslim at least 2 more cells thatn TandM and JSP?"
    def yesPath = Disrupt
    def noPath  = IR3AndRegimeChangeDecision
    def condition(ops: Int) = game.disruptMuslimTargets(ops) map game.getMuslim exists { m => 
      m.totalCells - m.totalTroopsAndMilitia >= 2 &&
      jihadSuccessPossible(m, false) 
    }
  }
  
  object IR3AndRegimeChangeDecision extends OperationDecision {
    val desc = "Islamist Rule resources > 3 and Regime Change possible?"
    def yesPath = RegimeChange
    def noPath  = WoiMuslimNoPenaltyDecision
    def condition(ops: Int) = game.islamistResources > 3 && game.regimeChangePossible(ops)
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
    def condition(ops: Int) = false
      // TODO:
      // Check OpP Flowchart to deploy from/to targets
      // They cannot be equal (track == track)
      // If a regime change country then Withdraw conditions must be met.
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
      m.disruptAffectsPrestige ||
      (m.sleeperCells == 0 && m.activeCells == 1)
    }
  }
  
  object WoiMuslimMinus1DrmDecision extends OperationDecision {
    val desc = "WoI in Muslim with DRM of -1?"
    def yesPath = WoiMuslimMinusOneDRM
    def noPath  = HomelandSecurity
    def condition(ops: Int) = 
      woiMuslimTargets(ops) exists (m => modifyWoiRoll(0, m, silent = true) == -1)
  }
  
  
  

  
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
    
        // val opsUsed = operationsFlowchart(maxOpsPlusReserves(card)) match {
        //   case RecruitOp    => recruitOperation(card)
        //   case TravelOp     => travelOperation(card)
        //   case PlotOp       => plotOperation(card)
        //   case MinorJihadOp => minorJihadOperation(card)
        //   case MajorJihadOp => majorJihadOperation(card)
        // }
        // homelandSecurity(card, opsUsed)
        
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