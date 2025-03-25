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
import awakening.JihadistBot

// Card Text:
// ------------------------------------------------------------------
// Select, reveal, and draw a card other than Oil Price Spike from the
// discard pile or a box. Add +1 to the Resources of each Oil Export
// country for the turn.
// Cancel effects of Fracking.
// ------------------------------------------------------------------
object Card_236 extends Card(236, "Oil Price Spike", Unassociated, 3, NoRemove, Lapsing, NoAutoTrigger) {
  // Used by the US Bot to determine if the executing the event would alert a plot
  // in the given country
  override
  def eventAlertsPlot(countryName: String, plot: Plot): Boolean = false

  // Used by the US Bot to determine if the executing the event would remove
  // the last cell on the map resulting in victory.
  override
  def eventRemovesLastCell(): Boolean = false

  def candidateCards(role: Role): List[Card] =
    (game.cardsDiscarded ::: game.cardsLapsing() ::: game.firstPlotCard().toList)
      .map(deck.apply)
      .filter(_.association == role)

  abstract class EnhJihadCardEntry(val cardNums: Set[Int]) {
    def this(cardNum: Int) = this(Set(cardNum))
    def conditionsMet: Boolean
    def willTake: Boolean = conditionsMet && deck(cardNums.head).botWillPlayEvent(Jihadist)
  }

  // Returns true if the printed conditions of the event are satisfied
  override
  def eventConditionsMet(role: Role) = true

  def enhJihadistBotCardCandidates = {
    def cardEntry(cardNum: Int, condition: Boolean): Option[Int] =
      if (condition) Some(cardNum) else None

    val usPostureTest = game.gwot match {
      case (worldPosture, value) => worldPosture == game.usPosture && value > 0
    }
    List(
      cardEntry(220, game.getMuslim(Syria).isGood), // Daraa (if Syria [Good])
      cardEntry(234, game.getMuslim(Syria).isGood), // Free Syrian Army (if Syria [Good])
      cardEntry(191, true), // Muslim Brotherhood
      cardEntry(176, true), // Change of State
      cardEntry(215, true), // Abu Bakr al-Baghdadi
      cardEntry(187, game.hasMuslim(_.inRegimeChange) && !game.caliphateDeclared), // Foreign Fighters (if any RC on board and no Caliphate marker on board)
      cardEntry(192, game.hasMuslim(_.inRegimeChange)), // Quagmire (if any RC on board)
      cardEntry(196, game.trainingCamp == None), // Training Camps (if Training Camps marker not on the board)
      cardEntry(175, true), // Censorship
      cardEntry(174, usPostureTest), // Boston Marathon (if World Posture same as US posture; not if World=0)
      cardEntry(240, usPostureTest), // US Election (if World Posture same as US posture; not if World=0)
      cardEntry(199, true), // US Consulate Attacked
      cardEntry(194, true), // Snowden
      cardEntry(171, true), // Abu Ghraib Jail Break
      cardEntry(170, true), // Theft of State
      cardEntry(221, true), // Flypaper
      cardEntry(190, true), // Martyrdom Operation
      cardEntry(173, true), // Arab Winter
      cardEntry(188, true)) // ISIL
      .flatten
      .filter(game.cardDiscarded)
  }

  def botCardDraw(role: Role): Unit = {  
    val cardNum = if (role == Jihadist && game.botEnhancements) {
      val enhCandidates = enhJihadistBotCardCandidates
      if (enhCandidates.nonEmpty)
        Some(enhCandidates.head)
      else {
        // draw any Jihadist event (priority to highest ops),
        // then any Neutral event (priority to highest ops),
        // then any US event (priority to highest ops)
        type CardFilter = JihadistBot.Filter[Card]
        val assocPriorities = List(
          new JihadistBot.Filter[Card] {
            val desc = "Jihadist Associated"
            def filter(cards: List[Card]): List[Card] = cards.filter(_.association == Jihadist)
          },
          new JihadistBot.Filter[Card] {
            val desc = "Unassociated"
            def filter(cards: List[Card]): List[Card] = cards.filter(_.association == Unassociated)
          },
          new JihadistBot.Filter[Card] {
            val desc = "US Associated"
            def filter(cards: List[Card]): List[Card] = cards.filter(_.association == US)
          },
        )
        val opsPriorities = List(
          new JihadistBot.Filter[Card] {
            val desc = "Highest Ops"
            def filter(cards: List[Card]): List[Card] = {
              val high = cards.map(_.printedOps).max
              cards.filter(card => card.printedOps == high)
            }
          },
        )

        val bestAssoc = JihadistBot.narrowCandidates(game.cardsDiscarded.map(deck.apply), assocPriorities)
        JihadistBot.topPriority(bestAssoc,  opsPriorities)
          .map(_.number)
      }
    }
    else if (candidateCards(role).nonEmpty) {
      val highOps = candidateCards(role).map(_.printedOps).max
      Some(shuffle(candidateCards(role).filter(_.printedOps == highOps).map(_.number)).head)
    }
    else
      None

    cardNum.foreach { cardNum =>
      val cardDisplay = deck(cardNum).numAndName
      log(s"\n$role Bot selects $cardDisplay", Color.Event)
      if (processCardDrawn(role, cardNum, cardLocation(cardNum).get)) {
        if (role == Jihadist && game.botEnhancements)
          log(s"\nShuffle $cardDisplay into the $role Bot's hand", Color.Info)
          else
          log(s"\nPlace $cardDisplay on top of the $role Bot's hand", Color.Info)
      }
    }
  }
  
  def wouldCauseUSWin = {
    val goodExporters = game.muslims.count(m => m.isGood && m.oilExporter)
    game.goodResources + goodExporters >= 12
  }

  def wouldCauseJihadistWin = {
    val irExporters = game.muslims.count(m => m.isIslamistRule && m.oilExporter)
    game.islamistResources + irExporters >= 6
  }

  // Returns true if the Bot associated with the given role will execute the event
  // on its turn.  This implements the special Bot instructions for the event.
  // When the event is triggered as part of the Human players turn, this is NOT used.
  override
  def botWillPlayEvent(role: Role): Boolean = role match {
    case US =>
      // Will not play if it would cause immediate victory for the Jihadist player
      val IR_OilExporters = game.muslims.count(m => m.isIslamistRule && m.oilExporter)
      (game.islamistResources + IR_OilExporters < 6 || !game.islamistAdjacency) &&
      candidateCards(US).nonEmpty

    case Jihadist if game.botEnhancements =>
      // Will not play if it would cause immediate victory for the US player
      // or there are 2 or more countries countries with printed resource value
      // of 3 that are at Good governance.
      val GoodOilExporters = game.muslims.count(m => m.isGood && m.oilExporter)
      val Good3ResExporters = game.muslims.count(m => m.isGood && m.oilExporter && JihadistBot.enhBotResourceValue(m) == 3)
      (game.goodResources + GoodOilExporters < 12) &&
      (Good3ResExporters < 2) &&
      (enhJihadistBotCardCandidates.nonEmpty || globalEventInPlay(Fracking))

    case Jihadist =>
      // Will not play if it would cause immediate victory for the US player
      wouldCauseJihadistWin ||
      (!wouldCauseUSWin && candidateCards(Jihadist).nonEmpty)
  }

 
  // Carry out the event for the given role.
  // forTrigger will be true if the event was triggered during the human player's turn
  // and it associated with the Bot player.
  override
  def executeEvent(role: Role): Unit = {
    // See Event Instructions table
    removeGlobalEventMarker(Fracking) // Cancels effects of "Fracking" marker

    if (isHuman(role)) {
      if (game.cardsDiscarded.nonEmpty || game.cardsLapsing().nonEmpty || game.firstPlotCard().nonEmpty)
        askCardDrawnFromDiscardOrBox(role, prohibited = Set(117, 118, 236))
          .foreach { cardNum =>
            val cardDisplay = deck(cardNum).numAndName
            log(s"\n$role selects $cardDisplay", Color.Event)
            displayLine(s"\nAdd $cardDisplay to your ($role) hand", Color.Info)
          }
    }
    else
      botCardDraw(role)

    log("\nThe Resource value of each Oil Exporter is increased by 1 for the rest of the turn.", Color.Event)
  }
}
