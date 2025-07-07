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

// Card Text:
// ------------------------------------------------------------------
// Select, reveal, and draw a card other than Oil Price Spike from
// the discard pile or a box.
// Add +1 to the resources of each Oil Exporter country for the turn.
// ------------------------------------------------------------------
object Card_117 extends Card(117, "Oil Price Spike", Unassociated, 3, NoRemove, Lapsing, NoAutoTrigger) {
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
  def eventConditionsMet(role: Role) = true

  def candidateCards(role: Role): List[Card] =
    (game.cardsDiscarded ::: game.cardsLapsing() ::: game.firstPlotCard().toList)
      .map(deck.apply)
      .filter(_.association == role)

   abstract class EnhJihadCardEntry(val cardNums: Set[Int]) {
    def this(cardNum: Int) = this(Set(cardNum))
    def conditionsMet: Boolean
    def willTake: Boolean = conditionsMet && deck(cardNums.head).botWillPlayEvent(Jihadist)
  }

  val enhancedJihadistCandidateEntries = List(
    new EnhJihadCardEntry(102) {  // Former Soviet Uniont
      override def conditionsMet = game.getMuslim(CentralAsia).isGood
    }, 
    new EnhJihadCardEntry(107) {  // Kurdistan
      override def conditionsMet =
        game.getMuslim(Iraq).isGood ||
        game.getMuslim(Iraq).isFair ||
        game.getMuslim(Turkey).isGood
    }, 
    new EnhJihadCardEntry(Set(87,88,89)) {  // Martyrdom Operations
      override def conditionsMet = true
    }, 
    new EnhJihadCardEntry(Set(104, 105)) {  // Iran
      override def conditionsMet = true
    }, 
    new EnhJihadCardEntry(120) {  // US Election
      override def conditionsMet = game.worldPosture == game.usPosture
    }, 
    new EnhJihadCardEntry(112) {  // Bin Ladin
      override def conditionsMet = game.prestigeLevel != Low
    }, 
    new EnhJihadCardEntry(93) {  // Taliban
      override def conditionsMet = game.prestigeLevel != Low
    }, 
    new EnhJihadCardEntry(111) {  // Zawahiri
      override def conditionsMet = game.prestigeLevel != Low
    }, 
    new EnhJihadCardEntry(108) {  // Musharraf
      override def conditionsMet =
        game.getMuslim(Pakistan).isGood &&
        !game.getMuslim(Pakistan).hasMarker(BenazirBhutto)
    }, 
    new EnhJihadCardEntry(Set(84, 85)) {  // Leak
      override def conditionsMet =
        globalEventInPlay(Renditions) ||
        globalEventInPlay(Wiretapping) ||
        globalEventInPlay(EnhancedMeasures)
    }, 
    new EnhJihadCardEntry(90) {  // Quagmire
      override def conditionsMet =
        game.hasMuslim(_.inRegimeChange)
    }, 
    new EnhJihadCardEntry(76) {  // Abu Ghurayb
      override def conditionsMet =
        game.hasMuslim(_.inRegimeChange)
    }, 
    new EnhJihadCardEntry(86) {  // Lebanon War
      override def conditionsMet = true
    }, 
    new EnhJihadCardEntry(63) {  // Gaza War
      override def conditionsMet = true
    }, 
    new EnhJihadCardEntry(92) {  // Saddam
      override def conditionsMet =
        game.fundingLevel != Ample &&
        game.getMuslim(Iraq).isPoor &&
        game.getMuslim(Iraq).isAdversary &&
        globalEventNotInPlay(SaddamCaptured)
    }, 
    new EnhJihadCardEntry(95) {  // Wahhabism
      override def conditionsMet =
        game.fundingLevel != Ample &&
        !game.getMuslim(SaudiArabia).isGood
    }, 
    new EnhJihadCardEntry(72) {  // Opium
      override def conditionsMet =
        game.getMuslim(Afghanistan).isIslamistRule &&
        game.cellsAvailable >= 3
    }, 
  )

  def enhancedJihadistBotEntries(): List[Int] = {
    val available = game.cardsDiscarded.toSet ++ game.cardsLapsing().toSet ++ game.firstPlotCard().toSet

    enhancedJihadistCandidateEntries
      .flatMap { entry =>
        val cardNums = available.intersect(entry.cardNums)
        if (cardNums.nonEmpty && entry.willTake)
          Some(cardNums.head)
        else
          None
      }
  }


  def botCardDraw(role: Role): Unit = {
    val enhCandidates = enhancedJihadistBotEntries()
    val cardNum = if (role == Jihadist && game.botEnhancements && enhCandidates.nonEmpty)
      Some(enhCandidates.head)
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
    game.islamistResources + irExporters >= 6 && (game.islamistAdjacency || isBot(Jihadist))
  }

  override
  def eventWouldResultInVictoryFor(role: Role): Boolean = role match {
    case Jihadist => wouldCauseJihadistWin
    case US => wouldCauseUSWin
  }

  // Returns true if the Bot associated with the given role will execute the event
  // on its turn.  This implements the special Bot instructions for the event.
  // When the event is triggered as part of the Human players turn, this is NOT used.
  override
  def botWillPlayEvent(role: Role): Boolean = role match {
    case US =>
      // Will not play if it would cause immediate victory for the Jihadist player
      wouldCauseUSWin ||
      (!wouldCauseJihadistWin && candidateCards(US).nonEmpty)

    case Jihadist if game.botEnhancements =>
      // Will not play if it would cause immediate victory for the US player
      // or there are 2 or more countries countries with printed resource value
      // of 3 that are at Good governance.
      val Good3ResExporters = game.muslims.count(m => m.isGood && m.oilExporter && m.printedResources == 3)
      wouldCauseJihadistWin ||
      (!wouldCauseUSWin && (Good3ResExporters < 2) && enhancedJihadistBotEntries().nonEmpty)

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
