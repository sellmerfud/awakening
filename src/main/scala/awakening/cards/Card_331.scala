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
// Play if World Hard and US Soft.
// Shift Alignment one box towards Adversary for one of these
// countries: Libya, Saudi Arabia, Pakistan,or any Muslim country with
// more than one Cell.
// Discard the following cards from both the US and Jihadist hands:
// Lone Wolf, JV/Copy Cat, Paris Attacks, False Flag Attacks,
// Vehicle-ramming Attacks, Barcelona Bombs.
// REMOVE
// ------------------------------------------------------------------
object Card_331 extends Card(331, "JASTA", Unassociated, 1, Remove, NoLapsing, NoAutoTrigger) {
  val NamedTargets = Set(Libya, SaudiArabia, Pakistan)

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
  def eventConditionsMet(role: Role) = game.worldPosture == Hard && game.usPosture == Soft

  val isShiftCandidate = (m: MuslimCountry) =>
    !m.truce &&
    m.alignment != Adversary &&
    (NamedTargets(m.name) || m.totalCells > 1)

  def shiftCandidates() = countryNames(game.muslims.filter(isShiftCandidate))

  val isEnhJihadBotCandidate = (m: MuslimCountry) =>
    isShiftCandidate(m) &&
    JihadistBot.enhBotResourceValue(m) >= 3

  def enhJihadBotCandidates = countryNames(game.muslims.filter(isEnhJihadBotCandidate))

  // Returns true if the Bot associated with the given role will execute the event
  // on its turn.  This implements the special Bot instructions for the event.
  // When the event is triggered as part of the Human players turn, this is NOT used.
  override
  def botWillPlayEvent(role: Role): Boolean = role match {
    case US => false  // Unplayable by the stanbdard Bots
    case Jihadist if game.botEnhancements => enhJihadBotCandidates.nonEmpty
    case Jihadist =>  false // Unplayable by the stanbdard Bots
  }

  // Carry out the event for the given role.
  // forTrigger will be true if the event was triggered during the human player's turn
  // and it associated with the Bot player.
  override
  def executeEvent(role: Role): Unit = {
    val EventCards = Set(207, 283, 182, 291, 294, 298)
    def targetCards = EventCards
      .filter(cardNum => cardFoundIn(List(FromDrawPile), cardNum))

    // The standard Bots do not play this event and if a Human player plays it
    // when playing against a standard Bot, the Bot's t hand is not inspected.
    // When playing against the enhanced Jihadist Bot, the bot's hand is inspected
    // and then reshuffled.

    val candidates = shiftCandidates()
    if (candidates.isEmpty)
      log(s"\nNone of the candidate countries can be shifted towards Adversary.", Color.Info)
    else {
      val target = if (isHuman(role))
        askCountry("Select country to shift toward Adversary: ", candidates)
      else
        JihadistBot.alignGovTarget(enhJihadBotCandidates).get
      shiftAlignmentRight(target)
    }
    
    if (targetCards.isEmpty)
      log(s"\nNone of the cards targeted by the event are in the draw pile.", Color.Info)
    else {
      // This event is only playable by the Human player, and the Bot's hand is not
      // inspected, so the Bot is never forced to discard cards.  The Human player
      // must still discard any of the target cards.

      if (hasCardInHand(game.humanRole)) {
        displayLine(s"\nYou (${game.humanRole}) must discard from your hand any of the cards mentioned in the event.", Color.Info)
        askCardsDiscarded(game.humanRole, targetCards.size, only = targetCards, lessOk = true)
      }
      else
        log(s"\nThe ${game.humanRole} does not have a card to discard.", Color.Event)
  
      if (game.botRole == Jihadist && game.botEnhancements && hasCardInHand(game.botRole) && targetCards.nonEmpty) {
        displayLine(s"\nInspect the ${game.botRole} Bot's hand and discard any of the cards mentioned in the event.", Color.Info)
        askCardsDiscarded(game.botRole, targetCards.size, only = targetCards, lessOk = true)
        displayLine(s"\nShuffle the ${game.botRole} Bot's hand.", Color.Info)

      }
      else {
        log(s"\nIn the solitaire game you may not inspect the ${game.botRole} Bot's hand", Color.Info)
        log(s"so the ${game.botRole} Bot does not discard andy cards.", Color.Info)
      }
    }
  }
}
