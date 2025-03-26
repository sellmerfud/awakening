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

// Card Text:
// ------------------------------------------------------------------
// Search through discard pile and Select, reveal and draw any one
// event card that causes Civil War or allows Regime Change
// (specific cards listed in 11.3.13).
// ------------------------------------------------------------------
object Card_217 extends Card(217, "Agitators", Unassociated, 2, NoRemove, NoLapsing, NoAutoTrigger) {

  val ValidCards = List(37, 39, 165, 188, 234, 133, 226, 238, 167, 152, 272, 277, 293)

  def cardCandidates = game.cardsDiscarded.filter(ValidCards.contains)

  // Priority from top to bottom:
  // #234 Free Syrian Army (if Syria [Good])
  // #188 ISIL
  // #238 Revolution
  // #152 Congress Acts
  // #167 Houthi Rebels (if 2+ cells on track,Yemen [Poor and Reaction-Awakening<1] and Iran not Ally)
  def enhJihadistBotCandidates = {
    List(234, 188, 238, 152, 167)
      .filter(game.cardDiscarded)
      .filter {
        case 234 =>
          game.getMuslim(Syria).isGood
        case 167 =>
          val yemen = game.getMuslim(Yemen)
          game.cellsAvailable > 1 &&
          yemen.isPoor &&
          yemen.reaction - yemen.awakening < 1 &&
          (game.isNonMuslim(Iran) || !game.getMuslim(Iran).isAlly)
        case _ =>
          true
      }
  }

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
  def eventConditionsMet(role: Role) = cardCandidates.nonEmpty

  // Returns true if the Bot associated with the given role will execute the event
  // on its turn.  This implements the special Bot instructions for the event.
  // When the event is triggered as part of the Human players turn, this is NOT used.
  override
  def botWillPlayEvent(role: Role): Boolean = role match {
    case Jihadist if game.botEnhancements => enhJihadistBotCandidates.nonEmpty
    case _ => true
  }

  // Carry out the event for the given role.
  // forTrigger will be true if the event was triggered during the human player's turn
  // and it associated with the Bot player.
  override
  def executeEvent(role: Role): Unit = {
    // See Event Instructions table
    log(s"\nThe $role player draws from the discard pile a card that either", Color.Event)
    log(s"causes Civil War or allows a Regime change.", Color.Event)
    if (isHuman(role)) {
      askCardDrawnFromDiscardPile(role, only = cardCandidates.toSet)
        .map(deck(_).numAndName)
        .foreach { cardDisplay =>
          log(s"\nAdd $cardDisplay to your hand.", Color.Event)
        }
    }
    else {
      // Enhanced Bot will take first availabe card in its priority list
      // Other Bots take candidate card nearest the bottom of the discard pile
      val cardNum = if (role == Jihadist && game.botEnhancements)
        enhJihadistBotCandidates.head
      else {
        val candidates = cardCandidates.toSet
        game.cardsDiscarded.reverse.find(candidates.contains).get
      }
      
      processCardDrawn(role, cardNum, FromDiscard)
      log(s"\nTake ${cardNumAndName(cardNum)} from the discard pile and", Color.Event)
      if (role == Jihadist && game.botEnhancements)
        log(s"shuffle it into the $role hand.", Color.Event)
      else
        log(s"place it on top of the $role hand.", Color.Event)
    }
  }
}
