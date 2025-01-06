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
// Play if 1st card of jihadist Action Phase. Use this and 2nd card to recruit. Ignore Funding.
// You may recruit in countries marked worse than Fair even if no cell or cadre.
// ------------------------------------------------------------------
object Card_053 extends Card(53, "Madrassas", Jihadist, 1, NoRemove, NoLapsing, NoAutoTrigger) {
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
  def eventConditionsMet(role: Role) =
    firstCardOfPhase(Jihadist) &&
    hasCardInHand(Jihadist)

  // Returns true if the Bot associated with the given role will execute the event
  // on its turn.  This implements the special Bot instructions for the event.
  // When the event is triggered as part of the Human players turn, this is NOT used.
  //
  // NOTE: This event ignores funding, so all available cells may be used
  //       to recruit
  override
  def botWillPlayEvent(role: Role): Boolean =
    game.botEnhancements == false &&  // Unplayable for Enhanced Bot
    game.cellsAvailable > 0

  // Carry out the event for the given role.
  // forTrigger will be true if the event was triggered during the human player's turn
  // and it associated with the Bot player.
  override
  def executeEvent(role: Role): Unit = {
    val  prompt = if (isHuman(role))
      "\nEnter card # of card you wish to use for recruiting: "
    else
      s"\nEnter card # of the next card in the $Jihadist Bot's hand: "
    
    val card = deck(askCardNumber(prompt, allowNone = false).get)
    decreaseCardsInHand(Jihadist, 1)
    addSecondCardToPlayedCard(card.number)
    logCardPlay(Jihadist, card, playable = false, secondCard = true)
    
    // Add Ops on the Madrassas card.
    val totalOps = card.ops + this.ops
    
    if (isHuman(role))
      humanRecruit(totalOps, ignoreFunding = true, madrassas = true)
    else {
      log()
      log(s"\n$Jihadist performs a Recruit operation ${opsString(totalOps)}")
      log(separator())
      JihadistBot.performRecruit(totalOps, None, ignoreFunding = true, madrassas = true)
    }
  }
}
