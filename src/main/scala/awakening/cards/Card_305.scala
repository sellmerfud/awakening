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
// Play if Trump Tweets ON.
// Test Caucasus. Add the GWOT modifier to the US Prestige modifier and
// if the result is negative, the US player must discard a number of
// event cards whose combined Operations value is equal to or exceeds
// the result. Any Jihadist event(s) discarded this way are triggered.
// ------------------------------------------------------------------
object Card_305 extends Card(305, "Presidential Whistleblower", Jihadist, 2, NoRemove, NoLapsing, NoAutoTrigger) {
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
  def eventConditionsMet(role: Role) = trumpTweetsON

  // Returns true if the Bot associated with the given role will execute the event
  // on its turn.  This implements the special Bot instructions for the event.
  // When the event is triggered as part of the Human players turn, this is NOT used.
  override
  def botWillPlayEvent(role: Role): Boolean =
    // Enhanced Jihad Bot will not play unless it will force
    // the US player to discard at least one card
    (!game.botEnhancements && game.getNonMuslim(Caucasus).isUntested && game.usPosture == Hard) ||
    (game.prestigeModifier - game.gwotPenalty < 0 && cacheYesOrNo("Does the US player have at least one card in hand? (y/n) "))

  // Carry out the event for the given role.
  // forTrigger will be true if the event was triggered during the human player's turn
  // and it associated with the Bot player.
  override
  def executeEvent(role: Role): Unit = {
    testCountry(Caucasus) // Event specifically says to test
    val result = game.prestigeModifier - game.gwotPenalty
    val opPoints = -result

    log(s"\nGWOT penalty (-${game.gwotPenalty}) + prestige modifier (${game.prestigeModifier}) = $result", Color.Event)
    log(separator(), Color.Event)
    if (result >= 0)
      log("There is no effect.", Color.Event)
    else {
      println()
      if (isHuman(role)) {
        log(s"Discard from the top of the $US Bot's hand until the combined")
        log(s"Operational value is at least $opPoints")
      }
      else {
        log(s"You ($US) must discard cards until the combined Operational value")
        log(s"is at least $opPoints")
      }

      def nextDiscard(num: Int, pointsDiscarded: Int): List[Int] = {
        if (pointsDiscarded >= opPoints)
          Nil
        else {
          val prompt = s"\nCard # of the ${ordinal(num)} discard (or blank if none) "
          askCardNumber(prompt) match {
            case None         => Nil
            case Some(cardNo) =>  cardNo :: nextDiscard(num + 1, pointsDiscarded + deck(cardNo).printedOps)
          }
        }
      }

      for (n <- nextDiscard(1, 0); card = deck(n)) {
        if (n == AvengerCard)
            avengerCardDrawn(discarded = false)
        else if (card.autoTrigger)
          autoTriggerCardDiscarded(n)
        else if (isBot(Jihadist) && card.eventWillTrigger(Jihadist)) {
          log()
          log(s"""The "${card.numAndName}" event is triggered.""", Color.Event)
          performCardEvent(card, Jihadist, triggered = true)
        }
        else{
          log()
          log(s"""The "${card.number}" event does not trigger.""", Color.Event)
          if (n == CriticalMiddle)
            criticalMiddleReminder()
        }
      }
    }
  }
}
