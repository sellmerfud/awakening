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
// Play if both sides have at least 1 card remaining.
// Draw a random card from your opponent's hand. Then, you may:
// (1) interrupt this Action Phase to play it as an event,
// (2) discard it,
// (3) return it, or
// (4) keep it by giving your opponent another card from your hand.
// ------------------------------------------------------------------
object Card_351 extends Card(351, "Advanced Persistent Threat (APT)", Unassociated, 3, NoRemove, NoLapsing, NoAutoTrigger) {
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
    hasCardInHand(US) && hasCardInHand(Jihadist)

  // Returns true if the Bot associated with the given role will execute the event
  // on its turn.  This implements the special Bot instructions for the event.
  // When the event is triggered as part of the Human players turn, this is NOT used.
  override
  def botWillPlayEvent(role: Role): Boolean = true

  // Carry out the event for the given role.
  // forTrigger will be true if the event was triggered during the human player's turn
  // and it associated with the Bot player.
  override
  def executeEvent(role: Role): Unit = {
    val opponent = role.opponent
    if (isHuman(role))
      log(s"\n$role takes the top card of the $opponent Bot's hand", Color.Event)
    else
      log(s"\n$role Bot take a random card from your ($opponent) hand", Color.Event)

    // This will return None if Avenger was drawn, triggers and discarded
    askCardDrawnFromOpponent(role, Some(s"What is the # of the card taken: "))
      .foreach { cardNum =>
        sealed trait Choice
        case object Event extends Choice
        case object Discard extends Choice
        case object Return extends Choice
        case object Keep extends Choice
        val card = deck(cardNum)
        val cardDisplay = card.numAndName
        val eventName = s""""${card.cardName}""""
        // Clear this in case it is need by the event that was drawn.
        cachedEventPlayableAnswer = None
        val action = if (isHuman(role)) {
          val choices = List(
            choice(true,              Event,  s"Play the $cardDisplay event"),
            choice(!card.autoTrigger, Discard,s"Discard $cardDisplay"),
            choice(true,              Return, s"Return $cardDisplay to the $opponent hand"),
            choice(true,              Keep,   s"Keep $cardDisplay and give another card to the $opponent")
          ).flatten
          askMenu("Choose one:", choices).head
        }
        else if (card.eventIsPlayable(role) && (!game.botEnhancements || card.botWillPlayEvent(role)))
          Event  // Bot will play event if possible
        else
          Discard // Otherwise Bot will discard (which will trigger any auto-event)

        action match {
          case Discard =>
            processDiscardedCard(role, cardNum)

          case Return =>
            log(s"\nReturn $cardDisplay to the top of the $opponent hand", Color.Event)
            decreaseCardsInHand(role, 1)
            increaseCardsInHand(role.opponent, 1)

          case Keep =>
            if (opponent == Jihadist && game.botEnhancements)
              log(s"\nKeep $cardDisplay and shuffle another card from your hand into the $opponent hand", Color.Event)
            else
              log(s"\nKeep $cardDisplay and place another card from your hand on the $opponent hand", Color.Event)
            askCardDrawnFromOpponent(role.opponent, Some(s"What is the # of the card given: "), except = Set(cardNum))

          case Event =>
            val eventRole = if (card.association == role ||  card.association == Unassociated)
              role
            else
              role.opponent

            addAdditionalCardToPlayedCard(card.number)
            decreaseCardsInHand(role, 1)
            log(s"\n$role executes the $cardDisplay event")
            log(separator())
            if (card.eventConditionsMet(eventRole)) {
              card.executeEvent(eventRole)
              if (card.markLapsingAfterExecutingEvent(role))
                putCardInLapsingBox(card.number)
              else if (card.removeAfterExecutingEvent(role))
                removeCardFromGame(card.number)
              else
                addCardToDiscardPile(card.number)
              }
              else {
                log(s"The event conditions are not satisfied.  The event has no effect.", Color.Event)                
                addCardToDiscardPile(card.number)
            }
        }
      }
  }
}
