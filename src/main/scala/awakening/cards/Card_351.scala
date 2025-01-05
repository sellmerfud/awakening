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
    cacheYesOrNo(s"Do both players have at least 1 card in hand? (y/n) ")

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
    val opponent = oppositeRole(role)
    if (isHuman(role))
      log(s"\nTake the top card of the $opponent hand", Color.Event)
    else
      log(s"\nTake a random card from your ($opponent) hand", Color.Event)

    val cardNum = askCardNumber(s"Card # of the card taken: ", allowNone = false).get
    val card = deck(cardNum)
    val cardDisplay = s""""${card.cardName}""""

    // Clear this in case it is need by the event that was drawn.
    cachedEventPlayableAnswer = None

    // Avenger card will trigger when randomly drawn.
    if (cardNum == AvengerCard) {
      log(s"\nThe $cardDisplay card was randomly drawn, so the event triggers", Color.Event)
      log(separator())
      decreaseCardsInHand(opponent, 1)
      card.executeEvent(US)
    }
    else {
      val action = if (isHuman(role)) {
        val choices = List(
          choice(card.eventIsPlayable(role), "event",  s"Play the $cardDisplay event"),
          choice(!card.autoTrigger,          "discard",s"Discard $cardDisplay"),
          choice(true,                       "return", s"Return $cardDisplay to the $opponent hand"),
          choice(true,                       "keep",   s"Keep $cardDisplay and give another card to the $opponent")
        ).flatten
        askMenu("Choose one:", choices).head
      }
      else if (card.eventIsPlayable(role))
        "event"  // Bot will play event if possible
      else
        "discard" // Otherwise Bot will discard (which will trigger any auto-event)

      action match {
        case "discard" =>
          log(s"\nPlace $cardDisplay in the discard pile", Color.Event)
          decreaseCardsInHand(opponent, 1)
          processDiscardedCard(cardNum)

        case "return"  =>
          log(s"\nReturn $cardDisplay to the top of the $opponent hand", Color.Event)

        case "keep"    =>
          log(s"\nKeep $cardDisplay and place another card from your hand the $opponent hand", Color.Event)
          askCardsDrawn(role, 1, FromOpponent)

        case _ => // Play the event
          decreaseCardsInHand(opponent, 1)
          addAdditionalCardToPlayedCard(card.number)
          log(s"\n$role executes the $cardDisplay event")
          log(separator())
          card.executeEvent(role)
          if (card.markLapsingAfterExecutingEvent(role))
            markCardAsLapsing(card.number)
          else if (card.removeAfterExecutingEvent(role))
            removeCardFromGame(card.number)
      }
    }
  }
}
