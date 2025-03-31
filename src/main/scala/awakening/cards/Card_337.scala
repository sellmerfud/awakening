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
import awakening.{ USBot, JihadistBot }

// Card Text:
// ------------------------------------------------------------------
// Play if Trump Tweets is ON.
// Flip Trump Tweets to OFF.
// If US play: Remove 1 Troop (from Track first) to Off Map Box until
//   end of next draw phase, then to Track.
//   Draw a card. LAPSING
// If Jihadist: US randomly discards 1 card.
// ------------------------------------------------------------------
object Card_337 extends Card(337, "US Border Crisis", Unassociated, 1, NoRemove, USLapsing, NoAutoTrigger) {
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
  def eventConditionsMet(role: Role) = role match {
    case US => trumpTweetsON && canPutTroopsInOffMapBox
    case Jihadist => trumpTweetsON && hasCardInHand(US)
  }
    

  // Returns true if the Bot associated with the given role will execute the event
  // on its turn.  This implements the special Bot instructions for the event.
  // When the event is triggered as part of the Human players turn, this is NOT used.
  override
  def botWillPlayEvent(role: Role): Boolean = true

  // Carry out the event for the given role.
  // forTrigger will be true if the event was triggered during the human player's turn
  // and it associated with the Bot player.
  override
  def executeEvent(role: Role): Unit = if (role == US) {
    setTrumpTweetsOFF()

    val withTroops = countryNames(game.countries.filter(c => !c.truce && c.troops > 0))
    val source = if (game.troopsAvailable > 0)
      "track"
    else if (isHuman(role))
      selectTroopsToPutOffMap(1).head.country
    else
      USBot.ebolaScareTarget(withTroops).get

    if (source != "track")
      addEventTarget(source)
    putTroopsInOffMapBox(source, 1)

    log(s"\nThe $role player draws a card.", Color.Event)
    askCardDrawnFromDrawPile(role)
      .map(deck(_))
      .foreach { cardDisplay =>
        if (isHuman(role))
          log(s"\nAdd $cardDisplay to your hand.", Color.Event)
        else
          log(s"\nPlace $cardDisplay on top of the $role Bot's hand.", Color.Event)
      }
  }
  else { // Jihadist
    setTrumpTweetsOFF()

    if (hasCardInHand(US)) {
      if (isHuman(US))
        log(s"\nYou ($US) must discard one random card.", Color.Event)
      else
        log(s"\nDiscard top card of the $US Bot's hand", Color.Event)
      askCardsDiscarded(US, 1)
    }
    else
      log(s"\nThe $US does not have a card to discard.", Color.Event)
  }
}
