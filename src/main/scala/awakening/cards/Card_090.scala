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
// Play if Prestige Medium or Low and a Regime Change country has a cell.
// US randomly discards 2 cards.
// Playable jihadist events on them happen.
// US Posture to Soft.
// ------------------------------------------------------------------
object Card_090 extends Card(90, "Quagmire", Jihadist, 3, NoRemove, NoLapsing, NoAutoTrigger) {
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
    (game.prestigeLevel == Medium || game.prestigeLevel == Low) &&
    game.hasMuslim (m => m.inRegimeChange && m.totalCells > 0)

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
    if (isHuman(role)) {
      log(s"\nDiscard the top two cards of the $Jihadist Bot's hand.", Color.Event)
      log(s"$Jihadist associated events will not be triggered.", Color.Event)
      askCardsDiscarded(2)
    }
    else {
      log(s"\nYou ($US) must randomly discard two cards", Color.Event)
      log("Playable Jihadist events on the discards are triggered", Color.Event)

      def nextDiscard(num: Int): Unit = {
        if (num <= 2) {
          val prompt = s"\nCard # of the ${ordinal(num)} discard (or blank if none) "
          askCardNumber(prompt) match {
            case Some(cardNum) =>
              val card = deck(cardNum)
              log(s"$card is discarded", Color.Event)
              if (cardNum == AvengerCard)
                  avengerCardDrawn(discarded = false)
              else if (card.autoTrigger)
                autoTriggerCardDiscarded(cardNum)
              else if (card.eventWillTrigger(Jihadist)) {
                log(s"""The "${card.name}" event is triggered.""")
                performCardEvent(card, Jihadist, triggered = true)
              }
              else {
                log(s"""The "${card.name}" event does not trigger.""")
                if (cardNum == CriticalMiddle)
                  criticalMiddleReminder()
              }
              nextDiscard(num + 1)

            case None => // exit
          }
        }
      }

      nextDiscard(1)
    }

    if (game.usPosture != Soft) {
      log(s"\nThe Quagmire event affects the $US posture.", Color.Event)
      setUSPosture(Soft)
    }
  }
}
