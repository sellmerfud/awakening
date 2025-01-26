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
// Play if Jihadist currently has more than 1 card.
// US randomly draws two cards from the Jihadist hand, reveals them,
// then discards one and returns the other.
// ------------------------------------------------------------------
object Card_267 extends Card(267, "Third Offset Strategy", US, 2, NoRemove, NoLapsing, NoAutoTrigger) {
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
  def eventConditionsMet(role: Role) = numCardsInHand(Jihadist) > 1

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
      log(s"\nTake the top two cards from the $Jihadist hand.", Color.Event)
      log(s"Keep one and return the other to the top of the $Jihadist hand.")
      // This will decrease the Jihadist hand size by 2
      val cards = askCardsDrawnFull(US, 2, List(FromRole(oppositeRole(role))))
      // In Avenger is drawn then  it will have been activated and automatically
      // placed in the discard pile leaveing us with only one card.
      sealed trait CardOption
      case class Discard(num: Int) extends CardOption
      case class Return(num: Int) extends CardOption
      val taken = cards.collect { case Right(num) => num }
      val actions = taken match {
        case num::Nil =>
          val choices = List(Discard(num) -> "Discard it", Return(num) -> s"Return it to $Jihadist hand")
          askMenu(s"\nWhat happens to ${cardNumAndName(num)}:", choices)

        case nums =>
          val choices = List(
            List(Discard(nums(0)), Return(nums(1))) -> s"${cardNumAndName(nums(0))}",
            List(Discard(nums(1)), Return(nums(0))) -> s"${cardNumAndName(nums(1))}",
          )
          askMenu("\nWhich card should be discarded:", choices).head
      }
      actions.foreach {
        case Discard(num) =>
          // We increase the Jihadist hand size first because processDiscardedCard()
          // will decrease it again.
          decreaseCardsInHand(US, 1)
          increaseCardsInHand(Jihadist, 1)
          processDiscardedCard(Jihadist, num)

        case Return(num) =>
          log(s"${cardNumAndName(num)} is returned to the $Jihadist hand.")
          decreaseCardsInHand(US, 1)
          increaseCardsInHand(Jihadist, 1)
      }
      
    }
    else {
      log(s"\nYou ($Jihadist) must randomly discard one card.", Color.Event)
      askCardsDiscarded(Jihadist, 1)
    }
  }
}
