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
import awakening.USBot

// Card Text:
// ------------------------------------------------------------------
// Remove up to 2 Cells in any one Muslim country OR Randomly discard
// 1 card from the Tihadist hand.
// SPECIAL: If this card is randomly drawn by another event, this event
// is immediately implemented. Do not draw a replacement card.
// ------------------------------------------------------------------
object Card_242 extends Card2(242, "Avenger", US, 1, NoRemove, NoLapsing, NoAutoTrigger) {
  // Used by the US Bot to determine if the executing the event would alert a plot
  // in the given country
  override
  def eventAlertsPlot(countryName: String, plot: Plot): Boolean = false

  // Used by the US Bot to determine if the executing the event would remove
  // the last cell on the map resulting in victory.
  override
  def eventRemovesLastCell(): Boolean =
    game.muslims.exists(m => USBot.wouldRemoveLastCell(m.name, 2))


  // Returns true if the printed conditions of the event are satisfied
  override
  def eventConditionsMet(role: Role) = true

  def getCandidates() = countryNames(game.muslims.filter(_.totalCells > 0))

  def getBotPreferred() = countryNames(game.muslims.filter(m => m.totalCells - m.totalTroopsAndMilitia > 4))

  def jihadistHasCardsInHand = cacheYesOrNo(s"Does the $Jihadist player have any cards in hand? (y/n) ")
  // Returns true if the Bot associated with the given role will execute the event
  // on its turn.  This implements the special Bot instructions for the event.
  // When the event is triggered as part of the Human players turn, this is NOT used.
  //
  // If the last two cells on the map are in a Muslim country the the Bot will go for the win.
  // Otherwise, the bot will only remove cells if there is a country where cells outnumber
  // troops and miliia by 5 or more, or if the Jihadist has no cards in hand.
  override
  def botWillPlayEvent(role: Role): Boolean =
    game.hasMuslim(_.totalCells > 0) || jihadistHasCardsInHand


  // Carry out the event for the given role.
  // forTrigger will be true if the event was triggered during the human player's turn
  // and it associated with the Bot player.
  override
  def executeEvent(role: Role, forTrigger: Boolean): Unit =
    if (isHuman(role)) {
      val choices = List(
        "remove" -> "Remove up to 2 cells in any one Muslim country",
        "discard" -> s"Discard top card of the $Jihadist hand",
      )
      askMenu("Choose one:", choices).head match {
        case "discard" =>
          println()
          log(s"Discard top card of the $Jihadist hand")
          askCardsDiscarded(1)

        case _ if getCandidates().isEmpty =>
          log("\nThere are no Muslim countries with cells.  The event has no effect.", Color.Event)

        case _ =>
          val target = askCountry("Remove cells from which country: ", getCandidates())
          val num = game.getMuslim(target).totalCells min 2
          val (actives, sleepers, sadr) = askCells(target, num, true)
          addEventTarget(target)
          removeCellsFromCountry(target, actives, sleepers, sadr, addCadre = true)
      }
    }
    else {
      // Bot
      // If no preferred candidates, the Bot will choose to have Jihadist discard
      // if it has cards in hand.  Otherwise it will remove from non-preferred country.
      val target = getBotPreferred() match {
        case Nil if jihadistHasCardsInHand => None
        case Nil => USBot.disruptPriority(getCandidates())
        case preferred => USBot.disruptPriority(preferred)
      }

      target match {
        case None if jihadistHasCardsInHand =>
          log(s"\nYou ($Jihadist) must discard one card randomly.", Color.Event)
          askCardsDiscarded(1)

        case None =>
          // Event was triggered during Jihadist turn and there are no cells in muslim countries.
          log("\nThere are no Muslim countries with cells.  The event has no effect.", Color.Event)

        case Some(name) =>
          val (actives, sleepers, sadr) = USBot.chooseCellsToRemove(name, 2)
          addEventTarget(name)
          removeCellsFromCountry(name, actives, sleepers, sadr, addCadre = true)
      }
    }
}
