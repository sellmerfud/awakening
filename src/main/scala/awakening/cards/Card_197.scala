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
// Search through the REMOVED cards and Select, reveal and draw 1 of
// Ayman al-Zawabiri, Abu Bakr al-Baghdadi, Abu Sayyaf (ISIL),
// Jihadi John, or Osama bin Ladin into hand.
// -1 Prestige.
// REMOVE
// ------------------------------------------------------------------
object Card_197 extends Card2(197, "Unconfirmed", Jihadist, 3, Remove, NoLapsing, NoAutoTrigger) {
  val UnconfirmedCandidates = List(215, 216, 219, 225, 237)

  def getCandidates() = game.cardsRemoved.filter(UnconfirmedCandidates.contains)
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
  def eventConditionsMet(role: Role) = true

  // Returns true if the Bot associated with the given role will execute the event
  // on its turn.  This implements the special Bot instructions for the event.
  // When the event is triggered as part of the Human players turn, this is NOT used.
  override
  def botWillPlayEvent(role: Role): Boolean = getCandidates().nonEmpty

  // Carry out the event for the given role.
  // forTrigger will be true if the event was triggered during the human player's turn
  // and it associated with the Bot player.
  override
  def executeEvent(role: Role, forTrigger: Boolean): Unit = {
    // See Event Instructions table
    if (getCandidates().isEmpty && game.prestige == 1) {
      log("\nNone of the listed cards is in the removed cards pile and US prestige is 1.", Color.Event)
      log("The event has no effect.", Color.Event)
    }
    else {
      if (getCandidates().nonEmpty) {
        val choices = getCandidates().map(n => n -> deck(n).numAndName)
        val prompt = if (isHuman(role))
          "\nWhich card did you select from the removed cards pile:"
        else
          s"\nThe $Jihadist Bot draws the card nearest the top of the removed cards pile:"

        val cardNum = askMenu(prompt, choices).head
        game = game.copy(cardsRemoved = game.cardsRemoved filterNot (_ == cardNum))
        log(s"\nThe $Jihadist draws the ${deck(cardNum).numAndName} from the removed cards pile.", Color.Event)
      }
      else
        log("\nNone of the listed cards is in the removed cards pile.", Color.Event)

      decreasePrestige(1)
    }
  }
}
