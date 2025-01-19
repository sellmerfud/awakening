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

import scala.util.Random.shuffle
import awakening.LabyrinthAwakening._

// Card Text:
// ------------------------------------------------------------------
// Select, reveal, and draw a card other than Oil Price Spike from
// the discard pile or a box.
// Add +1 to the resources of each Oil Exporter country for the turn.
// ------------------------------------------------------------------
object Card_117 extends Card(117, "Oil Price Spike", Unassociated, 3, NoRemove, Lapsing, NoAutoTrigger) {
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

  def candidateCards(role: Role): List[Card] =
    (game.cardsDiscarded ::: game.cardsLapsing() ::: game.firstPlotCard().toList)
      .map(deck.apply)
      .filter(_.association == role)


  def botCardDraw(role: Role): Unit = {
    val highOps = candidateCards(role).map(_.printedOps).max
    val cardNum = shuffle(candidateCards(role).filter(_.printedOps == highOps).map(_.number)).head

    log(s"\n$role Bot selects ${deck(cardNum).numAndName}", Color.Event)
    processCardDrawn(role, cardNum, cardLocation(cardNum).get)
  }
  // Returns true if the Bot associated with the given role will execute the event
  // on its turn.  This implements the special Bot instructions for the event.
  // When the event is triggered as part of the Human players turn, this is NOT used.
  override
  def botWillPlayEvent(role: Role): Boolean = role match {
    case US =>
      // Will not play if it would cause immediate victory for the Jihadist player
      val IR_OilExporters = game.muslims.count(m => m.isIslamistRule && m.oilExporter)
      (game.islamistResources + IR_OilExporters < 6 || !game.islamistAdjacency) &&
      candidateCards(US).nonEmpty

    case Jihadist =>
      // Will not play if it would cause immediate victory for the US player
      val US_OilExporters = game.muslims.count(m => m.isGood && m.oilExporter)
      (game.goodResources + US_OilExporters < 12) &&
      candidateCards(Jihadist).nonEmpty
  }

  // Carry out the event for the given role.
  // forTrigger will be true if the event was triggered during the human player's turn
  // and it associated with the Bot player.
  override
  def executeEvent(role: Role): Unit = {
    if (isHuman(role))
        askCardDrawnFromDiscardOrBox(role, prohibited = Set(117, 118, 236))
    else
      botCardDraw(role)
    log("\nThe Resource value of each Oil Exporter is increased by 1 for the rest of the turn.", Color.Event)
  }
}
