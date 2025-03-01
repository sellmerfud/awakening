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
// Select, reveal, and draw a card other than Oil Price Spike from the
// discard pile or a box. Add +1 to the Resources of each Oil Export
// country for the turn.
//Cancel effects of Fracking.
// ------------------------------------------------------------------
object Card_236 extends Card(236, "Oil Price Spike", Unassociated, 3, NoRemove, Lapsing, NoAutoTrigger) {
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
  def botWillPlayEvent(role: Role): Boolean = role match {
    case US =>
      // Unplayable if it would cause Jihadist instant victory
      val jihadOilExporters = game.muslims.count(m => m.isIslamistRule && m.oilExporter)
      game.islamistResources + jihadOilExporters < 6 || !game.islamistAdjacency

    case Jihadist if game.botEnhancements =>
      // Will not play if it would cause immediate victory for the US player
      // or there are 2 or more countries countries with printed resource value
      // of 3 that are at Good governance.
      val GoodOilExporters = game.muslims.count(m => m.isGood && m.oilExporter)
      val Good3ResExporters = game.muslims.count(m => m.isGood && m.oilExporter && m.printedResources == 3)
      (game.goodResources + GoodOilExporters < 12) &&
      (Good3ResExporters < 2) 
      // enhancedJihadistBotEntries().nonEmpty  To be done

    case Jihadist =>
      // Unplayable if it would cause US instant victory
      val usOilExporters = game.muslims.count(m => m.isGood && m.oilExporter)
      game.goodResources + usOilExporters < 12
  }

  // Carry out the event for the given role.
  // forTrigger will be true if the event was triggered during the human player's turn
  // and it associated with the Bot player.
  override
  def executeEvent(role: Role): Unit = {
    // See Event Instructions table
    removeGlobalEventMarker(Fracking) // Cancels effects of "Fracking" marker
    // Temporary until Florian works out how the bot will use this event.
    // Then we will have specific code here.
    Card_117.executeEvent(role)
  }
}
