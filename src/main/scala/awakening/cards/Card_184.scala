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

import scala.collection.mutable.ListBuffer
import awakening.LabyrinthAwakening._
import awakening.JihadistBot

// Card Text:
// ------------------------------------------------------------------
// Play if US Soft.
// US player Selects, reveals and draws a card from the discard pile
// (must be Obama Doctrine if available).
// Remove 3 Troops (from Track first) to Of Map Box. Return them to
// Track immediately upon play of Congress Acts, or if a WMID is resolved,
// or at the end of a draw phase in which a 3 Resource country is Islamist Rule.
// ------------------------------------------------------------------
object Card_184 extends Card(184, "Sequestration", Jihadist, 2, Remove, NoLapsing, NoAutoTrigger) {
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
  def eventConditionsMet(role: Role) = game.usPosture == Soft

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
    if (isHuman(US)) {
      log("\nUS player draws one card from the discard pile", Color.Event)
      log("Must draw \"Obama Doctrine\" if it is available.", Color.Event)
    }
    else {
      // See Event Instructions table
      log("\nUS Bot will draw \"Obama Doctrine\", or \"Congress Acts\", or", Color.Event)
      log("randomly among the US associated cards with the highest Ops.", Color.Event)
    }
    askCardsDrawn(US, 1, FromDiscard)

    val items = if (isHuman(role))
      selectTroopsToPutOffMap(3)
    else {
      val numFromTrack = 3 min game.troopsAvailable
      val numFromMap   = 3 - numFromTrack
      val botItems = new ListBuffer[MapItem]
      if (numFromTrack > 0)
        botItems += MapItem("track", numFromTrack)
      if (numFromMap > 0)
        botItems ++= JihadistBot.troopsToTakeOffMap(numFromMap, countryNames(game.countries filter (_.troops > 0)))
      botItems.toList
    }

    for (MapItem(name, num) <- items) {
      if (name != "track")
        addEventTarget(name)
      putTroopsInOffMapBox(name, num)
    }

    game = game.copy(sequestrationTroops = true)
    addGlobalEventMarker(Sequestration)
  }
}
