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
import awakening.JihadistBot

// Card Text:
// ------------------------------------------------------------------
// If US play, Blocks NPT Safeguards Ignored. Remove Iranian WMD
// (+1 Prestige if WMD removed; 11.3.1). Iran may resume Oil Exports.
// MARK & REMOVE.
// If Jihadist, -1 Prestige. Place 1 Reaction marker in a Shia-Mix country.
// Iran no longer an Oil Exporter.
// ------------------------------------------------------------------
object Card_232 extends Card(232, "Trade Embargo", Unassociated, 2, USRemove, NoLapsing, NoAutoTrigger) {
  // Used by the US Bot to determine if the executing the event would alert a plot
  // in the given country
  override
  def eventAlertsPlot(countryName: String, plot: Plot): Boolean = false

  // Used by the US Bot to determine if the executing the event would remove
  // the last cell on the map resulting in victory.
  override
  def eventRemovesLastCell(): Boolean = false

  def getReactionCandidates = countryNames(
    game.muslims.filter(m => m.isShiaMix && m.canTakeAwakeningOrReactionMarker)
  )
  // Returns true if the printed conditions of the event are satisfied
  override
  def eventConditionsMet(role: Role) = true

  // Returns true if the Bot associated with the given role will execute the event
  // on its turn.  This implements the special Bot instructions for the event.
  // When the event is triggered as part of the Human players turn, this is NOT used.
  override
  def botWillPlayEvent(role: Role): Boolean = role match {
    case US => true

    // Playable if Prestige>1 and Arab Winter not active
    case Jihadist if game.botEnhancements =>
      game.prestige > 1 &&
      (lapsingEventNotInPlay(ArabWinter) && getReactionCandidates.nonEmpty)

    case Jihadist =>
      game.prestige > 1 ||
      (lapsingEventNotInPlay(ArabWinter) && getReactionCandidates.nonEmpty) ||
      !game.getCountry(Iran).hasMarker(TradeEmbargoJihadist)
  }

  // Carry out the event for the given role.
  // forTrigger will be true if the event was triggered during the human player's turn
  // and it associated with the Bot player.
  override
  def executeEvent(role: Role): Unit = {
    if (role == US) {
      val iran = game.getCountry(Iran)
      if (iran.wmdCache > 0)
        removeCachedWMD(Iran, iran.wmdCache, bumpPrestige = game.useExpansionRules)
      addEventTarget(Iran)
      if (iran.hasMarker(TradeEmbargoJihadist)) {
        removeEventMarkersFromCountry(Iran, TradeEmbargoJihadist)
        log("\nIran may resume oil exports.", Color.Event)
      }
      addEventMarkersToCountry(Iran, TradeEmbargoUS)
    }
    else { // Jihadist
      decreasePrestige(1)
      if (lapsingEventInPlay(ArabWinter))
        log("\nCannot place a reaction marker. [Arab Winter]", Color.Event)
      else if (getReactionCandidates.isEmpty)
        log("\nThere are no Shix-Mix countries that can take a reaction marker.", Color.Event)
      else {
        val target = if (isHuman(role))
          askCountry("Select Shia-Mix country to place reaction marker: ", getReactionCandidates)
        else
          JihadistBot.markerTarget(getReactionCandidates).get
        addEventTarget(target)
        addReactionMarker(target)
      }

      addEventTarget(Iran)
      if (!game.getCountry(Iran).hasMarker(TradeEmbargoJihadist)) {
        // If Iran becomes Neutral or Ally, remove any Trade Embargo marker. [11.3.3.1]
        addEventMarkersToCountry(Iran, TradeEmbargoJihadist)
        log("\nIran is no longer an oil exporter.", Color.Event)
      }
    }
  }
}
