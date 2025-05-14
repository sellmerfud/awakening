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
// If US Hard, -1 Prestige.
// If US Soft, place 1 Cell anywhere.
// REMOVE
// ------------------------------------------------------------------
object Card_282 extends Card(282, "Executive Order 13492", Jihadist, 1, Remove, NoLapsing, NoAutoTrigger) {
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
  def botWillPlayEvent(role: Role): Boolean =
    (game.usPosture == Hard && game.prestige > 1) ||
    (game.usPosture == Soft && game.cellsAvailable > 0)

  // Place in the US if a WMD plot is available
  // then in Good country with no troops (priority to highest res*),
  // then in Fair 3+ res* country with a-r<1 (priority to highest res*, then best r-a),
  // finally use PRIORITY TABLE, Recruit/Travel To priorities
  def enhBotCellPlacementTarget: String = {
    val goodCandidates = game.muslims.filter(m => m.isGood && m.totalTroops == 0)
    val fairCandidates = game.muslims.filter { muslim =>
      muslim.isFair &&
      JihadistBot.enhBotResourceValue(muslim) > 2 &&
      muslim.awakening - muslim.reaction < 1
    }

    if (game.availablePlots.contains(PlotWMD))
      UnitedStates
    else if (goodCandidates.nonEmpty) {
      val priorities = List(JihadistBot.HighestPrintedResourcePriority)
      JihadistBot.topPriority(goodCandidates, priorities)
        .map(_.name)
        .get      
    }
    else if (fairCandidates.nonEmpty) {
      val priorities = List(
        JihadistBot.HighestPrintedResourcePriority,
        JihadistBot.HighestReactionMinusAwakeningPriority,
      )
      JihadistBot.topPriority(fairCandidates, priorities)
        .map(_.name)
        .get      
    }
    else
      JihadistBot.recruitTravelToPriority(countryNames(game.countries))
        .get
  }

  // Carry out the event for the given role.
  // forTrigger will be true if the event was triggered during the human player's turn
  // and it associated with the Bot player.
  override
  def executeEvent(role: Role): Unit = {
    if (game.usPosture == Hard && game.prestige > 1)
      decreasePrestige(1)
    else if (game.cellsAvailable > 0) {
      val target = if (isHuman(role))
        askCountry("Place a cell in which country: ", countryNames(game.countries))
      else if (game.botEnhancements)
        enhBotCellPlacementTarget
      else
        JihadistBot.cellPlacementPriority(false)(countryNames(game.countries)).get

      addEventTarget(target)
      addSleeperCellsToCountry(target, 1)
    }
    else
      log("\nThere are no available cells.  The event has no effect.", Color.Event)
  }
}
