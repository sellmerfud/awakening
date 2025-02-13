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
// Play if a country has Regime Change.
// Place 5 cells there and remove 1 Aid, if any, or, if none, place
// Besieged Regime.
// ------------------------------------------------------------------
object Card_081 extends Card(81, "Foreign Fighters", Jihadist, 3, NoRemove, NoLapsing, NoAutoTrigger) {
  // Used by the US Bot to determine if the executing the event would alert a plot
  // in the given country
  override
  def eventAlertsPlot(countryName: String, plot: Plot): Boolean = false

  // Used by the US Bot to determine if the executing the event would remove
  // the last cell on the map resulting in victory.
  override
  def eventRemovesLastCell(): Boolean = false

  def getCandidates = countryNames(game.muslims.filter(_.inRegimeChange))
  // Returns true if the printed conditions of the event are satisfied
  override
  def eventConditionsMet(role: Role) = getCandidates.nonEmpty

  // Returns true if the Bot associated with the given role will execute the event
  // on its turn.  This implements the special Bot instructions for the event.
  // When the event is triggered as part of the Human players turn, this is NOT used.
  override
  def botWillPlayEvent(role: Role): Boolean = {
    val haveTarget = game.getMuslims(getCandidates)
      .exists (m => m.aidMarkers > 0 || !m.besiegedRegime)

    game.cellsAvailable > 0 || haveTarget
  }

  // Carry out the event for the given role.
  // forTrigger will be true if the event was triggered during the human player's turn
  // and it associated with the Bot player.
  override
  def executeEvent(role: Role): Unit = if (isHuman(role)) {
    val candidates = if (game.cellsAvailable > 0)
      countryNames(game.muslims
        .filter(_.inRegimeChange))
    else
      countryNames(game.muslims
        .filter(m => m.inRegimeChange && (m.aidMarkers > 0 || !m.besiegedRegime)))
    val target = askCountry("Select country: ", candidates)

    addEventTarget(target)
    val m = game.getMuslim(target)
    val numCells = 5 min game.cellsAvailable
    if (numCells > 0)
      addSleeperCellsToCountry(target, numCells)
    else
      log(s"There are no cells available to place in $target")

    if (m.aidMarkers > 0)
      removeAidMarker(target, 1)
    else if (!m.besiegedRegime)
      addBesiegedRegimeMarker(target)

    if (jihadistChoosesToDeclareCaliphate(target, numCells))
      declareCaliphate(target)
  }
  else {  // Bot
    val numCells = 5 min game.cellsAvailable
    val regimeChange = game.muslims.filter(_.inRegimeChange)
    val withAid = game.muslims.filter(m => m.inRegimeChange && m.aidMarkers > 0)
    val notBesieged = game.muslims.filter(m => m.inRegimeChange && !m.besiegedRegime)
    val target = if (game.cellsAvailable > 0)
      JihadistBot.cellPlacementPriority(numCells >= 3)(countryNames(regimeChange)).get
    else if (withAid.nonEmpty)
      JihadistBot.markerTarget(countryNames(withAid)).get
      else
      JihadistBot.minorJihadTarget(countryNames(notBesieged)).get

    addEventTarget(target)
    val m = game.getMuslim(target)
    if (numCells > 0)
      addSleeperCellsToCountry(target, numCells)
    else
      log(s"There are no cells available to place in $target")

    if (m.aidMarkers > 0)
      removeAidMarker(target, 1)
    else if (!m.besiegedRegime)
      addBesiegedRegimeMarker(target)
    
    if (jihadistChoosesToDeclareCaliphate(target, numCells))
      declareCaliphate(target)    
  }
}
