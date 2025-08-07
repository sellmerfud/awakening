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
// Play if a non-Islamist Rule country has a cell.
// Replace the cell with any 2 available plot markers.
// ------------------------------------------------------------------
object Card_087 extends Card(87, "Martyrdom Operation", Jihadist, 3, NoRemove, NoLapsing, NoAutoTrigger) {
  // Used by the US Bot to determine if the executing the event would alert a plot
  // in the given country
  override
  def eventAlertsPlot(countryName: String, plot: Plot): Boolean = false

  // Used by the US Bot to determine if the executing the event would remove
  // the last cell on the map resulting in victory.
  override
  def eventRemovesLastCell(): Boolean = false

  def getCandidates = countryNames(game.countries.filter(c => !c.truce && !c.isIslamistRule && c.totalCells > 0))
  // Returns true if the printed conditions of the event are satisfied
  override
  def eventConditionsMet(role: Role) = getCandidates.nonEmpty


  def enhBotTarget = JihadistBot.cachedTarget("martyrdom-target") {
    JihadistBot.enhMartyrdomKSMTarget(getCandidates, martyrdom = true)
  }
  // Returns true if the Bot associated with the given role will execute the event
  // on its turn.  This implements the special Bot instructions for the event.
  // When the event is triggered as part of the Human players turn, this is NOT used.
  override
  def botWillPlayEvent(role: Role): Boolean = if (game.botEnhancements)
    game.availablePlots.nonEmpty && enhBotTarget.nonEmpty
  else
    game.availablePlots.nonEmpty


  // Carry out the event for the given role.
  // forTrigger will be true if the event was triggered during the human player's turn
  // and it associated with the Bot player.
  override
  def executeEvent(role: Role): Unit = {
    val candidates = getCandidates
    if (candidates.nonEmpty) {
      val (target, cell, sadr, plots) = if (isHuman(role)) {
        val targetName = askCountry("Select country: ", candidates)
        val (cell, sadr) = askCells(targetName, 1, sleeperFocus = false)
        (targetName, cell, sadr, askAvailablePlots(2, ops = 3))
      }
      else if (game.botEnhancements && enhBotTarget.nonEmpty) {
        // The enhBotTarget will only come up empty, if we were triggered
        // during the US turn.  Fall back to normal Bot code.
        val targetName = enhBotTarget.get
        val c = game.getCountry(targetName)
        val cell = if (c.pieces.activeCells > 0)
          Pieces(activeCells = 1)
        else
          Pieces(sleeperCells = 1)
        (targetName, cell, false, JihadistBot.selectPlotMarkers(targetName, 2, game.availablePlots))
      }
      else {
        // See Event Instructions table
        val targetName = JihadistBot.plotPriority(candidates).get
        val c = game.getCountry(targetName)
        val (cell, sadr) = if (c.pieces.activeCells > 0)
          (Pieces(activeCells = 1), false)
        else if (c.hasSadr)
          (Pieces(), true)
        else
          (Pieces(sleeperCells = 1), false)

        (targetName, cell, sadr, JihadistBot.selectPlotMarkers(targetName, 2, game.availablePlots))
      }

      addEventTarget(target)
      removeCellsFromCountry(target, cell, sadr, addCadre = true)
      for (plot <- plots)
        addAvailablePlotToCountry(target, plot)
    }
    else
        log("\nThere are no available plots.  The event has no effect.", Color.Event)
  }
}
