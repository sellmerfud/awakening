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
import awakening.{ USBot, JihadistBot }

// Card Text:
// ------------------------------------------------------------------
// If US play, remove a Cell, Cadre or Alert 1 Plot in any one
// non-Muslim country.
// If Jihadist, place an Active Cell or a level 1 Plot in any one
// non-Muslim country.
// ------------------------------------------------------------------
object Card_207 extends Card2(207, "JV / Copycat", Unassociated, 1, NoRemove, NoLapsing, NoAutoTrigger) {

  def getRemoveCellCandidates() = countryNames(game.nonMuslims.filter(_.totalCells > 0))

  def getAlertPlotCandidates() = countryNames(game.nonMuslims.filter(_.hasPlots))

  def getUSCandidates() = countryNames(game.nonMuslims.filter(n => n.totalCells > 0 || n.hasCadre || n.hasPlots))

  // Used by the US Bot to determine if the executing the event would alert a plot
  // in the given country
  override
  def eventAlertsPlot(countryName: String, plot: Plot): Boolean = {
    // Removing a Cell or Cadre in US is higher priority than alerting
    // a plot elsewhere
    val us = game.getNonMuslim(UnitedStates)
    us.hasPlots || (getAlertPlotCandidates().nonEmpty && us.totalCells == 0 && !us.hasCadre)
  }

  // Used by the US Bot to determine if the executing the event would remove
  // the last cell on the map resulting in victory.
  override
  def eventRemovesLastCell(): Boolean =
    getRemoveCellCandidates().exists(name => USBot.wouldRemoveLastCell(name, 1))

  // Returns true if the printed conditions of the event are satisfied
  override
  def eventConditionsMet(role: Role) = role match {
    case US => getUSCandidates().nonEmpty
    case Jihadist => game.cellsAvailable > 0 || game.availablePlots.contains(Plot1)
  }

  // Returns true if the Bot associated with the given role will execute the event
  // on its turn.  This implements the special Bot instructions for the event.
  // When the event is triggered as part of the Human players turn, this is NOT used.
  override
  def botWillPlayEvent(role: Role): Boolean = true

  // Carry out the event for the given role.
  // forTrigger will be true if the event was triggered during the human player's turn
  // and it associated with the Bot player.
  override
  def executeEvent(role: Role, forTrigger: Boolean): Unit = {
    // See Event Instructions table
    if (role == Jihadist) {
      if (isHuman(role)) {
        val name = askCountry("Select country: ", countryNames(game.nonMuslims))
        addEventTarget(name)
        testCountry(name)
        val choices = List(
          choice(game.cellsAvailable > 0,            "cell", "Place an active cell"),
          choice(game.availablePlots contains Plot1, "plot", "Place a levle 1 plot")
        ).flatten

        askMenu("Choose one:", choices).head match {
          case "cell" => addActiveCellsToCountry(name, 1)
          case "plot" => addAvailablePlotToCountry(name, Plot1)
        }
      }
      else {  // Jihadist Bot
        // If we have both a cell and a Plot1 available:
        // Place a cell if an WMD plot is available or if funding >= 8
        // Otherwise place a Plot1
        addEventTarget(UnitedStates)
        (game.cellsAvailable > 0, game.availablePlots.contains(Plot1)) match {
          case (true, false) =>
            addActiveCellsToCountry(UnitedStates, 1)
          case (false, true) =>
            addAvailablePlotToCountry(UnitedStates, Plot1, visible = true)
          case _ if game.availablePlots.contains(PlotWMD) || (game.funding >= 8) =>
            addActiveCellsToCountry(UnitedStates, 1)
          case _ =>
            addAvailablePlotToCountry(UnitedStates, Plot1, visible = true)
        }
      }
    }
    else {  // US
      if (isHuman(role)) {
        val name = askCountry("Select country: ", getUSCandidates())
        val n = game.getNonMuslim(name)
        val choices = List(
          choice(n.totalCells > 0, "cell" , "Remove a cell"),
          choice(n.hasCadre,       "cadre", "Remove cadre"),
          choice(n.hasPlots,       "plot" , "Alert a plot")
        ).flatten

        addEventTarget(name)
        askMenu("Choose one:", choices).head match {
          case "cell" =>
            val (actives, sleepers, sadr) = askCells(name, 1, sleeperFocus = true)
            removeCellsFromCountry(name, actives, sleepers, sadr, addCadre = true)

          case "cadre" =>
            removeCadreFromCountry(name)
          case _ =>
            performAlert(name, humanPickPlotToAlert(name))
        }
      }
      else {  // US Bot
        // Top priority is removing last cell on the map if possible
        if (eventRemovesLastCell()) {
          val c = game.getCountry(getRemoveCellCandidates().head)
          addEventTarget(c.name)
          removeCellsFromCountry(c.name, c.activeCells, c.sleeperCells, c.hasSadr, addCadre = true)
        }
        else {
          val name = if (getUSCandidates().contains(UnitedStates))
            UnitedStates
          else
            USBot.disruptPriority(getUSCandidates()).get

          addEventTarget(name)
          val n = game.getNonMuslim(name)
          if (n.hasPlots)
            performAlert(name, USBot.selectPriorityPlot(name::Nil).onMap)
          else if (n.totalCells > 0) {
            val (actives, sleepers, sadr) = USBot.chooseCellsToRemove(name, 1)
            removeCellsFromCountry(name, actives, sleepers, sadr, addCadre = true)
          }
          else
            removeCadreFromCountry(name)

        }
      }
    }
  }
}
