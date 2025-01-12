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
// In Nigeria: Place a level 2 or 3 Plot, or place up to 3 Cells.
// Each time this event is played, the Jihadist player may return this
// card to hand by discarding a non-US Associated 3 Ops card.
// ------------------------------------------------------------------
object Card_186 extends Card(186, "Boko Haram", Jihadist, 3, NoRemove, NoLapsing, NoAutoTrigger) {
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

  def havePlots = game.availablePlots.exists(p => p == Plot2 || p == Plot3)
  def havePlot3 = game.availablePlots.exists(_ == Plot3)
  def havePlot2 = game.availablePlots.exists(_ == Plot2)
  def haveCells = game.cellsAvailable > 0

  // Returns true if the Bot associated with the given role will execute the event
  // on its turn.  This implements the special Bot instructions for the event.
  // When the event is triggered as part of the Human players turn, this is NOT used.
  override
  def botWillPlayEvent(role: Role): Boolean = havePlots || haveCells

  // Carry out the event for the given role.
  // forTrigger will be true if the event was triggered during the human player's turn
  // and it associated with the Bot player.
  override
  def executeEvent(role: Role): Unit = {
    addEventTarget(Nigeria)
    if (havePlots || haveCells) {
      if (isHuman(role)) {
        val choices = List(
          choice(havePlots, "plot",  "Place a level 2 or level 3 Plot in Nigeria"),
          choice(haveCells, "cells", "Place up to 3 cells in Nigeria")
        ).flatten

        askMenu("Choose one:", choices).head match {
          case "plot" =>
            val options = List(Plot2, Plot3).filter(game.availablePlots.contains)
            val plot = if (options.size == 1)
              options.head
            else
              askSimpleMenu("Place which plot:", options)

            addAvailablePlotToCountry(Nigeria, plot)

          case _ =>
            val maxCells = 3 min game.cellsAvailable
            val num = askInt("\nPlace how many cells", 1, maxCells, Some(maxCells))
            addSleeperCellsToCountry(Nigeria, num)

            if (jihadistChoosesToDeclareCaliphate(Nigeria, num))
              declareCaliphate(Nigeria)
        }
      }
      else {
        // Bot
        val choices = if (game.getCountry(Nigeria).isNonMuslim)
          List((havePlot3, "plot3"), (havePlot2, "plot2"), (haveCells, "cells"))
        else
          List((haveCells, "cells"), (havePlot3, "plot3"), (havePlot2, "plot2"))

        val action = choices.dropWhile(_._1 == false).head._2

        action match {
          case "plot3" =>
            addAvailablePlotToCountry(Nigeria, Plot3)

          case "plot2" =>
            addAvailablePlotToCountry(Nigeria, Plot2)

          case _ =>
            val num = 3 min game.cellsAvailable
            addSleeperCellsToCountry(Nigeria, num)
            if (jihadistChoosesToDeclareCaliphate(Nigeria, num))
              declareCaliphate(Nigeria)
        }
      }
    }
    else
      log(s"No available $Plot2, $Plot3, or cells.  The event has no effect. ", Color.Event)

    if (isHuman(role)) {
      log("\nJihadist player may return Boko Haram to hand by discarding a", Color.Event)
      log("non-US associated 3 Ops card", Color.Event)
      askCardsDiscarded(1)
    }
    else
      log("\nThe Jihadist Bot does not return the Boko Haram card to hand.", Color.Event)
  }
}
