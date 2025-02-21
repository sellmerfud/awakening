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
import awakening.USBot

// Card Text:
// ------------------------------------------------------------------
// Activate half (rounded up) of all Sleeper Cells on the map,
// OR Alert all Plots on the map.
// -1 Prestige.
// ------------------------------------------------------------------
object Card_161 extends Card(161, "PRISM", US, 3, NoRemove, NoLapsing, NoAutoTrigger) {
  // Used by the US Bot to determine if the executing the event would alert a plot
  // in the given country
  override
  def eventAlertsPlot(countryName: String, plot: Plot): Boolean =
    game.hasCountry(_.hasPlots) // The event alerts ALL plot on the map.

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
    game.hasCountry(_.hasPlots) || game.sleeperCellsOnMap >= 5

  // Carry out the event for the given role.
  // forTrigger will be true if the event was triggered during the human player's turn
  // and it associated with the Bot player.
  override
  def executeEvent(role: Role): Unit = {
    if (isHuman(role)) {
      sealed trait Choice
      case object Activate extends Choice
      case object Alert extends Choice
      val choices = List(
        choice(game.sleeperCellsOnMap > 0, Activate, "Activate half (rounded up) of all sleeper cells on the map"),
        choice(game.alertTargets.nonEmpty, Alert, "Alert all plots on the map")
      ).flatten

      if (choices.isEmpty && game.prestige == 1)
        log("\nNo sleeper cells or plots on the map and prestige is 1. The event has no effect.", Color.Event)

      if (choices.nonEmpty) {
        askMenu("Choose one:", choices).head match {
          case Activate =>
            val numToFlip = (game.sleeperCellsOnMap + 1) / 2  // half rounded up
            // Ask which cells to activate
            val withSleepers = game.countries
              .filter(_.sleeperCells > 0)
              .map(c => MapItem(c.name, c.sleeperCells))

            println(s"Activate a total of ${amountOf(numToFlip, "sleeper cell")}")
            val toFlip = askMapItems(withSleepers.sortBy(_.country), numToFlip, "sleeper cell")

            // It is possible that the same country was chosen multiple times
            // Consolidate them so we do 1 flip per country.
            def flipNextBatch(remaining: List[MapItem]): Unit = {
              if (remaining.nonEmpty) {
                val name = remaining.head.country
                val (batch, next) = remaining.partition(_.country == name)
                val total = batch.foldLeft(0) { (sum, x) => sum + x.num }
                addEventTarget(name)
                flipSleeperCells(name, total)
                flipNextBatch(next)
              }
            }
            flipNextBatch(toFlip)

          case Alert =>
            for (c <- game.countries; p <- c.plots)  {// Alert all plots on the map
              addEventTarget(c.name)
              performAlert(c.name, humanPickPlotToAlert(c.name))
            }
        }
      }
      // Even of neither action above was possible.
      if (game.prestige > 1)
        decreasePrestige(1)
    }
    else {
      // See Event Instructions table
      if (game hasCountry (_.hasPlots))
        for (c <- game.countries; p <- c.plots)  {// Alert all plots on the map
          addEventTarget(c.name)
          performAlert(c.name, humanPickPlotToAlert(c.name))
        }
      else {
        val candidates = countryNames(game.countries.filter(_.sleeperCells > 0))
        def flipNext(numLeft: Int, targets: List[String]): Unit = {
          if (numLeft > 0 && targets.nonEmpty) {
            var name = USBot.disruptPriority(targets).get
            val c = game getCountry name
            addEventTarget(name)
            val num = numLeft min c.sleeperCells
            flipSleeperCells(name, num)
            flipNext(numLeft - num, targets.filterNot(_ == name))
          }
        }

        flipNext((game.sleeperCellsOnMap + 1) / 2, candidates)
      }
      decreasePrestige(1)
    }
  }
}
