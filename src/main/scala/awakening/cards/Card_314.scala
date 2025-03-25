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
// Play in any African countries (Test if needed).
// Place a total of 3 Cells and/or Plots in African countries,
// no more than 1 per country.
// REMOVE
// ------------------------------------------------------------------
object Card_314 extends Card(314, "Jihadist African Safari", Jihadist, 3, Remove, NoLapsing, NoAutoTrigger) {
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
    game.availablePlots.nonEmpty || game.cellsAvailable > 0


  // Carry out the event for the given role.
  // forTrigger will be true if the event was triggered during the human player's turn
  // and it associated with the Bot player.
  override
  def executeEvent(role: Role): Unit = {
    val africanCandidates = African.filter(name => !game.getCountry(name).truce)
    if (game.availablePlots.nonEmpty || game.cellsAvailable > 0) {
      case class Action(name: String, item: Either[Unit, Plot])

      val actions = if (isHuman(role)) {
        def nextAction(actionNum: Int, cellsRemaining: Int, plots: List[Plot], candidates: List[String]): List[Action] = {
          if (actionNum > 3 || (cellsRemaining == 0 && plots.isEmpty))
            Nil
          else {
            sealed trait Choice
            case object Cell extends Choice
            case object Plot extends Choice
            val choices = List(
              choice(cellsRemaining > 0, Cell, "Place a Cell"),
              choice(plots.nonEmpty,     Plot, "Place a Plot")
            ).flatten
            println()
            askMenu(s"${ordinal(actionNum)} action:", choices).head match {
              case Cell =>
                val target = askCountry("Place a cell in which country: ", candidates)
                Action(target, Left(())) :: nextAction(actionNum + 1, cellsRemaining - 1, plots, candidates.filterNot(_ == target))

              case Plot =>
                val target = askCountry("Place a plot in which country: ", candidates)
                val plot   = askPlots(plots, 1).head
                val index  = plots.indexOf(plot)
                val others = plots.take(index) ::: plots.drop(index + 1)
                Action(target, Right(plot)) :: nextAction(actionNum + 1, cellsRemaining, others, candidates.filterNot(_ == target))
            }
          }
        }
        nextAction(1, game.cellsAvailable, game.availablePlots, africanCandidates)
      }
      else {
        // Bot
        val plotsFirst = game.funding < 9
        val plotsToPlace = if (plotsFirst)
          JihadistBot.preparePlots(game.availablePlots).take(3)
        else
          JihadistBot.preparePlots(game.availablePlots).take((3 - game.cellsAvailable) max 0)
        val numCells = (3 - plotsToPlace.size) min game.cellsAvailable

        def nextAction(cellsLeft: Int, plots: List[Plot], candidates: List[String]): List[Action] = {
          if (cellsLeft == 0 && plots.isEmpty)
            Nil
          else if (plots.nonEmpty && (plotsFirst || cellsLeft == 0)) {
            val target = JihadistBot.plotPriority(candidates).get
            Action(target, Right(plots.head)) :: nextAction(cellsLeft, plots.tail, candidates.filterNot(_ == target))
          }
          else {
            val target = JihadistBot.cellPlacementPriority(false)(candidates).get
            Action(target, Left(())) :: nextAction(cellsLeft - 1, plots, candidates.filterNot(_ == target))
          }
        }

        nextAction(numCells, plotsToPlace, africanCandidates)
      }

      println()
      actions foreach {
        case Action(name, Left(_)) =>
          addEventTarget(name)
          addSleeperCellsToCountry(name, 1)

        case Action(name, Right(plot)) =>
          addEventTarget(name)
          addAvailablePlotToCountry(name, plot)
      }
    }
    else
      log("\nThere are no available cells or plots. The event has no effect.", Color.Event)
  }
}
