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
import awakening.LabyrinthAwakening.GameMode.next
import java.awt.RenderingHints.Key

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

  def africanCandidates = African.filter(name => !game.getCountry(name).truce)

  // Returns true if the printed conditions of the event are satisfied
  override
  def eventConditionsMet(role: Role) = true

  case object Cell
  case class EventAction(name: String, item: Either[Cell.type, Plot])

  def getStandardBotActions: List[EventAction] = {
    val plotsFirst = game.funding < 9
    val numPlotsToPlace = if (plotsFirst)
      game.availablePlots.size min 3
    else
      (3 - game.cellsAvailable) max 0
    val numCellsToPlace = (3 - numPlotsToPlace) min game.cellsAvailable

    def nextAction(cellsRemaining: Int, plotsRemaining: Int, plotMarkers: List[Plot], candidates: List[String]): List[EventAction] = {
      if ((cellsRemaining == 0 && plotsRemaining == 0) || candidates.isEmpty)
        Nil
      else if (plotsRemaining > 0 && (plotsFirst || cellsRemaining == 0)) {
        val target = JihadistBot.plotPriority(candidates).get
        val marker = JihadistBot.selectPlotMarkers(target, 1, plotMarkers).head
        val idx = plotMarkers.indexOf(marker)
        val remainingMarkers = plotMarkers.patch(idx, Nil, 1)
        val action = EventAction(target, Right(marker))
        action :: nextAction(cellsRemaining, plotsRemaining - 1, remainingMarkers, candidates.filterNot(_ == target))
      }
      else {
        val target = JihadistBot.cellPlacementPriority(false)(candidates).get
        val action = EventAction(target, Left(Cell))
        action :: nextAction(cellsRemaining - 1, plotsRemaining, plotMarkers, candidates.filterNot(_ == target))
      }
    }

    nextAction(numCellsToPlace, numPlotsToPlace, game.availablePlots, africanCandidates).reverse
  }


  // Ehanced Bot instructions:
  // Playable if at least 2 plot markers available and the following procedure yields any valid targets in this priority order:
  // a) Place plot markers (if depleted: place cell, if available) in a Good Muslim country (priority to highest res*, then with troops).
  // b) if Funding <8, place plots in Nigeria, then Kenya, then poor Muslim (priority to troops, then AID), then Fair Muslim (priority to troops, then AID).
  // c) if at least one plot marker has been placed so far: place cells, if available, using Recruit/Travel to priority table.
  // If no plotmarkers have been legally placed following a-c, the event is unplayable.
  def getEnhancedBotActions: List[EventAction] = {
    def nextAction(actionNum: Int, cellsRemaining: Int, plots: List[Plot], candidateNames: List[String], actions: Vector[EventAction]): List[EventAction] = {
      if (actionNum > 3 || (cellsRemaining == 0 && plots.isEmpty))
        actions.toList
      else {
        val havePlotted = actions.exists(_.item.isRight)
        val muslims = candidateNames.filter(isMuslim).map(game.getMuslim)
        val goodMuslims = muslims.filter(_.isGood)
        val fairMuslims = muslims.filter(_.isFair)
        val poorMuslims = muslims.filter(_.isPoor)
        val nigeriaOrKenya = candidateNames.contains(Nigeria) || candidateNames.contains(KenyaTanzania)

        val action = if (goodMuslims.nonEmpty) {
          val priorites = List(JihadistBot.HighestPrintedResourcePriority, JihadistBot.WithTroopsPriority)
          val target = JihadistBot.topPriority(goodMuslims, priorites)
            .map(_.name)
            .get

          val item = plots match {
            case Nil => Left(Cell)
            case _   => Right(JihadistBot.selectPlotMarkers(target, 1, plots).head)
          }
          Some(EventAction(target, item))
        }
        else if (game.funding < 8 && plots.nonEmpty && (nigeriaOrKenya || poorMuslims.nonEmpty || fairMuslims.nonEmpty)) {
          val priorities = List(JihadistBot.WithTroopsPriority, JihadistBot.WithAidPriority)
          val target = if (candidateNames.contains(Nigeria))
            Nigeria
          else if (candidateNames.contains(KenyaTanzania))
            KenyaTanzania
          else if (poorMuslims.nonEmpty)
            JihadistBot.topPriority(poorMuslims, priorities).map(_.name).get
          else
            JihadistBot.topPriority(fairMuslims, priorities).map(_.name).get
          Some(EventAction(target, Right(JihadistBot.selectPlotMarkers(target, 1, plots).head)))
        }
        else if (havePlotted && cellsRemaining > 0) {
          val target = JihadistBot.recruitTravelToPriority(candidateNames).get
          Some(EventAction(target, Left(Cell)))
        }
        else
          None

        action match {
          case None => // No more targets found
            actions.toList

          case Some(action @ EventAction(target, Right(plot))) =>
            val idx = plots.indexOf(plot)
            val remainingPlots = plots.patch(idx, Nil, 1)
            val remainingCadidates = candidateNames.filterNot(_ == target)
            nextAction(actionNum + 1, cellsRemaining, remainingPlots, remainingCadidates, actions :+ action)

          case Some(action @ EventAction(target, _)) => // Place cell
            val remainingCadidates = candidateNames.filterNot(_ == target)
            nextAction(actionNum + 1, cellsRemaining - 1, plots, remainingCadidates, actions :+ action)
        }
      }
    }

    // If we have not placed at least one Plot then disregard the whole lot
    nextAction(1, game.cellsAvailable, game.availablePlots, africanCandidates, Vector.empty) match {
      case actions if actions.exists(_.item.isRight) => actions
      case _ => Nil
    }
  }


  // Returns true if the Bot associated with the given role will execute the event
  // on its turn.  This implements the special Bot instructions for the event.
  // When the event is triggered as part of the Human players turn, this is NOT used.
  override
  def botWillPlayEvent(role: Role): Boolean = if (game.botEnhancements)
    game.availablePlots.size > 1 && getEnhancedBotActions.nonEmpty
  else
    game.availablePlots.nonEmpty || game.cellsAvailable > 0


  // Carry out the event for the given role.
  // forTrigger will be true if the event was triggered during the human player's turn
  // and it associated with the Bot player.
  override
  def executeEvent(role: Role): Unit = {
    if (game.availablePlots.nonEmpty || game.cellsAvailable > 0) {
      lazy val enhancedActions = getEnhancedBotActions 

      val actions = if (isHuman(role)) {
        def nextAction(actionNum: Int, cellsRemaining: Int, plots: List[Plot], candidates: List[String]): List[EventAction] = {
          if (actionNum > 3 || (cellsRemaining == 0 && plots.isEmpty))
            Nil
          else {
            sealed trait Choice
            case object CellChoice extends Choice
            case object PlotChoice extends Choice
            val choices = List(
              choice(cellsRemaining > 0, CellChoice, "Place a Cell"),
              choice(plots.nonEmpty,     PlotChoice, "Place a Plot")
            ).flatten
            println()

            askMenu(s"${ordinal(actionNum)} action:", choices).head match {
              case CellChoice =>
                val target = askCountry("Place a cell in which country: ", candidates)
                EventAction(target, Left(Cell)) :: nextAction(actionNum + 1, cellsRemaining - 1, plots, candidates.filterNot(_ == target))

              case PlotChoice =>
                val target = askCountry("Place a plot in which country: ", candidates)
                val plot   = askPlots(plots, 1).head
                val index  = plots.indexOf(plot)
                val others = plots.take(index) ::: plots.drop(index + 1)
                EventAction(target, Right(plot)) :: nextAction(actionNum + 1, cellsRemaining, others, candidates.filterNot(_ == target))
            }
          }
        }
        nextAction(1, game.cellsAvailable, game.availablePlots, africanCandidates).reverse
      }
      else if (game.botEnhancements && enhancedActions.nonEmpty)
        enhancedActions
      else 
        getStandardBotActions

      println()
      actions foreach {
        case EventAction(name, Left(_)) =>
          addEventTarget(name)
          addSleeperCellsToCountry(name, 1)

        case EventAction(name, Right(plot)) =>
          addEventTarget(name)
          addAvailablePlotToCountry(name, plot)
      }
    }
    else
      log("\nThere are no available cells or plots. The event has no effect.", Color.Event)
  }
}
