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

import scala.util.Random.shuffle
import awakening.LabyrinthAwakening._
import awakening.{ USBot, JihadistBot }

// Card Text:
// ------------------------------------------------------------------
// Place 1 Militia or 2 Cells in an African country
// (add 1 additional of either if in Mali or Nigeria).
// ------------------------------------------------------------------
object Card_201 extends Card(201, "Cross Border Support", Unassociated, 1, NoRemove, NoLapsing, NoAutoTrigger) {
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
  def eventConditionsMet(role: Role) = canPlaceMilitia || canPlaceCells

  val isMilitaCandidate = (c: Country) =>
    !c.truce && c.canTakeMilitia

  def getMilitiaCandidates = countryNames(game.getCountries(African).filter(isMilitaCandidate))

  def getCellsCandidates = countryNames(game.getCountries(African).filter(!_.truce))

  def canPlaceMilitia = game.militiaAvailable > 0 && getMilitiaCandidates.nonEmpty

  def canPlaceCells = game.cellsAvailable > 0 && getCellsCandidates.nonEmpty

  def goodNoTroopsCandidates = getCellsCandidates
    .map(game.getCountry)
    .collect {
      case m: MuslimCountry if m.isGood && m.totalTroops == 0 => m
    }

  // Returns true if the Bot associated with the given role will execute the event
  // on its turn.  This implements the special Bot instructions for the event.
  // When the event is triggered as part of the Human players turn, this is NOT used.
  override
  def botWillPlayEvent(role: Role): Boolean = role match {
    case US => canPlaceMilitia
    case Jihadist if game.botEnhancements => game.cellsAvailable > 1 && canPlaceCells
    case Jihadist => canPlaceCells
  }

  // Carry out the event for the given role.
  // forTrigger will be true if the event was triggered during the human player's turn
  // and it associated with the Bot player.
  override
  def executeEvent(role: Role): Unit = {
    sealed trait Action
    case object MilitiaAction extends Action
    case object CellsAction extends Action
    def extra(name: String): Int = if (name == Mali || name == Nigeria)
      1
    else
      0
    val choices = List(
      choice(canPlaceMilitia, MilitiaAction, "Place militia"),
      choice(canPlaceCells, CellsAction, "Place cells")
    ).flatten

    val (target, action) = role match {
      case r if isHuman(r) =>
        val action = askMenu("Choose one:", choices).headOption
        val name = action match {
          case Some(MilitiaAction) => askCountry("Select African country: ", getMilitiaCandidates)
          case Some(CellsAction) => askCountry("Select African country: ", getCellsCandidates)
          case _ => ""
        }
      (name, action)

      case US => // US Bot
        val name = shuffle(USBot.highestCellsMinusTandM(getMilitiaCandidates)).head
        (name, Some(MilitiaAction))

      case Jihadist if game.botEnhancements =>  // Enhanced Jihadist Bot
        val name = if (goodNoTroopsCandidates.nonEmpty) {
          val priorities = List(JihadistBot.HighestPrintedResourcePriority)
          JihadistBot.topPriority(goodNoTroopsCandidates, priorities)
            .map(_.name)
            .get
        }
        else
          JihadistBot.caliphatePriorityTarget(Mali::Nigeria::Nil) match {
            case Some(name) if game.cellsAvailable >= 3 => name
            case _ => JihadistBot.cellPlacementPriority(false)(getCellsCandidates).get
          }
        (name, Some(CellsAction))

      case Jihadist => // Jihadist Bot
        val name = JihadistBot.caliphatePriorityTarget(Mali::Nigeria::Nil) match {
          case Some(name) if game.cellsAvailable >= 3 => name
          case _ => JihadistBot.cellPlacementPriority(false)(getCellsCandidates).get
        }
        (name, Some(CellsAction))
    }

    action match {
      case Some(MilitiaAction) =>
        val num = (1 + extra(target)) min game.militiaAvailable
        addEventTarget(target)
        addMilitiaToCountry(target, num)

      case Some(CellsAction) =>
        val num = (2 + extra(target)) min game.cellsAvailable
        addSleeperCellsToCountry(target, num)
        if (jihadistChoosesToDeclareCaliphate(target, num))
          declareCaliphate(target)

      case None =>
        log("The event has no effect.", Color.Event)
    }
  }
}
