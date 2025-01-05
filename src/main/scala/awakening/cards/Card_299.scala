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
// Play if Caliphate Capital NOT on map.
// Place 1 Cell in a Good non-Muslim country (not US) and 1 Cell in a
// Fair non-Muslim country.
// REMOVE
// ------------------------------------------------------------------
object Card_299 extends Card(299, "Foreign Fighters Return", Jihadist, 2, Remove, NoLapsing, NoAutoTrigger) {
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
  def eventConditionsMet(role: Role) = !game.caliphateDeclared

  def getGoodCandidates() = countryNames(
    game.nonMuslims.filter(n => n.isGood && n.name != UnitedStates)
  )

  def getFairCandidates() = countryNames(
    game.nonMuslims.filter(_.isFair)
  )

  // Returns true if the Bot associated with the given role will execute the event
  // on its turn.  This implements the special Bot instructions for the event.
  // When the event is triggered as part of the Human players turn, this is NOT used.
  override
  def botWillPlayEvent(role: Role): Boolean =
    game.cellsAvailable > 0 &&
    (getGoodCandidates().nonEmpty || getFairCandidates().nonEmpty)

  // Carry out the event for the given role.
  // forTrigger will be true if the event was triggered during the human player's turn
  // and it associated with the Bot player.
  override
  def executeEvent(role: Role): Unit = {
    if (game.cellsAvailable == 0)
      log("\nThere are no availabe cells. The event has no effect.", Color.Event)
    else if (getGoodCandidates().isEmpty && getFairCandidates().isEmpty)
      log("\nThere are no valid target countries. The event has no effect.", Color.Event)
    else {
      val goodTarget: Option[String] = getGoodCandidates() match {
        case Nil => None
        case candidates if isHuman(role) =>
          Some(askCountry("Place a cell in which Good Non-Muslim country: ", candidates))
        case candidates =>
          JihadistBot.cellPlacementPriority(false)(candidates)
      }

      val fairTarget: Option[String] = getFairCandidates() match {
        case Nil => None
        case candidates if isHuman(role) =>
          Some(askCountry("Place a cell in which Fair Non-Muslim country: ", candidates))
        case candidates =>
          JihadistBot.cellPlacementPriority(false)(candidates)
      }

      for (target <- List(goodTarget, fairTarget).flatten) {
        addEventTarget(target)
        testCountry(target)
        addSleeperCellsToCountry(target, 1)
      }
    }
  }
}
