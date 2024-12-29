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
// Play if Iran not Ally.
// Remove 1 Militia from a Sunni Country or 2 Militia from a Shia-Mix Country.
// OR play if Iran not Islamist Rule.
// Remove 1 Cell from a Sunni Country or 2 Cells from a Shia-Mix Country.
// ------------------------------------------------------------------
object Card_209 extends Card2(209, "Quds Force", Unassociated, 1, NoRemove, NoLapsing, NoAutoTrigger) {
  // Used by the US Bot to determine if the executing the event would alert a plot
  // in the given country
  override
  def eventAlertsPlot(countryName: String, plot: Plot): Boolean = false

  // Used by the US Bot to determine if the executing the event would remove
  // the last cell on the map resulting in victory.
  override
  def eventRemovesLastCell(): Boolean =
    countryNames(game.muslims.filter(m => m.isSunni && m.totalCells > 0))
      .exists(name => USBot.wouldRemoveLastCell(name, 1)) ||
    countryNames(game.muslims.filter(m => m.isShiaMix && m.totalCells > 0))
      .exists(name => USBot.wouldRemoveLastCell(name, 2))

  def getMilitiaCandidates() = countryNames(game.muslims.filter(_.militia > 0))

  def getCellCandidates() = countryNames(game.muslims.filter(_.totalCells > 0))

  def iranNotAlly = game.getCountry(Iran).isNonMuslim || !game.getMuslim(Iran).isAlly

  def iranNotIR = !game.getCountry(Iran).isIslamistRule

  // Returns true if the printed conditions of the event are satisfied
  override
  def eventConditionsMet(role: Role) = iranNotAlly || iranNotIR

  // Returns true if the Bot associated with the given role will execute the event
  // on its turn.  This implements the special Bot instructions for the event.
  // When the event is triggered as part of the Human players turn, this is NOT used.
  override
  def botWillPlayEvent(role: Role): Boolean = role match {
    case US => iranNotIR && getCellCandidates().nonEmpty
    case Jihadist => iranNotAlly && getMilitiaCandidates().nonEmpty
  }

  // Carry out the event for the given role.
  // forTrigger will be true if the event was triggered during the human player's turn
  // and it associated with the Bot player.
  override
  def executeEvent(role: Role, forTrigger: Boolean): Unit = {
    // See Event Instructions table
    def numToRemove(name: String) = if (game.getMuslim(name).isSunni) 1 else 2

    val choices = List(
      choice(iranNotIR, "cells",   "Remove cell(s)"),
      choice(iranNotIR, "militia", "Remove militia"),
    ).flatten
    val orderedChoices = if (role == US) choices else choices.reverse

    val action = role match {
      case _ if isHuman(role) => askMenu("Choose one:", choices).head
      case US => "cells"
      case Jihadist => "militia"
    }

    val target = action match {
      case "cells" if getCellCandidates().isEmpty => None
      case "militia" if getMilitiaCandidates().isEmpty => None
      case "cells" if isHuman(role) => Some(askCountry("Select country with cells: ", getCellCandidates()))
      case "militia" if isHuman(role) => Some(askCountry("Select country with militia: ", getMilitiaCandidates()))
      case "cells" => USBot.disruptPriority(getCellCandidates())
      case _ => JihadistBot.minorJihadTarget(getMilitiaCandidates())
    }

    target match {
      case Some(name) if action == "cells" =>
        addEventTarget(name)
        val (actives, sleepers, sadr) = if (isHuman(role))
          askCells(name, numToRemove(name), sleeperFocus = true)
        else
          USBot.chooseCellsToRemove(name, numToRemove(name))
        removeCellsFromCountry(name, actives, sleepers, sadr, addCadre = true)

      case Some(name) =>
        addEventTarget(name)
        val numMilitia = numToRemove(name) min game.getMuslim(name).militia
        removeMilitiaFromCountry(name, numMilitia)

      case None if action == "cells "=>
        log("\nThere a no Muslim countries with cells.  The event has no effect.", Color.Event)

      case None =>
        log("\nThere a no Muslim countries with militia.  The event has no effect.", Color.Event)
    }
  }
}
