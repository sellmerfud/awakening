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
import awakening.{USBot, JihadistBot }

// Card Text:
// ------------------------------------------------------------------
// Play in Pakistan or an adjacent country (not in Iran if a Special
// Case country).
// Place either a Cell or a Militia there.
// Then remove 1 of the other type if present in the same country:
// ------------------------------------------------------------------
object Card_346 extends Card(346, "Pakistani Intelligence (ISI)", Unassociated, 2, NoRemove, NoLapsing, NoAutoTrigger) {
  // Used by the US Bot to determine if the executing the event would alert a plot
  // in the given country
  override
  def eventAlertsPlot(countryName: String, plot: Plot): Boolean = false

  val isMilitiaCandidate = (c: Country) => c match {
    case n: NonMuslimCountry => false
    case m: MuslimCountry => m.canTakeMilitia
  }

  val isCellCandidate = (c: Country) => c match {
    case n: NonMuslimCountry => !n.iranSpecialCase
    case m: MuslimCountry => true
  }

  def possibleCountries =
    game.getMuslim(Pakistan) :: game.adjacentCountries(Pakistan)

  def getMilitiaCandidates =
    countryNames(possibleCountries.filter(isMilitiaCandidate))

  def getCellCandidates =
    countryNames(possibleCountries.filter(isCellCandidate))

  def canPlaceMilitia = game.militiaAvailable > 0 && getMilitiaCandidates.nonEmpty

  def canPlaceCell = game.cellsAvailable > 0 && getCellCandidates.nonEmpty

  // Used by the US Bot to determine if the executing the event would remove
  // the last cell on the map resulting in victory.
  override
  def eventRemovesLastCell(): Boolean =
    canPlaceMilitia && // Have to place a milita in order to remove a cell
    getMilitiaCandidates.exists(name => USBot.wouldRemoveLastCell(name, 1))

  // Returns true if the printed conditions of the event are satisfied
  override
  def eventConditionsMet(role: Role) = canPlaceMilitia || canPlaceCell

  // Returns true if the Bot associated with the given role will execute the event
  // on its turn.  This implements the special Bot instructions for the event.
  // When the event is triggered as part of the Human players turn, this is NOT used.
  override
  def botWillPlayEvent(role: Role): Boolean = role match {
    case US => canPlaceMilitia
    case Jihadist => canPlaceCell
  }

  // Carry out the event for the given role.
  // forTrigger will be true if the event was triggered during the human player's turn
  // and it associated with the Bot player.
  override
  def executeEvent(role: Role): Unit = {
    val choices = List(
      choice(canPlaceMilitia, "militia", "Place a militia"),
      choice(canPlaceCell,    "cell", "Place a cell"),
    ).flatten
    val orderedChoices = if (role == US) choices else choices.reverse

    val (target, pieceType) = role match {
      case _ if isHuman(role) =>
        val pieceType = askMenu("Choose one:", orderedChoices).head
        val target = if (pieceType == "militia")
          askCountry("Place a milita in which country: ", getMilitiaCandidates)
        else
          askCountry("Place a cell in which country: ", getCellCandidates)
        (target, pieceType)
      case US =>
        val target = getMilitiaCandidates.find(name => USBot.wouldRemoveLastCell(name, 1)) getOrElse
          USBot.deployToPriority(getMilitiaCandidates).get
        (target, "militia")

      case Jihadist =>
        val target = JihadistBot.cellPlacementPriority(false)(getCellCandidates).get
        (target, "cell")
    }

    addEventTarget(target)
    if (pieceType == "militia") {
      addMilitiaToCountry(target, 1)
      if (game.getCountry(target).totalCells > 0) {
        val (actives, sleepers, sadr) = if (isHuman(role))
          askCells(target, 1, true)
        else
          USBot.chooseCellsToRemove(target, 1)
        removeCellsFromCountry(target, actives, sleepers, sadr, addCadre = true)
      }
    }
    else {
        addSleeperCellsToCountry(target, 1)
        if (game.isMuslim(target) && game.getMuslim(target).militia > 0)
          removeMilitiaFromCountry(target, 1)
    }
  }
}
