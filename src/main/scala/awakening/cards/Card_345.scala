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
// Play in a Civil War country adjacent to Turkey.
// Remove a Militia or a Cell.
// Place an Aid or a Besieged Regime marker.
// ------------------------------------------------------------------
object Card_345 extends Card(345, "Operation Euphrates Shield", Unassociated, 2, NoRemove, NoLapsing, NoAutoTrigger) {
  // Used by the US Bot to determine if the executing the event would alert a plot
  // in the given country
  override
  def eventAlertsPlot(countryName: String, plot: Plot): Boolean = false

  val isCandidate = (m: MuslimCountry) => m.civilWar && areAdjacent(m.name, Turkey)

  def getCandidates() = countryNames(game.muslims.filter(isCandidate))

  def jihadBotCandidates() = {
    val botTest =
      (m: MuslimCountry) => m.militia > 0 || (!m.besiegedRegime && JihadistBot.hasCellForTravel(m))
    countryNames(game.muslims.filter(m => isCandidate(m) && botTest(m)))
  }
  // Used by the US Bot to determine if the executing the event would remove
  // the last cell on the map resulting in victory.
  override
  def eventRemovesLastCell(): Boolean =
    getCandidates().exists(name => USBot.wouldRemoveLastCell(name, 1))

  // Returns true if the printed conditions of the event are satisfied
  override
  def eventConditionsMet(role: Role) = getCandidates().nonEmpty

  // Returns true if the Bot associated with the given role will execute the event
  // on its turn.  This implements the special Bot instructions for the event.
  // When the event is triggered as part of the Human players turn, this is NOT used.
  override
  def botWillPlayEvent(role: Role): Boolean = role match {
    case US => true
    case Jihadist => jihadBotCandidates().nonEmpty
  }

  // Carry out the event for the given role.
  // forTrigger will be true if the event was triggered during the human player's turn
  // and it associated with the Bot player.
  override
  def executeEvent(role: Role): Unit = {
    def removeChoices(target: String) = {
      val cellOK = game.getMuslim(target).totalCells > 0
      val miliiaOK = game.getMuslim(target).militia > 0
      val list = List(
        choice(cellOK, "cell", "Remove a cell"),
        choice(miliiaOK, "militia", "Remove a militia"),
      ).flatten
      if (role == US) list else list.reverse
    }

    def placeChoices(target: String) = {
      val besiegedOK = game.getMuslim(target).canTakeBesiegedRegimeMarker
      val aidOK = game.getMuslim(target).canTakeAidMarker
      val list = List(
        choice(aidOK, "aid", "Place an Aid marker"),
        choice(besiegedOK, "besiege", "Place a Besieged Regime marker"),
      ).flatten
      if (role == US) list else list.reverse
    }

    val (target, removeAction, placeAction) = role match {
      case _ if isHuman(role) =>
        val target = askCountry("Which country: ", getCandidates())
        val removeAction = askMenu("Choose one:", removeChoices(target)).headOption.getOrElse("cell")
        val placeAction  = askMenu("Choose one:", placeChoices(target)).head // can always place aid
        (target, removeAction, placeAction)

      case US =>
        val withCellCandidates = getCandidates().filter(name => game.getMuslim(name).totalCells > 0)
        val noMilitiaCandidates = getCandidates().filter(name => game.getMuslim(name).militia == 0)
        val (target, removeAction) = if (withCellCandidates.nonEmpty)
          (USBot.disruptPriority(withCellCandidates).get, "cell")
        else if (noMilitiaCandidates.nonEmpty)
          (USBot.markerAlignGovTarget(noMilitiaCandidates).get, "cell")
        else
          (USBot.markerAlignGovTarget(getCandidates()).get, "militia")
        (target, removeAction, "aid")

      case Jihadist =>
        val withMilitiaCandidates = jihadBotCandidates().filter(name => game.getMuslim(name).militia > 0)
        val notBesieged = jihadBotCandidates().filter(name => !game.getMuslim(name).besiegedRegime)
        val (target, removeAction) =  if (withMilitiaCandidates.nonEmpty)
          (JihadistBot.troopsMilitiaTarget(withMilitiaCandidates).get, "militia")
        else
          (JihadistBot.markerTarget(notBesieged).get, "cell")
        val placeAction = if (game.getMuslim(target).besiegedRegime) "aid" else "besiege"
        (target, removeAction, placeAction)
    }

    val m = game.getMuslim(target)
    addEventTarget(target)
    if (removeAction == "cell" && m.totalCells > 0) {
      val (actives, sleepers, sadr) = if (isHuman(role))
        askCells(target, 1, true)
      else
        USBot.chooseCellsToRemove(target, 1)
      removeCellsFromCountry(target, actives, sleepers, sadr, addCadre = true)
    }
    else if (removeAction == "militia" && m.militia > 0)
      removeMilitiaFromCountry(target, 1)

    if (placeAction == "aid")
      addAidMarker(target)
    else
      addBesiegedRegimeMarker(target)
  }
}
