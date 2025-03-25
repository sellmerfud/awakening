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
// Play in a country (not Iran or Syria) with both Awakening and Reaction markers present.
// Make Ally and Set to Fair. Replace all Cells there with Reaction markers.
// ------------------------------------------------------------------
object Card_162 extends Card(162, "SCAF", US, 3, NoRemove, NoLapsing, NoAutoTrigger) {
  // Used by the US Bot to determine if the executing the event would alert a plot
  // in the given country
  override
  def eventAlertsPlot(countryName: String, plot: Plot): Boolean = false

  // Used by the US Bot to determine if the executing the event would remove
  // the last cell on the map resulting in victory.
  override
  def eventRemovesLastCell(): Boolean = {
    val totalCellsOnMap = game.totalCellsOnMap
    getCandidates.exists(name => game.getMuslim(name).totalCells == totalCellsOnMap)
  }

  val isCandidate = (m: MuslimCountry) =>
    !m.truce &&
    m.name != Iran &&
    m.name != Syria &&
    m.awakening > 0 &&
    m.reaction > 0

  def getCandidates = countryNames(game.muslims.filter(isCandidate))


  // Returns true if the printed conditions of the event are satisfied
  override
  def eventConditionsMet(role: Role) = getCandidates.nonEmpty

  val isBotCandidate = (m: MuslimCountry) =>
    isCandidate(m) &&
    (m.totalCells == game.totalCellsOnMap || m.isAdversary || (m.isNeutral && !m.isGood))

  def getBotCanndidates = countryNames(game.muslims.filter(isBotCandidate))

  // Returns true if the Bot associated with the given role will execute the event
  // on its turn.  This implements the special Bot instructions for the event.
  // When the event is triggered as part of the Human players turn, this is NOT used.
  override
  def botWillPlayEvent(role: Role): Boolean = true

  // Carry out the event for the given role.
  // forTrigger will be true if the event was triggered during the human player's turn
  // and it associated with the Bot player.
  override
  def executeEvent(role: Role): Unit = {
    // If the event was triggered during Jihadist player's turn
    // then the Bot's preferred candidates may not exsist
    val botCandidates = getBotCanndidates match {
      case Nil => getCandidates
      case candidates => candidates
    }
    val botLastCellTarget = botCandidates.find { name =>
      game.getMuslim(name).totalCells == game.totalCellsOnMap
    }

    val target = if (isHuman(role))
      askCountry("Select country: ", getCandidates)
    else
      botLastCellTarget.getOrElse(USBot.scafTarget(botCandidates).get)

    addEventTarget(target)
    val m = game.getMuslim(target)
    if (m.alignment == Ally && m.isFair && m.totalCells == 0)
      log("\nThe event has no effect.", Color.Event)
    else {
      setAlignment(target, Ally)
      if (m.isGood)
        worsenGovernance(target, 1, canShiftToIR = false)
      else if (m.isPoor)
        improveGovernance(target, 1, canShiftToGood = false)
      removeCellsFromCountry(target, m.activeCells, m.sleeperCells, m.hasSadr, addCadre = true)
      if (m.totalCells > 0)
        addReactionMarker(target, m.totalCells)
    }
  }
}
