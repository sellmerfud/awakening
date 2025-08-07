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
// If US play: Remove 1 Cell each from a Sunni and a Shia-Mix country.
// -1 Funding. Blocks play of Mohamed Morsi Supporters.
// If Jihadist: Place 1 Cell each in a Sunni and a Shia-Mix country.
// +1 Funding. Blocks play of Abdel Fattah el-Sisi.
// ------------------------------------------------------------------
object Card_358 extends Card(358, "Political Islamism/Pan Arab Nationalism", Unassociated, 3, Remove, NoLapsing, NoAutoTrigger) {

  val AbdelFattahel_Sisi = 241
  // Used by the US Bot to determine if the executing the event would alert a plot
  // in the given country
  override
  def eventAlertsPlot(countryName: String, plot: Plot): Boolean = false

  def getSunniUSCandidates = countryNames(game.muslims.filter(m => !m.truce && m.isSunni && m.totalCells > 0))

  def getShiaUSCandidates = countryNames(game.muslims.filter(m => !m.truce && m.isShiaMix && m.totalCells > 0))

  // Used by the US Bot to determine if the executing the event would remove
  // the last cell on the map resulting in victory.
  override
  def eventRemovesLastCell(): Boolean = {
    val numSunni = getSunniUSCandidates
      .map(game.getMuslim)
      .filter(m => m.isSunni && m.totalCells > 0)
      .map(_.totalCells)
      .sum
    val numShia = getShiaUSCandidates
      .map(game.getMuslim)
      .filter(m => m.isShiaMix && m.totalCells > 0)
      .map(_.totalCells)
      .sum
    numSunni == 1 && numShia == 1 && game.totalCellsOnMap == 2
  }

  // Returns true if the printed conditions of the event are satisfied
  override
  def eventConditionsMet(role: Role) = true

  // Returns true if the Bot associated with the given role will execute the event
  // on its turn.  This implements the special Bot instructions for the event.
  // When the event is triggered as part of the Human players turn, this is NOT used.
  override
  def botWillPlayEvent(role: Role): Boolean = role match {
    case US =>
      true
    case Jihadist if game.botEnhancements => 
      // Playable if 2+ cells on track, Funding<9 and #241 Abdel Fattah al-Sisi not in discard pile.
      game.cellsAvailable > 1 &&
      game.funding < 8 &&
      cardFoundIn(FromDrawPile::Nil, AbdelFattahel_Sisi)

    case Jihadist => 
      true
  }

  // Carry out the event for the given role.
  // forTrigger will be true if the event was triggered during the human player's turn
  // and it associated with the Bot player.
  override
  def executeEvent(role: Role): Unit = {
    if (role == US) {
      if (getSunniUSCandidates.nonEmpty) {
        val (target, (cell, sadr)) = if (isHuman(role)) {
          val t = askCountry("Remove cell from which Sunni country: ", getSunniUSCandidates)
          (t, askCells(t, 1, true))
        }
        else {
          val t = USBot.disruptPriority(getSunniUSCandidates).get
          (t, USBot.chooseCellsToRemove(t, 1))
        }
        addEventTarget(target)
        removeCellsFromCountry(target, cell, sadr, addCadre = true)
      }

      if (getShiaUSCandidates.nonEmpty) {
        val (target, (cell, sadr)) = if (isHuman(role)) {
          val t = askCountry("Remove cell from which Shia-Mix country: ", getShiaUSCandidates)
          (t, askCells(t, 1, true))
        }
        else {
          val t = USBot.disruptPriority(getShiaUSCandidates).get
          (t, USBot.chooseCellsToRemove(t, 1))
        }
        addEventTarget(target)
        removeCellsFromCountry(target, cell, sadr, addCadre = true)
      }

      println()
      decreaseFunding(1)
      addGlobalEventMarker(PoliticalIslamismUS)
    }
    else { // Jihadist
      val sunniCandidates = countryNames(game.muslims.filter(_.isSunni))
      val shiaCandidates = countryNames(game.muslims.filter(_.isShiaMix))
      if (sunniCandidates.nonEmpty && game.cellsAvailable > 0) {
        val target = if (isHuman(role))
          askCountry("Place a cell in which Sunni country: ", sunniCandidates)
        else
          JihadistBot.cellPlacementPriority(false)(sunniCandidates).get

        addEventTarget(target)
        addSleeperCellsToCountry(target, 1)
      }

      if (shiaCandidates.nonEmpty && game.cellsAvailable > 0) {
        val target = if (isHuman(role))
          askCountry("Place a cell in which Shia-Mix country: ", shiaCandidates)
        else
          JihadistBot.cellPlacementPriority(false)(shiaCandidates).get

        addEventTarget(target)
        addSleeperCellsToCountry(target, 1)
      }

      println()
      increaseFunding(1)
      addGlobalEventMarker(PoliticalIslamismJihadist)
    }
  }
}
