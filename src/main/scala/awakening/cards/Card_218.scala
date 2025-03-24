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
// Play in a Civil War or Regime Change country where there are
// Cells and Militia.
// If one side has more Cells or Militia than the other, remove the
// difference so both sides are equal. Troops unaffected.
// ------------------------------------------------------------------
object Card_218 extends Card(218, "Al-Nusra Front", Unassociated, 2, NoRemove, NoLapsing, NoAutoTrigger) {
  // Used by the US Bot to determine if the executing the event would alert a plot
  // in the given country
  override
  def eventAlertsPlot(countryName: String, plot: Plot): Boolean = false

  // Used by the US Bot to determine if the executing the event would remove
  // the last cell on the map resulting in victory.
  override
  def eventRemovesLastCell(): Boolean = false

  val isCandidate = (m: MuslimCountry) =>
      (m.civilWar || m.inRegimeChange) && m.totalCells > 0 && m.militia > 0

  val isUSCandidate = (m: MuslimCountry) =>
    isCandidate(m) && m.totalCells > m.militia

  val isJihadistCandidate = (m: MuslimCountry) =>
    isCandidate(m) && m.militia - m.totalCells > 0

  val isEnhJihadistCandidate = (m: MuslimCountry) =>
    isCandidate(m) && m.militia - m.totalCells > 1

  def getCandidates = countryNames(game.muslims.filter(isCandidate))

  def getUSCandidates = countryNames(game.muslims.filter(isUSCandidate))

  // Enh Bot will only play if it would result in removing 2 or more militia
  def getJihadistCandidates = countryNames(game.muslims.filter(isJihadistCandidate))

  def getEnhJihadistCandidates = countryNames(game.muslims.filter(isEnhJihadistCandidate))

  // Returns true if the printed conditions of the event are satisfied
  override
  def eventConditionsMet(role: Role) = getCandidates.nonEmpty

  // Returns true if the Bot associated with the given role will execute the event
  // on its turn.  This implements the special Bot instructions for the event.
  // When the event is triggered as part of the Human players turn, this is NOT used.
  override
  def botWillPlayEvent(role: Role): Boolean = role match {
    case US => getUSCandidates.nonEmpty
    case Jihadist if game.botEnhancements => getEnhJihadistCandidates.nonEmpty
    case Jihadist => getJihadistCandidates.nonEmpty
  }

  // Carry out the event for the given role.
  // forTrigger will be true if the event was triggered during the human player's turn
  // and it associated with the Bot player.
  override
  def executeEvent(role: Role): Unit = {
    // See Event Instructions table
    val target = role match {
      case _ if isHuman(role) =>
        askCountry("Select country: ", getCandidates)
      case US =>
        USBot.disruptPriority(USBot.highestCellsMinusTandM(getUSCandidates)).get
      case Jihadist if game.botEnhancements =>
        // Priority to MJP, then Poor, then highest Res, then most militia.
        val priorites = List(
          JihadistBot.IsMajorJihadPriority,
          JihadistBot.PoorPriority,
          JihadistBot.HighestResourcePriority,
          JihadistBot.MostMilitiaPriority
        )
        JihadistBot.topPriority(game.getCountries(getEnhJihadistCandidates), priorites)
          .map(_.name)
          .get
      case Jihadist =>
        JihadistBot.minorJihadTarget(getJihadistCandidates).get
    }

    addEventTarget(target)
    val m = game.getMuslim(target)
    if (m.totalCells > m.militia) {
      val (actives, sleepers, sadr) = if (isHuman(role))
        askCells(target, m.totalCells - m.militia, sleeperFocus = true)
      else
        USBot.chooseCellsToRemove(target, m.totalCells - m.militia)
      removeCellsFromCountry(target, actives, sleepers, sadr, addCadre = true)
    }
    else if (m.militia > m.totalCells)
      removeMilitiaFromCountry(target, m.militia - m.totalCells)
    else
      log(s"\nEqual number of cells and militia in $target.  The event has no effect", Color.Event)
  }
}
