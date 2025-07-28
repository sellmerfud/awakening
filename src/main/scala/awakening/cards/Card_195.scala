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
// Play if able to Remove 3 Cells in a Civil War or Regime Change country.
// Remove 3 Cells. Place any 2 Available Plots (non-WMD) and make 2 Jihad
// rolls there. Ignore failures or shifts to Islamist Rule.
// ------------------------------------------------------------------
object Card_195 extends Card(195, "Taliban Resurgent", Jihadist, 3, NoRemove, NoLapsing, NoAutoTrigger) {
  // Used by the US Bot to determine if the executing the event would alert a plot
  // in the given country
  override
  def eventAlertsPlot(countryName: String, plot: Plot): Boolean = false

  // Used by the US Bot to determine if the executing the event would remove
  // the last cell on the map resulting in victory.
  override
  def eventRemovesLastCell(): Boolean = false

  val isCandidate = (m: MuslimCountry) =>
    !m.truce &&
    (m.civilWar || m.inRegimeChange) &&
    m.totalCells >= 3

  def getCandidates = countryNames(game.muslims.filter(isCandidate))

  // Playable in [Fair countries] OR [Poor countries with 5+ cells AND Funding<8]. Priority to Fair, then RC.
  def enhBotCandidates = game.muslims
    .filter { m =>
      isCandidate(m) &&
      m.isFair || (m.isPoor && m.pieces.totalCells >= 5 && game.funding < 8)
    }

  // Returns true if the printed conditions of the event are satisfied
  override
  def eventConditionsMet(role: Role) = getCandidates.nonEmpty

  def standardBotTarget: Option[String] = {
    val priorities = List(
      JihadistBot.GoodPriority,
      JihadistBot.FairPriority,
      new JihadistBot.CriteriaFilter("Poor with Troops and US Prestige > 1",
           JihadistBot.muslimTest(m => m.isPoor && game.prestige > 1)))

    val candidates = countryNames(JihadistBot.selectCandidates(game.getCountries(getCandidates), priorities))
    JihadistBot.minorJihadTarget(candidates)
  }

  def enhancedBotTarget: Option[String] = {
    val priorities = List(
      JihadistBot.FairPriority,
      JihadistBot.RegimeChangePriority,
    )

    JihadistBot.topPriority(enhBotCandidates, priorities).map(_.name)
  }

  // Returns true if the Bot associated with the given role will execute the event
  // on its turn.  This implements the special Bot instructions for the event.
  // When the event is triggered as part of the Human players turn, this is NOT used.
  override
  def botWillPlayEvent(role: Role): Boolean = if (game.botEnhancements)
    enhancedBotTarget.nonEmpty
  else
    standardBotTarget.nonEmpty

  // Carry out the event for the given role.
  // forTrigger will be true if the event was triggered during the human player's turn
  // and it associated with the Bot player.
  override
  def executeEvent(role: Role): Unit = {
    val (target, plots, (actives, sleepers, sadr)) = if (isHuman(role)) {
      val name = askCountry("Select country: ", getCandidates)
      val plots = askPlots(game.availablePlots.filterNot(_ == PlotWMD), 2)
      val m = game.getMuslim(name)
      if (m.totalCells == 3)
        (name, plots, (m.pieces.activeCells, m.pieces.sleeperCells, m.hasSadr))
      else {
        println("\nChoose cells to remove:")
        (name, plots, askCells(name, 3, sleeperFocus = false))
      }
    }
    else {
      // If event triggered during US turn then preferred target may not exist.
      // Enhanced Bot falls back to standard bot if necessary
      val target = if (game.botEnhancements)
        (enhancedBotTarget orElse standardBotTarget orElse JihadistBot.minorJihadTarget(getCandidates)).get
      else
        (standardBotTarget orElse JihadistBot.minorJihadTarget(getCandidates)).get

      val plots = JihadistBot.selectPlotMarkers(target, 2, game.availablePlots.filterNot(_ == PlotWMD))
      (target, plots, JihadistBot.chooseCellsToRemove(target, 3))
    }

    addEventTarget(target)
    removeCellsFromCountry(target, actives, sleepers, sadr, addCadre = true)
    if (plots.isEmpty)
      log(s"\nThere are no available non-WMD plots to place in $target.", Color.Event)
    else
      plots.foreach(p => addAvailablePlotToCountry(target, p))

    // It is possible that the country is no longer a Muslim country.
    // For example: Target is Nigeria and we jsut removed the last cell (rule 11.3.3.3)
    if (game.isMuslim(target))
      performJihads(JihadTarget(target, false, 0, 0, false, phantoms = 2, ignoreFailures = true)::Nil)
  }
}
