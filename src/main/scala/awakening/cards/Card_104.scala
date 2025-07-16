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
import awakening. { USBot, JihadistBot }

// Card Text:
// ------------------------------------------------------------------
// Select and test a Shia-Mix country.
// If US play, remove 1 cell there or from Iran.
// If jihadist, make 2 jihad rolls there. Ignore failures or shift to Islamist Rule.
// ------------------------------------------------------------------
object Card_104 extends Card(104, "Iran", Unassociated, 2, NoRemove, NoLapsing, NoAutoTrigger) {
  // Used by the US Bot to determine if the executing the event would alert a plot
  // in the given country
  override
  def eventAlertsPlot(countryName: String, plot: Plot): Boolean = false

  def getBotCellCandidates = {
    val iran = game.getCountry(Iran)
    val iranList = if (!iran.truce && iran.totalCells > 0)
      List(Iran)
    else
      Nil
    iranList ::: countryNames(
      game.muslims.filter(m => !m.truce && m.isShiaMix && m.totalCells > 0) // Bot only cares if cell can be removed
    )
  }

  def getBotJihadCandidates = {
    val jihadTest   = (m: MuslimCountry) =>
      !m.truce &&
      m.isShiaMix &&
      (m.isGood || m.isFair )
    countryNames(game.muslims.filter(jihadTest))
  }

  // Used by the US Bot to determine if the executing the event would remove
  // the last cell on the map resulting in victory.
  override
  def eventRemovesLastCell(): Boolean =
    getBotCellCandidates.exists (name => USBot.wouldRemoveLastCell(name, 1))

  // Returns true if the printed conditions of the event are satisfied
  override
  def eventConditionsMet(role: Role) = true

  // Returns true if the Bot associated with the given role will execute the event
  // on its turn.  This implements the special Bot instructions for the event.
  // When the event is triggered as part of the Human players turn, this is NOT used.
  override
  def botWillPlayEvent(role: Role): Boolean = role match {
    case US =>
      getBotCellCandidates.nonEmpty
    case Jihadist if game.botEnhancements =>
      getBotJihadCandidates.nonEmpty
    case Jihadist => 
      getBotJihadCandidates.nonEmpty
  }

  def shixMixCandidates =
    countryNames(game.muslims.filter(_.isShiaMix))

  def jihadStandardBotTarget(names: List[String]): Option[String] = {
    import JihadistBot._
    val priorities = List(GoodPriority, FairPriority) ::: minorJihadPriorities()
    botLog("Find \"Iran\" target", Color.Debug)
    topPriority(game.getMuslims(names), priorities).map(_.name)
  }
  
  def jihadEnhBotTarget(names: List[String]): Option[String] = {
    import JihadistBot._
    val priorities = List(
      new CriteriaFilter("Fair, Regime Change", muslimTest(m => m.isFair && m.inRegimeChange)),
      GoodPriority,
      FairPriority,
      HighestPrintedResourcePriority,
      WithAidPriority,
    )
    botLog("Find \"Iran\" target", Color.Debug)
    topPriority(game.getMuslims(names), priorities).map(_.name)
  }

  // Carry out the event for the given role.
  // forTrigger will be true if the event was triggered during the human player's turn
  // and it associated with the Bot player.
  override
  def executeEvent(role: Role): Unit = role match {
    case US if isHuman(role) =>
      val iranHasCell = game.getCountry(Iran).totalCells > 0
      val target = askCountry("Select a Shia-Mix country: ", shixMixCandidates)
      val targetHasCell = game.getCountry(target).totalCells > 0
      addEventTarget(target)
      testCountry(target)  // Event specifically says to test
      val removeCellFrom = (iranHasCell, targetHasCell) match {
        case (true, false) => Iran
        case (false, true) => target
        case (false, false) => ""
        case _ =>
          val choices = List(target -> target, Iran -> Iran)
          askMenu("Remove cell from:", choices).head
      }

      if (removeCellFrom != "") {
        val (active, sleeper, sadr) = askCells(removeCellFrom, 1, sleeperFocus = true)
        removeCellsFromCountry(removeCellFrom, active, sleeper, sadr, addCadre = true)
      }
      else
        log(s"\nThere is no cell to remove in either $target or $Iran.", Color.Event)

    case US if getBotCellCandidates.nonEmpty => // Bot
        val name = USBot.disruptPriority(getBotCellCandidates).get
        val (active, sleeper, sadr) = USBot.chooseCellsToRemove(name, 1)
        addEventTarget(name)
        testCountry(name) // Event specifically says to test
        removeCellsFromCountry(name, active, sleeper, sadr, addCadre = true)

    case US => // Should never get here!
      log("\nThere are cells in Shia-Mix countries or in Iran.  The event has no effect.", Color.Event)

    case Jihadist =>
      val name = if (isHuman(role))
        askCountry("Select a Shia-Mix country: ", shixMixCandidates)
      else if (game.botEnhancements)
        jihadEnhBotTarget(getBotJihadCandidates).get
      else
        jihadStandardBotTarget(getBotJihadCandidates).get

      addEventTarget(name)
      testCountry(name) // Event specifically says to test
      performJihads(JihadTarget(name, false, 0, 0, false, phantoms = 2, ignoreFailures = true)::Nil)
  }
}
