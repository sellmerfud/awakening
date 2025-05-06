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
import awakening.JihadistBot

// Card Text:
// ------------------------------------------------------------------
// Test one of Canada, Scandinavia or India.
// Then, place 1 Cell in any Hard country.
// ------------------------------------------------------------------
object Card_283 extends Card(283, "Lone Wolf", Jihadist, 1, NoRemove, NoLapsing, NoAutoTrigger) {
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
  def eventConditionsMet(role: Role) = true

  // Returns true if the Bot associated with the given role will execute the event
  // on its turn.  This implements the special Bot instructions for the event.
  // When the event is triggered as part of the Human players turn, this is NOT used.
  override
  def botWillPlayEvent(role: Role): Boolean = if (game.botEnhancements)
    game.cellsAvailable > 0
  else
    true

  // Test-priority: If US hard, priority to Unmarked, then Scandinavia, then Canada.
  // If US soft, priority to Soft, then Hard, then Scandinavia, then Canada.
  def enhBotTestTarget: String = {
    val ScandinaviaPriority = new JihadistBot.CriteriaFilter("Is Scandinavia?", _.name == Scandinavia)
    val CanadaPriority = new JihadistBot.CriteriaFilter("Is Canada?", _.name == Canada)
    val candidates = game.getNonMuslims(Canada::Scandinavia::India::Nil)
    val priorities = if (game.usPosture == Hard)
      List(
        JihadistBot.UnmarkedPriority, ScandinaviaPriority, CanadaPriority)
    else
      List(
        JihadistBot.SoftPosturePriority, JihadistBot.HardPosturePriority,
        ScandinaviaPriority, CanadaPriority)

    JihadistBot.topPriority(candidates, priorities)
      .map(_.name)
      .get
  }
  
  // If US hard, in US.
  // If US soft, priority to hard country with highest governance and no cells present.
  def enhBotCellPlacementTarget: String = if (game.usPosture == Hard)
    UnitedStates
  else {
    val candidates = game.nonMuslims.filter(_.isHard)
    val priorities = List(
      new JihadistBot.CriteriaFilter("No Cells present", JihadistBot.nonMuslimTest(n => n.totalCells == 0)),
      JihadistBot.HighestGovernance,
    )

    JihadistBot.topPriority(candidates, priorities)
      .map(_.name)
      .get
  }

  // Carry out the event for the given role.
  // forTrigger will be true if the event was triggered during the human player's turn
  // and it associated with the Bot player.
  override
  def executeEvent(role: Role): Unit = {
    val testCountries = Canada::Scandinavia::India::Nil

    val testTarget = if (isHuman(role))
      askCountry("Test which country: ", testCountries)
    else if (game.botEnhancements)
      enhBotTestTarget
    else {
      // The Standard Bot will only select an untested country if the GWOT penalty is zero
      // (or if there are no tested countries)
      val (untested, tested) = game.getNonMuslims(testCountries).partition(_.isUntested)
      game.gwotPenalty match {
        case 0 => shuffle(if (untested.nonEmpty) untested else tested).head.name
        case _ => shuffle(if (tested.nonEmpty) tested else untested).head.name
      }
    }

    addEventTarget(testTarget)
    log(s"\n$Jihadist selects $testTarget to be tested.", Color.Event)
    if (game.getNonMuslim(testTarget).isUntested)
      testCountry(testTarget)
    else
      log(s"\n$testTarget is not untested so there is no effect.", Color.Event)

    val hardCountries = countryNames(game.nonMuslims.filter(_.isHard))
    if (hardCountries.isEmpty)
      log("\nThere are no Non-Muslim countries with Hard posture.", Color.Event)
    else if (game.cellsAvailable == 0)
      log("\nThere are no available cells to place on the map.", Color.Event)
    else {
      println()
      val cellTarget = if (isHuman(role))
        askCountry("Place a cell in which country: ", hardCountries)
      else if (game.botEnhancements)
        enhBotCellPlacementTarget
      else
        JihadistBot.cellPlacementPriority(false)(hardCountries).get

      addEventTarget(cellTarget)
      addSleeperCellsToCountry(cellTarget, 1)
    }
  }
}
