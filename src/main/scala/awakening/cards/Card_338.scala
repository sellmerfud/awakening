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
// If US play: Remove a Cell.
// + 1 Prestige
// REMOVE
// If Jihadist play: Place up to 3 Cells in a single Muslim country.
// ------------------------------------------------------------------
object Card_338 extends Card(338, "Abu Muhammad al-Shimali", Unassociated, 2, USRemove, NoLapsing, NoAutoTrigger) {
  // Used by the US Bot to determine if the executing the event would alert a plot
  // in the given country
  override
  def eventAlertsPlot(countryName: String, plot: Plot): Boolean = false

  def removeCellCandidates = countryNames(game.countries.filter(c => !c.truce && c.totalCells > 0))

  def placeCellCandidates = countryNames(game.muslims.filter(!_.truce))

  // Used by the US Bot to determine if the executing the event would remove
  // the last cell on the map resulting in victory.
  override
  def eventRemovesLastCell(): Boolean =
    removeCellCandidates.exists(name => USBot.wouldRemoveLastCell(name, 1))


  // Returns true if the printed conditions of the event are satisfied
  override
  def eventConditionsMet(role: Role) = true

  override
  def eventWouldResultInVictoryFor(role: Role): Boolean = role match {
    case Jihadist =>
      !game.caliphateDeclared &&
      placeCellCandidates.nonEmpty &&
      game.cellsAvailable >= 3
      game.islamistResources == 5 &&
      (game.islamistAdjacency || isBot(Jihadist))

    case _ => false
  }

  def enhGoodMuslimWithoutTroopsTarget(candidates: List[String]): Option[String] = {
    val goodMuslims = candidates
      .map(game.getMuslim)
      .filter(m => m.isGood && m.totalTroops == 0)

    JihadistBot.topPriority(goodMuslims, JihadistBot.HighestPrintedResourcePriority::Nil)
      .map(_.name)
  }


  // Returns true if the Bot associated with the given role will execute the event
  // on its turn.  This implements the special Bot instructions for the event.
  // When the event is triggered as part of the Human players turn, this is NOT used.
  override
  def botWillPlayEvent(role: Role): Boolean = role match {
    case US =>
      removeCellCandidates.nonEmpty || game.prestige < MaxPrestige
    case Jihadist if game.botEnhancements =>      
      game.cellsAvailable > 1  // Playable if 2+ cells available. Priority to MJP.
    case Jihadist =>
      game.cellsAvailable > 0
  }

  // Carry out the event for the given role.
  // forTrigger will be true if the event was triggered during the human player's turn
  // and it associated with the Bot player.
  override
  def executeEvent(role: Role): Unit = {
    if (role == US) {
      if (removeCellCandidates.nonEmpty) {
        val (target, (actives, sleepers, sadr)) = if (isHuman(role)) {
          val t = askCountry("Remove a Cell from which country: ", removeCellCandidates)
          (t, askCells(t, 1, true))
        }
        else {
          val t = USBot.disruptPriority(removeCellCandidates).get
          (t, USBot.chooseCellsToRemove(t, 1))
        }

        addEventTarget(target)
        removeCellsFromCountry(target, actives, sleepers, sadr, addCadre = true)
      }
      else
        log("\nThere are no cells on the map to remove.", Color.Event)

      increasePrestige(1)
    }
    else if (game.cellsAvailable == 0)
      log("\nThere are no available cells.  The event has not effect.", Color.Event)
    else { // Jihadist
      val maxCells = game.cellsAvailable min 3
      val (target, num) = if (isHuman(role)) {
        val name = askCountry("Place cells in which country: ", placeCellCandidates)
        val num  = askInt("Place how many cells", 1, maxCells, Some(maxCells))
        (name, num)
      }
      else {
        // If we are placing 3 cells and we can declare caliphate for the win
        // then select that country
        val botTarget = if (maxCells == 3 && JihadistBot.possibleToDeclareCaliphate(placeCellCandidates))
            JihadistBot.cellPlacementPriority(true)(placeCellCandidates).get
        else if (game.botEnhancements && enhGoodMuslimWithoutTroopsTarget(placeCellCandidates).nonEmpty)
          enhGoodMuslimWithoutTroopsTarget(placeCellCandidates).get
        else if (game.botEnhancements && placeCellCandidates.exists(name => Some(name) == JihadistBot.majorJihadPriorityCountry))
          JihadistBot.majorJihadPriorityCountry.get
        else
          JihadistBot.cellPlacementPriority(false)(placeCellCandidates).get

        (botTarget, maxCells)

      }

      addEventTarget(target)
      addSleeperCellsToCountry(target, num)
      if (jihadistChoosesToDeclareCaliphate(target, num))
        declareCaliphate(target)
    }
  }
}
