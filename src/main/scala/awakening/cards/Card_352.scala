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
// US Play if more Troops + Milita than Cells in a Civil War Country.
// If US play, + 2 Prestige
// -1 Funding REMOVE
//
// If Jibadist: 3 additional Cells (5 if Caliphate Capital on Map or
// being created with this action) are available for Recruit while in
// the 9 box of Ample Funding (11.3.7). Place 3 Cells in a Poor
// Muslim country. Remove marker and capability if al-Baghdadi is
// removed by US play. MARK
// ------------------------------------------------------------------
object Card_352 extends Card(352, "al-Baghdadi", Unassociated, 3, USRemove, NoLapsing, NoAutoTrigger) {
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
  def eventConditionsMet(role: Role) = role match {
    case US => game.hasMuslim(m => m.civilWar && m.totalTroopsAndMilitia > m.totalCells)
    case Jihadist => true
  }

  def placeCellsCandidates() = countryNames(game.muslims.filter(m => !m.truce && m.isPoor))

  // Returns true if the Bot associated with the given role will execute the event
  // on its turn.  This implements the special Bot instructions for the event.
  // When the event is triggered as part of the Human players turn, this is NOT used.
  override
  def botWillPlayEvent(role: Role): Boolean = role match {
    case US =>
      true
    case Jihadist =>
      globalEventNotInPlay(AlBaghdadi) || (game.cellsAvailable > 0 && placeCellsCandidates().nonEmpty)
  }

  // Carry out the event for the given role.
  // forTrigger will be true if the event was triggered during the human player's turn
  // and it associated with the Bot player.
  override
  def executeEvent(role: Role): Unit = {
    if (role == US) {
      increasePrestige(2)
      decreaseFunding(1)
      removeGlobalEventMarker(AlBaghdadi)
    }
    else {  // role == Jihadist
      playExtraCellsEvent(AlBaghdadi)
      if (game.cellsAvailable > 0 && placeCellsCandidates().nonEmpty) {
        val target = if (isHuman(role))
          askCountry("Place cells in which country: ", placeCellsCandidates())
        else if (game.cellsAvailable >= 3 && (game.botEnhancements || game.islamistResources == 5)) {
          // If we are placing 3 cells and we can declare caliphate then
          // select that country
          JihadistBot.cellPlacementPriority(true)(placeCellsCandidates()).get
        }
        else
          JihadistBot.cellPlacementPriority(false)(placeCellsCandidates()).get

        val cellsToAdd = game.cellsAvailable min 3
        addEventTarget(target)
        addSleeperCellsToCountry(target, cellsToAdd)

        if (jihadistChoosesToDeclareCaliphate(target, cellsToAdd))
          declareCaliphate(target)
      }
    }
  }
}
