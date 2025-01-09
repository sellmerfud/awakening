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

// Card Text:
// ------------------------------------------------------------------
// Play if any cadres, cells, or plots in US, UK, or Canada.
// Remove them all (place no cadres).
// Draw a card.
// Can be blocked by Leak . 
// ------------------------------------------------------------------
object Card_031 extends Card(31, "Wiretapping", US, 2, NoRemove, NoLapsing, NoAutoTrigger) {
  val PotentialTargets = List(UnitedStates, UnitedKingdom, Canada)
  def getCandidates() = countryNames(
    game.getCountries(PotentialTargets) filter { c =>
      c.hasCadre || c.totalCells > 0 || c.plots.nonEmpty
    }
  )

  // Used by the US Bot to determine if the executing the event would alert a plot
  // in the given country
  override
  def eventAlertsPlot(countryName: String, plot: Plot): Boolean = PotentialTargets.contains(countryName)

  // Used by the US Bot to determine if the executing the event would remove
  // the last cell on the map resulting in victory.
  override
  def eventRemovesLastCell(): Boolean = {
    val totalCells = (getCandidates().map(name => game.getCountry(name).totalCells)).sum
    totalCells > 0 && totalCells == game.totalCellsOnMap
  }

  // Returns true if the printed conditions of the event are satisfied
  override
  def eventConditionsMet(role: Role) =
    globalEventNotInPlay(LeakWiretapping) &&
    getCandidates().nonEmpty

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
    for (name <- getCandidates(); c = game.getCountry(name)) {
      addEventTarget(name)
      removeCadreFromCountry(name)
      removeCellsFromCountry(name, c.activeCells, c.sleeperCells, c.hasSadr, addCadre = false)
      for (plot <- c.plots)
        performAlert(name, plot)
    }
    
    if (isHuman(role))
      log(s"$US draws one card and adds it to their hand")
    else
      log(s"Add one card to the top of the $US Bot hand")
    askCardsDrawn(US, 1, FromDrawPile::Nil)
    addGlobalEventMarker(Wiretapping)
  }
}
