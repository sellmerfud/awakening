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
// Remove 1 Cell from a Civil War country and place UNSCR 1973 there.
// UNSCR 1973 is 1 Troop and by itself does not suffer Prestige loss
// from Jihadist Operations. Remove once Civil War ends or when played
// again elsewhere.
// ------------------------------------------------------------------
object Card_150 extends Card(150, "UNSCR 1973", US, 2, NoRemove, NoLapsing, NoAutoTrigger) {
  // Used by the US Bot to determine if the executing the event would alert a plot
  // in the given country
  override
  def eventAlertsPlot(countryName: String, plot: Plot): Boolean = false

  // Can play in country that already has the marker if it also contains at least one cell.
  // Or in any other civil war country that does not have the maker.
  def getCandidates() = countryNames(
    game.muslims.filter { m =>
      (m.hasMarker(UNSCR_1973) && m.totalCells > 0) || (m.civilWar && !m.hasMarker(UNSCR_1973))
    }
  )
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
  def botWillPlayEvent(role: Role): Boolean = true

  // Carry out the event for the given role.
  // forTrigger will be true if the event was triggered during the human player's turn
  // and it associated with the Bot player.
  override
  def executeEvent(role: Role): Unit = {
    val name = if (isHuman(role))
      askCountry("Place UNSCR 1973 in which country: ", getCandidates())
    else if (eventRemovesLastCell())
      getCandidates().find(name => game.getMuslim(name).totalCells > 0).get
    else
      USBot.unscr1973Target(getCandidates()).get

    addEventTarget(name)
    val country = game.getMuslim(name)
    // If the target already contains the marker, then
    // we only remove a cell.
    val sameCountry = country.hasMarker(UNSCR_1973)

    if (!sameCountry) {
      // If marker is already on the map, remove it first.
      game.muslims.find(_.hasMarker(UNSCR_1973)) foreach { c =>
        removeEventMarkersFromCountry(c.name, UNSCR_1973)
      }
    }

    if (country.totalCells > 0) {
      val (actives, sleepers, sadr) = if (isHuman(role))
        askCells(name, 1, sleeperFocus = true)
      else
        USBot.chooseCellsToRemove(name, 1)
      removeCellsFromCountry(name, actives, sleepers, sadr, addCadre = true)
    }

    if (!sameCountry)
      addEventMarkersToCountry(name, UNSCR_1973)
  }
}
