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
// Play in a Poor, 1 or 2 Resource African country.
// If US play, place Serval there. Serval is 1 Troop. Remove if played
// again elsewhere. Also place 1 Militia.
// If Jibadist, place in Civil War and place 1 Cell (2 if Mali).
// ------------------------------------------------------------------
object Card_226 extends Card(226, "Operation Serval", Unassociated, 2, NoRemove, NoLapsing, NoAutoTrigger) {
  // Used by the US Bot to determine if the executing the event would alert a plot
  // in the given country
  override
  def eventAlertsPlot(countryName: String, plot: Plot): Boolean = false

  // Used by the US Bot to determine if the executing the event would remove
  // the last cell on the map resulting in victory.
  override
  def eventRemovesLastCell(): Boolean = false

  def getCandidates = countryNames(
    game.getCountries(African).filter {
      case m: MuslimCountry => m.resourceValue < 3 && m.isPoor
      case _ => false
    }
  )

  // Returns true if the printed conditions of the event are satisfied
  override
  def eventConditionsMet(role: Role) = getCandidates.nonEmpty

  // Returns true if the Bot associated with the given role will execute the event
  // on its turn.  This implements the special Bot instructions for the event.
  // When the event is triggered as part of the Human players turn, this is NOT used.
  override
  def botWillPlayEvent(role: Role): Boolean = role match {
    case US =>
      // Must have a serval target and can place either the
      // marker and/or militia there
      USBot.servalTarget(getCandidates)
        .map { name =>
          val target = game.getMuslim(name)
          !target.hasMarker(OperationServal) ||
          (game.militiaAvailable > 0 && target.canTakeMilitia)
        }
        .getOrElse(false)
    case Jihadist =>
      // Will play if it can place a cell or start a Civil War
      game.cellsAvailable > 0 ||
      JihadistBot.minorJihadTarget(getCandidates)
        .map(name => !game.getMuslim(name).civilWar)
        .getOrElse(false)
  }

  // Carry out the event for the given role.
  // forTrigger will be true if the event was triggered during the human player's turn
  // and it associated with the Bot player.
  override
  def executeEvent(role: Role): Unit = {
    // See Event Instructions table
    if (role == US) {
      val name = if (isHuman(role))
        askCountry("\nSelect country for Serval marker: ", getCandidates)
      else
        USBot.servalTarget(getCandidates).get

      // If the marker is already on the map, remove it first,
      // unless it is in the target country.

      val sameCountry = game.getMuslim(name).hasMarker(OperationServal)
      if (sameCountry)
        log(s"The Serval marker remains in $name.", Color.Event)
      else
        game.muslims
          .find(_.hasMarker(OperationServal))
          .foreach(c => removeEventMarkersFromCountry(c.name, OperationServal))

      addEventTarget(name)
      if (!sameCountry)
        addEventMarkersToCountry(name, OperationServal)
      addMilitiaToCountry(name, 1 min game.militiaAvailable)
    }
    else {  // Jihadist
      val name = if (isHuman(role))
        askCountry("Select country: ", getCandidates)
      else
        JihadistBot.minorJihadTarget(getCandidates).get

      addEventTarget(name)
      startCivilWar(name)
      val numCells = if (name == Mali) 2 else 1
      addSleeperCellsToCountry(name, numCells min game.cellsAvailable)
    }
  }
}
