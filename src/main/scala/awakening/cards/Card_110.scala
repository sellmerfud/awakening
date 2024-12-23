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
// Play if either Iraq, Syria, Lebanon, or Jordan has troops.
// If US play, +3 Prestige, REMOVE this card.
// If jihadist, place 3 cells and a Plot 2 there.
// ------------------------------------------------------------------
object Card_110 extends Card2(110, "Zarqawi", Unassociated, 2, USRemove, NoLapsing, NoAutoTrigger) {
  // Used by the US Bot to determine if the executing the event would alert a plot
  // in the given country
  override
  def eventAlertsPlot(countryName: String, plot: Plot): Boolean = false

  // Used by the US Bot to determine if the executing the event would remove
  // the last cell on the map resulting in victory.
  override
  def eventRemovesLastCell(): Boolean = false

  def getCandidates() = List(Iraq, Syria, Lebanon, Jordan)
    .filter(name => game.getMuslim(name).totalTroops > 0)

  // Returns true if the printed conditions of the event are satisfied
  override
  def eventConditionsMet(role: Role) = getCandidates().nonEmpty

  // Returns true if the Bot associated with the given role will execute the event
  // on its turn.  This implements the special Bot instructions for the event.
  // When the event is triggered as part of the Human players turn, this is NOT used.
  override
  def botWillPlayEvent(role: Role): Boolean = role match {
    case US => game.prestige < 12
    case Jihadist => game.cellsAvailable > 0 || game.availablePlots.contains(Plot2)
  }

  // Carry out the event for the given role.
  // forTrigger will be true if the event was triggered during the human player's turn
  // and it associated with the Bot player.
  override
  def executeEvent(role: Role, forTrigger: Boolean): Unit =
    if (role == US)
      increasePrestige(3)
    else { // Jihadist
      val candidates = getCandidates()
      val canWinByDeclaringCaliphate =
        game.cellsAvailable >= 3 &&
        game.islamistResources == 5 &&
        JihadistBot.caliphatePriorityTarget(candidates).nonEmpty
      // Can create Caliphate (only in Iraq,Syria,Lebanon,orJordan)
      // If the Jihadist Bot can win by declaring a caliphate then it will
      // do so, otherwise it will prioritze by Plot placement if possible.
      val name = if (isHuman(role))
        askCountry("Select country: ", candidates)
      else if (game.availablePlots.contains(Plot2) && !canWinByDeclaringCaliphate)
        JihadistBot.plotPriority(candidates).get
      else
        JihadistBot.cellPlacementPriority(true)(candidates).get

      addEventTarget(name)
      val num = 3 min game.cellsAvailable
      testCountry(name)
      addSleeperCellsToCountry(name, num)
      if (choosesToDeclareCaliphate(role, num, name))
        declareCaliphate(name)

      if (game.availablePlots.contains(Plot2)) {
        testCountry(name)
        addAvailablePlotToCountry(name, Plot2, visible = true)
      }
    }
}
