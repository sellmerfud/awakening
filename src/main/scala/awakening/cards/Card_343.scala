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
// US play if World Posture same as US (including 0).
// Jihadist play if Iran Adversary or Special Case and has a Cell.
//
// If US play: Remove 1 unavailable WMD from Iran (+1 Prestige if WMD
// removed, 11.3.1).
//
// If Jihadist: Roll a die:
//  (1-3) Cell removed and place a WMD in available.
//       If Iran is a Special Case country, flip Iran Country mat to Shia-Mix.
//       Set to Fair Adversary.
// (4-6) no effect.
// REMOVE
// ------------------------------------------------------------------
object Card_343 extends Card(343, "JCPOA", Unassociated, 2, JihadistRemove, NoLapsing, NoAutoTrigger) {
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
    case US =>
      game.worldPosture == game.usPosture || game.worldPosture == Even
    case Jihadist =>
      (isIranSpecialCase || game.getMuslim(Iran).isAdversary) && game.getCountry(Iran).totalCells > 0
  }

  // Returns true if the Bot associated with the given role will execute the event
  // on its turn.  This implements the special Bot instructions for the event.
  // When the event is triggered as part of the Human players turn, this is NOT used.
  override
  def botWillPlayEvent(role: Role): Boolean = role match {
    case US => game.getCountry(Iran).wmdCache > 0
    case Jihadist => isIranSpecialCase || game.getCountry(Iran).wmdCache > 0
  }

  // Carry out the event for the given role.
  // forTrigger will be true if the event was triggered during the human player's turn
  // and it associated with the Bot player.
  override
  def executeEvent(role: Role): Unit = {
    if (role == US) {
      addEventTarget(Iran)
      if (game.getCountry(Iran).wmdCache > 0)
        removeCachedWMD(Iran, 1)
      else
        log("\nThere is no unavailable WMD in Iran.", Color.Event)
    }
    else { // Jihadist
      addEventTarget(Iran)
      val die = getDieRoll(s"Enter event die roll: ")
      log(s"\nDie roll: $die", Color.Event)
      if (die < 4) {
        val (actives, sleepers, sadr) = if (isHuman(role))
          askCells(Iran, 1, false)
        else
          JihadistBot.chooseCellsToRemove(Iran, 1)

        removeCellsFromCountry(Iran, actives, sleepers, sadr, addCadre = true)

        if (game.getCountry(Iran).wmdCache > 0)
          moveWMDCacheToAvailable(Iran, 1)

        if (isIranSpecialCase) {
          flipIranToShiaMixMuslim()
          setGovernance(Iran, Fair, Some(Adversary))
        }
      }
      else
        log("No Effect.", Color.Event)
    }
  }
}
