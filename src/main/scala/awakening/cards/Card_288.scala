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
// Play if Caliphate Capital on map.
// Roll a die and do as follows:
// 1) Place a Cell in the US
// 2) Place a level 1 Plot in a random Schengen country
// 3) Place a random Reaction marker
// 4) Set the posture of a Fair non-Muslim country
// 5) Remove an Aid marker
// 6) Place a Besieged Regime marker
// ------------------------------------------------------------------
object Card_288 extends Card(288, "Soldiers of the Caliphate", Jihadist, 1, NoRemove, NoLapsing, NoAutoTrigger) {
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
  def eventConditionsMet(role: Role) = game.caliphateDeclared

  // Returns true if the Bot associated with the given role will execute the event
  // on its turn.  This implements the special Bot instructions for the event.
  // When the event is triggered as part of the Human players turn, this is NOT used.
  override
  def botWillPlayEvent(role: Role): Boolean = true

  val resultMsgs = Vector(
    "Place a Cell in the US",
    "Place a level 1 Plot in a random Schengen country",
    "Place a random Reaction marker",
    "Set the posture of a Fair non-Muslim country",
    "Remove an Aid marker",
    "Place a Besieged Regime marker",
  )
  // Carry out the event for the given role.
  // forTrigger will be true if the event was triggered during the human player's turn
  // and it associated with the Bot player.
  override
  def executeEvent(role: Role): Unit = {
    val die = getDieRoll(s"Enter event die roll: ")
    log(s"\nDie roll: $die - ${resultMsgs(die - 1)}", Color.Event)

    die match {
      case 1 =>
        if (game.cellsAvailable > 0) {
          addEventTarget(UnitedStates)
          addSleeperCellsToCountry(UnitedStates, 1)
        }
        else
          log("\nThere are no cells on the funding track.", Color.Event)

      case 2 =>
        if (game.availablePlots.contains(Plot1)) {
          val target = randomSchengenCountry.name
          addEventTarget(target)
          addAvailablePlotToCountry(target, Plot1, visible = true)
        }
        else
          log("\nThere are no available level 1 plots.", Color.Event)

      case 3 =>
          val target = randomConvergenceTarget.name
          addEventTarget(target)
          addReactionMarker(target)

      case 4 =>
        val candidates = countryNames(game.nonMuslims.filter(_.isFair))
        if (candidates.nonEmpty) {
          val (target, posture) = if (isHuman(role)) {
            val name = askCountry("Set the posture of which Fair country: ", candidates)
            (name, askPosture(name))
          }
          else
            (JihadistBot.posturePriority(candidates).get, oppositePosture(game.usPosture))

          addEventTarget(target)
          setCountryPosture(target, posture)
        }
        else
          log("\nThere are no Fair Non-Muslim countries.", Color.Event)

      case 5 =>
        val candidates = countryNames(game.muslims.filter(_.aidMarkers > 0))
        if (candidates.nonEmpty) {
          val target = if (isHuman(role))
            askCountry("Remove Aid marker from which country: ", candidates)
          else
            JihadistBot.markerTarget(candidates).get

          addEventTarget(target)
          removeAidMarker(target)
        }
        else
          log("\nThere are no Aid markers on the map.", Color.Event)

      case _ => // 6
        val candidates = countryNames(game.muslims.filter(_.canTakeBesiegedRegimeMarker))
        if (candidates.nonEmpty) {
          val target = if (isHuman(role))
            askCountry("Place a Besieged Regime marker in which country: ", candidates)
          else
            JihadistBot.markerTarget(candidates).get

          addEventTarget(target)
          addBesiegedRegimeMarker(target)
        }
        else
          log("\nThere are no countries that can take a Besieged Regime marker.", Color.Event) // Not likely
    }
  }
}
