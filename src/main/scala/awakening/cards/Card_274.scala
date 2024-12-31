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
// Play in a Civil War country.
// Place two Militia there.
// Then, roll 1 die. If the roll is less than or equal to the number
// of Militia + Troops - Cells, end Civil War there.
// If Civil War ends, +1 Prestige.
// Cannot be played in a caliphate country.
// ------------------------------------------------------------------
object Card_274 extends Card2(274, "Government of National Accord", US, 3, NoRemove, NoLapsing, NoAutoTrigger) {
  // Used by the US Bot to determine if the executing the event would alert a plot
  // in the given country
  override
  def eventAlertsPlot(countryName: String, plot: Plot): Boolean = false

  // Used by the US Bot to determine if the executing the event would remove
  // the last cell on the map resulting in victory.
  override
  def eventRemovesLastCell(): Boolean = false

  def getCandidates() = countryNames(
    game.muslims.filter(m => m.civilWar && !game.isCaliphateMember(m.name))
  )

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
  def executeEvent(role: Role, forTrigger: Boolean): Unit = {
    val numMilitia = game.militiaAvailable min 2
    val candidates = getCandidates()
    val target = if (isHuman(role))
      askCountry("Which Civil War country: ", getCandidates())
    else
      USBot.deployToPriority(getCandidates()).get

    log(s"\n$US chooses to target $target", Color.Event)
    if (numMilitia > 0)
      addMilitiaToCountry(target, numMilitia)
    else
      log(s"There are no availabe militia to place in $target.", Color.Event)

    val m = game.getMuslim(target)
    lazy val die = getDieRoll(s"Enter event die roll: ")

    (m.totalTroopsAndMilitia - m.totalCells) match {
      case x if x < 1 =>
        log("\nThere are less Troops and Militia than Cells, no die roll necessary.", Color.Event)

      case x if die <= x  =>
        log(s"Troops + Militia - Cells = $x")
        log(s"Die roll: $die, success!")
        log("The Civil War ends.")
        endCivilWar(target)
        increasePrestige(1)

      case x =>
        log(s"Troops + Militia - Cells = $x")
        log(s"Die roll: $die, failure")
        log("The Civil War does not end.")
    }
  }
}
