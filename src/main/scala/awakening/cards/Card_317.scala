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
// Play if any Shia-Mix country is in Civil War.
// Test Gulf States.
// Worsen Governance of Gulf States one level, but not to IR.
// OR Shift its Alignment one box towards Adversary.
// Blocks play of Arab NATO.
// Makes Iran, Gulf States, Saudi Arabia, and Yemen all adjacent through a Persian Gulf water link.
// MARK & REMOVE
// ------------------------------------------------------------------
object Card_317 extends Card(317, "Qatari Crisis", Jihadist, 3, Remove, NoLapsing, NoAutoTrigger) {
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
  def eventConditionsMet(role: Role) =
    game.hasMuslim(m => m.isShiaMix && m.civilWar)

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
    if (game.getMuslim(GulfStates).truce)
      log(s"\nCannot affect $GulfStates because it is under TRUCE.", Color.Event)
    else {
      addEventTarget(GulfStates)
      testCountry(GulfStates) // Event specifically says to test
      val gulfStates = game.getMuslim(GulfStates)
      val canWorsenGov = gulfStates.isGood || gulfStates.isFair
      val canShift = !gulfStates.isAdversary
      if (canWorsenGov && canShift) {
        if (isHuman(role)) {
          sealed trait Choice
          case object Worsen extends Choice
          case object Shift extends Choice
          val choices = List(
            choice(gulfStates.isGood || gulfStates.isFair, Worsen, "Worsen governance of Gulf States"),
            choice(!gulfStates.isAdversary, Shift,  "Shift alignment of Gulf States towards Adversary")
          ).flatten
          askMenu("Choose one:", choices).head match {
            case Worsen => worsenGovernance(GulfStates, 1, canShiftToIR = false)
            case Shift  => shiftAlignmentRight(GulfStates)
          }
        }
        else if (canWorsenGov)
          worsenGovernance(GulfStates, 1, canShiftToIR = false)
        else if (canShift)
          shiftAlignmentRight(GulfStates)
      }
    }

    log("\nIran, Gulf States, Saudi Arabia and Yemen are all now adjacent.", Color.Event)
    addGlobalEventMarker(QatariCrisis)
  }
}
