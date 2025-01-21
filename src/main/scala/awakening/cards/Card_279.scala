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
// Place up to 2 Advisors total in up to 2 non-Adversary Civil War
// countries without Troops.
// ------------------------------------------------------------------
object Card_279 extends Card(279, "SFABs", US, 3, NoRemove, NoLapsing, NoAutoTrigger) {
  // Used by the US Bot to determine if the executing the event would alert a plot
  // in the given country
  override
  def eventAlertsPlot(countryName: String, plot: Plot): Boolean = false

  // Used by the US Bot to determine if the executing the event would remove
  // the last cell on the map resulting in victory.
  override
  def eventRemovesLastCell(): Boolean = false

  def getCandidates() = countryNames(
    game.muslims.filter(m => m.civilWar && m.alignment != Adversary && m.totalTroops == 0)
  )

  // Returns true if the printed conditions of the event are satisfied
  // Max of 3 Advisors allowed on the map.
  override
  def eventConditionsMet(role: Role) = numAdvisorsOnMap < 3 && getCandidates().nonEmpty

  def numAdvisorsOnMap = game.muslims.map(_.numAdvisors).sum
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
    //  Up to two targets
    val maxTargets = 2 min (3 - numAdvisorsOnMap)

    if (isHuman(role)) {
      def nextPlacement(num: Int): Unit = if (num <= maxTargets) {
        val choices = getCandidates().map(name => Some(name) -> name) :+ (None, "Finished placing Advisors")
        displayLine(s"\n${amountOf(numAdvisorsOnMap, "Advisors marker")} of 3 total currently on the map.", Color.Info)
        askMenu(s"Select country for ${ordinal(num)} Advisors marker:", choices).head match {
          case Some(target) =>
            addEventTarget(target)
            addAdvisorsToCountry(target)
            nextPlacement(num + 1)
          case None =>
        }
      }
      nextPlacement(1)
    }
    else {
      // Bot
      for (num <- 1 to maxTargets) {
        val target = USBot.deployToPriority(getCandidates()).get
        addEventTarget(target)
        addAdvisorsToCountry(target)
      }
    }
  }
}
