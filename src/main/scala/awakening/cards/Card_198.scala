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

import scala.util.Random.shuffle
import awakening.LabyrinthAwakening._
import awakening.JihadistBot

// Card Text:
// ------------------------------------------------------------------
// Play if Troops in a Civil War or Regime Change country.
// Shift Alignment 1 box towards Adversary.
// Select Posture for 1 Unmarked non-Schengen country (not US).
// -1 Prestige.
// ------------------------------------------------------------------
object Card_198 extends Card(198, "US Atrocities", Jihadist, 3, NoRemove, NoLapsing, NoAutoTrigger) {
  // Used by the US Bot to determine if the executing the event would alert a plot
  // in the given country
  override
  def eventAlertsPlot(countryName: String, plot: Plot): Boolean = false

  // Used by the US Bot to determine if the executing the event would remove
  // the last cell on the map resulting in victory.
  override
  def eventRemovesLastCell(): Boolean = false

  val isCandidate = (m: MuslimCountry) =>
    m.totalTroops > 0 &&
    (m.civilWar || m.inRegimeChange)

  val isAlignCandidate = (m: MuslimCountry) =>
    m.totalTroops > 0 &&
    (m.civilWar || m.inRegimeChange) &&
    !m.isAdversary

  val isPostureCandidate = (n: NonMuslimCountry) =>
    n.isUntested &&
    !n.isSchengen &&
    n.name != UnitedStates &&
    n.name != Israel

  def getAlignCandidates = countryNames(game.muslims.filter(isAlignCandidate))

  def getPostureCandidates = countryNames(game.nonMuslims.filter(isPostureCandidate))

  // Returns true if the printed conditions of the event are satisfied
  override
  def eventConditionsMet(role: Role) = game.hasMuslim(isCandidate)

  // Returns true if the Bot associated with the given role will execute the event
  // on its turn.  This implements the special Bot instructions for the event.
  // When the event is triggered as part of the Human players turn, this is NOT used.
  override
  def botWillPlayEvent(role: Role): Boolean =
    getAlignCandidates.nonEmpty ||
    getPostureCandidates.nonEmpty ||
    game.prestige > 1

  // Carry out the event for the given role.
  // forTrigger will be true if the event was triggered during the human player's turn
  // and it associated with the Bot player.
  override
  def executeEvent(role: Role): Unit = {
    if (getAlignCandidates.isEmpty && getPostureCandidates.isEmpty && game.prestige == 1)
      log("\nThe event has no effect.", Color.Event)
    else {
      val alignTarget = getAlignCandidates match {
        case Nil =>
          log("\nThere are no qualifiying countries that can be shifted to adversary.", Color.Event)
          None

        case candidates if isHuman(role) =>
          Some(askCountry(s"Select country for alignment shift: ", candidates))

        case candidates =>
          JihadistBot.alignGovTarget(candidates)
      }

      val postureTarget = getPostureCandidates match {
        case Nil =>
          log("\nThere are unmarked non-Schengen countries.", Color.Event)
          None

        case candidates if isHuman(role) =>
          val name = askCountry(s"Select posture of which country: ", candidates)
          val posture = askPosture(name)
          Some((name, posture))

        case candidates =>
          Some((shuffle(candidates).head, oppositePosture(game.usPosture)))
      }

      alignTarget.foreach { name =>
        addEventTarget(name)
        shiftAlignmentRight(name)
      }

      postureTarget foreach { case (name, posture) =>
        addEventTarget(name)
        setCountryPosture(name, posture)
      }

      decreasePrestige(1)
    }
  }
}
