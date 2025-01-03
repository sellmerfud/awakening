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
// Play in one of Egypt, Syria, Pakistan, or Turkey if tested and not
// Islamist Rule or Civil War.
// Improve Governance 1 level.
// If US Soft, shift Alignment 1 box towards Adversary.
// REMOVE
// ------------------------------------------------------------------
object Card_270 extends Card(270, "Deep State", US, 3, Remove, NoLapsing, NoAutoTrigger) {
  // Used by the US Bot to determine if the executing the event would alert a plot
  // in the given country
  override
  def eventAlertsPlot(countryName: String, plot: Plot): Boolean = false

  // Used by the US Bot to determine if the executing the event would remove
  // the last cell on the map resulting in victory.
  override
  def eventRemovesLastCell(): Boolean = false

  val isCandidate = (name: String) => {
      val m = game.getMuslim(name)
      m.isTested && !(m.isIslamistRule || m.civilWar)
  }

  def getCandidates() = List(Egypt, Syria, Pakistan, Turkey).filter(isCandidate)

  def getBotCandidates() = getCandidates().filter(name => !game.getMuslim(name).isGood)

  def getBotTarget(candidates: List[String]) = USBot.cachedTarget("deep-state") {
    USBot.markerAlignGovTarget(candidates).get
  }

  // Returns true if the printed conditions of the event are satisfied
  override
  def eventConditionsMet(role: Role) = getCandidates().nonEmpty

  // Returns true if the Bot associated with the given role will execute the event
  // on its turn.  This implements the special Bot instructions for the event.
  // When the event is triggered as part of the Human players turn, this is NOT used.
  //
  // Bot will only play if US posture is Hard or if the priority target is already
  // has Adversary alignment.
  override
  def botWillPlayEvent(role: Role): Boolean = {
    getBotCandidates().nonEmpty &&
    (game.usPosture == Hard || game.getMuslim(getBotTarget(getBotCandidates())).isAdversary)
  }

  // Carry out the event for the given role.
  // forTrigger will be true if the event was triggered during the human player's turn
  // and it associated with the Bot player.
  override
  def executeEvent(role: Role, forTrigger: Boolean): Unit = {
    // When triggered during US turn the Bot's perferred candidates
    // may not be available.
    val target = if (isHuman(role))
      askCountry("Which country: ", getCandidates())
    else if (getBotCandidates().nonEmpty)
      getBotTarget(getBotCandidates())
    else
      getBotTarget(getCandidates())

    val m = game.getMuslim(target)
    println()
    addEventTarget(target)
    if (m.isGood)
      log(s"\nCannot improve governance of $target. Already at Good.", Color.Event)
    else
      improveGovernance(target, 1, true)
    game.usPosture match {
      case Soft if m.alignment == Adversary =>
        log(s"\nCannot shift alignment of $target. Already at Adversary.", Color.Event)
      case Soft =>
        shiftAlignmentRight(target)
      case _ =>
    }
  }
}
