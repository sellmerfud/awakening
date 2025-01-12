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
// Play in Afghanistan or Pakistan if Regime Change or Adversary,
/// or in India if there is a Cell.
// If US play: Shift Alignment one box towards Ally or Set to Hard. REMOVE
// If Jihadist: Place 1 Cell or a Level 1 Plot there.
// ------------------------------------------------------------------
object Card_328 extends Card(328, "Hafiz Saeed Khan", Unassociated, 1, USRemove, NoLapsing, NoAutoTrigger) {
  // Used by the US Bot to determine if the executing the event would alert a plot
  // in the given country
  override
  def eventAlertsPlot(countryName: String, plot: Plot): Boolean = false

  // Used by the US Bot to determine if the executing the event would remove
  // the last cell on the map resulting in victory.
  override
  def eventRemovesLastCell(): Boolean = false

  val Countries = List(Afghanistan, Pakistan, India)

  val isCandidate = (c: Country) => c match {
    case m: MuslimCountry => m.inRegimeChange || m.isAdversary
    case n: NonMuslimCountry => n.totalCells > 0
  }

  def getCandidates() = Countries.filter(name => isCandidate(game.getCountry(name)))

  val isUSBotCandidate = (c: Country) => c match {
    case m: MuslimCountry => (m.inRegimeChange && !m.isAlly) || (m.isAdversary && !m.isIslamistRule)
    case n: NonMuslimCountry => n.totalCells > 0 && n.posture == Soft && game.usPosture == Hard
  }

  def getUSBotCandidates() = Countries.filter(name => isUSBotCandidate(game.getCountry(name)))

  def getUSBotMuslimCandidates() = getUSBotCandidates().filter(game.isMuslim)

  def getUSBotNonMuslimCandidates() = getUSBotCandidates().filter(game.isNonMuslim)

  // Returns true if the printed conditions of the event are satisfied
  override
  def eventConditionsMet(role: Role) = getCandidates().nonEmpty

  // Returns true if the Bot associated with the given role will execute the event
  // on its turn.  This implements the special Bot instructions for the event.
  // When the event is triggered as part of the Human players turn, this is NOT used.
  override
  def botWillPlayEvent(role: Role): Boolean = role match {
    case US => getUSBotCandidates().nonEmpty
    case Jihadist => game.cellsAvailable > 0 || game.availablePlots.contains(Plot1)
  }

  // Carry out the event for the given role.
  // forTrigger will be true if the event was triggered during the human player's turn
  // and it associated with the Bot player.
  override
  def executeEvent(role: Role): Unit = {
    if (role == US) {
      val target = if (isHuman(role))
        askCountry("Which country: ", getCandidates())
      else if (getUSBotMuslimCandidates().nonEmpty)
        USBot.markerAlignGovTarget(getUSBotMuslimCandidates()).get
      else
        USBot.posturePriority(getUSBotNonMuslimCandidates()).get

      addEventTarget(target)
      if (game.isMuslim(target))
        shiftAlignmentLeft(target)
      else
        setCountryPosture(India, Hard)
    }
    else if (game.cellsAvailable > 0 || game.availablePlots.contains(Plot1)) { // Jihadist
      val (target, action) = if (isHuman(role)) {
        val name = askCountry("Which country: ", getCandidates())
        val choices = List(
          choice(game.availablePlots contains Plot1, "plot", "Place a level 1 Plot"),
          choice(game.cellsAvailable > 0,            "cell", "Place a Cell")
        ).flatten
        (name, askMenu("Choose one:", choices).head)
      }
      else if (game.availablePlots.contains(Plot1))
        (JihadistBot.plotPriority(getCandidates()).get, "plot")
      else
        (JihadistBot.cellPlacementPriority(false)(getCandidates()).get, "cell")

      addEventTarget(target)
      if (action == "plot")
        addAvailablePlotToCountry(target, Plot1, visible = true)
      else
        addSleeperCellsToCountry(target, 1)
    }
    else
      log("\nThere are no available cells or level 1 plots. The event has no effect.", Color.Event)
  }
}
