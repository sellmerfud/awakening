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
// Play in any Civil War or Regime Change country with multiple Cells.
// Remove all but 1 Cell. Increase Funding by ½ that number (rounded up).
// Worsen Governance 1 level (not to IR) or Shift Alignment 1 box toward
// Adversary.
// Not playable in a Caliphate country.
// ------------------------------------------------------------------
object Card_230 extends Card(230, "Sellout", Unassociated, 2, NoRemove, NoLapsing, NoAutoTrigger) {
  // Used by the US Bot to determine if the executing the event would alert a plot
  // in the given country
  override
  def eventAlertsPlot(countryName: String, plot: Plot): Boolean = false

  // Used by the US Bot to determine if the executing the event would remove
  // the last cell on the map resulting in victory.
  override
  def eventRemovesLastCell(): Boolean = false

  val isCandidate = (m: MuslimCountry) =>
    !m.truce &&
    m.totalCells > 1 &&
    (m.civilWar || m.inRegimeChange) &&
    !game.isCaliphateMember(m.name)

  def getCandidates = countryNames(game.muslims.filter(isCandidate))

  val isJihadistBotCandidate = (m: MuslimCountry) =>
    isCandidate(m) && !(m.isPoor && m.isAdversary)

  def getJihadistBotCandidates = countryNames(game.muslims.filter(isJihadistBotCandidate))

  // Playable in non-adversary countries with 2-4 cells.
  val isEnhJihadistBotCandidate = (m: MuslimCountry) =>
    isCandidate(m) &&
    !m.isAdversary &&
    m.cells >= 2 &&
    m.cells <= 4

  def getEnhJihadistBotCandidates = game.muslims.filter(isEnhJihadistBotCandidate)

  // Returns true if the printed conditions of the event are satisfied
  override
  def eventConditionsMet(role: Role) = getCandidates.nonEmpty

  // Returns true if the Bot associated with the given role will execute the event
  // on its turn.  This implements the special Bot instructions for the event.
  // When the event is triggered as part of the Human players turn, this is NOT used.
  //
  //  The US Bot treats Sellout as unplayable so we return an empty list
  //  The event has you remove all cells but 1 in a country and increase
  //  funding by half that number rounded up.
  //  Then either shift alignment toward Adversary or worsen governance toward Poor.
  //  If funding is at 9, then we will ensure the Bot only takes the event if it
  //  will be able to affect alignment/governance
  override
  def botWillPlayEvent(role: Role): Boolean = role match {
    case US =>
      false  // US Bot treats this as unplayable
    case Jihadist if game.botEnhancements =>
      getEnhJihadistBotCandidates.nonEmpty
    case Jihadist =>
      game.funding < 9 || getJihadistBotCandidates.nonEmpty
  }

  // Carry out the event for the given role.
  // forTrigger will be true if the event was triggered during the human player's turn
  // and it associated with the Bot player.
  override
  def executeEvent(role: Role): Unit = {
    sealed trait Choice
    case object Governance extends Choice
    case object Alignment extends Choice
    val (name, (actives, sleepers, sadr), action) = role match {
      case _ if isHuman(role) =>
        val name = askCountry("Select country: ", getCandidates)
        val m = game.getMuslim(name)
        val numCells = m.totalCells - 1
        displayLine(s"\nRemove ${amountOf(numCells, "cell")} from $name", Color.Event)
        val cells = askCells(name, numCells, sleeperFocus = (role == US))
        val choices = List(
          choice(!m.isPoor,      Governance, "Worsen governance 1 level"),
          choice(!m.isAdversary, Alignment,  "Shift alignment 1 box toward Adversary")
        ).flatten
        val action = askMenu("Choose one:", choices).headOption
        (name, cells, action)

      case Jihadist if game.botEnhancements =>
        // Priority to Neutral, then Fair, then RC, then highest Res.
        // If a poor country is chosen, shift alignment, else worsen governance.
        val priorities = List(
          JihadistBot.NeutralPriority,
          JihadistBot.FairMuslimFilter,
          JihadistBot.RegimeChangePriority,
          JihadistBot.HighestResourcePriority
        )
        val target = JihadistBot.topPriority(getEnhJihadistBotCandidates, priorities)
          .get
        val action = if (target.isPoor)
          Some(Alignment)
        else
          Some(Governance)
        val cells = JihadistBot.chooseCellsToRemove(target.name, target.totalCells - 1)
        (target.name, cells, action)

      case _ => // Jihadist Bot
        // If funding < 9 then there may not be a preferred candidate
        val possibleCandidates = getJihadistBotCandidates match {
          case Nil => getCandidates
          case cs => cs
        }
        val name = if (game.funding == 9)
          JihadistBot.alignGovTarget(possibleCandidates).get
        else {
          val candidates = game.getMuslims(possibleCandidates).sortBy(-_.totalCells)
          shuffle(candidates.takeWhile(_.totalCells == candidates.head.totalCells)).head.name
        }
        val m = game.getMuslim(name)
        val cells = JihadistBot.chooseCellsToRemove(name, m.totalCells - 1)
        val action = if (!m.isAdversary)
          Some(Alignment)
        else if (!m.isPoor)
          Some(Governance)
        else
          None
        (name, cells, action)
    }

    addEventTarget(name)
    removeCellsFromCountry(name, actives, sleepers, sadr, addCadre = true)
    val totalCells = actives + sleepers + (if (sadr) 1 else 0)
    increaseFunding((totalCells + 1) / 2)  // half of removed cells rounded up
    action match {
      case Some(Governance) => worsenGovernance(name, 1, canShiftToIR = false)
      case Some(Alignment) => shiftAlignmentRight(name)
      case _               => log(s"\n$name is already Poor Adversary.", Color.Event)
    }
  }
}
