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
// Play in a Civil War country with at least one Militia and Cell.
// Shift Alignment one box in either direction.
// ------------------------------------------------------------------
object Card_344 extends Card2(344, "Media Manipulation", Unassociated, 2, NoRemove, NoLapsing, NoAutoTrigger) {
  // Used by the US Bot to determine if the executing the event would alert a plot
  // in the given country
  override
  def eventAlertsPlot(countryName: String, plot: Plot): Boolean = false

  // Used by the US Bot to determine if the executing the event would remove
  // the last cell on the map resulting in victory.
  override
  def eventRemovesLastCell(): Boolean = false

  val isCandidate = (m: MuslimCountry) => m.civilWar && m.militia > 0 && m.totalCells > 0

  def getCandidates() = countryNames(game.muslims.filter(isCandidate))

  def getBotCandidates(role: Role) = {
    val canShift = role match {
      case US => (m: MuslimCountry) => !m.isAlly
      case Jihadist => (m: MuslimCountry) => !m.isAdversary
    }
    countryNames(game.muslims.filter(m => isCandidate(m) && canShift(m)))
  }

  // Returns true if the printed conditions of the event are satisfied
  override
  def eventConditionsMet(role: Role) = getCandidates().nonEmpty

  // Returns true if the Bot associated with the given role will execute the event
  // on its turn.  This implements the special Bot instructions for the event.
  // When the event is triggered as part of the Human players turn, this is NOT used.
  override
  def botWillPlayEvent(role: Role): Boolean = getBotCandidates(role).nonEmpty

  // Carry out the event for the given role.
  // forTrigger will be true if the event was triggered during the human player's turn
  // and it associated with the Bot player.
  override
  def executeEvent(role: Role, forTrigger: Boolean): Unit = {
    def choices(target: String) = {
      val m = game.getMuslim(target)
      val list = List(
        choice(!m.isAlly, shiftAlignmentLeft _, "Shift aligment toward Ally."),
        choice(!m.isAdversary, shiftAlignmentRight _, "Shift aligment toward Adversary."),
        ).flatten
      if (role == US) list else list.reverse
    }

    val (target, shiftAction) = role match {
      case _ if isHuman(role) =>
        val name = askCountry("Which country: ", getCandidates())
        val action = askMenu("Choose one:", choices(name)).head
        (name, action)
        
      case US =>
        (USBot.markerAlignGovTarget(getBotCandidates(US)).get, shiftAlignmentLeft _)

      case Jihadist =>
        (JihadistBot.alignGovTarget(getBotCandidates(Jihadist)).get, shiftAlignmentRight _)
    }

    shiftAction(target)
  }
}
