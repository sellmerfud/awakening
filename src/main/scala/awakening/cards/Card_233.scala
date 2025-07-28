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
// Play in a Civil War country.
// End Civil War. Replace all Militia and Cells with an equal number
// of Awakening and Reaction markers.
// Troops to Track. Set Alignment to Neutral and roll Governance.
// Other markers remain.
// Cannot be played in a Caliphate country.
// ------------------------------------------------------------------
object Card_233 extends Card(233, "UN Ceasefire", Unassociated, 2, NoRemove, NoLapsing, NoAutoTrigger) {
  // Used by the US Bot to determine if the executing the event would alert a plot
  // in the given country
  override
  def eventAlertsPlot(countryName: String, plot: Plot): Boolean = false

  val isCandidate = (m: MuslimCountry) =>
    !m.truce &&
    m.civilWar &&
    !game.isCaliphateMember(m.name)

  def getCandidates = countryNames(game.muslims.filter(_.civilWar))

  // Used by the US Bot to determine if the executing the event would remove
  // the last cell on the map resulting in victory.
  override
  def eventRemovesLastCell(): Boolean = getCandidates.exists { name =>
    val numCells = game.getCountry(name).totalCells
    USBot.wouldRemoveLastCell(name, numCells)
  }

  // Returns true if the printed conditions of the event are satisfied
  override
  def eventConditionsMet(role: Role) = getCandidates.nonEmpty

  def arabWinter = lapsingEventInPlay(ArabWinter)

  val isUSBotCandidate = (m: MuslimCountry) =>
    isCandidate(m) &&
    (m.totalCells == game.totalCellsOnMap ||
    arabWinter ||
    (m.pieces.militia >= m.totalCells && !arabWinter))

  val isJihadistBotCandidate = (m: MuslimCountry) =>
    isCandidate(m) &&
    ((m.totalCells >= m.pieces.militia && !arabWinter) ||
    (m.totalTroopsAndMilitia >= m.totalCells && arabWinter))

  def getUSBotCandidates = countryNames(game.muslims.filter(isUSBotCandidate))

  def getJihadistBotCandidates = countryNames(game.muslims.filter(isJihadistBotCandidate))

  // Returns true if the Bot associated with the given role will execute the event
  // on its turn.  This implements the special Bot instructions for the event.
  // When the event is triggered as part of the Human players turn, this is NOT used.
  override
  def botWillPlayEvent(role: Role): Boolean = role match {
    case US => getUSBotCandidates.nonEmpty
    case Jihadist if game.botEnhancements => false
    case Jihadist => getJihadistBotCandidates.nonEmpty
  }

  // Carry out the event for the given role.
  // forTrigger will be true if the event was triggered during the human player's turn
  // and it associated with the Bot player.
  override
  def executeEvent(role: Role): Unit = {
    // See Event Instructions table
    val name = role match {
      case _ if isHuman(role) => askCountry("\nSelect a Civil War country: ", getCandidates)
      case US => USBot.unCeasefireTarget(getUSBotCandidates).get
      case Jihadist => JihadistBot.unCeasefireTarget(getJihadistBotCandidates).get
    }

    addEventTarget(name)
    val m = game.getMuslim(name) // country state before any of of the changes
    endCivilWar(name)  // This will remove the militia

    // If Arab Winter is in effect, then placing of Awakeing/Reaction markers
    // is prohibited.  According to the designer (Trevor) this would prevent
    // the cells being "replaced"
    // Note: any milita will have been removed when the Civil War ended above
    if (lapsingEventInPlay(ArabWinter)) {
      if (m.pieces.militia > 0)
        log("\nMilitia are not replaced by awakening markers. [Arab Winter]", Color.Event)
      if (m.totalCells > 0)
        log("\nCells are not replaced with reaction markers. [Arab Winter]", Color.Event)
    }
    else {
      addAwakeningMarker(name, m.pieces.militia)
      removeCellsFromCountry(name, m.pieces.activeCells, m.pieces.sleeperCells, m.hasSadr, addCadre = true)
      addReactionMarker(name, m.totalCells)
    }
    moveTroops(name, "track", m.pieces.usTroops)
    setAlignment(name, Neutral)
    rollGovernance(name)
  }
}
