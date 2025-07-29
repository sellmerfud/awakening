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
// Play in a Civil War country with a Cell and Troops and/or Militia.
// Remove a Cell and a Militia or Troop there. Continue to do so
// one-for-one until one or both sides no longer remain.
// Cannot be played in a Caliphate country.
// ------------------------------------------------------------------
object Card_350 extends Card(350, "UN Peace Envoy", Unassociated, 2, NoRemove, NoLapsing, NoAutoTrigger) {
  // Used by the US Bot to determine if the executing the event would alert a plot
  // in the given country
  override
  def eventAlertsPlot(countryName: String, plot: Plot): Boolean = false

  val isCandidate = (m: MuslimCountry) =>
    !m.truce &&
    m.civilWar &&
    m.totalCells > 0 &&
    m.totalTroopsAndMilitia > 0 &&
    !game.isCaliphateMember(m.name)

  val isUSBotCandidate = (m: MuslimCountry) =>
    isCandidate(m) && m.totalTroopsAndMilitia > m.totalCells

  val isJihadistBotCandidate = (m: MuslimCountry) =>
    isCandidate(m) && m.totalCells > m.totalTroopsAndMilitia

  def getCandidates = countryNames(game.muslims.filter(isCandidate))

  def getUSBotCandidates = countryNames(game.muslims.filter(isUSBotCandidate))

  def getJihadistBotCandidates = countryNames(game.muslims.filter(isJihadistBotCandidate))

  def getLastCellCandidate: Option[String] = 
    getCandidates
      .find { name =>
        val m = game.getMuslim(name)
        m.totalTroopsAndMilitia >= m.totalCells &&
        m.totalCells == game.totalCellsOnMap
      }

  // Used by the US Bot to determine if the executing the event would remove
  // the last cell on the map resulting in victory.
  //
  // Note: the US Bot candidates only consider countries where TandM > cells,
  //       however, when checking for removing the last cell we must consider
  //       countries where TandM >= cells so we cal getCandidate rather than
  //       getUSBotCandidates
override
  def eventRemovesLastCell(): Boolean = getLastCellCandidate.nonEmpty

  // Returns true if the printed conditions of the event are satisfied
  override
  def eventConditionsMet(role: Role) = getCandidates.nonEmpty

  // Returns true if the Bot associated with the given role will execute the event
  // on its turn.  This implements the special Bot instructions for the event.
  // When the event is triggered as part of the Human players turn, this is NOT used.
  //
  override
  def botWillPlayEvent(role: Role): Boolean = role match {
    case US => eventRemovesLastCell() || getUSBotCandidates.nonEmpty
    case Jihadist if game.botEnhancements => false
    case Jihadist => getJihadistBotCandidates.nonEmpty
  }

  // Carry out the event for the given role.
  // forTrigger will be true if the event was triggered during the human player's turn
  // and it associated with the Bot player.
  override
  def executeEvent(role: Role): Unit = {
    val target = role match {
      case _ if isHuman(role) =>
        askCountry("Which country: ", getCandidates)

      case US if getLastCellCandidate.nonEmpty =>
        getLastCellCandidate.get

      case US =>
        USBot.disruptPriority(getUSBotCandidates).get

      case Jihadist =>
        JihadistBot.troopsMilitiaTarget(getJihadistBotCandidates).get
    }

    def nextRemoval(): Unit = {
      val m = game.getMuslim(target)
      if (m.totalCells > 0 && m.totalTroopsAndMilitia > 0) {
        val ((cell, sadr), usUnit) = role match {
          case _ if isHuman(role) =>
            (askCells(target, 1, role == US), askTroopOrMilitia("Remove which US piece:", target))
          case US =>
          (USBot.chooseCellsToRemove(target, 1), USBot.chooseTroopOrMilitiaToRemove(target))
          case Jihadist =>
          (JihadistBot.chooseCellsToRemove(target, 1), JihadistBot.chooseTroopOrMilitiaToRemove(target))
        }

        removeCellsFromCountry(target, cell, sadr, addCadre = true)
        usUnit match {
          case MilitiaCube         => removeMilitiaFromCountry(target, 1)
          case TroopCube           => moveTroops(target, "track", 1)
          case TroopMarker(name)   => removeEventMarkersFromCountry(target, CountryMarker(name))
        }
        nextRemoval()
      }
    }

    addEventTarget(target)
    nextRemoval()
  }
}
