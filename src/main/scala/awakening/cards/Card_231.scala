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
// If US play, remove up to 3 Cells.
// If Jihadist, remove up to 2 Militia.
// ------------------------------------------------------------------
object Card_231 extends Card(231, "Siege of Kobanigrad", Unassociated, 2, NoRemove, NoLapsing, NoAutoTrigger) {
  // Used by the US Bot to determine if the executing the event would alert a plot
  // in the given country
  override
  def eventAlertsPlot(countryName: String, plot: Plot): Boolean = false

  def getCandidates = countryNames(game.muslims.filter(_.civilWar))

  def getCellsCandidates = countryNames(game.muslims.filter(m => m.civilWar && m.totalCells > 0))

  def getMilitiaCandidates = countryNames(game.muslims.filter(m => m.civilWar && m.militia > 0))

  // Used by the US Bot to determine if the executing the event would remove
  // the last cell on the map resulting in victory.
  override
  def eventRemovesLastCell(): Boolean =
    getCandidates.exists(name => USBot.wouldRemoveLastCell(name, 3))

  // Returns true if the printed conditions of the event are satisfied
  override
  def eventConditionsMet(role: Role) = getCandidates.nonEmpty

  // Returns true if the Bot associated with the given role will execute the event
  // on its turn.  This implements the special Bot instructions for the event.
  // When the event is triggered as part of the Human players turn, this is NOT used.
  override
  def botWillPlayEvent(role: Role): Boolean = role match {
    case US => getCellsCandidates.nonEmpty
    case Jihadist => getMilitiaCandidates.nonEmpty
  }

  // Carry out the event for the given role.
  // forTrigger will be true if the event was triggered during the human player's turn
  // and it associated with the Bot player.
  override
  def executeEvent(role: Role): Unit = {
    sealed trait Choice
    case object Cells extends Choice
    case object Militia extends Choice
    val actionChoices = List(
      choice(getCellsCandidates.nonEmpty,   Cells,   "Remove cells."),
      choice(getMilitiaCandidates.nonEmpty, Militia, "Remove militia."),
    ).flatten
    val orderedActionChoices = if (role == US) actionChoices else actionChoices.reverse

    val action = role match {
      case _ if isHuman(role) => askMenu("Choose one:", orderedActionChoices).head
      case US => Cells
      case Jihadist => Militia
    }

    action match {
      case Cells if isHuman(role) =>
        getCellsCandidates match {
          case Nil =>
            log("\nThere are no Civil War countries with cells.  The event has no effect.", Color.Event)
          case candidates =>
            val name = askCountry("Select country: ", candidates)
            val (actives, sleepers, sadr) = askCells(name, 3 min game.getMuslim(name).totalCells, sleeperFocus = true)
            addEventTarget(name)
            removeCellsFromCountry(name, actives, sleepers, sadr, addCadre = true)
        }

      case Cells =>
        val name = USBot.disruptPriority(getCellsCandidates).get
        val (actives, sleepers, sadr) = USBot.chooseCellsToRemove(name, 3 min game.getMuslim(name).totalCells)
        addEventTarget(name)
        removeCellsFromCountry(name, actives, sleepers, sadr, addCadre = true)

      case Militia  if isHuman(role) =>
        getMilitiaCandidates match {
          case Nil =>
            log("\nThere are no Civil War countries with militia.  The event has no effect.", Color.Event)
          case candidates =>
            val name = askCountry("Select country: ", candidates)
            addEventTarget(name)
            removeMilitiaFromCountry(name, 2 min (game getMuslim name).militia)
        }

      case Militia if game.botEnhancements =>
        // Priority to MJP, then Poor, then highest Res, then most militia.
        val priorities = List(
          JihadistBot.IsMajorJihadPriority,
          JihadistBot.PoorPriority,
          JihadistBot.HighestPrintedResourcePriority,
          JihadistBot.MostMilitiaPriority,
        )
        val name = JihadistBot.topPriority(game.getCountries(getMilitiaCandidates), priorities)
          .map(_.name)
          .get
        addEventTarget(name)
        removeMilitiaFromCountry(name, 2 min game.getMuslim(name).militia)

      case Militia =>
        val name = JihadistBot.minorJihadTarget(getMilitiaCandidates).get
        addEventTarget(name)
        removeMilitiaFromCountry(name, 2 min game.getMuslim(name).militia)
    }
  }
}
