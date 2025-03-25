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
// If US play, remove a cell from a Shia-Mix country within 3 countries of Lebanon.
// If jihadist, Lebanon to Poor Neutral.
// ------------------------------------------------------------------
object Card_103 extends Card(103, "Hizballah", Unassociated, 2, NoRemove, NoLapsing, NoAutoTrigger) {
  // Used by the US Bot to determine if the executing the event would alert a plot
  // in the given country
  override
  def eventAlertsPlot(countryName: String, plot: Plot): Boolean = false

  def getCandidates = countryNames(
    game.muslims
      .filter(m => !m.truce && m.isShiaMix && m.totalCells > 0 && distance(m.name, Lebanon) <= 3)
  )
  // Used by the US Bot to determine if the executing the event would remove
  // the last cell on the map resulting in victory.
  override
  def eventRemovesLastCell(): Boolean =
    getCandidates.exists (name => USBot.wouldRemoveLastCell(name, 1))


  // Returns true if the printed conditions of the event are satisfied
  override
  def eventConditionsMet(role: Role) = true

  // Returns true if the Bot associated with the given role will execute the event
  // on its turn.  This implements the special Bot instructions for the event.
  // When the event is triggered as part of the Human players turn, this is NOT used.
  override
  def botWillPlayEvent(role: Role) = role match {
    case US => getCandidates.nonEmpty
    case Jihadist if game.botEnhancements =>
      val lebanon = game.getMuslim(Lebanon)
      !lebanon.truce &&
      (lebanon.isGood || (lebanon.isFair && lebanon.isAlly))
    case Jihadist =>
      val lebanon = game.getMuslim(Lebanon)
      !lebanon.truce &&
      !lebanon.isAdversary &&
      !lebanon.isIslamistRule &&
      !(lebanon.isPoor && lebanon.isNeutral)
  }

  // Carry out the event for the given role.
  // forTrigger will be true if the event was triggered during the human player's turn
  // and it associated with the Bot player.
  override
  def executeEvent(role: Role): Unit = role match {
    case US if getCandidates.nonEmpty =>
      val (name, (active, sleeper, sadr)) = if (isHuman(role)) {
        val name = askCountry("Remove a cell from which country: ", getCandidates)
        (name, askCells(name, 1, sleeperFocus = true))
      }
      else {
        val name = USBot.disruptPriority(getCandidates).get
        (name, USBot.chooseCellsToRemove(name, 1))
      }
      addEventTarget(name)
      removeCellsFromCountry(name, active, sleeper, sadr, addCadre = true)

    case US =>
      log("\nThere are no Shia-Mix countries will a cell within three countries of Lebanon.", Color.Event)
      log("The event has no effect.", Color.Event)

    case Jihadist if game.getMuslim(Lebanon).truce =>
      log("\nLebanon is under TRUCE. The event has no effect.", Color.Event)

    case Jihadist if !game.getMuslim(Lebanon).isPoor || !game.getMuslim(Lebanon).isNeutral =>
      addEventTarget(Lebanon)
      setGovernance(Lebanon, Poor, Some(Neutral))

    case Jihadist =>
      log("\nLebanon is already Poor and Neutral. The event has no effect.", Color.Event)
  }
}
