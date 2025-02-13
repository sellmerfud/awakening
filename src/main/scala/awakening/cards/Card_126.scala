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
// Remove up to 2 Cells in any one Muslim country, OR
// randomly discard 1 card from the Jihadist hand.
// ------------------------------------------------------------------
object Card_126 extends Card(126, "Reaper", US, 1, NoRemove, NoLapsing, NoAutoTrigger) {
  // Used by the US Bot to determine if the executing the event would alert a plot
  // in the given country
  override
  def eventAlertsPlot(countryName: String, plot: Plot): Boolean = false

  def getCandidates = countryNames(game.muslims.filter(_.totalCells > 0))

  // Used by the US Bot to determine if the executing the event would remove
  // the last cell on the map resulting in victory.
  override
  def eventRemovesLastCell(): Boolean =
    getCandidates.exists(USBot.wouldRemoveLastCell(_, 2))


  // Returns true if the printed conditions of the event are satisfied
  override
  def eventConditionsMet(role: Role) = true

  // Returns true if the Bot associated with the given role will execute the event
  // on its turn.  This implements the special Bot instructions for the event.
  // When the event is triggered as part of the Human players turn, this is NOT used.
  override
  def botWillPlayEvent(role: Role): Boolean =
    getCandidates.nonEmpty ||
    hasCardInHand(Jihadist)

  // Carry out the event for the given role.
  // forTrigger will be true if the event was triggered during the human player's turn
  // and it associated with the Bot player.
  override
  def executeEvent(role: Role): Unit = {
    if (getCandidates.nonEmpty || hasCardInHand(Jihadist)) {
      if (isHuman(role)) {
        val choices = List(
          choice(getCandidates.nonEmpty, "remove",  "Remove up to 2 cells in one Muslim country"),
          choice(hasCardInHand(Jihadist),  "discard", "Discard 1 card from the Jihadist hand")
        ).flatten
        askMenu("Choose one:", choices).head match {
          case "discard" =>
            log("\nDiscard the top card in the Jihadist hand", Color.Event)
            askCardsDiscarded(Jihadist, 1)

          case _ =>
            val target = askCountry("Remove 2 cells in which country? ", getCandidates)
            val (actives, sleepers, sadr) = askCells(target, 2, sleeperFocus = true)
            addEventTarget(target)
            println()
            removeCellsFromCountry(target, actives, sleepers, sadr, addCadre = true)
        }
      }
      else {
        // First remove cells if it would remove last cell on the map
        val candidates = getCandidates
        if (candidates.size == 1 && USBot.wouldRemoveLastCell(candidates.head, 2)) {
          val target = candidates.head
          addEventTarget(target)
          val (actives, sleepers, sadr) = USBot.chooseCellsToRemove(target, 2)
          println()
          removeCellsFromCountry(target, actives, sleepers, sadr, addCadre = true)
        }
        else {
          val nonIRWith5Cells = countryNames(game.muslims.filter(m => !m.isIslamistRule && m.totalCells >= 5))
          if (nonIRWith5Cells.isEmpty && hasCardInHand(Jihadist)) {
            println()
            log(s"\nYou ($Jihadist) must discard one random card", Color.Event)
            askCardsDiscarded(Jihadist, 1)
          }
          else {
            val target = if (nonIRWith5Cells.nonEmpty)
              USBot.disruptPriority(nonIRWith5Cells).get
            else
              USBot.disruptPriority(candidates).get
            addEventTarget(target)
            val (actives, sleepers, sadr) = USBot.chooseCellsToRemove(target, 2)
            println()
            removeCellsFromCountry(target, actives, sleepers, sadr, addCadre = true)
          }
        }
      }
    }
    else
      log(s"\nThe event has no effect.", Color.Event)
  }
}
