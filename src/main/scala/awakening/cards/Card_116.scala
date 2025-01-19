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
import awakening.JihadistBot

// Card Text:
// ------------------------------------------------------------------
// US may play if any plots in Ally or non-Muslim countries:
// Reveal and remove them, draw 2 cards, REMOVE this card.
// If jihadist play, place an available plot in a non-Islamist Rule
// country with a cell.
// ------------------------------------------------------------------
object Card_116 extends Card(116, "KSM", Unassociated, 3, USRemove, NoLapsing, NoAutoTrigger) {
  def getUSCandidates() = countryNames(
    (game.nonMuslims ::: game.muslims.filter(_.isAlly))
      .filter(_.plots.nonEmpty)
  )

  def getJihadistCandidates() = countryNames(
    (game.nonMuslims ::: game.muslims.filter(!_.isIslamistRule))
      .filter(_.totalCells > 0)
  )

  // Used by the US Bot to determine if the executing the event would alert a plot
  // in the given country
  override
  def eventAlertsPlot(countryName: String, plot: Plot): Boolean =
    getUSCandidates().contains(countryName)

  // Used by the US Bot to determine if the executing the event would remove
  // the last cell on the map resulting in victory.
  override
  def eventRemovesLastCell(): Boolean = false

  // Returns true if the printed conditions of the event are satisfied
  override
  def eventConditionsMet(role: Role) = role match {
    case US => getUSCandidates().nonEmpty
    case Jihadist => getJihadistCandidates().nonEmpty
  }

  // Returns true if the Bot associated with the given role will execute the event
  // on its turn.  This implements the special Bot instructions for the event.
  // When the event is triggered as part of the Human players turn, this is NOT used.
  override
  def botWillPlayEvent(role: Role): Boolean = role match {
    case US => true
    case Jihadist => game.availablePlots.nonEmpty
  }

  // Carry out the event for the given role.
  // forTrigger will be true if the event was triggered during the human player's turn
  // and it associated with the Bot player.
  override
  def executeEvent(role: Role): Unit = role match {
    case US =>
      for (name <- getUSCandidates(); c = game.getCountry(name)) {
        addEventTarget(name)
        for (plot <- c.plots)
          performAlert(name, plot)
      }
      log("\nThe US player draws 2 cards", Color.Event)
      askMultipleCardsDrawnFromDrawPile(role, 2)

    case Jihadist if game.availablePlots.nonEmpty =>
      val (name, plot) = if (isHuman(role)) {
        val name = askCountry("Select country: ", getJihadistCandidates())
        (name, askAvailablePlots(1, ops = 3).head)
      }
      else if (getJihadistCandidates().contains(UnitedStates))
        (UnitedStates, JihadistBot.preparePlots(game.availablePlots).head)
      else {
        val name = JihadistBot.plotPriority(getJihadistCandidates()).get
        (name, JihadistBot.preparePlots(game.availablePlots).head)
      }

      addEventTarget(name)
      addAvailablePlotToCountry(name, plot)

    case Jihadist =>
      log("\nThere are no available plots. The event has no effect.", Color.Event)
  }
}
