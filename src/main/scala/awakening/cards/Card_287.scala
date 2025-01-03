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
// Play if Lebanon is not Ally and Iran Adversary or a Special Case country.
// Place one of the following in or within two spaces of Lebanon:
// - Reaction marker
// - Besieged Regime
// - Cell
// ------------------------------------------------------------------
object Card_287 extends Card(287, "Sayyed Hassan Nasrallah", Jihadist, 1, NoRemove, NoLapsing, NoAutoTrigger) {
  // Used by the US Bot to determine if the executing the event would alert a plot
  // in the given country
  override
  def eventAlertsPlot(countryName: String, plot: Plot): Boolean = false

  // Used by the US Bot to determine if the executing the event would remove
  // the last cell on the map resulting in victory.
  override
  def eventRemovesLastCell(): Boolean = false

  def getReactionCandidates() = countryNames(
    game.muslims.filter(m => m.canTakeAwakeningOrReactionMarker && distance(Lebanon, m.name) <= 2)
  )

   def getBesiegedRegimeCandidates() = countryNames(
     game.muslims.filter(m => m.canTakeBesiegedRegimeMarker && distance(Lebanon, m.name) <= 2)
   )

   def getCellCandidates() = countryNames(
     game.countries.filter(c => distance(Lebanon, c.name) <= 2)
   )

   def canPlaceReaction = lapsingEventNotInPlay(ArabWinter) && getReactionCandidates().nonEmpty

   def canPlaceBesieged = getBesiegedRegimeCandidates().nonEmpty

   def canPlaceCell = game.cellsAvailable > 0 && getCellCandidates().nonEmpty

  // Returns true if the printed conditions of the event are satisfied
  override
  def eventConditionsMet(role: Role) =
    !game.getMuslim(Lebanon).isAlly &&
    (isIranSpecialCase || game.getMuslim(Iran).isAdversary)

  // Returns true if the Bot associated with the given role will execute the event
  // on its turn.  This implements the special Bot instructions for the event.
  // When the event is triggered as part of the Human players turn, this is NOT used.
  override
  def botWillPlayEvent(role: Role): Boolean =
    canPlaceReaction || canPlaceBesieged || canPlaceCell

  // Carry out the event for the given role.
  // forTrigger will be true if the event was triggered during the human player's turn
  // and it associated with the Bot player.
  override
  def executeEvent(role: Role, forTrigger: Boolean): Unit = {
    if (isHuman(role)) {
      val choices = List(
        choice(canPlaceReaction, "reaction", "Place a reaction marker"),
        choice(canPlaceBesieged, "besieged", "Place a besieged regime marker"),
        choice(canPlaceCell,     "cell",     "Place a cell")
      ).flatten

      askMenu("Choose one:", choices).headOption match {
        case Some("reaction") =>
          val target = askCountry("Place reaction marker in which country: ", getReactionCandidates())
          addEventTarget(target)
          addReactionMarker(target)

        case Some("besieged") =>
          val target = askCountry("Place besieged regime marker in which country: ", getBesiegedRegimeCandidates())
          addEventTarget(target)
          addBesiegedRegimeMarker(target)

        case Some(_) =>
          val target = askCountry("Place cell in which country: ", getCellCandidates())
          addEventTarget(target)
          testCountry(target)
          addSleeperCellsToCountry(target, 1)

        case None =>
          // Very unlikely!
          log("The are no candidate countries that can take reaction marker, besigned regime, or cell.", Color.Event)
      }
    }
    else {
      // Bot will place a reaction marker if possible, then besieged regime, then cell
      if (canPlaceReaction) {
        val target = JihadistBot.markerTarget(getReactionCandidates()).get
        addEventTarget(target)
        addReactionMarker(target)
      }
      else if (canPlaceBesieged) {
        val target = JihadistBot.markerTarget(getBesiegedRegimeCandidates()).get
        addEventTarget(target)
        addBesiegedRegimeMarker(target)
      }
      else {
        val target = JihadistBot.cellPlacementPriority(false)(getCellCandidates()).get
        addEventTarget(target)
        testCountry(target)
        addSleeperCellsToCountry(target, 1)
      }
    }
  }
}
