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
// Do any 2 of the following in Somalia or in Adjacent African countries:
// place 1 Reaction marker,
// place 1 Cell,
// place a level 1 or 2 Plot,
// place 1 Besieged Regime marker,
// Select, reveal and draw Pirates, Boko Haram, or Islamic Maghreb from discard pile.
// ------------------------------------------------------------------
object Card_172 extends Card(172, "Al-Shabaab", Jihadist, 2, NoRemove, NoLapsing, NoAutoTrigger) {
  // Used by the US Bot to determine if the executing the event would alert a plot
  // in the given country
  override
  def eventAlertsPlot(countryName: String, plot: Plot): Boolean = false

  // Used by the US Bot to determine if the executing the event would remove
  // the last cell on the map resulting in victory.
  override
  def eventRemovesLastCell(): Boolean = false

  // Somalia and adjacent African countries
  val PossibleCountries = Somalia::getAdjacent(Somalia).filter(African.contains).sorted

  val canPlaceReaction = (c: Country) =>
    lapsingEventNotInPlay(ArabWinter) &&
    c.isMuslim &&
    c.asInstanceOf[MuslimCountry].canTakeAwakeningOrReactionMarker

  val canPlaceBesiegedRegime = (c: Country) =>
    c.isMuslim &&
    c.asInstanceOf[MuslimCountry].canTakeBesiegedRegimeMarker

  val isCandidate = (c: Country) =>
    game.cellsAvailable > 0 ||
    game.availablePlots.exists(p => p == Plot1 || p == Plot2) ||
    canPlaceBesiegedRegime(c) ||
    canPlaceReaction(c)

  def getCandidates = PossibleCountries.filter(name => isCandidate(game.getCountry(name)))

  def getReactionCandidates = getCandidates.filter(name => canPlaceReaction(game.getCountry(name)))

  def getBesiegeCandidates = getCandidates.filter(name => canPlaceBesiegedRegime(game.getCountry(name)))

  def candidateCards() = {
    val targets = List(73, 183, 186, 169)
    targets.filter(game.cardsDiscarded.contains)
  }

  // Returns true if the printed conditions of the event are satisfied
  override
  def eventConditionsMet(role: Role) =
    getCandidates.nonEmpty ||
    candidateCards().nonEmpty


  // Returns true if the Bot associated with the given role will execute the event
  // on its turn.  This implements the special Bot instructions for the event.
  // When the event is triggered as part of the Human players turn, this is NOT used.
  override
  def botWillPlayEvent(role: Role): Boolean = true

  // Carry out the event for the given role.
  // forTrigger will be true if the event was triggered during the human player's turn
  // and it associated with the Bot player.
  override
  def executeEvent(role: Role): Unit = {
      sealed trait Choice
      case object PlaceReaction extends Choice
      case object PlaceCell extends Choice
      case object PlacePlot1 extends Choice
      case object PlacePlot2 extends Choice
      case object Besiege extends Choice
      case object Draw extends Choice

    if (isHuman(role)) {
      val canReaction = getReactionCandidates.nonEmpty
      val canCell     = game.cellsAvailable > 0
      val canPlot1    = game.availablePlots.contains(Plot1)
      val canPlot2    = game.availablePlots.contains(Plot2)
      val canBesiege  = getBesiegeCandidates.nonEmpty
      val canDraw     = candidateCards().nonEmpty
      val choices = List(
        choice(canReaction,PlaceReaction, "Place 1 Reaction marker"),
        choice(canCell,    PlaceCell,     "Place 1 cell"),
        choice(canPlot1,   PlacePlot1,    "Place a level 1 plot"),
        choice(canPlot2,   PlacePlot2,    "Place a level 2 plot"),
        choice(canBesiege, Besiege,       "Place a besieged regime marker"),
        choice(canDraw,    Draw,          "Select Pirates, Boko Haram, or Islamic Maghreb from discard pile")
      ).flatten

      askMenu("Do any 2 of the following:", choices, 2, repeatsOK = false) foreach { action =>
        println()
        action match {
          case PlaceReaction =>
            val target = askCountry("Place reaction marker in which country: ", getReactionCandidates)
            addEventTarget(target)
            addReactionMarker(target)
          case PlaceCell =>
            val target = askCountry("Place a cell in which country: ", getCandidates)
            addEventTarget(target)
            addSleeperCellsToCountry(target, 1)
          case PlacePlot1 =>
            val target = askCountry("Place a level 1 plot in which country: ", getCandidates)
            addEventTarget(target)
            addAvailablePlotToCountry(target, Plot1)
          case PlacePlot2 =>
            val target = askCountry("Place a level 2 plot in which country: ", getCandidates)
            addEventTarget(target)
            addAvailablePlotToCountry(target, Plot2)
          case Besiege  =>
            val target = askCountry("Place besieged regime marker in which country: ", getBesiegeCandidates)
            addEventTarget(target)
            addBesiegedRegimeMarker(target)
          case Draw =>
            askCardDrawnFromDiscardPile(role, only = candidateCards().toSet)
        }
      }
    }
    else {
      // See Event Instructions table
      val besiegeTarget = JihadistBot.markerTarget(
        getBesiegeCandidates.filter(name => !game.getMuslim(name).isIslamistRule)
      )
      val cellTarget = if (game.cellsAvailable > 0)
        JihadistBot.cellPlacementPriority(false)(getCandidates)
      else
        None
      val reactionTarget = JihadistBot.markerTarget(getReactionCandidates)
      val plotTarget = if (game.availablePlots.exists(p => p == Plot1 || p == Plot2))
        JihadistBot.plotTarget(getCandidates, game.funding >= 7)
      else
        None
      val actions = List(
        besiegeTarget.map(_ => Besiege),
        cellTarget.map(_ => PlaceCell),
        reactionTarget.map(_ => PlaceReaction),
        plotTarget.map(_ => PlacePlot2),
        if (candidateCards().nonEmpty) Some("draw") else None,
      ).flatten.take(2)

      actions foreach { action =>
        println()
        action match {
          case Besiege  =>
            addEventTarget(besiegeTarget.get)
            addBesiegedRegimeMarker(besiegeTarget.get)
          case PlaceCell =>
            addEventTarget(cellTarget.get)
            addSleeperCellsToCountry(cellTarget.get, 1)
          case PlaceReaction =>
            addEventTarget(reactionTarget.get)
            addReactionMarker(reactionTarget.get)
          case PlacePlot1|PlacePlot2 =>
            // Use Plot 2 if available otherwise Plot 1
            val plot = (game.availablePlots.sorted.dropWhile(p => p != Plot1 && p != Plot2)).head
            addEventTarget(plotTarget.get)
            addAvailablePlotToCountry(plotTarget.get, plot)
          case Draw =>
            val cardNum = shuffle(candidateCards()).head
            processCardDrawn(role, cardNum, FromDiscard)
        }
      }
    }
  }
}
