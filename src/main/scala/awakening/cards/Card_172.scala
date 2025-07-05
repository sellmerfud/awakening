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
import scala.collection.mutable.ListBuffer

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
    !c.truce && (
      game.cellsAvailable > 0 ||
      game.availablePlots.exists(p => p == Plot1 || p == Plot2) ||
      canPlaceBesiegedRegime(c) ||
      canPlaceReaction(c)
    )

  def getCandidates = PossibleCountries.filter(name => isCandidate(game.getCountry(name)))

  def getReactionCandidates = getCandidates.filter(name => canPlaceReaction(game.getCountry(name)))

  def getBesiegeCandidates = getCandidates.filter(name => canPlaceBesiegedRegime(game.getCountry(name)))

  def botBesiegeCandidates = getBesiegeCandidates.filter(name => !game.getMuslim(name).isIslamistRule)

  def enhGoodMuslimPlotCandidates = PossibleCountries
      .filter(isMuslim)
      .filter { name =>
        val m = game.getMuslim(name)
        !m.truce && m.isGood
      }

  def enhFairMuslimPlotCandidates = PossibleCountries
      .filter(isMuslim)
      .filter { name =>
        val m = game.getMuslim(name)
        !m.truce && m.isFair
      }

  val Pirates1 = 73  // From base game
  val Pirates2 = 183 // From Awakening expansion
  val BokoHaram = 186
  val IslamicMaghreb = 169
  def candidateCards = {
    // The order here is important because the Enhanced Bot will
    // take the first one in this list that is available
    val targets = List(Pirates2, Pirates1, BokoHaram, IslamicMaghreb)
    targets.filter(game.cardsDiscarded.contains)
  }

  // Returns true if the printed conditions of the event are satisfied
  override
  def eventConditionsMet(role: Role) =
    getCandidates.nonEmpty || candidateCards.nonEmpty


  // Returns true if the Bot associated with the given role will execute the event
  // on its turn.  This implements the special Bot instructions for the event.
  // When the event is triggered as part of the Human players turn, this is NOT used.
  override
  def botWillPlayEvent(role: Role): Boolean = true

  sealed trait Action
  case object PlaceReaction extends Action
  case object PlaceCell extends Action
  case object PlacePlot1 extends Action
  case object PlacePlot2 extends Action
  case object PlotInGoodMuslim extends Action
  case object PlotInFairMuslim extends Action
  case object PlotInKenya extends Action
  case object Besiege extends Action
  case object Draw extends Action

  // See Event Instructions table
  def standardBotActions: List[Action] = {
    val actions = new ListBuffer[Action]()
    var havePlot = game.availablePlots.exists(p => p == Plot1 || p == Plot2)

    if (botBesiegeCandidates.nonEmpty)
      actions.append(Besiege)

    if (game.cellsAvailable > 0)
      actions.append(PlaceCell)

    if (getReactionCandidates.nonEmpty)
      actions.append(PlaceReaction)

    if (havePlot && getCandidates.nonEmpty)
      actions.append(PlacePlot2)

    if (candidateCards.nonEmpty)
      actions.append(Draw)

    actions.toList.take(2)
  }

  // If possible, draw Boko Haram, then Pirates, then Islamic Maghreb from discard pile.
  // If Somalia and/or adjacent Muslim country at Good, place highest plot possible there (priority to Troops).
  // Place highest plot possible in Kenya if [Funding <9] and/or [US hard and Kenya hard].
  // Place reaction marker in Muslim (priority to highest # of reaction markers present, then highest awakening-reaction, then highest # of cells, then Yemen)
  // Place cell (same priorities as in no.4)
  // Place Besieged regime marker in Muslim (same priorities as in no.4)
  // If Somalia and/or adjacent Muslim country at Fair, place highest plot possible there (priority to Troops).
  def enhBotActions: List[Action] = {
    val actions = new ListBuffer[Action]()
    var havePlot = game.availablePlots.exists(p => p == Plot1 || p == Plot2)
    val plotInKenya = game.funding < MaxFunding || (game.usPosture == Hard && game.getNonMuslim(KenyaTanzania).isHard)

    // First draw one or two cards if possible
    if (candidateCards.nonEmpty)
      actions.append(Draw)

    if (havePlot && enhGoodMuslimPlotCandidates.nonEmpty) {
      actions.append(PlotInGoodMuslim)
      havePlot = false
    }

    if (havePlot && plotInKenya) {
      actions.append(PlotInKenya)
      havePlot = false
    }

    if (getReactionCandidates.nonEmpty)
      actions.append(PlaceReaction)

    if (game.cellsAvailable > 0)
      actions.append(PlaceCell)

    if (getBesiegeCandidates.nonEmpty)
      actions.append(Besiege)

    if (havePlot && enhFairMuslimPlotCandidates.nonEmpty)
      actions.append(PlotInFairMuslim)

    actions.toList.take(2)
  }

  // Carry out the event for the given role.
  // forTrigger will be true if the event was triggered during the human player's turn
  // and it associated with the Bot player.
  override
  def executeEvent(role: Role): Unit = {

    if (isHuman(role)) {
      val canReaction = getReactionCandidates.nonEmpty
      val canCell     = game.cellsAvailable > 0
      val canPlot1    = game.availablePlots.contains(Plot1)
      val canPlot2    = game.availablePlots.contains(Plot2)
      val canBesiege  = getBesiegeCandidates.nonEmpty
      val canDraw     = candidateCards.nonEmpty
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
            log(s"\nThe $role player draws one of the listed cards from the discard pile.", Color.Event)
            askCardDrawnFromDiscardPile(role, only = candidateCards.toSet)
              .map(deck(_).numAndName)
              .foreach { cardDisplay =>
                log(s"\nAdd $cardDisplay to your hand.", Color.Event)
              }
          case _ => // Avoid compiler warning
        }
      }
    }
    else {
      def bestPlot = (game.availablePlots.sorted.dropWhile(p => p != Plot1 && p != Plot2)).head
      // (priority to highest # of reaction markers present, then highest awakening-reaction, then highest # of cells, then Yemen)
      val enhBotMarkerCellPriorities = List(
        JihadistBot.MostReactionMarkersPriority,
        JihadistBot.HighestAwakeningMinusReactionPriority,
        JihadistBot.MostCellsPriority,
        new JihadistBot.CriteriaFilter( "Is Yemen", _.name == Yemen)
      )
      def enhMarkerCellTarget(candidates: List[String]): String =
        JihadistBot.topPriority(game.getCountries(candidates), enhBotMarkerCellPriorities)
          .map(_.name)
          .get
      val enhBotPlotPriorities = List(JihadistBot.WithTroopsPriority)
      def enhPlotTarget(candidates: List[String]): String =
        JihadistBot.topPriority(game.getCountries(candidates), enhBotPlotPriorities)
          .map(_.name)
          .get

      val actions = if (game.botEnhancements)
        enhBotActions
      else
        standardBotActions

      actions foreach { action =>
        println()
        action match {
          case Besiege =>
            val target = if (game.botEnhancements)
              enhMarkerCellTarget(getBesiegeCandidates)
            else
              JihadistBot.markerTarget(getBesiegeCandidates).get
            addEventTarget(target)
            addBesiegedRegimeMarker(target)

          case PlaceCell =>
            val target = if (game.botEnhancements)
              enhMarkerCellTarget(getCandidates)
            else
              JihadistBot.cellPlacementPriority(false)(getCandidates).get
            addEventTarget(target)
            addSleeperCellsToCountry(target, 1)

          case PlaceReaction =>
            val target = if (game.botEnhancements)
              enhMarkerCellTarget(getReactionCandidates)
            else
              JihadistBot.markerTarget(getReactionCandidates).get
            addEventTarget(target)
            addReactionMarker(target)

          case PlacePlot2|PlacePlot1 =>
            // Standard bot only
            val target = JihadistBot.plotTarget(getCandidates, game.funding >= 7).get
            addEventTarget(target)
            addAvailablePlotToCountry(target, bestPlot)

          case PlotInGoodMuslim =>
            // Enhanced Bot only
            val target = enhPlotTarget(enhGoodMuslimPlotCandidates)
            addEventTarget(target)
            addAvailablePlotToCountry(target, bestPlot)

          case PlotInFairMuslim =>
            // Enhanced Bot only
            val target = enhPlotTarget(enhFairMuslimPlotCandidates)
            addEventTarget(target)
            addAvailablePlotToCountry(target, bestPlot)

          case PlotInKenya =>
            // Enhanced Bot only
            addEventTarget(KenyaTanzania)
            addAvailablePlotToCountry(KenyaTanzania, bestPlot)

          case Draw =>
            // The enhanced bot take the first one in the list
            val cardNum = if (game.botEnhancements)
              candidateCards.head  // Enhanced bot take first in list sorted by priority
            else
              shuffle(candidateCards).head  // Standard Bot take one at random
            val display = cardNumAndName(cardNum)

            log(s"\nThe $role Bot draws $display from the discard pile.", Color.Event)
            processCardDrawn(role, cardNum, FromDiscard)

            log(s"\nTake $display from the discard pile and", Color.Event)
            if (game.botEnhancements)
              log(s"shuffle it into the $role Bot's hand.", Color.Event)
            else
              log(s"place it on top of the $role Bot's hand.", Color.Event)
        }
      }
    }
  }
}
