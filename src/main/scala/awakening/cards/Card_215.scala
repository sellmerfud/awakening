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
// US may play if Caliphate Capital on map.
// If US play, +3 Prestige, REMOVE.
// If Jihadist, place up to 3 Cells total in Syria/Iraq (may come from anywhere).
// Then either place 1 Cell (from Track) via Schengen Table roll OR
// draw Paris Attacks or Training Camps from discard pile.
//
// Normal Jihadist Bot event instructions:
//   Place Cells from Track only. Select card nearest bottom if possible, else Schengen.
// Enhanced Jihadist Bot event instructions:
//   Place MAX. CELLS POSSIBLE, FROM TRACK FIRST, THEN USE TRAVEL FROM PRIORITIES.
//    Priorities for 2nd part of event:
//       Select "Training Camps" from discard pile
//       Select "Paris Attacks" from discard pile
//       Place cell using Schengen table.

// ------------------------------------------------------------------
object Card_215 extends Card(215, "Abu Bakr al-Baghdadi", Unassociated, 2, USRemove, NoLapsing, NoAutoTrigger) {
  // Used by the US Bot to determine if the executing the event would alert a plot
  // in the given country
  override
  def eventAlertsPlot(countryName: String, plot: Plot): Boolean = false

  // Used by the US Bot to determine if the executing the event would remove
  // the last cell on the map resulting in victory.
  override
  def eventRemovesLastCell(): Boolean = false

  // Returns true if the printed conditions of the event are satisfied
  // US only if Caliphate capital on the map.
  override
  def eventConditionsMet(role: Role) = role == Jihadist || game.caliphateDeclared


  // Enhanced Bot will only play if it can place at least one cell in Syria/Iraq
  def enhJihadistBotAbuBakrPlayable: Boolean = {
    val withCells = game.countries.filter { c =>
      c.name != Syria && c.name != Iraq &&
      (JihadistBot.hasCellForTravel(c, Syria, placement = true) || JihadistBot.hasCellForTravel(c, Iraq, placement = true))
    }
    game.cellsAvailable > 0 || withCells.nonEmpty
  }

  // Returns true if the Bot associated with the given role will execute the event
  // on its turn.  This implements the special Bot instructions for the event.
  // When the event is triggered as part of the Human players turn, this is NOT used.
  override
  def botWillPlayEvent(role: Role): Boolean = role match {
    // US Bot will play event if Presting == 12 to remove the card from the game.
    case US => true
    case Jihadist if game.botEnhancements => enhJihadistBotAbuBakrPlayable
    case Jihadist => game.cellsAvailable > 0
  }

  val TCNum = 196  // Training Camps card
  val PANum = 182  // Paris Attacks card
  val CardTargets = List(PANum, TCNum)
  def cardCandidates = CardTargets.filter(game.cardsDiscarded.contains)

  // Carry out the event for the given role.
  // forTrigger will be true if the event was triggered during the human player's turn
  // and it associated with the Bot player.
  override
  def executeEvent(role: Role): Unit = {
    if (role == US)
      increasePrestige(3)
    else {
      // Jihadist
      // See Event Instructions table
      // Can possibly declare Caliphate, only in Syria or Iraq
      if (isHuman(role)) {
        println("\nPlace up to 3 cells in Syria and/or Iraq.")
        println(separator())
        val inSyria = askInt("How many cells do you wish to place in Syria? ", 0, 3)
        val inIraq = askInt("How many cells do you wish to place in Iraq? ", 0, 3 - inSyria)
        val targets = List((Syria, inSyria),(Iraq, inIraq)).filterNot(_._2 == 0)
        for ((target, num) <- targets) {
          addEventTarget(target)
          val withCells = countryNames(game.countries.filter(c => c.name != target && c.cells > 0))
          println(s"\nChoose ${amountOf(num, "cell")} to place in $target")
          val sources = askCellsFromAnywhere(num, trackOK = true, withCells, sleeperFocus = false)
          println()
          moveCellsToTarget(target, sources)
          // Count how many were actually placed (based on availability)
          val totalPlaced = (sources map (_.total)).sum
          if (jihadistChoosesToDeclareCaliphate(target, num))
            declareCaliphate(target)
        }

        if (game.cellsAvailable == 0 && cardCandidates.isEmpty) {
          log(s"\nNo cells available to place in Schengen country.", Color.Event)
          log(s"\nNeither ${orList(CardTargets.map(cardNumAndName))} is in the discard pile.", Color.Event)
        }
        else {
          sealed trait Choice
          case object PlaceCell extends Choice
          case class DrawCard(num: Int) extends Choice
          val cardChoices = cardCandidates
            .map(num => DrawCard(num) -> s"Draw [${cardNumAndName(num)}] from the discard pile")
          val choices = if (game.cellsAvailable > 0)
            (PlaceCell -> "Place a cell from the track in a random Schengen country") :: cardChoices
          else
            cardChoices

          askMenu("\nChoose one:", choices).head match {
            case PlaceCell =>
              log(s"\n$role places a cell in a random Schengen country.", Color.Event)
              val schengen = randomSchengenCountry
              addEventTarget(schengen.name)
              addSleeperCellsToCountry(schengen.name, 1)

            case DrawCard(cardNum) =>
              processCardDrawn(role, cardNum, FromDiscard)
              log(s"\n$role takes [${cardNumAndName(cardNum)}] from the discard pile", Color.Event)
          }
        }
      }
      else if (game.botEnhancements) {
        // Enhanced Bot will move cells using Travel From priorities once all
        // cells on the track have been used.
        val target = JihadistBot.cellPlacementPriority(true)(Syria::Iraq::Nil).get
        val sourceCountries = countryNames(game.countries.filter(c => JihadistBot.hasCellForTravel(c, target, placement = true)))
        addEventTarget(target)

        val placements = JihadistBot.selecCellsToPlace(target, sourceCountries, 3)
        val totalPlaced = placements.map(_.total).sum
        moveCellsToTarget(target, placements)

        if (jihadistChoosesToDeclareCaliphate(target, totalPlaced))
          declareCaliphate(target)

        if (cardCandidates.nonEmpty) {
          val cardNum = if (cardCandidates.contains(TCNum))
            TCNum
          else
            PANum

          processCardDrawn(role, cardNum, FromDiscard)
          log(s"\nTake [${cardNumAndName(cardNum)}] from the discard pile and", Color.Event)
          log(s"shuffle it into the $role hand", Color.Event)
        }
        else if (game.cellsAvailable > 0) {
          log(s"\n$role places a cell in a random Schengen country.", Color.Event)
          val schengen = randomSchengenCountry
          addEventTarget(schengen.name)
          addSleeperCellsToCountry(schengen.name, 1)
        }
      }
      else {
        val target = JihadistBot.cellPlacementPriority(true)(Syria::Iraq::Nil).get
        addEventTarget(target)
        val num = 3 min game.cellsAvailable
        addSleeperCellsToCountry(target, num)
        if (jihadistChoosesToDeclareCaliphate(target, num))
          declareCaliphate(target)

        val cardChoices = game.cardsDiscarded
          .reverse   // Will take the on closes to the bottom
          .filter(CardTargets.contains)

        if (cardChoices.nonEmpty) {
          val cardNum = cardChoices.head
          processCardDrawn(role, cardNum, FromDiscard)
          log(s"\nTake [${cardNumAndName(cardNum)}] from the discard pile and", Color.Event)
          log(s"place it on top of the $role hand", Color.Event)
        }
        else if (game.cellsAvailable > 0) {
          log(s"\n$role places a cell in a random Schengen country.", Color.Event)
          val schengen = randomSchengenCountry
          addEventTarget(schengen.name)
          addSleeperCellsToCountry(schengen.name, 1)
        }
      }
    }
  }
}
