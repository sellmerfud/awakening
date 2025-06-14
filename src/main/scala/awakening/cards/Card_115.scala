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
// Play if Indonesia or an adjacent country has a cell and is Ally or Hard.
// If US play, remove the cell, draw 2 cards, REMOVE this card.
// If jihadist, place an available plot there.
// ------------------------------------------------------------------
object Card_115 extends Card(115, "Hambali", Unassociated, 3, USRemove, NoLapsing, NoAutoTrigger) {
  // Used by the US Bot to determine if the executing the event would alert a plot
  // in the given country
  override
  def eventAlertsPlot(countryName: String, plot: Plot): Boolean = false

  def validCountries = game.getCountries(IndonesiaMalaysia :: getAdjacent(IndonesiaMalaysia))
  def getMuslimCandidates = validCountries
    .filter {
      case m: MuslimCountry => !m.truce && m.totalCells > 0 && m.isAlly
      case _ => false
    }

  def getNonMuslimCandidates = validCountries
    .filter {
      case n: NonMuslimCountry => !n.truce && n.totalCells > 0 && n.isHard
      case _ => false
    }

  def getCandidates = countryNames(getMuslimCandidates:::getNonMuslimCandidates)

  // Used by the US Bot to determine if the executing the event would remove
  // the last cell on the map resulting in victory.
  override
  def eventRemovesLastCell(): Boolean = getCandidates.exists(name => USBot.wouldRemoveLastCell(name, 1))

  // Returns true if the printed conditions of the event are satisfied
  override
  def eventConditionsMet(role: Role) = getCandidates.nonEmpty

  // Returns true if the Bot associated with the given role will execute the event
  // on its turn.  This implements the special Bot instructions for the event.
  // When the event is triggered as part of the Human players turn, this is NOT used.
  override
  def botWillPlayEvent(role: Role): Boolean = role match {
    case US =>
      true
      
    case Jihadist if game.botEnhancements =>
      val indonesia = game.getMuslim(IndonesiaMalaysia)
      val pakistan = game.getMuslim(Pakistan)
      game.availablePlots.nonEmpty && (
        (indonesia.governance < Poor && getMuslimCandidates.exists(_.name == IndonesiaMalaysia)) ||
        (pakistan.governance < Poor && getMuslimCandidates.exists(_.name == Pakistan)) ||
        (game.funding < 8 && getNonMuslimCandidates.nonEmpty)
      )

    case Jihadist =>
      game.availablePlots.nonEmpty
  }

  def enhBotTarget(names: List[String]): String = {
    import JihadistBot.{ CriteriaFilter, muslimTest, nonMuslimTest }
    val muslimPriorities = List(
      new CriteriaFilter("Indonesia with Good governance", muslimTest(m => m.name == IndonesiaMalaysia && m.isGood)),
      new CriteriaFilter("Pakistan with Good governance", muslimTest(m => m.name == Pakistan && m.isGood)),
      new CriteriaFilter("Indonesia with Fair governance", muslimTest(m => m.name == IndonesiaMalaysia && m.isFair)),
      new CriteriaFilter("Pakistan with Fair governance", muslimTest(m => m.name == Pakistan && m.isFair)),
    )
    val nonMuslimPriorities = if (game.funding < 8)
      List(new CriteriaFilter("Non-Muslim", nonMuslimTest(_ => true)))
    else
      Nil
    val priorities = muslimPriorities:::nonMuslimPriorities
    JihadistBot.botLog("Find \"Hambali\" target", Color.Debug)
    JihadistBot.topPriority(game.getCountries(getCandidates), priorities).map(_.name).get
  }

  // Carry out the event for the given role.
  // forTrigger will be true if the event was triggered during the human player's turn
  // and it associated with the Bot player.
  override
  def executeEvent(role: Role): Unit =
    role match {
      case US =>
        val (name, (active, sleeper, sadr)) = if (isHuman(role)) {
          val name = askCountry("Select country: ", getCandidates)
          (name, askCells(name, 1, sleeperFocus = true))
        }
        else {
          val name = USBot.disruptPriority(getCandidates).get
          (name, USBot.chooseCellsToRemove(name, 1))
        }

        addEventTarget(name)
        removeCellsFromCountry(name, active, sleeper, sadr, addCadre = true)
        log(s"\nThe $US player draws two cards", Color.Event)
        askMultipleCardsDrawnFromDrawPile(role, 2) match {
          case Nil =>
          case cards =>
            val cardDisplay = andList(cards.map(deck(_).numAndName))
            if (isHuman(role))
              log(s"\nAdd $cardDisplay to your hand.", Color.Event)
            else if (role == Jihadist && game.botEnhancements)
              log(s"\nShuffle $cardDisplay into the $role Bot's hand.", Color.Event)
            else 
              log(s"\nPlace $cardDisplay on top of the $role Bot's hand.", Color.Event)
        }

      case Jihadist if game.availablePlots.nonEmpty =>
        val (name, plot) = if (isHuman(role)) {
          val name = askCountry("Select country: ", getCandidates)
          (name, askAvailablePlots(1, ops = 3).head)
        }
        else {
          val name = if (game.botEnhancements)
            enhBotTarget(getCandidates)
          else
            JihadistBot.plotPriority(getCandidates).get
          (name, JihadistBot.selectPlotMarkers(name, 1, game.availablePlots).head)
        }

        addEventTarget(name)
        addAvailablePlotToCountry(name, plot)

      case Jihadist =>
        log("\nNo available plots. The event has no effect", Color.Event)
    }
}
