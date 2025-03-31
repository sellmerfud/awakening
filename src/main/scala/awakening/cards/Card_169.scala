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
// Play in one of the following if Poor:
// Libya, Algeria/Tunisia, Morocco, Mali or Nigeria.
// Place 1 Cell or +1 Funding.
// If country is in Civil War or part of Caliphate, place 2 Cells or +2 Funding.
// Roll Serbia's Posture.
// All Travel to/within Schengen countries must roll as if not Adjacent this Turn.
// ------------------------------------------------------------------
object Card_169 extends Card(169, "Islamic Maghreb", Jihadist, 1, NoRemove, Lapsing, NoAutoTrigger) {
  // Used by the US Bot to determine if the executing the event would alert a plot
  // in the given country
  override
  def eventAlertsPlot(countryName: String, plot: Plot): Boolean = false

  // Used by the US Bot to determine if the executing the event would remove
  // the last cell on the map resulting in victory.
  override
  def eventRemovesLastCell(): Boolean = false

  val CandidateCountries = Set(Libya, AlgeriaTunisia, Morocco, Mali, Nigeria)
  val isCandidate = (c: Country) => CandidateCountries(c.name) && !c.truce && c.isPoor
  val isImprovedCandidate = (c: Country) => c match {
    case _: NonMuslimCountry => false
    case m: MuslimCountry => m.civilWar || game.isCaliphateMember(m.name)
  }

  def getCandidates = countryNames(
    game.getCountries(CandidateCountries.toList).filter(isCandidate)
  )

  def getImprovedCandidates = countryNames(
    game.getCountries(CandidateCountries.toList).filter(c => isCandidate(c) && isImprovedCandidate(c))
  )
  // Returns true if the printed conditions of the event are satisfied
  override
  def eventConditionsMet(role: Role) = getCandidates.nonEmpty

  // Returns true if the Bot associated with the given role will execute the event
  // on its turn.  This implements the special Bot instructions for the event.
  // When the event is triggered as part of the Human players turn, this is NOT used.
  override
  def botWillPlayEvent(role: Role): Boolean = if (game.botEnhancements)
    game.usPosture == Soft && game.funding < 9 // Playable if US soft and Funding<9. Always raise Funding.
  else
    game.funding < 8 || game.cellsAvailable > 0

  // Carry out the event for the given role.
  // forTrigger will be true if the event was triggered during the human player's turn
  // and it associated with the Bot player.
  override
  def executeEvent(role: Role): Unit = {
    sealed trait Choice
    case object Funding extends Choice
    case object Cells extends Choice
    def number(target: String) = if (isImprovedCandidate(game.getCountry(target)))
        2
      else
        1
    val (target, action) = if (isHuman(role)) {
      val target = askCountry("Select country: ", getCandidates)
      val action = if (game.cellsAvailable == 0 && game.funding == 9)
        None
      else {
        val choices = List(
          Some(Cells)   -> s"Place ${amountOf(number(target), "cell")} in $target",
          Some(Funding) -> s"Increase funding by ${number(target)}")
        askMenu("Choose one:", choices).head
      }
      (target, action)
    }
    else {
      val candidates = getImprovedCandidates match {
        case Nil => getCandidates
        case c => c
      }
      val target = JihadistBot.cellPlacementPriority(false)(candidates).get
      val action = (game.cellsAvailable > 0, game.funding) match {
        case (false, 9) => None
        case (false, _) => Some(Funding)
        case (_, f) if game.botEnhancements && f < 9 => Some(Funding)
        case (_, f) if f < 8 => Some(Funding) // Standard Bot
        case (true, _) => Some(Cells)
        case _ => None
      }
      (target, action)
    }

    log(s"\nJihadist chooses $target", Color.Event)
    action match {
      case Some(Funding) =>
        increaseFunding(number(target))

      case Some(Cells) =>
        addEventTarget(target)
        addSleeperCellsToCountry(target, number(target) min game.cellsAvailable)

      case None =>
        log("\nThere are no cells available to place and funding is at 9.", Color.Event)
    }

    addEventTarget(Serbia)
    rollCountryPosture(Serbia)
    log("\nTravel to/within Schengen countries requires a roll for the rest of this turn", Color.Event)
  }
}
