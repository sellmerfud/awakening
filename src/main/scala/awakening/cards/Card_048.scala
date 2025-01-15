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

// Card Text:
// ------------------------------------------------------------------
// Play if 1st card of jihadist Action Phase and a cell is available.
// Recruit in the US with the 2nd card. Succeeding on rolls of 1 or
// 2 and ignoring any US event.
// ------------------------------------------------------------------
object Card_048 extends Card(48, "Adam Gadahn", Jihadist, 1, NoRemove, NoLapsing, NoAutoTrigger) {
  // Used by the US Bot to determine if the executing the event would alert a plot
  // in the given country
  override
  def eventAlertsPlot(countryName: String, plot: Plot): Boolean = false

  // Used by the US Bot to determine if the executing the event would remove
  // the last cell on the map resulting in victory.
  override
  def eventRemovesLastCell(): Boolean = false

  // Returns true if the printed conditions of the event are satisfied
  override
  def eventConditionsMet(role: Role) =
    firstCardOfPhase(Jihadist) &&
    game.cellsToRecruit > 0    &&
    cacheYesOrNo(s"Does the $Jihadist player have another card in hand? (y/n) ")

  // Returns true if the Bot associated with the given role will execute the event
  // on its turn.  This implements the special Bot instructions for the event.
  // When the event is triggered as part of the Human players turn, this is NOT used.
  override
  def botWillPlayEvent(role: Role): Boolean = !game.botEnhancements // Not played by Enhanced Bot

  // Carry out the event for the given role.
  // forTrigger will be true if the event was triggered during the human player's turn
  // and it associated with the Bot player.
  override
  def executeEvent(role: Role): Unit = {
    val  prompt = if (isHuman(role))
      "Enter card # of card you wish to use for recruit in the US: "
    else
      s"Enter card # of the next card in the $Jihadist Bot's hand: "
    
    val card = deck(askCardNumber(prompt, allowNone = false).get)
    addSecondCardToPlayedCard(card.number)
    logCardPlay(Jihadist, card, playable = false, secondCard = true)

    def nextRecruit(completed: Int): Unit = 
      if (completed < card.ops && game.cellsToRecruit > 0) {
        val ord = ordinal(completed + 1)
        val die     = getDieRoll(s"Enter $ord recruit die roll: ", Some(role))
        val success = die < 3
        val result  = if (success) "succeeds" else "fails"
        log(s"$ord recruit $result with a roll of $die")
        if (success) {
          val numCells = if (game.jihadistIdeology(Potent)) {
            log(s"$Jihadist Bot with Potent Ideology places two cells for each success")
            2
          }
          else
            1
          addSleeperCellsToCountry(UnitedStates, numCells min game.cellsToRecruit)
        }
        nextRecruit(completed + 1)
      }

    sealed trait EventAction
    case object RecruitAction extends EventAction
    case object TriggerAction extends EventAction

    val actions = if (card.autoTrigger && isBot(role))
      List(TriggerAction, RecruitAction)
    else if (card.autoTrigger) {
      println()
      displayLine(s"""The "${card.cardName}" event will autonmatically trigger""", Color.Event)

      val choices = List(
        List(RecruitAction, TriggerAction) -> "Recruit before triggering the event",
        List(TriggerAction, RecruitAction) -> "Trigger the event before recruiting",
      )
      askMenu("\nChoose one:", choices).head
    }
    else
      List(RecruitAction)

    for (action <- actions) {
      action match {
        case TriggerAction =>
          log()
          log(s"""The "${card.cardName}" event triggers""", Color.Event)
          log(separator())
          card.executeEvent(US)  // Role does not matter for auto-trigger events
        case RecruitAction =>
          log()
          log(s"$Jihadist recruits in the US using: $card")
          log(separator())
          log("Recruit rolls will succeed with a roll of 1 or 2")
          if (game.cellsToRecruit == 1)
            log(s"There is 1 cell available for recruitment")
          else
            log(s"There are ${game.cellsToRecruit} cells available for recruitment")

          addEventTarget(UnitedStates)
          nextRecruit(0)
      }          
    }
  }
}
