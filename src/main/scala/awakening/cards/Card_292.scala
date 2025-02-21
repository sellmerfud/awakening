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
// Place a Cadre in up to 3 countries.
// Test if Unmarked.
// ------------------------------------------------------------------
object Card_292 extends Card(292, "Amaq News Agency", Jihadist, 2, NoRemove, NoLapsing, NoAutoTrigger) {
  // Used by the US Bot to determine if the executing the event would alert a plot
  // in the given country
  override
  def eventAlertsPlot(countryName: String, plot: Plot): Boolean = false

  // Used by the US Bot to determine if the executing the event would remove
  // the last cell on the map resulting in victory.
  override
  def eventRemovesLastCell(): Boolean = false

  // Note: The code only allows 1 cadre marker per country.
  // I have since realized that this is incorrect.
  def getCandidates = countryNames(game.countries)

  def getBotCandidates = {
    val isCandidate = if (game.botEnhancements)
      (c: Country) => !c.hasCadre && c.totalCells == 0 && !JihadistBot.isCadreRemovalCandidate(c)
    else
      (c: Country) => !c.hasCadre && c.totalCells == 0
    countryNames(game.countries.filter(isCandidate))
  }

  // Returns true if the printed conditions of the event are satisfied
  override
  def eventConditionsMet(role: Role) = true

  // Returns true if the Bot associated with the given role will execute the event
  // on its turn.  This implements the special Bot instructions for the event.
  // When the event is triggered as part of the Human players turn, this is NOT used.
  override
  def botWillPlayEvent(role: Role): Boolean = getBotCandidates.nonEmpty

  // Carry out the event for the given role.
  // forTrigger will be true if the event was triggered during the human player's turn
  // and it associated with the Bot player.
  override
  def executeEvent(role: Role): Unit = {
    val numCadres = if (isHuman(role))
      askInt("Place how many cadres", 1, 3, Some(3))
    else
      3

    def nextCadre(num: Int, candidates: List[String]): Unit =
      if (num <= numCadres && candidates.nonEmpty) {
        val target = if (isHuman(role))
          askCountry(s"Place ${ordinal(num)} cadre is which country: ", candidates)
        else
          JihadistBot.recruitTravelToPriority(candidates).get

        addEventTarget(target)
        addCadreToCountry(target)
        nextCadre(num + 1, candidates.filterNot(_ == target))
      }

    // Used when the event was triggered during US turn
    val botCriteria = List(
      new JihadistBot.CriteriaFilter("No Cadre present", _.hasCadre == false),
      new JihadistBot.CriteriaFilter("No cells present", _.totalCells == 0)
    )

    if (isHuman(role))
      nextCadre(1, getCandidates)
      else if (getBotCandidates.nonEmpty)
        nextCadre(1, getBotCandidates)
      else
        log(s"\nThe Jihadist Bot chooses to not place any cadres.", Color.Event)
  }
}
