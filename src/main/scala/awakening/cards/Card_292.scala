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
import scala.collection.mutable.ListBuffer

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

  def getCandidates = countryNames(game.countries.filter(!_.truce))

  def getStandardBotCandidates = {
    val isCandidate =
      (c: Country) => !c.truce && !c.hasCadre && c.totalCells == 0
    countryNames(game.countries.filter(isCandidate))
  }

  // Untested Non-Muslims (only if US posture is Hard)
  def getEnhBotNonMuslimCandidates = if (game.usPosture == Hard) {
    val isCandidate = (n: NonMuslimCountry) =>
      n.isUntested &&
      !n.truce &&
      !JihadistBot.isCadreRemovalCandidate(n)

    game.nonMuslims.filter(isCandidate)
  }
  else
    Nil

  def getEnhBotMuslimCandidates = {
    val isCandidate = (m: MuslimCountry) =>
      !m.truce &&
      !m.hasCadre &&
      m.totalCells == 0 &&
      !JihadistBot.isCadreRemovalCandidate(m)

    game.muslims.filter(isCandidate)
  }

  // Returns true if the printed conditions of the event are satisfied
  override
  def eventConditionsMet(role: Role) = true

  // Returns true if the Bot associated with the given role will execute the event
  // on its turn.  This implements the special Bot instructions for the event.
  // When the event is triggered as part of the Human players turn, this is NOT used.
  override
  def botWillPlayEvent(role: Role): Boolean = if (game.botEnhancements)
    game.usPosture == Hard &&
    game.hardSoftDelta < 2 &&
    game.gwotPenalty == 0 &&
    (getEnhBotNonMuslimCandidates.nonEmpty || getEnhBotMuslimCandidates.nonEmpty)
  else
    getStandardBotCandidates.nonEmpty


  // Carry out the event for the given role.
  // forTrigger will be true if the event was triggered during the human player's turn
  // and it associated with the Bot player.
  override
  def executeEvent(role: Role): Unit = {
    if (isHuman(role)) {
      val numCadres = askInt("Place how many cadres", 0, 3, Some(3))

      def nextCadre(num: Int, candidates: List[String]): Unit =
        if (num <= numCadres && candidates.nonEmpty) {
          val target = askCountry(s"Place ${ordinal(num)} cadre is which country: ", candidates)
  
          addEventTarget(target)
          addCadreToCountry(target)
          nextCadre(num + 1, candidates.filterNot(_ == target))
        }

      if (numCadres > 0)
        nextCadre(1, getCandidates)
      else
        log(s"\nThe Jihadist does not place any cadres.", Color.Event)
    }
    else if (game.botEnhancements) {
      // Enhanced Bot
      val targets = new ListBuffer[String]

      def nextMuslimTarget(remaining: Int, candidates: List[String]): Unit =
        if (remaining > 0 && candidates.nonEmpty) {
          val target = JihadistBot.recruitTravelToPriority(candidates).get
          targets += target
          nextMuslimTarget(remaining - 1, candidates.filterNot(_ == target))
        }
      
      // Place one cadre in an unmarked Non-Muslim country
      if (getEnhBotNonMuslimCandidates.nonEmpty)
        targets += JihadistBot.topPriority(getEnhBotNonMuslimCandidates, JihadistBot.EnhUnmarkedNonMuslimTravelPriorities)
            .map(_.name)
            .get
      // Place the remaining cadres in qualifiying Muslim countries
      nextMuslimTarget(3 - targets.size, countryNames(getEnhBotMuslimCandidates))

      if (targets.isEmpty)
        log(s"\nThe Jihadist Bot does not place any cadres.", Color.Event)
      else
        for (target <- targets.toList) {
          addEventTarget(target)
          addCadreToCountry(target)
        }
    }
    else {
      // Standard Bot
      def nextCadre(num: Int, candidates: List[String]): Unit =
        if (num <= 3 && candidates.nonEmpty) {
          val target = JihadistBot.recruitTravelToPriority(candidates).get
          addEventTarget(target)
          addCadreToCountry(target)
          nextCadre(num + 1, candidates.filterNot(_ == target))
        }
      
      if (getStandardBotCandidates.nonEmpty)
        nextCadre(1, getStandardBotCandidates)
      else
        log(s"\nThe Jihadist Bot does not place any cadres.", Color.Event)
    }
  }
}
