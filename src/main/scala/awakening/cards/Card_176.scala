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
// Play in a Tested Shia-Mix country, Jordan, or Morocco and not at
// Good, Islamist Rule, Civil War or Regime Change.
// Remove all pieces and markers; return to Unmarked status.
// ------------------------------------------------------------------
object Card_176 extends Card(176, "Change of State", Jihadist, 2, NoRemove, NoLapsing, NoAutoTrigger) {
  // Used by the US Bot to determine if the executing the event would alert a plot
  // in the given country
  override
  def eventAlertsPlot(countryName: String, plot: Plot): Boolean = false

  // Used by the US Bot to determine if the executing the event would remove
  // the last cell on the map resulting in victory.
  override
  def eventRemovesLastCell(): Boolean = false

  val isCandidate = (m: MuslimCountry) => {
    !m.truce &&
    (m.name == Jordan || m.name == Morocco || (m.isTested && m.isShiaMix)) &&
    !(m.isGood || m.isIslamistRule || m.civilWar || m.inRegimeChange)
  }
  val isBotCandidate = (m: MuslimCountry) => isCandidate(m) && m.isFair && m.isAlly

  def getCandidates = countryNames(game.muslims.filter(isCandidate))

  def getStandardBotCandidates = countryNames(game.muslims.filter(isBotCandidate))

    // ------------------------------------------------------------------
  // Get target for the Status Quo event
  def standardBotTarget(names: List[String]): Option[String] = {
    val flowchart = List(
      new JihadistBot.CriteriaFilter("Fair Ally", JihadistBot.muslimTest(m => m.isFair && m.isAlly)))
    val priorities =
      List(JihadistBot.WorstJihadDRMPriority,
      JihadistBot.FairPriority,
      JihadistBot.HighestResourcePriority,
           // Ths No Troops is on the event card but does not make sense to me
           // since the troop would be removed when the country becomes unmarked?
           new JihadistBot.CriteriaFilter("No Troops", JihadistBot.muslimTest(m => m.totalTroops == 0)))

    JihadistBot.botLog("Find \"Change of State\" target", Color.Debug)
    val candidates = game.getCountries(names)
    val favorableCandidates = JihadistBot.selectCandidates(candidates, flowchart)
    // If the event was triggered by US play then there the flowchart may
    // fail to find a "favorable candidate"
    if (favorableCandidates.nonEmpty)
      JihadistBot.topPriority(favorableCandidates, priorities).map(_.name)
    else
      JihadistBot.topPriority(candidates, priorities).map(_.name)
  }

  // Playable in non-adversary, non-Training Camps countries. Unplayable unless one of the following candidates can be chosen:
  // Fair 2+ res country with [awakening-reaction>1] or [awakening-reaction=0 or 1 AND less than 3 cells],
  // If none: Poor 2+ res country with [awakening-reaction>1], 
  //
  // priority to highest awakening-reaction, then highest res, then aid, then ally).
  def getEnhBotCandidates = {
    val nonAdversaryOrTC = (m: MuslimCountry) => !m.isAdversary && !game.isTrainingCamp(m.name)
    val fairCandidates = game.muslims
      .filter { m =>
        val delta = m.awakening - m.reaction
        m.isFair &&
        JihadistBot.enhBotResourceValue(m) >= 2 &&
        isCandidate(m) &&
        nonAdversaryOrTC(m) &&
        (delta > 1 || (delta >= 0 && m.pieces.totalCells < 3))
      }
    val poorCandidates = game.muslims
      .filter { m =>
        val delta = m.awakening - m.reaction
        m.isPoor &&
        JihadistBot.enhBotResourceValue(m) >= 2 &&
        isCandidate(m) &&
        nonAdversaryOrTC(m) &&
        delta > 1
      }

    if (fairCandidates.nonEmpty)
      fairCandidates
    else
      poorCandidates
  }

  def enhBotTarget(candidates: List[MuslimCountry]): Option[String] = {
    val priorities = List(
      JihadistBot.HighestAwakeningMinusReactionPriority,
      JihadistBot.HighestPrintedResourcePriority,
      JihadistBot.WithAidPriority,
      JihadistBot.AllyPriority,    
    )

    JihadistBot.topPriority(candidates, priorities)
      .map(_.name)
  }

  // Returns true if the printed conditions of the event are satisfied
  override
  def eventConditionsMet(role: Role) = getCandidates.nonEmpty

  // Returns true if the Bot associated with the given role will execute the event
  // on its turn.  This implements the special Bot instructions for the event.
  // When the event is triggered as part of the Human players turn, this is NOT used.
  override
  def botWillPlayEvent(role: Role): Boolean = if (game.botEnhancements)
    getEnhBotCandidates.nonEmpty
  else
    getStandardBotCandidates.nonEmpty

  // Carry out the event for the given role.
  // forTrigger will be true if the event was triggered during the human player's turn
  // and it associated with the Bot player.
  override
  def executeEvent(role: Role): Unit = {
    val target = if (isHuman(role))
      askCountry("Select country: ", getCandidates)
    else if (game.botEnhancements)
      enhBotTarget(getEnhBotCandidates)
        .orElse(enhBotTarget(game.getMuslims(getStandardBotCandidates)))
        .orElse(enhBotTarget(game.getMuslims(getCandidates)))
        .get
    else if (getStandardBotCandidates.nonEmpty)
      standardBotTarget(getStandardBotCandidates).get
    else
      standardBotTarget(getCandidates).get

    addEventTarget(target)
    setCountryToUntested(target) // Strip the country of all markers and make it Untested
  }
}
