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
// End Civil War in one country. If Cells outnumber Militia + Troops,
// make Poor Adversary. If Militia + Troops outnumber Cells, make Fair Ally.
// Otherwise no change in Governance or Alignment.
// ------------------------------------------------------------------
object Card_235 extends Card(235, "Qadhafi", Unassociated, 3, NoRemove, NoLapsing, NoAutoTrigger) {
  // Used by the US Bot to determine if the executing the event would alert a plot
  // in the given country
  override
  def eventAlertsPlot(countryName: String, plot: Plot): Boolean = false

  // Used by the US Bot to determine if the executing the event would remove
  // the last cell on the map resulting in victory.
  override
  def eventRemovesLastCell(): Boolean = false

  def getCandidates = countryNames(game.muslims.filter(m => !m.truce && m.civilWar))

  def usBotQadhafiCandidates =
    game.muslims
      .filter(m => !m.truce && m.civilWar && m.totalTroopsAndMilitia > m.totalCells)

  // Bot will not execute this in the Caliphate capital
  def jihadistBotQadhafiCandidates =
    game.muslims
      .filter(m =>
        !m.truce &&
        m.civilWar &&
        !game.isCaliphateCapital(m.name) &&
        m.totalCells > m.totalTroopsAndMilitia
      )

  def enhJihadistBotCandidates = game.muslims
    .filter { m =>
      !m.truce &&
      m.civilWar &&
      JihadistBot.enhBotResourceValue(m) >= 2 &&
      !m.isAdversary &&
      !game.isCaliphateMember(m.name) &&
      m.totalCells > m.totalTroopsAndMilitia
    }

  // Returns true if the printed conditions of the event are satisfied
  override
  def eventConditionsMet(role: Role) = getCandidates.nonEmpty


  // Returns true if the Bot associated with the given role will execute the event
  // on its turn.  This implements the special Bot instructions for the event.
  // When the event is triggered as part of the Human players turn, this is NOT used.
  override
  def botWillPlayEvent(role: Role): Boolean = role match {
    case US =>
      usBotQadhafiCandidates.nonEmpty
    case Jihadist if game.botEnhancements =>
      // Playable in 2+ resource*, non-adversary, non-Caliphate CW countries with Cells>TandM available.
      enhJihadistBotCandidates.nonEmpty
    case Jihadist =>
      jihadistBotQadhafiCandidates.nonEmpty
  }

  // Carry out the event for the given role.
  // forTrigger will be true if the event was triggered during the human player's turn
  // and it associated with the Bot player.
  override
  def executeEvent(role: Role): Unit = {
    val name = role match {
      case _ if isHuman(role) =>
        askCountry("Select country in Civil War: ", getCandidates)
      case US =>
        val priorities = List(
          new USBot.CriteriaFilter(
            "TandM > Cells",
            USBot.muslimTest(m => m.totalTroopsAndMilitia > m.totalCells)),
          USBot.HighestResourcePriority)

        USBot.topPriority(usBotQadhafiCandidates, priorities)
          .map(_.name)
          .get

      case Jihadist if game.botEnhancements =>
        val priorities = List(
          JihadistBot.IsMajorJihadPriority,
          JihadistBot.FairPriority,
          JihadistBot.MostMilitiaPriority,
        )
        JihadistBot.topPriority(jihadistBotQadhafiCandidates, priorities)
          .map(_.name)
          .get

      case Jihadist =>
        val priorities = List(
          new JihadistBot.CriteriaFilter(
            "Not Caliphate Capital",
            JihadistBot.muslimTest(m => !game.isCaliphateCapital(m.name))),
          new JihadistBot.CriteriaFilter(
            "Cells > TandM",
            JihadistBot.muslimTest(m => m.totalCells > m.totalTroopsAndMilitia)),
          JihadistBot.HighestResourcePriority)

        JihadistBot.topPriority(jihadistBotQadhafiCandidates, priorities)
          .map(_.name)
          .get
    }

    val m = game.getMuslim(name)
    addEventTarget(name)
    endCivilWar(name)
    if (m.totalCells > m.totalTroopsAndMilitia)
      setGovernance(name, Poor, Some(Adversary))
    else if (m.totalTroopsAndMilitia > m.totalCells)
      setGovernance(name, Fair, Some(Ally))
    else
      log(s"\nThe governance and alignment of $name does not change", Color.Event)
  }
}
