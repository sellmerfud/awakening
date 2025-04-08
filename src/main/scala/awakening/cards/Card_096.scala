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
// US side selects Scandinavia's Posture.
// Then jihadists place an available Plot 1 or, if any Islamist Rule,
// any available plot in a non-Islamist Rule Muslim country.
// ------------------------------------------------------------------
object Card_096 extends Card(96, "Danish Cartoons", Unassociated, 1, Remove, NoLapsing, NoAutoTrigger) {
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
  def eventConditionsMet(role: Role) = true

  def getPlotCandidates = if (game.numIslamistRule == 0)
    game.availablePlots.filter(_ == Plot1)
  else
    game.availablePlots

  // Good Muslim, priority to highest Resource, then with troops
  // Fair Auto-Recruit Muslim, priority to RC
  // If [Prestige>low]: Poor Muslim with troops, priority to RC, then AID
  // Fair 3 Resource Muslim
  // Fair Pakistan
  // If [Funding <8]: Poor Muslim, priority to Troops, then AID, then Adversary, then Neutral.
  // Else unplayable
  def enhBotCandidates: Option[(List[Country], List[JihadistBot.CountryFilter])] = {
    val buf = new ListBuffer[(List[Country], List[JihadistBot.CountryFilter])]
    val goodPriorities = List(JihadistBot.HighestResourcePriority, JihadistBot.WithTroopsPriority)
    val fairAutoRecruitPriorities = List(JihadistBot.HighestResourcePriority, JihadistBot.WithTroopsPriority)
    val poorWithTroopsPriorities = List(JihadistBot.RegimeChangePriority, JihadistBot.WithAidPriority)
    val poorPriorities = List(JihadistBot.WithTroopsPriority, JihadistBot.WithAidPriority,
      JihadistBot.AdversaryPriority, JihadistBot.NeutralPriority)

    val good = game.muslims.filter(_.isGood)
    val fairAutoRecruit = game.muslims.filter(m => m.isFair && m.autoRecruit)
    val poorWithTroops = if (game.prestigeLevel != Low)
      game.muslims.filter(m => m.isPoor && m.totalTroops > 0)
    else
      Nil
    val fair3Resource = game.muslims.filter(m => m.isFair && m.printedResources == 3)
    val fairPakistan = game.muslims.filter(m => m.isFair && m.name == Pakistan)
    val poor = if (game.funding < 8)
      game.muslims.filter(m => m.isPoor)
    else
      Nil


    buf.append(good -> goodPriorities)
    buf.append(fairAutoRecruit -> fairAutoRecruitPriorities)
    buf.append(poorWithTroops -> poorWithTroopsPriorities)
    buf.append(fair3Resource -> Nil)
    buf.append(fairPakistan -> Nil)
    buf.append(poor -> poorPriorities)
    buf.toList
      .filter(_._1.nonEmpty)
      .headOption
  }
  
  // Returns true if the Bot associated with the given role will execute the event
  // on its turn.  This implements the special Bot instructions for the event.
  // When the event is triggered as part of the Human players turn, this is NOT used.
  // Enhanced Jihadist Bot:
  // Playable if US soft or/and any IR on board and one of the enh Bot candidate lists is non-empty
  override
  def botWillPlayEvent(role: Role): Boolean = role match {
    case US =>
      getPlotCandidates.isEmpty
    case Jihadist if game.botEnhancements =>
      (game.usPosture == Soft || game.hasMuslim(_.isIslamistRule)) &&
      getPlotCandidates.nonEmpty &&
      enhBotCandidates.nonEmpty

    case Jihadist =>
      getPlotCandidates.nonEmpty
  }

  // Carry out the event for the given role.
  // forTrigger will be true if the event was triggered during the human player's turn
  // and it associated with the Bot player.
  override
  def executeEvent(role: Role): Unit = {
    log("\nUS selects Scandinavia's Posture.", Color.Event)
    val posture = if (isHuman(US))
      askPosture(Scandinavia)
    else
      game.usPosture

    addEventTarget(Scandinavia)
    setCountryPosture(Scandinavia, posture)

    val plotCandidates = getPlotCandidates
    if (plotCandidates.nonEmpty) {
      if (game.numIslamistRule == 0)
        log(s"\n$Jihadist places any available plot in a Non-Islamist Rule muslim country.", Color.Event)
      else
        log(s"\n$Jihadist places an available Plot-1 in a Non-Islamist Rule muslim country.", Color.Event)

      val plot = plotCandidates match {
        case p::Nil => p
        case plots if isHuman(Jihadist) => askPlots(plots, 1).head
        case plots => JihadistBot.preparePlots(plots).head
      }
      val candidates = countryNames(game.muslims.filter(m => !m.truce && !m.isIslamistRule))
      val name = if (isHuman(Jihadist))
        askCountry(s"Place $plot in which country: ", candidates)
      else if (game.botEnhancements) {
        val (candidates, priorities) = enhBotCandidates.get
        JihadistBot.topPriority(candidates, priorities)
          .map(_.name)
          .get
      }
      else
        JihadistBot.plotPriority(candidates).get

      addEventTarget(name)
      addAvailablePlotToCountry(name, plot)
    }
    else if (game.numIslamistRule == 0)
      log("\nThere is no available level 1 plot.", Color.Event)
    else
      log("\nThere are no available Plots.", Color.Event)
  }
}
