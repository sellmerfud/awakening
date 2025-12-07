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
// Play in Turkey or in one Adjacent non-Schengen country (but not Russia).
// Place or remove (not flip) an Aid, Besieged Regime, Awakening, Reaction
// or Posture marker, OR place or Remove up to 2 total Militia and/or Cells.
// ------------------------------------------------------------------
object Card_205 extends Card(205, "Erdogan Effect", Unassociated, 1, NoRemove, NoLapsing, NoAutoTrigger) {

  val CandidateCountries = List(Turkey, Serbia, Iraq, Caucasus, Syria, Iran).sorted

  def muslimCandidates = CandidateCountries.filter(name => !underTruce(name) && game.isMuslim(name))

  def nonMuslimCandidates = CandidateCountries.filter(game.isNonMuslim)

  def awakeReactCandidates = if (lapsingEventInPlay(ArabWinter))
    Nil
  else
    muslimCandidates.filter(name => game.getMuslim(name).canTakeAwakeningOrReactionMarker)

  def removeCellsCandidates = CandidateCountries.filter(game.getCountry(_).totalCells > 0)

  // Used by the US Bot to determine if the executing the event would alert a plot
  // in the given country
  override
  def eventAlertsPlot(countryName: String, plot: Plot): Boolean = false

  // Used by the US Bot to determine if the executing the event would remove
  // the last cell on the map resulting in victory.
  override
  def eventRemovesLastCell(): Boolean =
        removeCellsCandidates.exists(name => USBot.wouldRemoveLastCell(name, 2))


  // Returns true if the printed conditions of the event are satisfied
  override
  def eventConditionsMet(role: Role) = true


  def removeAwakeningCandidates =
    muslimCandidates
      .filter(name => game.getMuslim(name).awakening > 0)

  def removeTwoMilitiaCandidates =
    muslimCandidates
      .filter(name => game.getMuslim(name).pieces.militia > 1)

  def removeHardPostureCandidates = if (game.usPosture == Hard)
    nonMuslimCandidates
      .filter(name => game.getNonMuslim(name).isHard)
  else
    Nil

  def removeSoftPostureCandidates = if (game.usPosture == Soft)
    nonMuslimCandidates
      .filter(name => game.getNonMuslim(name).isSoft)
  else
    Nil

  def removeAidCandidates =
    muslimCandidates
      .filter(name => game.getMuslim(name).aidMarkers > 0)


  // Returns true if the Bot associated with the given role will execute the event
  // on its turn.  This implements the special Bot instructions for the event.
  // When the event is triggered as part of the Human players turn, this is NOT used.
  // Enhanced Jihadist Bot:
  //   Place reaction marker.
  //   Next, remove awakening marker.
  //   Next, remove 2 Militia.
  //   Next, if US hard, remove a hard posture marker.
  //   Next, place cell(s).
  //   Next, remove AID.
  //   Next, if US soft, remove a soft posture marker.
  //   Else unplayable.
  //   Priority to MJP, then highest Res.

  override
  def botWillPlayEvent(role: Role): Boolean = role match {
    case US =>
      removeCellsCandidates.nonEmpty || awakeReactCandidates.nonEmpty
    
    case Jihadist if game.botEnhancements =>
      awakeReactCandidates.nonEmpty ||
      removeAwakeningCandidates.nonEmpty ||
      removeTwoMilitiaCandidates.nonEmpty ||
      removeHardPostureCandidates.nonEmpty ||
      game.cellsAvailable > 0 ||
      removeAidCandidates.nonEmpty ||
      removeSoftPostureCandidates.nonEmpty

    case Jihadist =>
      game.cellsAvailable > 0 || awakeReactCandidates.nonEmpty
  }

  // Carry out the event for the given role.
  // forTrigger will be true if the event was triggered during the human player's turn
  // and it associated with the Bot player.
  override
  def executeEvent(role: Role): Unit = {
    sealed trait Choice
    case object AddAid extends Choice
    case object DelAid extends Choice
    case object AddBesieged extends Choice
    case object DelBesieged extends Choice
    case object AddAwakening extends Choice
    case object DelAwakening extends Choice
    case object AddReaction extends Choice
    case object DelReaction extends Choice
    case object AddMilitia extends Choice
    case object DelMilitia extends Choice
    case object AddCells extends Choice
    case object DelCells extends Choice
    case object AddPosture extends Choice
    case object DelPosture extends Choice

    if (isHuman(role)) {
      val name = askCountry("Select country: ", CandidateCountries)
      val choices = game.getCountry(name) match {
        case m: MuslimCountry =>
          val canAwake = lapsingEventNotInPlay(ArabWinter) && m.canTakeAwakeningOrReactionMarker
          val canBesiege = m.canTakeBesiegedRegimeMarker
          val canMilitia = game.militiaAvailable > 0 && m.canTakeMilitia
          List(
            choice(m.canTakeAidMarker,      AddAid, "Place aid marker"),
            choice(m.aidMarkers > 0,        DelAid, "Remove aid marker"),
            choice(canBesiege,              AddBesieged, "Place besieged regime marker"),
            choice(m.besiegedRegime,        DelBesieged, "Remove besieged regime marker"),
            choice(canAwake,                AddAwakening, "Place awakening marker"),
            choice(m.awakening > 0,         DelAwakening, "Remove awakening marker"),
            choice(canAwake,                AddReaction, "Place reaction marker"),
            choice(m.reaction > 0,          DelReaction, "Remove reaction marker"),
            choice(canMilitia,              AddMilitia, "Place 2 militia"),
            choice(m.pieces.militia > 0,           DelMilitia, "Remove 2 militia"),
            choice(game.cellsAvailable > 0, AddCells, "Place 2 cells"),
            choice(m.totalCells > 0,        DelCells, "Remove 2 cells")
          ).flatten
        case n: NonMuslimCountry =>
          List(
            choice(n.isUntested && n.canChangePosture,  AddPosture, "Place posture marker"),
            choice(n.isTested && n.canChangePosture,    DelPosture, "Remove posture marker"),
            choice(game.cellsAvailable > 0,             AddCells, "Place 2 cells"),
            choice(n.totalCells > 0,                    DelCells, "Remove 2 cells")
          ).flatten
      }

      if (choices.isEmpty) {
        log(s"\nThere are no valid actions that can be taken in $name", Color.Event)
        log("The event has no effect.", Color.Event)
      }
      else {
        addEventTarget(name)
        if (game.getCountry(name).isMuslim && lapsingEventInPlay(ArabWinter))
          displayLine("\nCannot place awakening/reaction markers. [Arab Winter]", Color.Info)
        askMenu("Choose one:", choices).head match {
          case AddAid => addAidMarker(name)
          case DelAid => removeAidMarker(name)
          case AddBesieged => addBesiegedRegimeMarker(name)
          case DelBesieged => removeBesiegedRegimeMarker(name)
          case AddAwakening => addAwakeningMarker(name)
          case DelAwakening => removeAwakeningMarker(name)
          case AddReaction => addReactionMarker(name)
          case DelReaction => removeReactionMarker(name)
          case AddMilitia => addMilitiaToCountry(name, 2 min game.militiaAvailable)
          case DelMilitia => removeMilitiaFromCountry(name, 2 min game.getMuslim(name).pieces.militia)
          case AddCells => addSleeperCellsToCountry(name, 2 min game.cellsAvailable)
          case DelCells =>
            val (cells, sadr) = askCells(name, 2, role == US)
            removeCellsFromCountry(name, cells, sadr, addCadre = true)
          case AddPosture =>
            val posture = askPosture(name)
            setCountryPosture(name, posture)
          case DelPosture => setCountryPosture(name, PostureUntested)
        }
      }
    }
    else if (role == Jihadist && game.botEnhancements) {
      val priorites = List(
        JihadistBot.IsMajorJihadPriority,
        JihadistBot.HighestPrintedResourcePriority
      )
      def priorityTarget(candidates: List[String]) = JihadistBot.topPriority(candidates.map(game.getCountry), priorites)
        .map(_.name)
        .get

      if (awakeReactCandidates.nonEmpty) {
        val target = priorityTarget(awakeReactCandidates)
        addEventTarget(target)
        addReactionMarker(target)
      }
      else if (removeAwakeningCandidates.nonEmpty) {
        val target = priorityTarget(removeAwakeningCandidates)
        addEventTarget(target)
        removeReactionMarker(target)
      }
      else if (removeTwoMilitiaCandidates.nonEmpty) {
        val target = priorityTarget(removeTwoMilitiaCandidates)
        addEventTarget(target)
        removeMilitiaFromCountry(target, 2)
      }
      else if (removeHardPostureCandidates.nonEmpty) {
        val target = priorityTarget(removeHardPostureCandidates)
        addEventTarget(target)
        setCountryToUntested(target)
      }
      else if (game.cellsAvailable > 0) {
        val target = priorityTarget(muslimCandidates)
        addEventTarget(target)
        addSleeperCellsToCountry(target, 2 max game.cellsAvailable)
      }
      else if (removeAidCandidates.nonEmpty) {
        val target = priorityTarget(removeAidCandidates)
        addEventTarget(target)
        removeAidMarker(target, 1)
      }
      else {
        val target = priorityTarget(removeSoftPostureCandidates)
        addEventTarget(target)
        setCountryToUntested(target)
      }
    }
    else if (role == Jihadist ) {
      // Jihadist Bot only affects cells or reaction markers and the
      // speical rule for this card says to use the cell placement priorities
      val name = JihadistBot.cellPlacementPriority(false)(CandidateCountries).get
      addEventTarget(name)
      if (game.cellsAvailable > 0)
        addSleeperCellsToCountry(name, 2 min game.cellsAvailable)
      else
        addReactionMarker(name)
    }
    else {  // US Bot
      removeCellsCandidates match {
        case Nil =>
          val name = USBot.markerAlignGovTarget(awakeReactCandidates).get
          addEventTarget(name)
          removeReactionMarker(name)

        case candidates =>
          val name = USBot.disruptPriority(candidates).get
          addEventTarget(name)
          val (cells, sadr) = USBot.chooseCellsToRemove(name, 2)
          removeCellsFromCountry(name, cells, sadr, addCadre = true)
      }
    }
  }
}
