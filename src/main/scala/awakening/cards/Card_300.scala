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
// Execute a Jihad with this card's Operations value, then all
// participating Cells that remain become Sleeper
// (except in a Caliphate country).
// ------------------------------------------------------------------
object Card_300 extends Card(300, "Going Underground", Jihadist, 2, NoRemove, NoLapsing, NoAutoTrigger) {
  // Used by the US Bot to determine if the executing the event would alert a plot
  // in the given country
  override
  def eventAlertsPlot(countryName: String, plot: Plot): Boolean = false

  // Used by the US Bot to determine if the executing the event would remove
  // the last cell on the map resulting in victory.
  override
  def eventRemovesLastCell(): Boolean = false

  def getCandidates = game.jihadTargets.sorted

  def botMajorJihadCandidates() = countryNames(
    game.getMuslims(game.majorJihadTargets(2))
      .filter { m => m.isPoor && JihadistBot.majorJihadSuccessPossible(m) }
  )

  def botMinorJihadCandidates() = countryNames(
    game.getMuslims(game.jihadTargets)
      .filter { m =>
        (m.isFair || m.isGood) &&
        JihadistBot.minorJihadSuccessPossible(m)
      }
  )

  // Returns true if the printed conditions of the event are satisfied
  override
  def eventConditionsMet(role: Role) = getCandidates.nonEmpty

  // Returns true if the Bot associated with the given role will execute the event
  // on its turn.  This implements the special Bot instructions for the event.
  // When the event is triggered as part of the Human players turn, this is NOT used.
  override
  def botWillPlayEvent(role: Role): Boolean = if (game.botEnhancements)
    false
  else
    botMajorJihadCandidates().nonEmpty || botMinorJihadCandidates().nonEmpty

  def getStandardBotJihadTargets: List[JihadTarget] = {
    if (botMajorJihadCandidates().nonEmpty) {
      val target = JihadistBot.majorJihadPriorityCountry match {
        case Some(name) if botMajorJihadCandidates().contains(name) => name
        case _ => JihadistBot.majorJihadTarget(botMajorJihadCandidates()).get
      }
      val m = game.getMuslim(target)
      val active = m.pieces.activeCells min 2
      val sleeper = (2 - active) min 2
      val cells = Pieces(activeCells = active, sleeperCells = sleeper)
      List(JihadTarget(target, true, cells, false))
    }
    else {
      def nextTarget(opsLeft: Int, candidates: List[String], targets: Vector[JihadTarget]): Vector[JihadTarget] = {
        if (opsLeft == 0 || candidates.isEmpty)
          targets
        else {
          val name = JihadistBot.minorJihadTarget(candidates).get
          val country = game.getCountry(name)
          val active = opsLeft min country.pieces.activeCells
          val sleeper = ((opsLeft - active) max 0) min country.pieces.sleeperCells
          val cells = Pieces(activeCells = active, sleeperCells = sleeper)
          val target = JihadTarget(name, false, cells, false)
          val remainingCandidates = candidates.filterNot(_ == name)
          nextTarget(opsLeft - active - sleeper, remainingCandidates, targets :+ target)
        }
      }

      val possibles = botMinorJihadCandidates() match {
        case Nil => getCandidates
        case preferred => preferred
      }
      nextTarget(2, possibles, Vector.empty).toList
    }
  }

  // The enhance bot will only be executing this event if it was triggered
  // during the US turn.  The Bot will only select a single country even
  // if it means wasting a Op.
  // Priority to Good country with highest res*,
  // then Fair 2+ res* country with a-r<2 (priority to highest res*, then best r-a),
  // then Poor country with AID and a-r< 3,
  // then Poor country with best r-a,
  // then Non-auto-recuit, non-MJP country with 1 cell
  def getEnhancedBotJihadTargets: List[JihadTarget] = {
    def nextTarget(opsLeft: Int, candidates: List[MuslimCountry], targets: Vector[JihadTarget]): Vector[JihadTarget] = {
      // CWS: I'm leaving this code such that it could select multiple targets in case Florian
      //      changes his mind and wishes to allow that.
      if (opsLeft == 0 || candidates.isEmpty || targets.nonEmpty)
          targets
      else {
        val goodCandidates = candidates.filter(_.isGood)
        val fairCandidates = candidates.filter(m => m.isFair && JihadistBot.enhBotResourceValue(m) > 1 &&  m.awakening - m.reaction < 2)
        val poorAidCandidates = candidates.filter(m => m.isPoor && m.aidMarkers > 0 && m.awakening - m.reaction < 3)
        val poorCandidates = candidates.filter(_.isPoor)
        val nonMJPWith1Cell = candidates.filter {m =>
          !m.autoRecruit &&
          Some(m.name) != JihadistBot.majorJihadPriorityCountry &&
          m.pieces.totalCells == 1
        }
        val goodPriorities = List(JihadistBot.HighestPrintedResourcePriority)
        val fairPriorities = List(JihadistBot.HighestPrintedResourcePriority, JihadistBot.HighestReactionMinusAwakeningPriority)
        val poorPriorities = List(JihadistBot.HighestReactionMinusAwakeningPriority)
        def logCandidates(muslims: List[MuslimCountry], desc: String) =
          JihadistBot.botLog(s"$desc: [${muslims.map(_.name).mkString(", ")}]")

        val (preferred, priorities) = if (goodCandidates.nonEmpty) {
          logCandidates(goodCandidates, "Good Muslims")
          (goodCandidates, goodPriorities)
        }
        else if (fairCandidates.nonEmpty) {
          logCandidates(fairCandidates, "Fair 2+ Res* Muslims with a-r < 2")
          (fairCandidates, fairPriorities)
        }
        else if (poorAidCandidates.nonEmpty) {
          logCandidates(poorAidCandidates, "Poor Muslims with aid and a-r < 3")
          (poorAidCandidates, poorPriorities)
        }
        else if (poorCandidates.nonEmpty) {
          logCandidates(poorCandidates, "Poor Muslims")
          (poorCandidates, poorPriorities)
        }
        else if (nonMJPWith1Cell.nonEmpty) {
          logCandidates(nonMJPWith1Cell, "Non-auto-recuit, non-MJP country with 1 cell")
          (nonMJPWith1Cell, poorPriorities)
        }
        else {
          logCandidates(candidates, "Muslims")
          (candidates, Nil)
        }

        val muslim = JihadistBot.topPriority(preferred, priorities)
          .get
          .asInstanceOf[MuslimCountry]

        if (muslim.isPoor && muslim.majorJihadOK(opsLeft) && JihadistBot.majorJihadSuccessPossible(muslim)) {
          val active = muslim.pieces.activeCells min opsLeft
          val sleeper = (opsLeft - active) min opsLeft
          val cells = Pieces(activeCells = active, sleeperCells = sleeper)
          val target = JihadTarget(muslim.name, true, cells, false)
          nextTarget(0, Nil, targets :+ target)
        }
        else {
          val active = opsLeft min muslim.pieces.activeCells
          val sleeper = ((opsLeft - active) max 0) min muslim.pieces.sleeperCells
          val cells = Pieces(activeCells = active, sleeperCells = sleeper)
          val target = JihadTarget(muslim.name, false, cells, false)
          val remainingCandidates = candidates.filterNot(_.name == muslim.name)
          nextTarget(opsLeft - active - sleeper, remainingCandidates, targets :+ target)
        }
      }
    }

    nextTarget(2, game.getMuslims(getCandidates), Vector.empty).toList
  }

  // Carry out the event for the given role.
  // forTrigger will be true if the event was triggered during the human player's turn
  // and it associated with the Bot player.
  override
  def executeEvent(role: Role): Unit = {
    val targets = if (isHuman(role))
      getHumanJihadTargets(2, getCandidates)
    else if (game.botEnhancements)
      getEnhancedBotJihadTargets
    else
      getStandardBotJihadTargets

    for (t <- targets)
      addEventTarget(t.name)

    val results = performJihads(targets.toList)
    val makeSleepers = results.exists {
      case (name, successes, false) => !game.isCaliphateMember(name) && successes > 0
      case (name, successes, true)  => !game.isCaliphateMember(name) && successes > 1
      case _ => false
    }

    if (makeSleepers) {
      log("\nSuccessful participating cells become sleeper cells.", Color.Event)
      log(separator())
      for ((name, successes, sadr) <- results) {
        // Remaining cells that participated become sleepers
        val successCells = successes - (if (sadr) 1 else 0)  // Sadr does not flip

        if (successCells > 0 && !game.isCaliphateMember(name))
          hideActiveCells(name, successCells)
      }
    }
  }
}
