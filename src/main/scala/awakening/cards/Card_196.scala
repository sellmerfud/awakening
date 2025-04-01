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

import scala.util.Random.shuffle
import awakening.LabyrinthAwakening._
import awakening.JihadistBot
import awakening.USBot.MuslimAlertResult

// Card Text:
// ------------------------------------------------------------------
// Place Training Camps in a Tested, non-Good Muslim Country.
// While on map, 3 (5 if in a Caliphate Country) additional Cells are
// available to Recruit while in the 9 box of Ample Funding (11.3.7).
// Place 2 Cells there now. May Auto-Recruit there. Cells traveling
// from there receive a -1 modifier. Remove Training Camps if host
// country becomes Good or if there are no Cells or Cadre there, or
// if played again elsewhere.
// ------------------------------------------------------------------
object Card_196 extends Card(196, "Training Camps", Jihadist, 3, NoRemove, NoLapsing, NoAutoTrigger) {
  // Used by the US Bot to determine if the executing the event would alert a plot
  // in the given country
  override
  def eventAlertsPlot(countryName: String, plot: Plot): Boolean = false

  // Used by the US Bot to determine if the executing the event would remove
  // the last cell on the map resulting in victory.
  override
  def eventRemovesLastCell(): Boolean = false

  val isCandidate = (m: MuslimCountry) => !m.truce && m.isTested && !m.isGood
  val isNonAutoRecruitCandidate = (m: MuslimCountry) => isCandidate(m) && !m.autoRecruit
  val isAutoRecruitCandidate = (m: MuslimCountry) => isCandidate(m) && m.autoRecruit

  def getCandidates = countryNames(game.muslims.filter(isCandidate))

  def getNonAutoRecruitCandidates = countryNames(game.muslims.filter(isNonAutoRecruitCandidate))

  // If MJP marked and not auto-recruit, place in MJP.
  // Next, place in non-auto-recruit Poor Muslim, priority to no TandM, then best r-a, then highest res.
  // Next, place in MJP if auto-recruit.
  // Next, place in auto-recruit Poor Muslim, priority to no TandM, then best r-a, then highest res.
  // Else unplayable.
  def getEnhBotTarget: Option[String] = {
    val priorities = List(
      JihadistBot.NoTandMFilter,
      JihadistBot.HighestReactionMinusAwakeningPriority,
      JihadistBot.HighestResourcePriority,
    )

    // Pick the best target from a list of candidates by narrowing them
    // using the our priorities.  Then pick a random target from the final
    // narrowed list.
    // Exception: If the Training Camp marker is on the map, and it's current location
    // makes the final narrowed list, then we choose it to indicate that
    // the camp should not be moved.
    def bestLocation(candidates: List[MuslimCountry]): Option[String] = {
      JihadistBot.narrowCandidates(candidates, priorities) match {
        case Nil => None  // Only if original candidate list was empty
        case narrowed =>
          game.trainingCamp match {
            case Some(name) if narrowed.exists(_.name == name) =>
              game.trainingCamp
            case _ =>
              // Either no existing Training Camp or we found
              // a higher priority location
              shuffle(narrowed).headOption.map(_.name)
          }
      }
    }

    val poorNonAutoRecruitCandidates = game.muslims.filter(m => m.isPoor && isNonAutoRecruitCandidate(m))
    val poorAutoRecruitCandidates = game.muslims.filter(m => m.isPoor && isAutoRecruitCandidate(m))

    // If the Training Camp is already in the MJP, then we don't
    // want to change its location
    if (JihadistBot.majorJihadPriorityCountry == game.trainingCamp)
      game.trainingCamp
    else {
      val mjpNonAutoRecruit = JihadistBot.majorJihadPriorityCountry
        .filter( name => isNonAutoRecruitCandidate(game.getMuslim(name)))

      val mjpAutoRecruit = JihadistBot.majorJihadPriorityCountry
        .filter( name => isAutoRecruitCandidate(game.getMuslim(name)))

      JihadistBot.botLog(s"MJP (non-auto-recruit) Training Camps target: $mjpNonAutoRecruit", Color.Debug)  
      JihadistBot.botLog(s"MJP (auto-recruit) Training Camps target: $mjpAutoRecruit", Color.Debug)  

      JihadistBot.botLog("Find Poor (non-auto-recruit) Training Camps candidates", Color.Debug)
      val poorNonAutoRecruit = bestLocation(poorNonAutoRecruitCandidates)
      JihadistBot.botLog(s"Poor (non-auto-recruit) Training Camps target: $poorNonAutoRecruit", Color.Debug)

      JihadistBot.botLog("Find Poor (auto-recruit) Training Camps candidates", Color.Debug)
      val poorAutoRecruit = bestLocation(poorAutoRecruitCandidates)
      JihadistBot.botLog(s"Poor (auto-recruit) Training Camps target: $poorAutoRecruit", Color.Debug)

      mjpNonAutoRecruit orElse poorNonAutoRecruit orElse mjpAutoRecruit orElse poorAutoRecruit
    }  
  }

  // Returns true if the printed conditions of the event are satisfied
  override
  def eventConditionsMet(role: Role) = getCandidates.nonEmpty

  // Returns true if the Bot associated with the given role will execute the event
  // on its turn.  This implements the special Bot instructions for the event.
  // When the event is triggered as part of the Human players turn, this is NOT used.
  //
  // The Enhanced Bot will not play if it would place and exising training camps
  // marker in the same country where it already exists.
  override
  def botWillPlayEvent(role: Role): Boolean = if (game.botEnhancements) {
    // If we have a valid target and the Training Camp is not already there.
    getEnhBotTarget.nonEmpty && getEnhBotTarget != game.trainingCamp
  }
  else
    true // Standard Bot will always play the event

  // Carry out the event for the given role.
  // forTrigger will be true if the event was triggered during the human player's turn
  // and it associated with the Bot player.
  override
  def executeEvent(role: Role): Unit = {
    lazy val enhBotTarget = getEnhBotTarget
    val target = if (isHuman(role))
      askCountry("Place Training Camps in which country: ", getCandidates)
    else if (game.botEnhancements && enhBotTarget.nonEmpty)
      enhBotTarget.get
    else
      getNonAutoRecruitCandidates match {
        case Nil => JihadistBot.cellPlacementPriority(false)(getCandidates).get
        case nonAutoRecruit => JihadistBot.cellPlacementPriority(false)(nonAutoRecruit).get
      }

    println() 
    addEventTarget(target)
    
    // This call handles the case where the Training Camp
    // is already in the given target location.
    playExtraCellsEvent(TrainingCamps, target)

    val cellsToAdd = game.cellsAvailable min 2
    if (cellsToAdd > 0)
      addSleeperCellsToCountry(target, cellsToAdd)
  }
}
