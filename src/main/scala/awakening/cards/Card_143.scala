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
import awakening.scenarios.MittsTurn
import awakening.USBot

// Card Text:
// ------------------------------------------------------------------
// Play if US Soft.
// Do any 2 of the following:
//   place 1 Awakening marker,
//   place 1 Aid Marker,
//  +1 Prestige,
//  -1 Funding,
//  Select posture of 1 Schengen country,
//  Select, draw and reveal Reaper, Operation New Dawn or Advisors from discard pile.
//
// When playing the Mitt's Turn scenario the following changes are used:
// - The card has 3 Ops
// - The US must be Hard (not Soft) to play the event.
// - Three options (not two) are selected.
// ------------------------------------------------------------------
object Card_143 extends Card(143, "Obama Doctrine", US, 2, NoRemove, NoLapsing, NoAutoTrigger) {

  def isMittsTurnScenario = game.scenarioName == MittsTurn.name

  override
  def ops: Int = if (isMittsTurnScenario) 3 else printedOps

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
  def eventConditionsMet(role: Role) = if (isMittsTurnScenario)
    game.usPosture == Hard
  else
    game.usPosture == Soft

  // Returns true if the Bot associated with the given role will execute the event
  // on its turn.  This implements the special Bot instructions for the event.
  // When the event is triggered as part of the Human players turn, this is NOT used.
  override
  def botWillPlayEvent(role: Role): Boolean = true

  def awakeingCandidates = if (lapsingEventInPlay(ArabWinter))
    Nil
  else
    countryNames(game.muslims.filter(_.canTakeAwakeningOrReactionMarker))

  def canPlaceAwakening = awakeingCandidates.nonEmpty

  def aidCandidates = countryNames(game.muslims)
  
  val DiscardTargets = Set(121, 126, 127, 128, 144)
  def cardDrawCandidates = game.cardsDiscarded
    .filter(DiscardTargets.contains)

  // Carry out the event for the given role.
  // forTrigger will be true if the event was triggered during the human player's turn
  // and it associated with the Bot player.
  override
  def executeEvent(role: Role): Unit = {
    val numActions = if (isMittsTurnScenario)
      3
    else
      2
    val canDraw = cardDrawCandidates.nonEmpty

    if (isHuman(role)) {

      def nextAction(actionNum: Int, used: Set[String]): Unit = if (actionNum <= numActions) {
        val choices = List(
          choice(canPlaceAwakening && !used("awakening"), "awakening", "Place 1 Awakening marker"),
          choice(!used("aid"),               "aid",       "Place 1 Aid marker"),
          choice(game.prestige < 12 && !used("prestige"), "prestige",  "+1 Prestige"),
          choice(game.funding > 1 && !used("funding"),    "funding",   "-1 Funding"),
          choice(!used("posture"),                        "posture",   "Select posture of 1 Schengen country"),
          choice(canDraw && !used("draw"),                "draw",      "Select Reaper, Operation New Dawn, or Advisors from discard pile.")
        ).flatten

        if (cardDrawCandidates.isEmpty)
          displayLine("None of Reaper, Operation New Dawn, or Advisors is in the discard pile.", Color.Info)

        val action = askMenu(s"Choose ${ordinal(actionNum)} of $numActions actions:", choices).head

        action match {
          case "awakening" =>
            val target = askCountry("\nPlace awakening marker in which country: ", awakeingCandidates)
            addEventTarget(target)
            addAwakeningMarker(target)

          case "aid" =>
            val target = askCountry("\nPlace aid marker in which country: ", aidCandidates)
            addEventTarget(target)
            addAidMarker(target)

          case "prestige" =>
            println()
            increasePrestige(1)

          case "funding"  =>
            println()
            decreaseFunding(1)

          case "posture" =>
            val target  = askCountry("\nSelect posture of which Schengen country: ", Schengen)
            val posture = askPosture(target)
            addEventTarget(target)
            setCountryPosture(target, posture)

          case _ =>
              askCardDrawnFromDiscardPile(role, only = cardDrawCandidates.toSet)
        }
        nextAction(actionNum + 1, used + action)
      }

      nextAction(1, Set.empty)
    }
    else {
      // See Event Instructions table
      var actions = List(
        if (game.prestige < 12) Some("prestige") else None,
        if (game.funding  >  1) Some("funding") else None,
        if (canPlaceAwakening ) Some("awakening") else None,
        Some("aid")
      ).flatten take numActions

      actions foreach {
        case "prestige" =>
          increasePrestige(1)

        case "funding" =>
          decreaseFunding(1)

        case "awakening" =>
          val target = USBot.markerAlignGovTarget(awakeingCandidates).get
          addEventTarget(target)
          addAwakeningMarker(target)

        case _ =>
          val target = USBot.markerAlignGovTarget(aidCandidates).get
          addEventTarget(target)
          addAidMarker(target)
      }
    }
  }
}
