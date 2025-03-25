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
import awakening.USBot

// Card Text:
// ------------------------------------------------------------------
// Roll a die and modify by US Prestige modifier:
// 0) Random reaction marker
// 1) -1 US Prestige
// 2) Remove an Aid marker
// 3) +1 Funding
// 4) -1 Funding
// 5) Place an Aid marker
// 6) +1 US Prestige
// 7-8) Random Awakening Marker
// Then, place or flip Trump Tweets to ON.
// ------------------------------------------------------------------
object Card_251 extends Card(251, "Trump Tweets", US, 1, NoRemove, NoLapsing, NoAutoTrigger) {
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

  // Returns true if the Bot associated with the given role will execute the event
  // on its turn.  This implements the special Bot instructions for the event.
  // When the event is triggered as part of the Human players turn, this is NOT used.
  override
  def botWillPlayEvent(role: Role): Boolean = true

  val resultMsgs = Vector(
  "Random reaction marker",
  "-1 US Prestige",
  "Remove an Aid marker",
  "+1 Funding",
  "-1 Funding",
  "Place an Aid marker",
  "+1 US Prestige",
  "Random Awakening Marker", // 7
  "Random Awakening Marker", // 8
  "Random Awakening Marker"  // 9 possible if Hillary Wins scenario and Prestite > 10
  )
  // Carry out the event for the given role.
  // forTrigger will be true if the event was triggered during the human player's turn
  // and it associated with the Bot player.
  override
  def executeEvent(role: Role): Unit = {
    def convergenceTarget: String = {
      randomConvergenceTarget match {
        case m if m.truce => convergenceTarget
        case m => m.name
      }
    }
    def logNotZero(value: Int, msg: String): Unit =
      if (value != 0)
        log(f"$value%+2d $msg")

    def removeAidTarget: Option[String] =
      if (isHuman(role))
        countryNames(game.muslims.filter(m => !m.truce && m.aidMarkers > 0)) match {
          case Nil => None
          case candidates =>
            Some(askCountry("Remove an aid marker from which country: ", candidates))
        }
      else
        USBot.removeAidTarget

    def addAidTarget: Option[String] = {
      countryNames(game.muslims.filter(m => !m.truce && m.canTakeAidMarker)) match {
        case Nil =>
          None
        case candidates if isHuman(role) =>
          Some(askCountry("Place an aid marker in which country: ", candidates))
        case candidates =>
          USBot.markerAlignGovTarget(candidates)
      }
    }

    //  Note: if scenario is HillaryWins then we add a +1 modifer to the die roll
    val hillaryMod  = if (game.scenarioName == awakening.scenarios.HillaryWins.name) 1 else 0
    val prestigeMod = game.prestigeModifier
    val die = getDieRoll("Enter event die roll: ", Some(role))
    val modRoll = die + hillaryMod + prestigeMod

    if (modRoll == die)
      log(s"\nDie roll: $die - ${resultMsgs(die)}", Color.Event)
    else
      log(s"\nDie roll: $die", Color.Event)
    logNotZero(prestigeMod, "Prestige modifier")
    logNotZero(hillaryMod,  s"${awakening.scenarios.HillaryWins.name} scenario rule")
    if (modRoll != die)
      log(s"Modified roll: $modRoll - ${resultMsgs(modRoll)}", Color.Event)
    println()
    modRoll match {
      case 0 =>
        log("\nPlace a random reaction marker.", Color.Event)
        val target = convergenceTarget
        addEventTarget(target)
        addReactionMarker(target)

      case 1 =>
        log("\nSubtract 1 from US Prestige.", Color.Event)
        decreasePrestige(1)

      case 2 =>
        log("\nRemove an Aid marker.", Color.Event)
        removeAidTarget match {
          case Some(target) =>
            addEventTarget(target)
            removeAidMarker(target)
          case None =>
            log("There are no Aid markers on the map.", Color.Event)
        }

      case 3 =>
        log("\nAdd 1 to Jihadist Funding.", Color.Event)
        increaseFunding(1)

      case 4 =>
        log("\nSubtract 1 from Jihadist Funding.", Color.Event)
        decreaseFunding(1)

      case 5 =>
        addAidTarget match {
          case Some(target) =>
            addEventTarget(target)
            addAidMarker(target)
          case None =>
            // This will not happen!
            log("\nThere are no countries that can take an Aid marker.", Color.Event)
        }

      case 6 =>
        log("\nAdd 1 to US Prestige.", Color.Event)
        increasePrestige(1)

      case _ =>
        log("\nPlace a random awakening marker.", Color.Event)
        val target = convergenceTarget
        addEventTarget(target)
        addAwakeningMarker(target)
    }

    setTrumpTweetsON()
  }
}
