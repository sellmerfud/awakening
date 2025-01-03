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
// Play if a Plot was resolved in a non-Mus-lim country last Resolve Plots phase.
// If US play, Select Posture of 1 Schengen and 1 non-Schengen country (not US).
// If Jihadist, Set US Posture to opposite of World (N/A if World at 0),
// OR roll 1 die; Prestige reduced by half that number rounded up.
// ------------------------------------------------------------------
object Card_224 extends Card2(224, "Je Suis Charlie", Unassociated, 2, NoRemove, NoLapsing, NoAutoTrigger) {
  // Used by the US Bot to determine if the executing the event would alert a plot
  // in the given country
  override
  def eventAlertsPlot(countryName: String, plot: Plot): Boolean = false

  // Used by the US Bot to determine if the executing the event would remove
  // the last cell on the map resulting in victory.
  override
  def eventRemovesLastCell(): Boolean = false

  def getCandidates() = game.resolvedPlotTargets.filter(t => !t.isMuslim)

  // Returns true if the printed conditions of the event are satisfied
  override
  def eventConditionsMet(role: Role) = getCandidates().nonEmpty

  // Returns true if the Bot associated with the given role will execute the event
  // on its turn.  This implements the special Bot instructions for the event.
  // When the event is triggered as part of the Human players turn, this is NOT used.
  override
  def botWillPlayEvent(role: Role): Boolean = role match {
    case US => game.hasNonMuslim(n => n.canChangePosture && n.posture != game.usPosture)
    case Jihadist => (game.usPosture == Hard && game.worldPosture == Hard) || game.prestige > 1
  }

  // Carry out the event for the given role.
  // forTrigger will be true if the event was triggered during the human player's turn
  // and it associated with the Bot player.
  override
  def executeEvent(role: Role, forTrigger: Boolean): Unit = {
    // See Event Instructions table
    if (role == US) {
      val nonSchengen = countryNames(game.nonMuslims.filter(n => n.canChangePosture && !n.isSchengen))
      if (isHuman(role)) {
        val schengen  = askCountry("\nSelect posture of which Schengen country: ", Schengen)
        addEventTarget(schengen)
        setCountryPosture(schengen, askPosture(schengen))

        val other  = askCountry("Select posture of which non-Schengen country: ", nonSchengen)
        addEventTarget(other)
        setCountryPosture(other, askPosture(other))
      }
      else {
        def setPosture(candidates: List[String], candidatesSame: List[String]): Unit = {
          val target = candidates match {
            case Nil => candidatesSame.head
            case xs  => USBot.posturePriority(xs).get
          }
          addEventTarget(target)
          setCountryPosture(target, game.usPosture)
        }

        // It is possible (unlikely) that there are no countries that
        // can be set to the oppoosite posture. In that case we simply
        // select a country with the same posture so there is no change.
        val (shengens, schengensSame) = game.nonMuslims
          .filter(n => n.isSchengen && n.canChangePosture)
          .partition(_.posture != game.usPosture)
        val (others, othersSame) = game.nonMuslims
          .filter(n => !n.isSchengen && n.canChangePosture)
          .partition(_.posture != game.usPosture)

        shengens match {
          case Nil =>
          case candidates =>
            USBot.posturePriority(countryNames(candidates)) foreach { name =>
              addEventTarget(name)
              setCountryPosture(name, game.usPosture)
            }
        }
      }
    }
    else { // Jihadist
      if (isHuman(role)) {
        val canPosture = (game.usPosture == game.worldPosture)
        val choices = List(
          choice(canPosture,        "posture" , "Set US posture to opposite of World"),
          choice(game.prestige > 1, "prestige", "Reduce US prestige by 1/2 die roll (rounded up)")
        ).flatten

        if (choices.isEmpty)
          log("\nThe event has no effect.", Color.Event)
        else {
          askMenu("Choose one:", choices).head match {
            case "posture"  => setUSPosture(oppositePosture(game.worldPosture))
            case "prestige" =>
              val die = getDieRoll("Enter prestige die roll: ", Some(role))
              log(s"\nDie roll: $die", Color.Event)
              decreasePrestige((die + 1)/ 2)
          }
        }
      }
      else {
        // Jihadist Bot
        if (game.usPosture == Hard && game.worldPosture == Hard)
          setUSPosture(Soft)
        else {
          val die = getDieRoll("Enter prestige die roll: ")
          log(s"\nDie roll: $die", Color.Event)
          decreasePrestige((die + 1)/ 2)
        }
      }
    }
  }
}
