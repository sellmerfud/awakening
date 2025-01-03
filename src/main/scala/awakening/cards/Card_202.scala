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
// Set your opponent's Reserves to 0 and add the value lost to your
// Reserves,
// OR Select Posture of either China, Russia or India,
// OR place or Remove a Cadre.
// ------------------------------------------------------------------
object Card_202 extends Card(202, "Cyber Warfare", Unassociated, 1, NoRemove, NoLapsing, NoAutoTrigger) {
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

  val PostureCountries = List(China, India, Russia)

  def getBotPostureCandidates(role: Role) = role match {
    case US =>
      PostureCountries.filter { name =>
        val c = game.getNonMuslim(name)
        c.isUntested || c.posture != game.usPosture
      }
    case Jihadist =>
      PostureCountries.filter { name =>
        val c = game.getNonMuslim(name)
        c.isUntested || c.posture == game.usPosture
      }
  }

  // Returns true if the Bot associated with the given role will execute the event
  // on its turn.  This implements the special Bot instructions for the event.
  // When the event is triggered as part of the Human players turn, this is NOT used.
  override
  def botWillPlayEvent(role: Role): Boolean = role match {
    case US => game.reserves.jihadist > 0 || getBotPostureCandidates(US).nonEmpty
    case Jihadist => game.reserves.us > 0 || getBotPostureCandidates(Jihadist).nonEmpty
  }

  // Carry out the event for the given role.
  // forTrigger will be true if the event was triggered during the human player's turn
  // and it associated with the Bot player.
  override
  def executeEvent(role: Role, forTrigger: Boolean): Unit = {
    val resValue = if (role == US)
      game.reserves.jihadist
    else
      game.reserves.us
    val cadres = game.hasCountry(_.hasCadre)
      val choices = List(
        choice(resValue > 0, "reserves", s"Steal opponent's ${amountOf(resValue,"reserve Op")}"),
        choice(true,         "posture",  "Set the posture of China, Russia, or India"),
        choice(true,         "place",    "Place a cadre"),
        choice(cadres,       "remove",   "Remove a cadre")
      ).flatten

    if (isHuman(role)) {
      askMenu("Choose one:", choices).head match {
        case "reserves" =>
          clearReserves(oppositeRole(role))
          addToReserves(role, resValue)

        case "posture"  =>
          val name = askCountry("Set the posture of which country? ", PostureCountries)
          val posture = askPosture(name)
          addEventTarget(name)
          setCountryPosture(name, posture)

        case "place"    =>
          val candidates = countryNames(game.countries.filter(c => !c.hasCadre))
          val name = askCountry("Place a cadre in which country? ", candidates)
          addEventTarget(name)
          testCountry(name)
          addCadreToCountry(name)

        case _          =>
          val candidates = countryNames(game.countries.filter(c => c.hasCadre ))
          val name = askCountry("Remove cadre from which country? ", candidates)
          addEventTarget(name)
          removeCadreFromCountry(name)
      }
    }
    else {
      // Bot
      // See Event Instructions table
      getBotPostureCandidates(role) match {
        case Nil =>
          clearReserves(oppositeRole(role))
          addToReserves(role, resValue)

        case candidates if role == US =>
          val name = USBot.markerAlignGovTarget(candidates).get
          addEventTarget(name)
          setCountryPosture(name, game.usPosture)

        case candidates =>
          val name = JihadistBot.alignGovTarget(candidates).get
          addEventTarget(name)
          setCountryPosture(name, oppositePosture(game.usPosture))
      }
    }
  }
}
