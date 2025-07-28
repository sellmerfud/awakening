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

// Card Text:
// ------------------------------------------------------------------
// Play if Populism/Euro-scepticism in Event Box, or a Schengen Country
// has a Cell, or a Plot was resolved in a Schengen Country in the last US
// Action Phase.
// If US play: Set UK to Hard. UK Recruit level is reduced to 1.
//             As long as UK is Hard, no Celis may travel there from
//             Schengen Countries. MARK & REMOVE
// If Jibadist: -1 Prestige. Move closest Cell (Jihadist choice if equidistant) to the UK.
// ------------------------------------------------------------------
object Card_324 extends Card(324, "BREXIT", Unassociated, 1, USRemove, NoLapsing, NoAutoTrigger) {
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
  def eventConditionsMet(role: Role) =
    globalEventInPlay(Euroscepticism) ||
    (game.hasNonMuslim(n => n.isSchengen && n.totalCells > 0)) ||
    game.resolvedPlotTargets.exists(t => Schengen.contains(t.name))

  // Returns true if the Bot associated with the given role will execute the event
  // on its turn.  This implements the special Bot instructions for the event.
  // When the event is triggered as part of the Human players turn, this is NOT used.
  override
  def botWillPlayEvent(role: Role): Boolean = true

  // Carry out the event for the given role.
  // forTrigger will be true if the event was triggered during the human player's turn
  // and it associated with the Bot player.
  override
  def executeEvent(role: Role): Unit =
    role match {
      case US =>
        addEventTarget(UnitedKingdom)
        setCountryPosture(UnitedKingdom, Hard)
        addEventMarkersToCountry(UnitedKingdom, BREXIT)

      case Jihadist =>
        decreasePrestige(1)
        addEventTarget(UnitedKingdom)
        val source = closestWithCells(UnitedKingdom) match {
          case Nil => None
          case candidates if isHuman(role) =>
            Some(askCountry(s"Move cell to $UnitedKingdom from which country: ", candidates))
          case candidates if game.botEnhancements =>
            JihadistBot.enhancedTravelFromTarget(UnitedKingdom, candidates, autoSuccess = true) orElse
            JihadistBot.standardTravelFromTarget(UnitedKingdom, candidates, inPlaceOk = false) orElse
            shuffle(candidates).headOption
          case candidates =>
            JihadistBot.standardTravelFromTarget(UnitedKingdom, candidates, inPlaceOk = false) orElse
            shuffle(candidates).headOption
        }

        source.foreach { name =>
          moveCellsBetweenCountries(name, UnitedKingdom, 1, game.getCountry(name).pieces.activeCells > 0, forTravel = false)
        }
    }
}
