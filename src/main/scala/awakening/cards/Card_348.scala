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
// Play if Trump Tweets is ON.
// Flip Trump Taxets to OFF:
// If US play: No Cells may travel to the US directly
//   from: Iran, Iraq, Libya, Somalia, Sudan, Syria, Yemen, or
//   any Civil War, Regime Change, and Islamist Rule country:
//   Any Cells that Travel to the US from any other country become Active.
// If Jihadist: Cancels all cards currently in play with Travel Restrictions:
//   Biometrics, Patriot Act, Islamic Maghreb, Travel Ban, and BREXIT.
//   Remove an Awakening marker.
// ------------------------------------------------------------------
object Card_348 extends Card(348, "Travel Ban", Unassociated, 2, NoRemove, NoLapsing, NoAutoTrigger) {
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
  def eventConditionsMet(role: Role) = role match {
    case US => trumpTweetsON && globalEventNotInPlay(TravelBan)
    case Jihadist => trumpTweetsON
  }

  def getAwakeningCandidates = countryNames(
    game.muslims.filter(m => !m.truce && m.awakening > 0)
  )

  // Returns true if the Bot associated with the given role will execute the event
  // on its turn.  This implements the special Bot instructions for the event.
  // When the event is triggered as part of the Human players turn, this is NOT used.
  override
  def botWillPlayEvent(role: Role): Boolean = role match {
    case US =>
      true
    case Jihadist if game.botEnhancements =>
      // Playable if an Awakening marker can be removed
      getAwakeningCandidates.nonEmpty
      true
    case Jihadist =>
      true
  }

  // Carry out the event for the given role.
  // forTrigger will be true if the event was triggered during the human player's turn
  // and it associated with the Bot player.
  override
  def executeEvent(role: Role): Unit = {
    setTrumpTweetsOFF()
    if (role == US)
      addGlobalEventMarker(TravelBan)
    else {
      removeGlobalEventMarker(TravelBan)
      List(PatriotAct, BREXIT)
        .foreach(removeCountryEventMarkerAnywhere)
      removeLapsingEvents(List(Biometrics, IslamicMaghreb))
      val candidates = getAwakeningCandidates
      if (candidates.nonEmpty) {
        val target = if (isHuman(role))
          askCountry("Remove awakening marker from which country: ", candidates)
        else
          JihadistBot.markerTarget(candidates).get
        removeAwakeningMarker(target)
      }
      else
        log("\nThere are no awakening markers to remove from the map.", Color.Event)
    }
  }
}
