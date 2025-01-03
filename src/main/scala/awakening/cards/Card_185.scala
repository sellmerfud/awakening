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
// Remove all Troops from a Jihadist player designated country with
// Troops. Set Alignment to Neutral and place an Aid marker. Remove
// any Regime Change marker; other markers/pieces remain.
// REMOVE
// ------------------------------------------------------------------
object Card_185 extends Card(185, "al-Maliki", Jihadist, 3, Remove, NoLapsing, NoAutoTrigger) {
  // Used by the US Bot to determine if the executing the event would alert a plot
  // in the given country
  override
  def eventAlertsPlot(countryName: String, plot: Plot): Boolean = false

  // Used by the US Bot to determine if the executing the event would remove
  // the last cell on the map resulting in victory.
  override
  def eventRemovesLastCell(): Boolean = false

  val isCandidate = (c: Country) => c.totalTroops > 0

  def getCandidates() = countryNames(game.countries.filter(isCandidate))

  // Returns true if the printed conditions of the event are satisfied
  override
  def eventConditionsMet(role: Role) = getCandidates().nonEmpty

  val isBotCandidate = (c: Country) =>
    isCandidate(c) &&
    !JihadistBot.muslimTest(m => m.caliphateCapital || m.inRegimeChange)(c)

  def getBotCandidates() = countryNames(game.countries.filter(isBotCandidate))

  // Returns true if the Bot associated with the given role will execute the event
  // on its turn.  This implements the special Bot instructions for the event.
  // When the event is triggered as part of the Human players turn, this is NOT used.
  //
  // Bot will not play this in Caliphate Capital or in a country under Regime Change
  override
  def botWillPlayEvent(role: Role): Boolean = getBotCandidates().nonEmpty

  // Carry out the event for the given role.
  // forTrigger will be true if the event was triggered during the human player's turn
  // and it associated with the Bot player.
  override
  def executeEvent(role: Role, forTrigger: Boolean): Unit = {
    // The only non-muslim country that may contain troops is the Philippines
    // if (abu Sayyaf is in effect)
    val target = if (isHuman(role))
      askCountry("Select country: ", getCandidates())
    else {
      // See Event Instructions table
      // Only choose the Philippines if it is the only candidate
      val eventPriorities = List(
        JihadistBot.AllyPriority,
        JihadistBot.NeutralPriority
      )

      // When triggered during US turn preferred canidates may be empty
      val candidates = getBotCandidates() match {
        case Nil => JihadistBot.narrowCandidates(getCandidates(), eventPriorities, allowBotLog = false)
        case c => JihadistBot.narrowCandidates(c, eventPriorities, allowBotLog = false)
      }

      JihadistBot.troopsMilitiaTarget(candidates).get
    }

    addEventTarget(target)
    removeAllTroopsFromCountry(target)
    if (game.isMuslim(target)) {
      setAlignment(target, Neutral)
      addAidMarker(target)
      endRegimeChange(target)
    }
  }
}
