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
import awakening.USBot
import awakening.USBot.PlotInCountry

// Card Text:
// ------------------------------------------------------------------
// Play if you can select a Plot in a Muslim country.
// If unblocked, the Plot subtracts rather than adds Funding (if WMD, Funding to 1).
// ------------------------------------------------------------------
object Card_122 extends Card(122, "Backlash", US, 1, NoRemove, NoLapsing, NoAutoTrigger) {
  // Used by the US Bot to determine if the executing the event would alert a plot
  // in the given country
  override
  def eventAlertsPlot(countryName: String, plot: Plot): Boolean = false

  // Used by the US Bot to determine if the executing the event would remove
  // the last cell on the map resulting in victory.
  override
  def eventRemovesLastCell(): Boolean = false

  val isCandidate = (m: MuslimCountry) =>
    !m.truce &&
    m.plots.exists(!_.backlashed) &&
    !game.isCaliphateMember(m.name)

  def getCandidates = countryNames(game.muslims.filter(isCandidate))

  // Returns true if the printed conditions of the event are satisfied
  override
  def eventConditionsMet(role: Role) = getCandidates.nonEmpty

  // Returns true if the Bot associated with the given role will execute the event
  // on its turn.  This implements the special Bot instructions for the event.
  // When the event is triggered as part of the Human players turn, this is NOT used.
  override
  def botWillPlayEvent(role: Role): Boolean = game.funding > 1

  // Carry out the event for the given role.
  // forTrigger will be true if the event was triggered during the human player's turn
  // and it associated with the Bot player.
  override
  def executeEvent(role: Role): Unit = {
    if (isHuman(role)) {
      val target = askCountry(s"Backlash in which country: ", getCandidates)
      // Pick a random plot in the country
      addEventTarget(target)
      val m = game.getMuslim(target)
      val (backlashed, clean) = m.plots.partition(_.backlashed)
      val plot :: remaining = shuffle(clean)
      val newPlots = plot.copy(backlashed = true) :: remaining ::: backlashed
      game = game.updateCountry(m.copy(plots = newPlots))
      log(s"\nBacklash applied to a plot in $target", Color.Event)
    }
    else {
      val plots = for {
        name <- getCandidates
        m = game.getMuslim(name)
        plot <- m.plots
        if !plot.backlashed
      } yield PlotInCountry(plot, m)

      // Pick the highest priority plot among the countries
      val PlotInCountry(plotOnMap, country) = USBot.priorityPlot(plots)
      val (matching, other) = country.plots.partition(_ == plotOnMap)
      val newPlots = matching.head.copy(backlashed = true) :: matching.tail ::: other
      addEventTarget(country.name)
      val m = country.asInstanceOf[MuslimCountry]
      game = game.updateCountry(m.copy(plots = newPlots))
      log(s"\nBacklash applied to a $plotOnMap in ${country.name}", Color.Event)
    }
  }
}
