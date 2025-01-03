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

// Card Text:
// ------------------------------------------------------------------
// Play if there is a Cell in Israel.
// Activate all Cells in Israel. Place a Plot there equal to or less
// than the number of Cells (WMD requires 3 or more Cells).
// If US Embasy to Jerusalem has been Removed, place 2 Plots whose
// combined value does not exceed the number of Cells.
// ------------------------------------------------------------------
object Card_311 extends Card2(311, "Gaza Border Protests", Jihadist, 3, NoRemove, NoLapsing, NoAutoTrigger) {
  // Used by the US Bot to determine if the executing the event would alert a plot
  // in the given country
  override
  def eventAlertsPlot(countryName: String, plot: Plot): Boolean = false

  // Used by the US Bot to determine if the executing the event would remove
  // the last cell on the map resulting in victory.
  override
  def eventRemovesLastCell(): Boolean = false

  def availablePlots(numCells: Int) =
    game.availablePlots.filter(_.number <= numCells).sorted

  // Returns true if the printed conditions of the event are satisfied
  override
  def eventConditionsMet(role: Role) = {
    val numCells = game.getCountry(Israel).totalCells
    numCells > 0 && availablePlots(numCells).nonEmpty
  }

  // Returns true if the Bot associated with the given role will execute the event
  // on its turn.  This implements the special Bot instructions for the event.
  // When the event is triggered as part of the Human players turn, this is NOT used.
  override
  def botWillPlayEvent(role: Role): Boolean = true

  // Carry out the event for the given role.
  // forTrigger will be true if the event was triggered during the human player's turn
  // and it associated with the Bot player.
  override
  def executeEvent(role: Role, forTrigger: Boolean): Unit = {
    val USEmbassyToJerusalem = 254
    val maxPlots = if (game.cardRemoved(USEmbassyToJerusalem)) 2 else 1
    val numCells = game.getCountry(Israel).totalCells

    addEventTarget(Israel)
    flipAllSleepersCells(Israel)

    if (maxPlots == 1 && numCells > 2)
      displayLine("\nPlace any available plot in Israel", Color.Event)
    else if (maxPlots == 1)
      displayLine(s"\nPlace an available plot with level <= ${numCells} in Israel", Color.Event)
    else if (numCells == 1)
      displayLine(s"\nPlace and available level 1 plot in Israel", Color.Event)
    else
      displayLine(s"\nPlace up to 2 plots with combined levels <= ${numCells} in Israel", Color.Event)

    val plots = if (isHuman(role)) {
      def nextPlot(numLeft: Int, plotSum: Int, remainingPlots: List[Plot]): List[Plot] = {
        if (numLeft == 0)
          Nil
        else
          remainingPlots.dropWhile(_.number > numCells - plotSum) match {
            case Nil => Nil
            case ps  =>
              val p = askPlots(ps, 1).head
              val index = ps.indexOf(p)
              val pps = ps.take(index) ::: ps.drop(index + 1)
              p :: nextPlot(numLeft - 1, plotSum + p.number, pps)
          }
      }

      println()
      nextPlot(maxPlots, 0, availablePlots(numCells))
    }
    else {
      // Bot
      def nextPlot(numLeft: Int, plotSum: Int, remainingPlots: List[Plot]): List[Plot] = {
        if (numLeft == 0)
          Nil
        else
          remainingPlots.dropWhile(_.number > numCells - plotSum) match {
            case Nil => Nil
            case p::ps => p::nextPlot(numLeft - 1, plotSum + p.number, ps)
          }
      }

      nextPlot(maxPlots, 0, availablePlots(numCells))
    }

    println()
    for (p <- plots)
      addAvailablePlotToCountry(Israel, p)
  }
}
