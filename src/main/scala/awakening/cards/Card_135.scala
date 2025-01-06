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
// Draw a random card from the Jihadist hand and add it to the US hand,
// OR temporarily reveal all Jihadist Available and/or placed WMD and
// remove one (+1 Prestige if WMD Removed; 11.3.1).
// ------------------------------------------------------------------
object Card_135 extends Card(135, "Delta / SEALS", US, 2, NoRemove, NoLapsing, NoAutoTrigger) {
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
  def botWillPlayEvent(role: Role): Boolean =
    game.availablePlots.contains(PlotWMD) ||
    hasCardInHand(Jihadist)


  // Carry out the event for the given role.
  // forTrigger will be true if the event was triggered during the human player's turn
  // and it associated with the Bot player.
  override
  def executeEvent(role: Role): Unit = {
    if (isHuman(role)) {
      val choices = List(
        "reveal" -> "Reveal all WMD plots and remove one",
        "draw"   -> "Randomly draw 1 card from the Jihadist hand")

      askMenu("Choose one:", choices).head match {
        case "draw" =>
          log("\nDraw the top card in the Jihadist Bot's hand", Color.Event)
          askCardsDrawn(US, 1, FromOpponent)

        case _ =>
          val Available = "Available Plots box"
          val wmdOnMap = for (c <- game.countries; PlotOnMap(PlotWMD, _) <- c.plots)
            yield c.name
          val wmdAvailable = game.availablePlots.sorted.takeWhile(_ == PlotWMD)
          if (wmdOnMap.isEmpty && wmdAvailable.isEmpty)
            log("\nThere are no WMD plots on the map or in the available plots box.", Color.Event)
          else {
            log(s"\nWMD plots in the available plots box: ${wmdAvailable.size}")
            val onMapDisplay = if (wmdOnMap.isEmpty) "none" else wmdOnMap.mkString(", ")
            log(s"WMD plots on the map: " + onMapDisplay)
            val target = if (wmdOnMap.isEmpty)
              Available
            else if (wmdAvailable.isEmpty)
              askCountry("Select country with WMD: ", wmdOnMap)
            else {
              val choices = (Available :: wmdOnMap).map(n => n -> s"Remove WMD plot in $n")
              askMenu("Choose one", choices).head
            }

            if (target == Available)
              removeAvailableWMD(1)
            else {
              addEventTarget(target)
              removePlacedWMD(target, 1)
            }
          }
      }
    }
    else {
      // See Event Instructions table
      // If there are any WMD plots in the available box, the bot
      // will remove one. Otherwise take a US player's random card.
      if (game.availablePlots.contains(PlotWMD))
        removeAvailableWMD(1)
      else {
        log(s"\nYou ($Jihadist) must place one random card on top of the $US hand", Color.Event)
        askCardsDrawn(US, 1, FromOpponent)
      }
    }
  }
}
