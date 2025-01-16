
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
// Copyright (c) 2017 Curt Sellmer
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

package awakening.scenarios

import scala.annotation.tailrec
import awakening.LabyrinthAwakening._

object TrumpTakesCommand extends Scenario {
  val name           = "Trump Takes Command"
  val startingMode   = ForeverWarMode
  val allowsCampaign = false
  val prestige       = 6
  val usPosture      = Hard
  val funding        = 4
  val availablePlots = Plot1::Plot1::Plot1::Plot2::Plot2::Plot3::PlotWMD::Nil
  val removedPlots   = List.fill(4)(PlotWMD)  // 1 per scenario rules plus 3 extra
  val countries = List(
    DefaultNigeria.copy(postureValue = Soft, sleeperCells = 1),
    DefaultAlgeriaTunisia.copy(governance = Good, alignment = Neutral),
    DefaultLibya.copy(governance = Poor, alignment = Adversary, civilWar = true, sleeperCells = 1),
    DefaultLebanon.copy(governance = Poor, alignment = Adversary, sleeperCells = 1),
    DefaultSyria.copy(isSunni = false, governance = Fair, alignment = Neutral, civilWar = true,
                      militia = 2, sleeperCells = 1, wmdCache = 1, markers = Advisors :: Nil),
    DefaultMuslimIran.copy(governance = IslamistRule, alignment = Adversary, sleeperCells = 3, wmdCache = 0),
    DefaultGulfStates.copy(governance = Fair, alignment = Ally, troops = 2),
    DefaultAfghanistan.copy(governance = Poor, alignment = Ally, troops = 2, militia = 1, sleeperCells = 1, reaction = 1),
    DefaultPakistan.copy(militia = 2),
    DefaultYemen.copy(governance = Poor, alignment = Neutral, civilWar = true, militia = 1, sleeperCells = 2),
    DefaultKenyaTanzania.copy(postureValue = Hard, cadres = 1))
  val markersInPlay = List(TrumpTweetsON)
  val cardsRemoved = List(303, 340, 343)
  val offMapTroops = 0
  
  override val notes = Seq(
    "For end game scoring the Jihadist player is awarded 1 bonus point",
    "of Resources at Islamist Rule if no WMD was alerted or resolved",
    "during the game."
  )
  
  override val additionalSetup: () => Unit = () => {
    // Roll to place Awakening markers in two different random muslim countries.
    @tailrec def getAwakeningTarget(forbidden: Option[String] = None): String = randomMuslimCountry match {
      case m if m.canTakeAwakeningOrReactionMarker && Some(m.name) != forbidden => m.name
      case _ => getAwakeningTarget(forbidden)
    }
    
    val first = getAwakeningTarget(None)
    val second = getAwakeningTarget(Some(first))
    
    
    log("\nAs part of setup we place an Awakening marker in two different random Muslim countries")
    log(separator())
    for (name <- List(first, second)) {
      testCountry(name)
      addAwakeningMarker(name)
    }
  }
  
}
