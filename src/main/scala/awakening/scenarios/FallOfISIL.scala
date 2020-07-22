
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

import awakening.LabyrinthAwakening._

object FallOfISIL extends Scenario {
  val name           = "Fall Of ISIL"
  val startingMode   = ForeverWarMode
  val prestige       = 5
  val usPosture      = Hard
  val funding        = 6
  val availablePlots = Plot1::Plot1::Plot1::Plot2::Plot2::Plot3::Nil
  val removedPlots   = Nil
  val countries = List(
    DefaultMuslimNigeria.copy(governance = Poor, alignment = Ally, sleeperCells = 1),
    DefaultSyria.copy(isSunni = false, governance = Fair, alignment = Neutral, civilWar = true,
                      militia = 2, activeCells = 4, caliphateCapital = true, wmdCache = 1),
    DefaultIraq.copy(governance = Poor, alignment = Neutral, civilWar = true, militia = 2, 
                     activeCells = 3, markers = Advisors :: Nil), // cells active because part of caliphate
    DefaultGulfStates.copy(governance = Fair, alignment = Ally, troops = 2),
    DefaultAfghanistan.copy(governance = Poor, alignment = Ally, troops = 2, sleeperCells = 1),
    DefaultIran.copy(wmdCache = 2),
    DefaultBenelux.copy(postureValue = Hard))
  val markersInPlay = List.empty[String]
  val cardsRemoved = List.empty[Int]
  val offMapTroops = 0
}
