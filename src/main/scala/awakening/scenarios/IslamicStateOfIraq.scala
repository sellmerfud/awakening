
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

object IslamicStateOfIraq extends Scenario {
  val name           = "Islamic State of Iraq & the Levant (ISIL)"
  val startingMode   = AwakeningMode
  val allowsCampaign = true
  val prestige       = 5
  val usPosture      = Hard
  val funding        = 7
  val availablePlots = Plot1::Plot1::Plot1::Plot2::Plot2::Plot3::Nil
  val removedPlots   = List.fill(5)(PlotWMD)  // 2 removed from Syria and 3 extra
  val countries = List(
    DefaultSyria.copy(isSunni = false, governance = Fair, alignment = Neutral, civilWar = true, 
                      militia = 3, caliphateCapital = true, activeCells = 4, wmdCache = 0),
    DefaultIran.copy(wmdCache = 1),
    DefaultIraq.copy(governance = Poor, alignment = Neutral, civilWar = true, militia = 2,
                      activeCells = 3),
    DefaultGulfStates.copy(governance = Fair, alignment = Ally, troops = 2),
    DefaultAfghanistan.copy(governance = Fair, alignment = Ally, troops = 2, sleeperCells = 1),
    DefaultPakistan.copy(governance = Poor, alignment = Ally, cadres = 1),
    DefaultMuslimNigeria.copy(sleeperCells = 2),
    DefaultUnitedKingdom.copy(postureValue = Hard),
    DefaultFrance.copy(postureValue = Hard),
    DefaultBenelux.copy(postureValue = Hard))
  val markersInPlay = List(Sequestration)
  val cardsRemoved = 133::151::174::184::185::188::194::234::237::Nil
  val offMapTroops = 3
}
