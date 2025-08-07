
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

object MittsTurn extends Scenario {
  val name           = "Mitt's Turn"
  val startingMode   = AwakeningMode
  val allowsCampaign = true
  val prestige       = 7
  val usPosture      = Hard
  val funding        = 6
  val availablePlots = Plot1::Plot1::Plot1::Plot2::Plot2::Plot3::Nil
  val removedPlots   = List.fill(3)(PlotWMD)  // 3 extra
  val countries = List(
    DefaultSyria.copy(isSunni = false, wmdCache = 2),
    DefaultAlgeriaTunisia.copy(governance = Poor, alignment = Neutral, awakening = 1),
    DefaultIran.copy(wmdCache = 1),
    DefaultIraq.copy(governance = Poor, alignment = Ally, pieces = Pieces(usTroops = 2, sleeperCells = 1)),
    DefaultGulfStates.copy(governance = Fair, alignment = Ally, pieces = Pieces(usTroops = 2)),
    DefaultAfghanistan.copy(governance = Poor, alignment = Ally, pieces = Pieces(usTroops = 6, sleeperCells = 2),
      regimeChange = TanRegimeChange),
    DefaultPakistan.copy(governance = Fair, alignment = Neutral, pieces = Pieces(sleeperCells = 2)),
    DefaultUnitedKingdom.copy(postureValue = Hard),
    DefaultFrance.copy(postureValue = Hard),
    DefaultBenelux.copy(postureValue = Soft),
    DefaultRussia.copy(postureValue = Soft))
  val markersInPlay = List.empty[GlobalMarker]
  val cardsRemoved = List.empty[Int]
  val offMapTroops = 0
}
