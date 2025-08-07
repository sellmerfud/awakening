
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

object YouCanCallMeAl extends Scenario {
  val name           = "You Can Call Me Al"
  val startingMode   = LabyrinthMode
  val allowsCampaign = true
  val prestige       = 7
  val usPosture      = Soft
  val funding        = 9
  val availablePlots = Plot1::Plot1::Plot1::Plot2::Plot2::Plot3::Nil
  val removedPlots   = List.fill(6)(PlotWMD)  // 3 unused until events and 3 extra
  val countries = List(
    DefaultLibya.copy(governance = Poor, alignment = Adversary),
    DefaultSyria.copy(governance = Fair, alignment = Adversary),
    DefaultIraq.copy(governance = Poor, alignment = Adversary),
    DefaultSaudiArabia.copy(governance = Poor, alignment = Ally, pieces = Pieces(usTroops = 2)),
    DefaultGulfStates.copy(governance = Fair, alignment = Ally, pieces = Pieces(usTroops = 2)),
    DefaultPakistan.copy(governance = Fair, alignment = Neutral),
    DefaultAfghanistan.copy(governance = IslamistRule, alignment = Adversary, pieces = Pieces(sleeperCells = 4)),
    DefaultSomalia.copy(besiegedRegime = true)
  )
  val markersInPlay = List.empty[GlobalMarker]
  val cardsRemoved = List(78)
  val offMapTroops = 0
}
