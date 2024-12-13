
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
import awakening.JihadistBot

object Anaconda extends Scenario {
  val name           = "Anaconda"
  val startingMode   = LabyrinthMode
  val allowsCampaign = true
  val prestige       = 8
  val usPosture      = Hard
  val funding        = 6
  val availablePlots = Plot1::Plot1::Plot1::Plot2::Plot2::Plot3::Nil
  val removedPlots   = Nil
  val countries = List(
    DefaultLibya.copy(governance = Poor, alignment = Adversary),
    DefaultSyria.copy(governance = Fair, alignment = Adversary),
    DefaultIraq.copy(governance = Poor, alignment = Adversary),
    DefaultSaudiArabia.copy(governance = Poor, alignment = Ally, troops = 2),
    DefaultGulfStates.copy(governance = Fair, alignment = Ally, troops = 2),
    DefaultPakistan.copy(governance = Poor, alignment = Ally, sleeperCells = 1, 
                         markers = List(FATA)),
    DefaultAfghanistan.copy(governance = Poor, alignment = Ally, sleeperCells = 1, 
                            troops = 6, regimeChange = TanRegimeChange),
    DefaultSomalia.copy(besiegedRegime = true),
    DefaultCentralAsia.copy(governance = Poor, alignment = Ally),
    DefaultUnitedStates.copy(markers = List(PatriotAct)))
  val markersInPlay = List.empty[String]
  val cardsRemoved = List(43, 109)
  val offMapTroops = 0
  
  override val additionalSetup: () => Unit = () => {
    // The Jihadist player places one cell in each of three different
    // countries. (Not the United States)
    val candidates = countryNames(game.countries) filterNot (_ == UnitedStates)
    val targets = if (game.humanRole == Jihadist) {
      println("\nChoose three countries where you would like to place a sleeper cell:")
      askCountries(3, candidates)
    }
    else
      JihadistBot.multipleTargets(3, candidates)(JihadistBot.recruitTravelToPriority)
    
    for (name <- targets) {
      testCountry(name)
      addSleeperCellsToCountry(name, 1)
    }
  }
}
