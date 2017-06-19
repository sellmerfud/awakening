
// Labyrinth Awakening
//
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

object MissionAccomplished extends MissionAccomplishedDetails {
  val name           = "Mission Accomplished"
  val scenarioType = LabyrinthScenario
}

object MissionAccomplishedCampaign extends MissionAccomplishedDetails {
  val name           = "Mission Accomplished -- Campaign"
  val scenarioType = CampaignScenario
}

trait MissionAccomplishedDetails extends Scenario {
  val prestige       = 3
  val usPosture      = Hard
  val funding        = 5
  val availablePlots = Plot1::Plot1::Plot1::Plot2::Plot2::Plot3::Nil
  val countries = List(
    DefaultLibya.copy(governance = Poor, alignment = Adversary),
    DefaultSyria.copy(governance = Fair, alignment = Adversary, sleeperCells = 1),
    DefaultIraq.copy(governance = Poor, alignment = Ally, sleeperCells = 3,
                    troops = 6, regimeChange = TanRegimeChange),
    DefaultIran.copy(sleeperCells = 1),
    DefaultSaudiArabia.copy(governance = Poor, alignment = Ally, sleeperCells = 1),
    DefaultGulfStates.copy(governance = Fair, alignment = Ally, troops = 2),
    DefaultPakistan.copy(governance = Fair, alignment = Ally, sleeperCells = 1, 
                         markers = List(FATA)),
    DefaultAfghanistan.copy(governance = Poor, alignment = Ally, sleeperCells = 1, 
                            troops = 5, regimeChange = TanRegimeChange),
    DefaultSomalia.copy(besiegedRegime = true),
    DefaultCentralAsia.copy(governance = Fair, alignment = Neutral),
    DefaultIndonesiaMalaysia.copy(governance = Fair, alignment = Neutral, sleeperCells = 1),
    DefaultPhilippines.copy(postureValue = Soft, sleeperCells = 1, /* troops = 2, */
                             markers = List(AbuSayyaf)),
    DefaultUnitedKingdom.copy(postureValue = Hard),
    DefaultUnitedStates.copy(markers = List(PatriotAct, NEST)))
  val markersInPlay = List(EnhancedMeasures, Renditions, Wiretapping)
  val cardsRemoved = List(43, 109, 5, 57, 116, 37)
  val offMapTroops = 0
  
  override val additionalSetup: () => Unit = () => {
    // The Jihadist player rolls the posture of each Schengen country.
    for (name <- Schengen)
      rollCountryPosture(name, false)
    logWorldPosture()
  }
}
