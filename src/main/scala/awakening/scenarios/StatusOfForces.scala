
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

class StatusOfForces extends Scenario {
  val name           = "Status of Force Agreement"
  val cardDeckName   = AwakeningDeck
  val prestige       = 6
  val usPosture      = Soft
  val funding        = 6
  val availablePlots = Plot1::Plot1::Plot1::Plot2::Plot2::Plot3::Nil
  val countries = List(
    NonMuslimCountry(Canada),
    NonMuslimCountry(UnitedStates, posture = Soft),
    NonMuslimCountry(UnitedKingdom, recruitOverride = 2, posture = Hard),
    NonMuslimCountry(Serbia),
    NonMuslimCountry(Israel, posture = Hard),
    NonMuslimCountry(India),
    NonMuslimCountry(Scandinavia),
    NonMuslimCountry(EasternEurope),
    NonMuslimCountry(Benelux, posture = Soft),
    NonMuslimCountry(Germany),
    NonMuslimCountry(Italy),
    NonMuslimCountry(France, recruitOverride = 2, posture = Hard),
    NonMuslimCountry(Spain, recruitOverride = 2),
    NonMuslimCountry(Russia, governance = Fair),
    NonMuslimCountry(Caucasus, governance = Fair),
    NonMuslimCountry(China, governance = Fair),
    NonMuslimCountry(KenyaTanzania, governance = Fair),
    NonMuslimCountry(Thailand, governance = Fair),
    NonMuslimCountry(Philippines, governance = Fair, recruitOverride = 3),
    NonMuslimCountry(Iran, governance = Fair, wmdCache = 1, iranSpecialCase = true),
    NonMuslimCountry(Nigeria, governance = Poor),
    
    MuslimCountry(Morocco, resources = 2),
    MuslimCountry(AlgeriaTunisia, resources = 2, oilProducer = true,
                  governance = Poor, alignment = Neutral, awakening = 1),
    MuslimCountry(Libya, resources = 1, oilProducer = true, governance = Fair, alignment = Ally),
    MuslimCountry(Egypt, resources = 3, governance = Fair, alignment = Neutral, 
                  awakening = 1, reaction = 1),
    MuslimCountry(Sudan, resources = 1, oilProducer = true),
    MuslimCountry(Somalia, resources = 1),
    MuslimCountry(Jordan, resources = 1),
    MuslimCountry(Syria, resources = 2, wmdCache = 2, governance = Poor, alignment = Neutral,
                  awakening = 1, reaction = 1, sleeperCells = 2),
    MuslimCountry(CentralAsia, resources = 2),
    MuslimCountry(Turkey, isSunni = false, resources = 2),
    MuslimCountry(Lebanon, isSunni = false, resources = 1),
    MuslimCountry(Yemen, isSunni = false, resources = 1),
    MuslimCountry(Iraq, isSunni = false, resources = 3, oilProducer = true,
                  governance = Poor, alignment = Neutral, militia = 1, sleeperCells = 2,
                  awakening = 1, reaction = 1),
    MuslimCountry(SaudiArabia, isSunni = false, resources = 3, oilProducer = true),
    MuslimCountry(GulfStates, isSunni = false, resources = 3, oilProducer = true,
                  governance = Fair, alignment = Ally, troops = 2, awakening = 1),
    MuslimCountry(Pakistan, isSunni = false, resources = 2, wmdCache = 3,
                  governance = Poor, alignment = Neutral, sleeperCells = 1, reaction = 1),
    MuslimCountry(Afghanistan, isSunni = false, resources = 1,
                  governance = Poor, alignment = Ally, troops = 6, sleeperCells = 2,
                  regimeChange = TanRegimeChange),
    MuslimCountry(IndonesiaMalaysia, resources = 3, oilProducer = true),
    MuslimCountry(Mali, resources = 1)
  )
  val markersInPlay = List.empty[String]
  val cardsRemoved = 133::185::237::Nil
  val offMapTroops = 0
}
