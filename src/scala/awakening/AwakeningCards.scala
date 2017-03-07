// Labyrinth Awakening
//
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
package awakening


// Card definitions for the the deck of cards in the
// Awakening expansion.

import scala.util.Random.shuffle
import scala.collection.immutable.ListMap
import LabyrinthAwakening._

object AwakeningCards extends CardDeck {
  val GulfUnionCountries = List(GulfStates, SaudiArabia, Yemen, Jordan, Morocco).sorted
  
  // Various tests used by the card events
  val advisorsCandidate = (m: MuslimCountry) => !m.isAdversary && m.civilWar && m.totalTroops == 0 && !m.hasMarker("Advisors")
  val humanitarianAidCandidate = (m: MuslimCountry) => m.canTakeAidMarker && m.totalCells > 0
  val peshmergaCandidate = (m: MuslimCountry) => (m.name == Iraq || m.name == Syria) && m.totalCells > 0
  val arabSpringFalloutCandidate = (m: MuslimCountry) => {
    m.awakening == 0 &&
    m.canTakeAwakeningOrReactionMarker && 
    (game.adjacentMuslims(m.name) forall (_.awakening == 0))
  }
  val strikeEagleCandidate = (m: MuslimCountry) => m.name != Pakistan && m.isPoor && !m.isAlly && m.wmdCache > 0
  val tahrirCandidate = (m: MuslimCountry) => m.name != Egypt && m.canTakeAwakeningOrReactionMarker && m.awakening == 0
  
  val unscr1973Candidate = (m: MuslimCountry) => {
    // Can play in country that already has the marker if it also contains at least one cell.
    // Or in any other civil war country that does not have the maker.
    (m.hasMarker("UNSCR 1973") && m.totalCells > 0) || (m.civilWar && !m.hasMarker("UNSCR 1973"))
  }
  val massTurnoutCandidate = (m: MuslimCountry) => m.inRegimeChange && m.awakening > 0
  val scafCandidate = (m: MuslimCountry) => 
    m.name != Iran && m.name != Syria && m.awakening > 0 && m.reaction > 0 &&
    (!m.isAlly || !m.isFair || m.totalCells > 0)
  val statusQuoCandidate = (m: MuslimCountry) => 
    m.regimeChange == TanRegimeChange && (m.totalTroopsAndMilitia / 2) > m.totalCells
  
  val coupCandidate = (m: MuslimCountry) => 
    m.resources == 1 && (m.isPoor || m.isGood) && 
    m.totalCells >= 2 && !(m.civilWar && m.besiegedRegime)
    
  val islamicMaghrebCountry = Set(AlgeriaTunisia, Libya, Mali, Morocco, Nigeria)
  val islamicMaghrebCandidate = (m: MuslimCountry) => islamicMaghrebCountry(m.name) && m.isPoor
  val theftOfStateCandidate = (m: MuslimCountry) => m.isPoor && m.awakening > 0
  // Countries with cells that are not within two countries with troops/advisor
  def specialForcesCandidates: List[String] = {
    // First find all muslim countries with troops or "Advisors"
    val withForces = (game.muslims filter (m => m.totalTroops > 0 || m.hasMarker("Advisors"))) map (_.name)
    // Next get all countries that contain any cells and filter out the ones are are not 
    // within two of a country with forces.
    countryNames(game.countries filter { country =>
      country.totalCells > 0 && (withForces exists (forces => distance(forces, country.name) <= 2))
    })
  }
  
  // Countries with an awakening maker or adjacent to a country with an awakening marker.
  def faceBookCandidates: List[String] = countryNames(game.muslims filter { m =>
    m.awakening > 0 || (game.adjacentMuslims(m.name) exists (_.awakening > 0))
  })
  
  
  val cardMap: Map[Int, Card] = Map(
    // ------------------------------------------------------------------------
    entry(new Card(121, "Advisors", US, 1,
      NoRemove, CountryMarker, NoLapsing, NoAutoTrigger,
      (_: Role) => (game.muslims count (_.hasMarker("Advisors"))) < 3 && (game hasMuslim advisorsCandidate)
      ,
      (role: Role) => {
        val candidates = countryNames(game.muslims filter advisorsCandidate)
        val target = if (role == game.humanRole)
          askCountry(s"Advisors in which country: ", candidates)
        else {
          log("!!! Bot event not yet implemented !!!")
          candidates.head
        }
        println()
        testCountry(target)
        addEventMarkersToCountry(target, "Advisors")
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(122, "Backlash", US, 1,
      NoRemove, NoMarker, NoLapsing, NoAutoTrigger,
      (_: Role) => game hasCountry (_.plots exists (p => !p.backlashed))
      ,
      (role: Role) => {
        val candidates = countryNames(game.countries filter (_.plots exists (p => !p.backlashed)))
        val countryName = if (role == game.humanRole)
          askCountry(s"Backlash in which country: ", candidates)
        else {
          log("!!! Bot event not yet implemented !!!")
          candidates.head
        }
        // Pick a random plot in the country
        val country = game.getCountry(countryName)
        val PlotOnMap(target, _) :: remaining = shuffle(country.plots)
        country match {
          case m: MuslimCountry    => game = game.updateCountry(m.copy(plots = PlotOnMap(target, true) :: remaining))
          case n: NonMuslimCountry => game = game.updateCountry(n.copy(plots = PlotOnMap(target, true) :: remaining))
        }
        println()
        log(s"Backlash applied to a plot in $countryName")
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(123, "Humanitarian Aid", US, 1,
      NoRemove, NoMarker, NoLapsing, NoAutoTrigger,
      (_: Role) => game hasMuslim humanitarianAidCandidate
      ,
      (role: Role) => {
        val candidates = countryNames(game.muslims filter humanitarianAidCandidate)
        val countryName = if (role == game.humanRole)
          askCountry(s"Humanitarian Aid in which country: ", candidates)
        else {
          log("!!! Bot event not yet implemented !!!")
          candidates.head
        }
        println()
        addAidMarker(countryName)
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(124, "Pearl Roundabout", US, 1,
      NoRemove, NoMarker, NoLapsing, NoAutoTrigger,
      (_: Role) => globalEventNotInPlay("Bloody Thursday")
      ,
      (role: Role) => {
        println()
        testCountry(GulfStates)
        addAwakeningMarker(GulfStates)
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(125, "Peshmerga", US, 1,
      NoRemove, NoMarker, NoLapsing, NoAutoTrigger,
      (_: Role) => (game hasMuslim peshmergaCandidate) && game.militiaAvailable > 0
      ,
      (role: Role) => {
        val candidates = countryNames(game.muslims filter peshmergaCandidate)
        val target = if (role == game.humanRole)
          askCountry(s"Select country: ", candidates)
        else {
          log("!!! Bot event not yet implemented !!!")
          candidates.head
        }
        println()
        addMilitiaToCountry(target, 2 min game.militiaAvailable)
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(126, "Reaper", US, 1,
      NoRemove, NoMarker, NoLapsing,  NoAutoTrigger, AlwaysPlayable,
      (role: Role) => {
        if (role == game.humanRole) {
          if (game.muslims exists (_.totalCells > 0)) {
            val choices = ListMap(
              "remove"  -> "Remove up to 2 cell in one Muslim country",
              "discard" -> "Randomly discard 1 card from the Jihadist hand")
            askMenu(choices).head match {
              case "discard" => log("Discard the top card in the Jihadist hand")
              case _ =>
              val candidates = game.muslims filter (_.totalCells > 0)
              val target = askCountry("Remove 2 cells in which country? ", countryNames(candidates))
              val (actives, sleepers) = askCells(target, 2)
              println()
              removeCellsFromCountry(target, actives, sleepers, addCadre = true)
            }
          }
          else
            log("Discard the top card in the Jihadist hand")
        }
        else {
          log("!!! Bot event not yet implemented !!!")
        }
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(127, "Reaper", US, 1,
      NoRemove, NoMarker, NoLapsing, NoAutoTrigger,
      (role: Role) => cardMap(126).eventConditions(role),
      (role: Role) => cardMap(126).executeEvent(role)
    )),
    // ------------------------------------------------------------------------
    entry(new Card(128, "Reaper", US, 1,
      NoRemove, NoMarker, NoLapsing, NoAutoTrigger,
      (role: Role) => cardMap(126).eventConditions(role),
      (role: Role) => cardMap(126).executeEvent(role)
    )),
    // ------------------------------------------------------------------------
    entry(new Card(129, "Special Forces", US, 1,
      NoRemove, NoMarker, NoLapsing, NoAutoTrigger,
      (_ : Role) => specialForcesCandidates.nonEmpty
      ,
      (role: Role) => {
        if (role == game.humanRole) {
          val target = askCountry("Remove cell in which country: ", specialForcesCandidates)
          val (actives, sleepers) = askCells(target, 1)
          println()
          removeCellsFromCountry(target, actives, sleepers, addCadre = true)
        }
        else {
          log("!!! Bot event not yet implemented !!!")
        }
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(130, "Special Forces", US, 1,
      NoRemove, NoMarker, NoLapsing, NoAutoTrigger,
      (role: Role) => cardMap(129).eventConditions(role),
      (role: Role) => cardMap(129).executeEvent(role)
    )),
    // ------------------------------------------------------------------------
    entry(new Card(131, "Arab Spring Fallout", US, 2,
      NoRemove, NoMarker, NoLapsing, NoAutoTrigger,
      (_: Role) => game hasMuslim arabSpringFalloutCandidate
      ,
      (role: Role) => {
        val candidates = countryNames(game.muslims filter arabSpringFalloutCandidate)
        val targets = if (candidates.size <= 2)
          candidates
        else if (role == game.humanRole) {
          val first  = askCountry("Select first country: ", candidates)
          val second = askCountry("Select second country: ", candidates filterNot (_ == first))
          first::second::Nil
        }
        else {
          log("!!! Bot event not yet implemented !!!")
          shuffle(candidates) take 2
        }
        
        println()
        targets foreach { target =>
          testCountry(target)
          addAwakeningMarker(target)
        }
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(132, "Battle of Sirte", US, 2,
      Remove, NoMarker, NoLapsing, NoAutoTrigger,
      (_: Role) => game hasMuslim (_.civilWar)
      ,
      (role: Role) => {
        val candidates = countryNames(game.muslims filter (_.civilWar))
        val target = if (role == game.humanRole)
          askCountry("Select civil war country: ", candidates)
        else {
          log("!!! Bot event not yet implemented !!!")
          candidates.head
        }
        val (active, sleeper) = if (role == game.humanRole)
          askCells(target, 1)
        else {
          log("!!! Bot event not yet implemented !!!")
          val m = game.getMuslim(target)
          // TODO: This will probably become a botSelectCels() funcion as it is likely to be common.
          if (m.sleeperCells > 0) (0, 1)
          else if (m.activeCells > 0) (1, 0)
          else (0, 0)
        }
        println()
        removeCellsFromCountry(target, active, sleeper, addCadre = true)
        addMilitiaToCountry(target, game.militiaAvailable min 2)
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(133, "Benghazi Falls", US, 2,
      Remove, NoMarker, NoLapsing, NoAutoTrigger,
      (_: Role) => {
       val libya = game.getMuslim(Libya)
       val italy = game.getNonMuslim(Italy)
       // Conditions must be met AND the event must have some effect
       ((libya :: game.adjacentMuslims(Libya)) exists (_.awakening > 0)) &&
       (!libya.civilWar || game.militiaAvailable > 0 || italy.posture != Hard)
      }
      ,
      (role: Role) => {
        println()
        testCountry(Libya)
        startCivilWar(Libya)
        addMilitiaToCountry(Libya, game.militiaAvailable min 2)
        setCountryPosture(Italy, Hard)
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(134, "Civil Resistance", US, 2,
      NoRemove, NoMarker, NoLapsing, NoAutoTrigger, AlwaysPlayable,
      (role: Role) => {
        val sunnis = countryNames(game.muslims filter (m=> m.isSunni && m.canTakeAwakeningOrReactionMarker))
        if (sunnis.nonEmpty) {  // This should never happen, but let's be defensive
          val sunniTarget = if (role == game.humanRole)
            askCountry("Select a Sunni country: ", sunnis)
          else {
            log("!!! Bot event not yet implemented !!!")
            sunnis.head
          }
          println()
          testCountry(sunniTarget)
          addAwakeningMarker(sunniTarget)
        }
        
        if (randomShiaMixList exists (_.canTakeAwakeningOrReactionMarker)) {
          def doRandomShiaMix: Unit = {
            val shiaMix = randomShiaMixCountry
            if (shiaMix.canTakeAwakeningOrReactionMarker) {
              println()
              testCountry(shiaMix.name)
              addAwakeningMarker(shiaMix.name)
            }
            else
              doRandomShiaMix
          }
          doRandomShiaMix
        }
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(135, "Delta / SEALS", US, 2,
      NoRemove, NoMarker, NoLapsing, NoAutoTrigger, AlwaysPlayable,
      (role: Role) => {
        if (role == game.humanRole) {
          val choices = ListMap(
            "reveal"  -> "Reveal all WMD plots and remove one",
            "discard" -> "Randomly discard 1 card from the Jihadist hand")
          askMenu(choices).head match {
            case "discard" => log("Discard the top card in the Jihadist Bot's hand")
            case _ =>
              val wmdOnMap = for (c <- game.countries; PlotOnMap(PlotWMD, _) <- c.plots)
                yield c.name
              val wmdAvailable = game.availablePlots takeWhile (_ == PlotWMD)
              if (wmdOnMap.isEmpty && wmdAvailable.isEmpty)
                log("There are no WMD plots on the map or in the available plots box")
              else {
                log(s"WMD plots in the available plots box: ${wmdAvailable.size}")
                val onMapDisplay = if (wmdOnMap.isEmpty) "none" else wmdOnMap.mkString(", ")
                log(s"WMD plots on the map: " + onMapDisplay)
                val target = if (wmdOnMap.isEmpty) "available"
                else if (wmdAvailable.isEmpty)
                  askCountry("Select country with WMD: ", wmdOnMap)
                else
                  askCountry("Select 'available' or country with WMD: ", "available" :: wmdOnMap)
              
                if (target == "available") {
                  log(s"Permanently remove one $PlotWMD from the available plots box")
                  game = game.copy(availablePlots = game.availablePlots.sorted.tail).adjustPrestige(1)
                }
                else {
                  log(s"Permanently remove one $PlotWMD from the $target")
                  game.getCountry(target) match {
                    case m: MuslimCountry =>
                      game = game.updateCountry(m.copy(plots = m.plots.sorted.tail)).adjustPrestige(1)
                    case n: NonMuslimCountry =>
                      game = game.updateCountry(n.copy(plots = n.plots.sorted.tail)).adjustPrestige(1)
                  }
                }
                log(s"Increase prestige by +1 to ${game.prestige} for removing a WMD plot")
              }
          }
        }
        else {
          // If there are any WMD plots in the available box, the bot
          // will remove one. Otherwise the US player must discard a random card.
          if (game.availablePlots contains PlotWMD) {
            // The sort order for plots put WMD's first.
            game = game.copy(availablePlots = game.availablePlots.sorted.tail).adjustPrestige(1)
            log(s"Permanently remove one $PlotWMD from the available plots box")
            log(s"Increase prestige by +1 to ${game.prestige} for removing an WMD plot")
          }
          else
            log("Discard one card at random from the Jihadist player's hand.")
        }
        
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(136, "Factional Infighting", US, 2,
      NoRemove, NoMarker, NoLapsing, NoAutoTrigger, AlwaysPlayable,
      (role: Role) => {
        val candidates = countryNames(game.muslims filter (m => !m.isIslamistRule && m.totalCells > 0))
        if (candidates.nonEmpty) {
          val target = if (role == game.humanRole)
            askCountry("Select a country: ", candidates)
          else {
            log("!!! Bot event not yet implemented !!!")
            candidates.head
          }
          val m = game.getMuslim(target)
          val (actives, sleepers) = if (m.totalCells <= 2)
            (m.activeCells, m.sleeperCells)
          else if (m.sleeperCells == 0)
            (2, 0)
          else {
            // We have 3 or more cells and at least one is a sleeper
            val sleepers = (m.sleeperCells - 1) min 2  // resereve 1 sleeper to flip to active
            (2 - sleepers, sleepers)
          }
          println()
          removeCellsFromCountry(target, actives, sleepers, addCadre = true)
          if (game.getMuslim(target).sleeperCells > 0)
            flipSleeperCells(target, 1)
        }
        // Also, decrease funding by 1 even if no cells affected.
        decreaseFunding(1)
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(137, "FMS", US, 2,
      NoRemove, NoMarker, NoLapsing, NoAutoTrigger,
      // Note: No militia allowed in Good countries!
      (_: Role) => game.militiaAvailable > 0 && (game hasMuslim (m => m.isAlly && !m.isGood))
      ,
      (role: Role) => {
        val candidates = countryNames(game.muslims filter (m => m.isAlly && !m.isGood))
        val target = if (role == game.humanRole)
          askCountry("Select country: ", candidates)
        else {
          log("!!! Bot event not yet implemented !!!")
          candidates.head
        }
        println()
        addMilitiaToCountry(target, game.militiaAvailable min 3)
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(138, "Intel Community", US, 2,
      NoRemove, NoMarker, NoLapsing, NoAutoTrigger,
      (role: Role) => role == game.humanRole  // The bot treats this as unplayable
      ,
      (role: Role) => {
        log("US player may inspect the Jihadist hand.")
        val cadres = countryNames(game.countries filter (_.hasCadre))
        if (cadres.isEmpty)
          log("No cadres on the map to remove")
        else 
          removeCadre(askCountry("Select country with cadre: ", cadres))
        
        // US player conducts a 1 Op operations.
        println()
        log("US player conducts an operation with 1 Op")
        humanExecuteOperation(1)
        println()
        log("US player may now play an extra card in this action phase")
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(139, "Int'l Banking Regime", US, 2,
      NoRemove, NoMarker, NoLapsing, NoAutoTrigger, AlwaysPlayable,
      (role: Role) => {
        val die = dieRoll
        log(s"Die roll: $die")
        println()
        decreaseFunding((die + 1) / 2)  // Half rounded up
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(140, "Maersk Alabama", US, 2,
      Remove, GlobalMarker, NoLapsing, NoAutoTrigger, AlwaysPlayable,
      (role: Role) => {
        if (game.reserves.us < 2) {
          game = game.copy(reserves = game.reserves.copy(us = game.reserves.us + 1))
          log(s"Add 1 Ops value to US reserves")
        }
        else
          log(s"US reserves are already at max of 2 Ops")
        println()
        increasePrestige(1)
        addGlobalEventMarker("Maersk Alabama")
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(141, "Malala Yousafzai", US, 2,
      Remove, NoMarker, NoLapsing, NoAutoTrigger,
      (_: Role) => globalEventNotInPlay("3 Cups of Tea") &&  // If not blocked and can have at least one effect
        (game.funding > 1 || game.prestige < 12 || game.getMuslim(Pakistan).canTakeAwakeningOrReactionMarker)
      ,
      (role: Role) => {
        println()
        increasePrestige(1)
        decreaseFunding(1)
        testCountry(Pakistan)
        addAwakeningMarker(Pakistan)
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(142, "Militia", US, 2,
      NoRemove, NoMarker, NoLapsing, NoAutoTrigger,
      (_: Role) => game hasMuslim (m => (m.civilWar || m.inRegimeChange) && game.militiaAvailable > 0)
      ,
      (role: Role) => {
        val candidates = countryNames(game.muslims filter (m => m.civilWar || m.inRegimeChange))
        val target = if (role == game.humanRole)
          askCountry("Place militia in which country: ", candidates)
        else {
          log("!!! Bot event not yet implemented !!!")
          candidates.head
        }
        println()
        addMilitiaToCountry(target, game.getMuslim(target).resources min game.militiaAvailable)
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(143, "Obama Doctrine", US, 2,
      NoRemove, NoMarker, NoLapsing, NoAutoTrigger,
      (_: Role) => game.usPosture == Soft
      ,
      (role: Role) => {
        if (role == game.humanRole) {
          def item(test: Boolean, x: (String, String)) = if (test) Some(x) else None
          val canAwakening = game hasMuslim (_.canTakeAwakeningOrReactionMarker)
          val canAid       = game hasMuslim (_.canTakeAidMarker)
          val items = List(
            item(canAwakening,       "awakening" -> "Place 1 Awakening marker"),
            item(canAid,             "aid"       -> "Place 1 Aid marker"),
            item(game.prestige < 12, "prestige"  -> "+1 Prestige"),
            item(game.funding > 1,   "funding"   -> "-1 Funding"),
            item(true,               "posture"   -> "Select posture of 1 Schengen country"),
            item(true,               "draw"      -> "Select Reaper, Operation New Dawn, or Advisors from discard pile.")
          ).flatten 
          println("Do any 2 of the following:")
          askMenu(ListMap(items:_*), 2, repeatsOK = false) foreach { action =>
            println()
            action match {
              case "awakening" =>
                val target = askCountry("Place awakening marker in which country: ",
                             countryNames(game.muslims filter (_.canTakeAwakeningOrReactionMarker)))
                testCountry(target)
                addAwakeningMarker(target)
              case "aid" =>
                val target = askCountry("Place aid marker in which country: ",
                             countryNames(game.muslims filter (_.canTakeAidMarker)))
                testCountry(target)
                addAidMarker(target)
              case "prestige" => increasePrestige(1)
              case "funding"  => decreaseFunding(1)
              case "posture" =>
                setCountryPosture(
                  askCountry("Select posture of which Schengen country: ", Schengen),
                  askOneOf("New posture (Soft or Hard): ", Seq(Soft, Hard), allowAbort = true).get)
              case _ =>
                log("Select Reaper, Operation New Dawn, or Advisors from discard pile")
            }
          }
        }
        else {
          log("!!! Bot event not yet implemented !!!")
        }
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(144, "Operation New Dawn", US, 2,
      NoRemove, NoMarker, NoLapsing, NoAutoTrigger,
      (_: Role) => game.troopsOnMap > 0 && game.militiaAvailable > 0
      ,
      (role: Role) => {
        val candidates = countryNames(game.muslims filter (_.troops > 0))
        val target = if (role == game.humanRole)
          askCountry("Replace troops in which country: ", candidates)
        else {
          log("!!! Bot event not yet implemented !!!")
          candidates.head
        }
        val num = 2 min game.getMuslim(target).troops min game.militiaAvailable
        println()
        takeTroopsOffMap(target, num)
        addMilitiaToCountry(target, num)      
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(145, "Russian Aid", US, 2,
      NoRemove, NoMarker, NoLapsing, NoAutoTrigger,
      (_: Role) => game hasMuslim (_.civilWar)
      ,
      (role: Role) => {
        val candidates = countryNames(game.muslims filter (_.civilWar))
        val target = if (role == game.humanRole)
          askCountry("Place militia and aid in which country: ", candidates)
        else {
          log("!!! Bot event not yet implemented !!!")
          candidates.head
        }
        println()
        addMilitiaToCountry(target, 1 min game.militiaAvailable)
        addAidMarker(target)
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(146, "Sharia", US, 2,
      NoRemove, NoMarker, NoLapsing, NoAutoTrigger,
      (_: Role) => game hasMuslim (_.canTakeAwakeningOrReactionMarker)
      ,
      (role: Role) => {
        // Get candidates in this priority order:
        // 1. Muslims with besieged regime markers that can take an awakening marker
        // 2. Muslims with besiged regime markers (cannot take awakening because of Civil War)
        // 3. Muslims that can take an awakening marker.
        val possibles = List(
          game.muslims filter (m => m.besiegedRegime && m.canTakeAwakeningOrReactionMarker),
          game.muslims filter (_.besiegedRegime),
          game.muslims filter (_.canTakeAwakeningOrReactionMarker)
        )
        val candidates = countryNames((possibles dropWhile (_.isEmpty)).head)
        val target = if (role == game.humanRole)
          askCountry("Select country: ", candidates)
        else {
          log("!!! Bot event not yet implemented !!!")
          candidates.head
        }
        println()
        testCountry(target)
        removeBesiegedRegimeMarker(target)
        if (game.getMuslim(target).canTakeAwakeningOrReactionMarker)
          addAwakeningMarker(target)
        else
          log(s"Cannot add an awakening marker to $target because it is in Civil War")
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(147, "Strike Eagle", US, 2,
      NoRemove, NoMarker, NoLapsing, NoAutoTrigger,
      (_: Role) => game hasMuslim strikeEagleCandidate
      ,
      (role: Role) => {
        val candidates = countryNames(game.muslims filter strikeEagleCandidate)
        val target = if (role == game.humanRole)
          askCountry("Select country: ", candidates)
        else {
          log("!!! Bot event not yet implemented !!!")
          candidates.head
        }
        println()
        removeCachedWMD(target, 1)
        if (game.cellsAvailable(ignoreFunding = true) > 0)
          addSleeperCellsToCountry(Israel, 1, ignoreFunding = true)
        increaseFunding(1)
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(148, "Tahrir Square", US, 2,
      NoRemove, NoMarker, NoLapsing, NoAutoTrigger,
      (role: Role) => (game getMuslim Egypt).canTakeAwakeningOrReactionMarker || (game hasMuslim tahrirCandidate)
      ,
      (role: Role) => {
        val candidates = countryNames(game.muslims filter tahrirCandidate)
        
        if (game.getMuslim(Egypt).canTakeAwakeningOrReactionMarker) {
          testCountry(Egypt)
          addAwakeningMarker(Egypt, 2)
          addReactionMarker(Egypt)
        }
        
        if (candidates.nonEmpty) {
          val target = if (role == game.humanRole)
            askCountry("Place 1 awakening marker in which country: ", candidates)
          else {
            log("!!! Bot event not yet implemented !!!")
            candidates.head
          }
          testCountry(target)
          addAwakeningMarker(target)
        }
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(149, "UN Nation Building", US, 2,
      NoRemove, NoMarker, NoLapsing, NoAutoTrigger,
      (role: Role) => game hasMuslim (m => m.inRegimeChange || m.civilWar)
      ,
      (role: Role) => {
        val candidates = countryNames(game.muslims filter (m => m.inRegimeChange || m.civilWar))
        val (target, die) = if (role == game.humanRole) {
          val t = askCountry("Select country: ", candidates)
          val d = if (game.getMuslim(t).warOfIdeasOK(3, ignoreRegimeChange = true)) 
            humanDieRoll("Enter War of Ideas die roll: ", allowAbort = true)
          else
            0
          (t, d)
        }
        else {
          log("!!! Bot event not yet implemented !!!")
          (candidates.head, dieRoll)
        }
        
        addAidMarker(target)
        if (game.getMuslim(target).warOfIdeasOK(3, ignoreRegimeChange = true))
          performWarOfIdeas(target, die, ignoreGwotPenalty = true)
        else
          log(s"$target does not meet the requirements for War of Ideas")
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(150, "UNSCR 1973", US, 2,
      NoRemove, CountryMarker, NoLapsing, NoAutoTrigger,
      (role: Role) => game hasMuslim unscr1973Candidate
      ,
      (role: Role) => {
        val candidates = countryNames(game.muslims filter unscr1973Candidate)
        val target = if (role == game.humanRole)
          askCountry("Place UNSCR 1973 in which country: ", candidates)
        else {
          log("!!! Bot event not yet implemented !!!")
          candidates.head
        }
        val m = game.getMuslim(target)
        // If the target already contains the marker, then
        // we only remove a cell.
        val sameCountry = m.hasMarker("UNSCR 1973")

        if (!sameCountry) {
          // If marker is already on the map, remove it first.
          game.muslims find (_.hasMarker("UNSCR 1973")) foreach { c =>
            removeEventMarkersFromCountry(c.name, "UNSCR 1973")
          }
        }

        if (m.totalCells > 0) {
          val (actives, sleepers) = if (role == game.humanRole)
            askCells(target, 1, sleeperFocus = true)
          else {
            log("!!! Bot event not yet implemented !!!")
            if (m.sleeperCells > 0) (0, 1) else (1, 0)
          }
          removeCellsFromCountry(target, actives, sleepers, addCadre = true)
        }

        if (!sameCountry)
          addEventMarkersToCountry(target, "UNSCR 1973")
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(151, "UNSCR 2118", US, 2,
      Remove, NoMarker, NoLapsing, NoAutoTrigger,
      (role: Role) => {
        val syria = game.getMuslim(Syria)
        !syria.isUntested && syria.wmdCache > 0
      }
      ,
      (role: Role) => {
        val syria = game.getMuslim(Syria)
        removeCachedWMD(Syria, if (syria.isAlly) syria.wmdCache else 1)
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(152, "Congress Acts", US, 3,
      NoRemove, NoMarker, NoLapsing, NoAutoTrigger,
      (role: Role) => globalEventInPlay("Sequestration") || (
        game.troopsAvailable >= 6 && (game hasMuslim (m => m.civilWar && !m.inRegimeChange))
      )
      ,
      (role: Role) => {
        removeGlobalEventMarker("Sequestration")
        val candidates = countryNames(game.muslims filter (m => m.civilWar && !m.inRegimeChange))
        if (game.troopsAvailable >= 6 && candidates.nonEmpty) {
          val (target, numTroops) = if (role == game.humanRole) {
            val t = askCountry("Select country: ", candidates)
            val n = askInt("Deploy how many troops from the track: ", 6, game.troopsAvailable, Some(6))
            (t, n)
          }
          else {
            log("!!! Bot event not yet implemented !!!")
            (candidates.head, 6)
          }
          performRegimeChange("track", target, numTroops)
          endCivilWar(target)
        }
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(153, "Facebook", US, 3,
      NoRemove, NoMarker, NoLapsing, NoAutoTrigger,
      (role: Role) => globalEventInPlay("Smartphones") && faceBookCandidates.nonEmpty
      ,
      (role: Role) => {
        val candidates = faceBookCandidates
        val targets = if (candidates.size < 4)
          candidates
        else if (role == game.humanRole) {
          // Choose three targets
          def nextTarget(num: Int, possibles: List[String]): List[String] = {
            if (num > 3) Nil
            else {
              val target = askCountry(s"${ordinal(num)} country: ", possibles)
              target :: nextTarget(num + 1, possibles filterNot(_ == target))
            }
          }
          nextTarget(1, candidates)
        }
        else {
          log("!!! Bot event not yet implemented !!!")
          candidates take 3
        }
        targets foreach (target => addAwakeningMarker(target))
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(154, "Facebook", US, 3,
      NoRemove, NoMarker, NoLapsing, NoAutoTrigger,
      (role: Role) => cardMap(153).eventConditions(role),
      (role: Role) => cardMap(153).executeEvent(role)
    )),
    // ------------------------------------------------------------------------
    entry(new Card(155, "Fracking", US, 3,
      NoRemove, GlobalMarker, NoLapsing, NoAutoTrigger, AlwaysPlayable,
      (role: Role) => {
        addGlobalEventMarker("Fracking")
        rollPrestige()
        log("US player draws a card.")
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(156, "Gulf Union", US, 3,
      NoRemove, NoMarker, NoLapsing, NoAutoTrigger,
      (role: Role) => game.militiaAvailable > 0 || 
             (GulfUnionCountries exists (name => game.getMuslim(name).militia > 0))
      ,
      (role: Role) => {
        // Card allow placing militia or respositioning militia in the Gulf Union countries.
        val existingMilitia = (GulfUnionCountries exists (name => game.getMuslim(name).militia > 0))
        val actions = List(
          if (game.militiaAvailable > 0) Some("place")      else None,
          if (existingMilitia)           Some("reposition") else None
        ).flatten
        val choices = ListMap(
          "place"      -> "Place up to 4 militia in one Gulf Union (or adjacent) country",
          "reposition" -> "Repositon militia in Gulf Union countries")
        val maxMilitia = 4 min game.militiaAvailable
        val placeCandidates = (GulfUnionCountries.foldLeft(Set.empty[String]) { (candidates, name) =>
          candidates + name ++ (getAdjacent(name) filter (n => game.isMuslim(n) && n != Iran))
        }).toList.sorted
          
        if (role == game.humanRole) {
          val action = if (actions.size == 1) actions.head 
          else askMenu(choices).head
          action match {
            case "place" =>
              val target = askCountry("Place militia in which country: ", placeCandidates)
              val num    = askInt(s"Place how many militia in $target", 1, maxMilitia, Some(maxMilitia))
              testCountry(target)
              addMilitiaToCountry(target, num)
              
            case _ => // Reposition militia with the Gulf Union countries
              val totalMilitia = GulfUnionCountries.foldLeft(0) { (sum, name) =>
                sum + game.getMuslim(name).militia
              }
              println(s"There are a total of $totalMilitia militia in the Gulf Union countries.")
              println("Assume that we start by taking all of those militia off of the map.")
              println("You will be prompted with the name of each Gulf Union country one by one.")
              println("Specify the number of militia that you would like in each country:")
              case class GulfUnion(name: String, newMilitia: Int) {
                val muslim = game.getMuslim(name)
                def hasLess = newMilitia < muslim.militia
                def noChange = newMilitia == muslim.militia
              }
              def nextCountry(members: List[String], remaining: Int): List[GulfUnion] = {
                members match {
                  case Nil                     => Nil
                  case x::Nil                  => GulfUnion(x, remaining) :: Nil
                  case x::xs if remaining == 0 => GulfUnion(x, 0) :: nextCountry(xs, 0)
                  case x::xs                   =>
                    val num = askInt(s"How many militia in $x", 0, remaining)
                    GulfUnion(x, num) :: nextCountry(xs, remaining - num)
                }
              }
              val placements = nextCountry(GulfUnionCountries, maxMilitia) filterNot (_.noChange)
              if (placements.isEmpty)
                log("No change to the position of the existing militia in the Gulf Union")
              else {
                for (p <- placements; if p.hasLess) {
                  testCountry(p.name)
                  val m = game.getMuslim(p.name) // Get fresh copy after testCountry()
                  game = game.updateCountry(m.copy(militia = p.newMilitia))
                  log(s"Remove ${p.muslim.militia - p.newMilitia} militia from ${p.name}")
                }
                for (p <- placements; if !p.hasLess) {
                  testCountry(p.name)
                  val m = game.getMuslim(p.name) // Get fresh copy after testCountry()
                  game = game.updateCountry(m.copy(militia = p.newMilitia))
                  log(s"Add ${p.newMilitia - p.muslim.militia} militia to ${p.name}")
                }
              }
          } 
        }
        else {
          log("!!! Bot event not yet implemented !!!")
          val target = shuffle(GulfUnionCountries).head
          testCountry(target)
          addMilitiaToCountry(target, maxMilitia)
        }
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(157, "Limited Deployment", US, 3,
      NoRemove, NoMarker, NoLapsing, NoAutoTrigger,
      (role: Role) => game.usPosture == Hard && (game hasMuslim (_.civilWar))
      ,
      (role: Role) => {
        val candidates = countryNames(game.muslims filter (_.civilWar))
        val (target, adjacent) = if (role == game.humanRole) {
          val t = askCountry("Select country: ", candidates)
          val adjacents = getAdjacentMuslims(t) filter (n => game.getMuslim(n).canTakeAwakeningOrReactionMarker)
          if (adjacents.nonEmpty)
            (t, Some(askCountry("Place awakening marker in which adjacent country: ", adjacents)))
          else
            (t, None)
        }
        else {
          log("!!! Bot event not yet implemented !!!")
          val t = candidates.head
          val a = (getAdjacentMuslims(t) filter (n => game.getMuslim(n).canTakeAwakeningOrReactionMarker)).headOption
          (t, a)
        }
        
        moveTroops("track", target, 2 min game.troopsAvailable)
        addAidMarker(target)
        adjacent foreach { m => addAwakeningMarker(m) }
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(158, "Mass Turnout", US, 3,
      NoRemove, NoMarker, NoLapsing, NoAutoTrigger,
      (role: Role) => game hasMuslim massTurnoutCandidate
      ,
      (role: Role) => {
        val candidates = countryNames(game.muslims filter massTurnoutCandidate)
        val target = if (role == game.humanRole)
          askCountry("Select regime change country: ", candidates)
        else {
          log("!!! Bot event not yet implemented !!!")
          candidates.head
        }
        improveGovernance(target)
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(159, "NATO", US, 3,
      NoRemove, CountryMarker, NoLapsing, NoAutoTrigger,
      (role: Role) => game.gwotPenalty == 0 && (game hasMuslim (m => m.inRegimeChange || m.civilWar))
      ,
      (role: Role) => {
        val candidates = countryNames(game.muslims filter (m => m.inRegimeChange || m.civilWar))
        val target = if (role == game.humanRole)
          askCountry("Select country for NATO: ", candidates)
        else {
          log("!!! Bot event not yet implemented !!!")
          candidates.head
        }
        
        addAidMarker(target)
        (game.muslims find (_.hasMarker("NATO")) map (_.name)) match {
          case Some(`target`) =>
            log(s"NATO marker remains in $target")
          case Some(current) =>
            removeEventMarkersFromCountry(current, "NATO")
            addEventMarkersToCountry(target, "NATO")
          case None =>
            addEventMarkersToCountry(target, "NATO")
        }
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(160, "Operation Neptune Spear", US, 3,
      NoRemove, NoMarker, NoLapsing, NoAutoTrigger, AlwaysPlayable,
      (role: Role) => {
        val cards = List("Ayman al-Zawahiri", "Abu Bakr al-Baghdadi", "Abu Sayyaf (ISIL)",
                         "Jihadi John", "Osama bin Ladin")
        log("US player takes one of the following cards from the discard pile:")
        log(orList(cards))
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(161, "PRISM", US, 3,
      NoRemove, NoMarker, NoLapsing, NoAutoTrigger, AlwaysPlayable,
      (role: Role) => {
        val actions = List(
          if (game.sleeperCellsOnMap > 0) Some("activate") else None,
          if (game.alertTargets.nonEmpty) Some("alert") else None
        ).flatten
        val choices = ListMap(
          "activate" -> "Activate half (rounded up) of all sleeper cells on the map",
          "alert"    -> "Alert all plots on the map"
        )
          
        if (role == game.humanRole) {
          if (actions.nonEmpty) {
            val action = if (actions.size == 1)
              actions.head
            else 
              askMenu(choices).head

            if (action == "activate") {
              val numToFlip = (game.sleeperCellsOnMap + 1) / 2  // half rounded up
              // Ask which cells to activate
              // If all sleepers on the map are in a single country then, don't bother.
              case class WithSleepers(name: String, num: Int)
              val withSleepers = for (c <- game.countries; if c.sleeperCells > 0)
                yield WithSleepers(c.name, c.sleeperCells)
              
              def nextFlip(targets: List[WithSleepers], mustFlip: Int): List[WithSleepers] = {
                targets match {
                  case _  if mustFlip == 0 => Nil  // The required number have been flipped
                  case t::Nil              => WithSleepers(t.name, mustFlip) :: Nil // Last country remaining
                  case _ =>
                    val maxName = (targets map (_.name.length)).max
                    val fmt = "%%-%ds  (%%s)".format(maxName)
                    def disp(t: WithSleepers) = fmt.format(t.name, amountOf(t.num, "sleeper cell"))
                    val opts = targets map (t => t.name -> disp(t))
                    val choices = ListMap(opts:_*)
                    println()
                    println(s"${amountOf(mustFlip, "activation")} remaining, choose country")
                    // println("Choose next country to activate sleeper cells")
                    val name  = askMenu(choices).head
                    val limit = (targets find (_.name == name)).get.num min mustFlip
                    val num   = askInt("Activate how many", 1, limit)
                    val newTargets = targets map { t => if (t.name == name) t.copy(num = t.num - num) else t }
                    WithSleepers(name, num) :: nextFlip(newTargets filterNot (_.num == 0), mustFlip - num)
                }
              }
              
              println(s"Activate a total of ${amountOf(numToFlip, "sleeper cell")}")
              val toFlip = nextFlip(withSleepers.sortBy(_.name), numToFlip)
                // It is possible that the same country was chosen multiple times
                // Consolidate them so we do 1 flip per country.
              def flipNextBatch(remaining: List[WithSleepers]): Unit = {
                if (remaining.nonEmpty) {
                  val name = remaining.head.name
                  val (batch, next) = remaining.partition(_.name == name)
                  val total = batch.foldLeft(0) { (sum, x) => sum + x.num }
                  flipSleeperCells(name, total)
                  flipNextBatch(next)
                }
              }
              flipNextBatch(toFlip.sortBy(_.name))
            }
            else
              for (c <- game.countries; p <- c.plots) // Alert all plots on the map
                performAlert(c.name)
          }
          // Even of neither action above was possible.
          decreasePrestige(1)
        }
        else {
          log("!!! Bot event not yet implemented !!!")
        }
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(162, "SCAF", US, 3,
      NoRemove, NoMarker, NoLapsing, NoAutoTrigger,
      (role: Role) => game hasMuslim scafCandidate
      ,
      (role: Role) => {
        val candidates = countryNames(game.muslims filter scafCandidate)
        val target = if (role == game.humanRole)
          askCountry("Select country: ", candidates)
        else {
          log("!!! Bot event not yet implemented !!!")
          candidates.head
        }
        val m = game.getMuslim(target)
        shiftAlignment(target, Ally)
        if (m.isPoor)
          improveGovernance(target, 1)
        removeCellsFromCountry(target, m.activeCells, m.sleeperCells, addCadre = true)
        addReactionMarker(target, m.totalCells)
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(163, "Status Quo", US, 3,
      NoRemove, NoMarker, NoLapsing, NoAutoTrigger,
      (role: Role) => game hasMuslim statusQuoCandidate
      ,  
      (role: Role) => {
        val candidates = countryNames(game.muslims filter statusQuoCandidate)
        val target = if (role == game.humanRole)
          askCountry("Select country: ", candidates)
        else {
          log("!!! Bot event not yet implemented !!!")
          candidates.head
        }
        removeRegimeChangeMarker(target)
        shiftAlignment(target, Ally)
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(164, "Bloody Thursday", Jihadist, 1,
      NoRemove, GlobalMarker, NoLapsing, NoAutoTrigger, AlwaysPlayable,
      (role: Role) => {
        addGlobalEventMarker("Bloody Thursday")
        val candidates = countryNames(game.muslims filter (_.awakening > 0))
        if (candidates.nonEmpty) {
          val target = if (role == game.humanRole)
            askCountry("Select country with awakening marker: ", candidates)
          else {
            log("!!! Bot event not yet implemented !!!")
            candidates.head
          }
          removeAwakeningMarker(target)
        }
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(165, "Coup", Jihadist, 1,
      NoRemove, NoMarker, NoLapsing, NoAutoTrigger,
      (role: Role) => game hasMuslim coupCandidate
      ,
      (role: Role) => {
        val candidates = countryNames(game.muslims filter coupCandidate)
        val target = if (role == game.humanRole)
          askCountry("Select country: ", candidates)
        else {
          log("!!! Bot event not yet implemented !!!")
          candidates.head
        }
        startCivilWar(target)
        addBesiegedRegimeMarker(target)
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(166, "Ferguson", Jihadist, 1,
      NoRemove, NoMarker, Lapsing, NoAutoTrigger, AlwaysPlayable,
      (role: Role) => {
        log("Jihadist play my block 1 US Associated event played later this turn.")
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(167, "Houthi Rebels", Jihadist, 1,
      Remove, NoMarker, NoLapsing, NoAutoTrigger,
      (role: Role) => {
        game.getMuslim(Yemen).isPoor &&
        (game.isNonMuslim(Iran) || !game.getMuslim(Iran).isAlly) &&
        !(game.getMuslim(Yemen).civilWar && game.cellsAvailable == 0)
      }
      ,
      (role: Role) => {
        addSleeperCellsToCountry(Yemen, 2 min game.cellsAvailable)
        startCivilWar(Yemen)
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(168, "IEDs", Jihadist, 1,
      NoRemove, NoMarker, NoLapsing, NoAutoTrigger,
      (role: Role) => game hasMuslim (m =>
        (m.inRegimeChange || m.civilWar) && m.totalCells > 0 && m.totalTroops > 0
      )
      ,  
      (role: Role) => {
        log("US player randomly discards one card")
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(169, "Islamic Maghreb", Jihadist, 1,
      NoRemove, NoMarker, Lapsing, NoAutoTrigger,
      (role: Role) => (game hasMuslim islamicMaghrebCandidate) &&
                      (game.funding < 9 || game.cellsAvailable > 0)
      ,  
      (role: Role) => {
        val candidates = countryNames(game.muslims filter islamicMaghrebCandidate)
        val target = if (role == game.humanRole)
          askCountry("Select country: ", candidates)
        else {
          log("!!! Bot event not yet implemented !!!")
          candidates.head
        }
        
        val num = if (game.getMuslim(target).civilWar || game.isCaliphateMember(target)) 2 else 1
        addSleeperCellsToCountry(target, num min game.cellsAvailable)
        increaseFunding(num)
        rollCountryPosture(Serbia)
        log("Travel to/within Schengen countries requires a roll for the rest this turn")
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(170, "Theft of State", Jihadist, 1,
      NoRemove, NoMarker, NoLapsing, NoAutoTrigger,
      (role: Role) => game hasMuslim theftOfStateCandidate
      ,
      (role: Role) => {
        val candidates = countryNames(game.muslims filter theftOfStateCandidate)
        val target = if (role == game.humanRole)
          askCountry("Select country: ", candidates)
        else {
          log("!!! Bot event not yet implemented !!!")
          candidates.head
        }
        removeAwakeningMarker(target)
        addReactionMarker(target)
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(171, "Abu Ghraib Jail Break", Jihadist, 2,
      Remove, NoMarker, NoLapsing, NoAutoTrigger, AlwaysPlayable,
      (role: Role) => {
        testCountry(Iraq)
        addActiveCellsToCountry(Iraq, 1 min game.cellsAvailable)
        addReactionMarker(Iraq)
        decreasePrestige(1)
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(172, "Al-Shabaab", Jihadist, 2,
      NoRemove, NoMarker, NoLapsing, NoAutoTrigger, AlwaysPlayable,
      (role: Role) => {
        if (role == game.humanRole) {
          val candidates = Somalia :: getAdjacent(Somalia).sorted
          val reactionCandidates = candidates filter game.isMuslim filter { name =>
            game.getMuslim(name).canTakeAwakeningOrReactionMarker
          }
          val besiegeCandidates = candidates filter game.isMuslim filter { name =>
            !(game.getMuslim(name).canTakeAwakeningOrReactionMarker ||
              game.getMuslim(name).besiegedRegime)
          }
          val canReaction = reactionCandidates.nonEmpty
          val canCell     = game.cellsAvailable > 0
          val canPlot1    = game.availablePlots exists (_ == Plot1)
          val canPlot2    = game.availablePlots exists (_ == Plot2)
          val canBesiege  = besiegeCandidates.nonEmpty
          def item(test: Boolean, x: (String, String)) = if (test) Some(x) else None
          val items = List(
            item(canReaction,"reaction" -> "Place 1 Reaction marker"),
            item(canCell,    "cell"     -> "Place 1 cell"),
            item(canPlot1,   "plot1"    -> "Place a level 1 plot"),
            item(canPlot2,   "plot2"    -> "Place a level 2 plot"),
            item(canBesiege, "besiege"  -> "Place a besieged regime marker"),
            item(true,       "draw"     -> "Select Pirates, Boko Haram, or Islamic Maghreb from discard pile")
          ).flatten 
          println("Do any 2 of the following:")
          askMenu(ListMap(items:_*), 2, repeatsOK = false) foreach { action =>
            println()
            action match {
              case "reaction" =>
                val target = askCountry("Place reaction marker in which country: ", reactionCandidates)
                testCountry(target)
                addReactionMarker(target)
              case "cell" =>
                val target = askCountry("Place a cell in which country: ", candidates)
                testCountry(target)
                addSleeperCellsToCountry(target, 1)
              case "plot1" =>
                val target = askCountry("Place a level 1 plot in which country: ", candidates)
                testCountry(target)
                addAvailablePlotToCountry(target, Plot1)
              case "plot2" =>
                val target = askCountry("Place a level 2 plot in which country: ", candidates)
                testCountry(target)
                addAvailablePlotToCountry(target, Plot2)
              case "besiege"  =>
              val target = askCountry("Place besieged regime marker in which country: ", besiegeCandidates)
              testCountry(target)
              addBesiegedRegimeMarker(target)
              case _ =>
                log("Select Pirates, Boko Haram, or Islamic Maghreb from discard pile")
            }
          }
        }
        else {
          log("!!! Bot event not yet implemented !!!")
        }
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(173, "Arab Winter", Jihadist, 2,
      NoRemove, NoMarker, Lapsing, NoAutoTrigger, AlwaysPlayable,
      (role: Role) => {
        val candidates = countryNames(game.muslims filter (_.awakening > 0))
        if (candidates.isEmpty)
          log(s"There are no reaction markers on the map")
        else {
          val targets = if (candidates.size == 1)
            candidates
          else if (role == game.humanRole) {
            val target1 = askCountry("Select first country: ", candidates)
            val target2 = askCountry("Select second country: ", candidates filterNot (_ == target1))
            (target1::target2::Nil)
          }
          else {
            log("!!! Bot event not yet implemented !!!")
            candidates take 2
          }
          targets foreach (removeAwakeningMarker(_))
        }
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(174, "Boston Marathon", Jihadist, 2,
      Remove, NoMarker, NoLapsing, NoAutoTrigger, AlwaysPlayable,
      (role: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(175, "Censorship", Jihadist, 2,
      NoRemove, NoMarker, NoLapsing, NoAutoTrigger, AlwaysPlayable,
      (role: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(176, "Change of State", Jihadist, 2,
      NoRemove, NoMarker, NoLapsing, NoAutoTrigger, AlwaysPlayable,
      (role: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(177, "Gaza Rockets", Jihadist, 2,
      NoRemove, NoMarker, NoLapsing, NoAutoTrigger, AlwaysPlayable,
      (role: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(178, "Ghost Soldiers", Jihadist, 2,
      NoRemove, NoMarker, NoLapsing, NoAutoTrigger, AlwaysPlayable,
      (role: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(179, "Korean Crisis", Jihadist, 2,
      NoRemove, NoMarker, Lapsing, NoAutoTrigger, AlwaysPlayable,
      (role: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(180, "Mosul Central Bank", Jihadist, 2,
      Remove, NoMarker, NoLapsing, NoAutoTrigger, AlwaysPlayable,
      (role: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(181, "NPT Safeguards Ignored", Jihadist, 2,
      NoRemove, NoMarker, NoLapsing, NoAutoTrigger, AlwaysPlayable,  
      (role: Role) => {
        // Note: Remove on die roll of 1-3
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(182, "Paris Attacks", Jihadist, 2,
      NoRemove, NoMarker, NoLapsing, NoAutoTrigger, AlwaysPlayable,
      (role: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(183, "Pirates", Jihadist, 2,
      Remove, GlobalMarker, NoLapsing, NoAutoTrigger,
      (role: Role) => globalEventNotInPlay("Maersk Alabama")
      ,
      (role: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(184, "Sequestration", Jihadist, 2,
      Remove, GlobalMarker, NoLapsing, NoAutoTrigger, AlwaysPlayable,
      (role: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(185, "al-Maliki", Jihadist, 3,
      Remove, NoMarker, NoLapsing, NoAutoTrigger, AlwaysPlayable,
      (role: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(186, "Boko Haram", Jihadist, 3,
      NoRemove, NoMarker, NoLapsing, NoAutoTrigger, AlwaysPlayable,
      (role: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(187, "Foreign Fighters", Jihadist, 3,
      NoRemove, NoMarker, NoLapsing, NoAutoTrigger, AlwaysPlayable,
      (role: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(188, "ISIL", Jihadist, 3,
      Remove, NoMarker, NoLapsing, NoAutoTrigger, AlwaysPlayable,
      (role: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(189, "Jihadist Videos", Jihadist, 3,
      NoRemove, NoMarker, NoLapsing, NoAutoTrigger, AlwaysPlayable,
      (role: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(190, "Martyrdom Operation", Jihadist, 3,
      NoRemove, NoMarker, NoLapsing, NoAutoTrigger, AlwaysPlayable,
      (role: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(191, "Muslim Brotherhood", Jihadist, 3,
      Remove, NoMarker, NoLapsing, NoAutoTrigger, AlwaysPlayable,
      (role: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(192, "Quagmire", Jihadist, 3,
      NoRemove, NoMarker, NoLapsing, NoAutoTrigger, AlwaysPlayable,
      (role: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(193, "Regional al-Qaeda", Jihadist, 3,
      NoRemove, NoMarker, NoLapsing, NoAutoTrigger, AlwaysPlayable,
      (role: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(194, "Snowden", Jihadist, 3,
      Remove, NoMarker, NoLapsing, NoAutoTrigger, AlwaysPlayable,
      (role: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(195, "Taliban Resurgent", Jihadist, 3,
      NoRemove, NoMarker, NoLapsing, NoAutoTrigger, AlwaysPlayable,
      (role: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(196, "Training Camps", Jihadist, 3,
      NoRemove, CountryMarker, NoLapsing, NoAutoTrigger,
      (_: Role) => game hasMuslim (m => !(m.isUntested || m.isGood))
      ,
      (_: Role) => {
        val candidates = countryNames(game.muslims filter (m => !(m.isUntested || m.isGood)))
        val target = if (game.humanRole == Jihadist)
          askCountry("Place Training Camps in which country: ", candidates)
        else {
          log("!!! Bot event not yet implemented !!!")
          candidates.head
        }
        println()
        val priorCapacity = game.trainingCampCapacity
        game.trainingCamp foreach { name => 
          removeEventMarkersFromCountry(name, "Training Camps")
        }
        addEventMarkersToCountry(target, "Training Camps")
        updateTrainingCampCapacity(priorCapacity)
        val cellsToAdd = game.cellsAvailable(ignoreFunding = true) min 2
        addSleeperCellsToCountry(target, cellsToAdd, ignoreFunding = true)
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(197, "Unconfirmed", Jihadist, 3,
      Remove, NoMarker, NoLapsing, NoAutoTrigger, AlwaysPlayable,
      (role: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(198, "US Atrocities", Jihadist, 3,
      NoRemove, NoMarker, NoLapsing, NoAutoTrigger, AlwaysPlayable,
      (role: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(199, "US Consulate Attacked", Jihadist, 3,
      NoRemove, NoMarker, Lapsing, NoAutoTrigger, AlwaysPlayable,
      (role: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(200, "Critical Middle", Unassociated, 1,
      NoRemove, NoMarker, NoLapsing, NoAutoTrigger, AlwaysPlayable,
      (role: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(201, "Cross Border Support", Unassociated, 1,
      NoRemove, NoMarker, NoLapsing, NoAutoTrigger, AlwaysPlayable,
      (role: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(202, "Cyber Warfare", Unassociated, 1,
      NoRemove, NoMarker, NoLapsing, NoAutoTrigger, AlwaysPlayable,
      (role: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(203, "Day of Rage", Unassociated, 1,
      Remove, NoMarker, NoLapsing, NoAutoTrigger, AlwaysPlayable,
      (role: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(205, "Erdoğan Effect", Unassociated, 1,
      NoRemove, NoMarker, NoLapsing, NoAutoTrigger, AlwaysPlayable,
      (role: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(204, "Ebola Scare", Unassociated, 1,
      Remove, NoMarker, USLapsing, NoAutoTrigger, AlwaysPlayable,
      (role: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(206, "Friday of Anger", Unassociated, 1,
      NoRemove, NoMarker, NoLapsing, NoAutoTrigger, AlwaysPlayable,
      (role: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(207, "JV / Copycat", Unassociated, 1,
      NoRemove, NoMarker, NoLapsing, NoAutoTrigger, AlwaysPlayable,
      (role: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(208, "Kinder – Gentler", Unassociated, 1,
      NoRemove, NoMarker, NoLapsing, NoAutoTrigger, AlwaysPlayable,
      (role: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(209, "Quds Force", Unassociated, 1,
      NoRemove, NoMarker, NoLapsing, NoAutoTrigger, AlwaysPlayable,
      (role: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(210, "Sectarian Violence", Unassociated, 1,
      NoRemove, NoMarker, NoLapsing, NoAutoTrigger, AlwaysPlayable,
      (role: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(211, "Smartphones", Unassociated, 1,
      NoRemove, GlobalMarker, NoLapsing, NoAutoTrigger, AlwaysPlayable,
      (role: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(212, "Smartphones", Unassociated, 1,
      NoRemove, GlobalMarker, NoLapsing, NoAutoTrigger, AlwaysPlayable,
      (role: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(213, "Smartphones", Unassociated, 1,
      NoRemove, GlobalMarker, NoLapsing, NoAutoTrigger, AlwaysPlayable,
      (role: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(214, "3 Cups of Tea", Unassociated, 2,
      NoRemove, GlobalMarker, NoLapsing, NoAutoTrigger, AlwaysPlayable,
      (role: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(215, "Abu Bakr al-Baghdadi", Unassociated, 2,
      USRemove, NoMarker, NoLapsing, NoAutoTrigger, AlwaysPlayable,
      (role: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(216, "Abu Sayyaf (ISIL)", Unassociated, 2,
      USRemove, NoMarker, NoLapsing, NoAutoTrigger, AlwaysPlayable,
      (role: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(217, "Agitators", Unassociated, 2,
      NoRemove, NoMarker, NoLapsing, NoAutoTrigger, AlwaysPlayable,
      (role: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(218, "Al-Nusra Front", Unassociated, 2,
      NoRemove, NoMarker, NoLapsing, NoAutoTrigger, AlwaysPlayable,
      (role: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(219, "Ayman al-Zawahiri", Unassociated, 2,
      USRemove, NoMarker, NoLapsing, NoAutoTrigger, AlwaysPlayable,
      (role: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(220, "Daraa", Unassociated, 2,
      Remove, NoMarker, NoLapsing, NoAutoTrigger, AlwaysPlayable,
      (role: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(221, "FlyPaper", Unassociated, 2,
      NoRemove, NoMarker, NoLapsing, NoAutoTrigger, AlwaysPlayable,
      (role: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(222, "Hagel", Unassociated, 2,
      Remove, NoMarker, NoLapsing, NoAutoTrigger, AlwaysPlayable,
      (role: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(223, "Iranian Elections", Unassociated, 2,
      Remove, NoMarker, NoLapsing, NoAutoTrigger, AlwaysPlayable,
      (role: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(224, "Je Suis Charlie", Unassociated, 2,
      NoRemove, NoMarker, NoLapsing, NoAutoTrigger, AlwaysPlayable,
      (role: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(225, "Jihadi John", Unassociated, 2,
      USRemove, NoMarker, NoLapsing, NoAutoTrigger, AlwaysPlayable,
      (role: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(226, "Operation Serval", Unassociated, 2,
      NoRemove, GlobalMarker, NoLapsing, NoAutoTrigger, AlwaysPlayable,
      (role: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(227, "Popular Support", Unassociated, 2,
      NoRemove, NoMarker, NoLapsing, NoAutoTrigger, AlwaysPlayable,
      (role: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(228, "Popular Support", Unassociated, 2,
      NoRemove, NoMarker, NoLapsing, NoAutoTrigger, AlwaysPlayable,
      (role: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(229, "Prisoner Exchange", Unassociated, 2,
      NoRemove, NoMarker, NoLapsing, NoAutoTrigger, AlwaysPlayable,
      (role: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(230, "Sellout", Unassociated, 2,
      NoRemove, NoMarker, NoLapsing, NoAutoTrigger, AlwaysPlayable,
      (role: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(231, "Siege of Kobanigrad", Unassociated, 2,
      NoRemove, NoMarker, NoLapsing, NoAutoTrigger, AlwaysPlayable,
      (role: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(232, "Trade Embargo", Unassociated, 2,
      USRemove, GlobalMarker, NoLapsing, NoAutoTrigger, AlwaysPlayable,
      (role: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(233, "UN Ceasefire", Unassociated, 2,
      NoRemove, NoMarker, NoLapsing, NoAutoTrigger, AlwaysPlayable,
      (role: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(234, "Free Syrian Army", Unassociated, 3,
      Remove, NoMarker, NoLapsing, NoAutoTrigger, AlwaysPlayable,
      (role: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(235, "Qadhafi", Unassociated, 3,
      NoRemove, NoMarker, NoLapsing, NoAutoTrigger, AlwaysPlayable,
      (role: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(236, "Oil Price Spike", Unassociated, 3,
      NoRemove, NoMarker, Lapsing, NoAutoTrigger, AlwaysPlayable,
      (role: Role) => {
        // Removes "Fracking" marker
      }
      
    )),
    // ------------------------------------------------------------------------
    entry(new Card(237, "Osama bin Ladin", Unassociated, 3,
      USRemove, NoMarker, NoLapsing, NoAutoTrigger, AlwaysPlayable,
      (role: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(238, "Revolution", Unassociated, 3,
      NoRemove, NoMarker, NoLapsing, NoAutoTrigger, AlwaysPlayable,
      (role: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(239, "Truce", Unassociated, 3,
      NoRemove, NoMarker, Lapsing, NoAutoTrigger, AlwaysPlayable,
      (role: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(240, "US Election", Unassociated, 3,
      NoRemove, NoMarker, NoLapsing, AutoTrigger,
      (_: Role) => false  // No directly playable, but will always auto trigger
      ,
      (role: Role) => ()
    ))
  )
}