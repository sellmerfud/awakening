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
  // Various tests used by the card events
  val canTakeAdvisors = (m: MuslimCountry) => !m.isAdversary && m.civilWar && m.troops == 0 && !m.hasMarker("Advisors")
  val canTakeHumanitarianAid = (m: MuslimCountry) => m.canTakeAidMarker && m.totalCells > 0
  val canTakePeshmerga = (m: MuslimCountry) => (m.name == Iraq || m.name == Syria) && m.totalCells > 0
  val canArabSpringFallout = (m: MuslimCountry) => {
    m.awakening == 0 &&
    m.canTakeAwakeningOrReactionMarker && 
    (game.adjacentMuslims(m.name) forall (_.awakening == 0))
  }
  val canTakeStrikeEagle = (m: MuslimCountry) => (m.name != Pakistan && m.isPoor && !m.isAlly && m.wmdCache > 0)
  
  
  def specialForcesTargets: List[String] = {
    // First find all muslim countries with troops or "Advisors"
    val withForces = (game.muslims filter (m => m.troops > 0 || m.hasMarker("Advisors"))) map (_.name)
    // Next get all countries that contain any cells and filter out the ones are are not 
    // within to of a country with forces.
    countryNames(game.countries filter { country =>
      country.totalCells > 0 && (withForces exists (forces => distance(forces, country.name) <= 2))
    })
  }
  
  val cardMap: Map[Int, Card] = Map(
    // ------------------------------------------------------------------------
    entry(new Card(121, "Advisors", US, 1,
      NoRemove, CountryMarker, NoLapsing, NoAutoTrigger,
      (_: Role) => (game.muslims count (_.hasMarker("Advisors"))) < 3 && (game hasMuslim canTakeAdvisors)
      ,
      (_: Role) => {
        val candidates = countryNames(game.muslims filter canTakeAdvisors)
        val target = if (game.humanRole == US)
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
      (_: Role) => {
        val candidates = countryNames(game.countries filter (_.plots exists (p => !p.backlashed)))
        val countryName = if (game.humanRole == US)
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
      (_: Role) => game hasMuslim canTakeHumanitarianAid
      ,
      (_: Role) => {
        val candidates = countryNames(game.muslims filter canTakeHumanitarianAid)
        val countryName = if (game.humanRole == US)
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
      (_: Role) => {
        println()
        testCountry(GulfStates)
        addAwakeningMarker(GulfStates)
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(125, "Peshmerga", US, 1,
      NoRemove, NoMarker, NoLapsing, NoAutoTrigger,
      (_: Role) => (game hasMuslim canTakePeshmerga) && game.militiaAvailable > 0
      ,
      (_: Role) => {
        val candidates = countryNames(game.muslims filter canTakePeshmerga)
        val target = if (game.humanRole == US)
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
      (_: Role) => {
        if (game.humanRole == US) {
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
      NoRemove, NoMarker, NoLapsing, NoAutoTrigger, AlwaysPlayable,
      (r: Role) => cardMap(126).executeEvent(r)
    )),
    // ------------------------------------------------------------------------
    entry(new Card(128, "Reaper", US, 1,
      NoRemove, NoMarker, NoLapsing, NoAutoTrigger, AlwaysPlayable, 
      (r: Role) => cardMap(126).executeEvent(r)
    )),
    // ------------------------------------------------------------------------
    entry(new Card(129, "Special Forces", US, 1,
      NoRemove, NoMarker, NoLapsing, NoAutoTrigger,
      (_: Role) => specialForcesTargets.nonEmpty
      ,
      (_: Role) => {
        if (game.humanRole == US) {
          val target = askCountry("Remove cell in which country: ", specialForcesTargets)
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
      (_: Role) => specialForcesTargets.nonEmpty
      ,
      (r: Role) => cardMap(129).executeEvent(r)
    )),
    // ------------------------------------------------------------------------
    entry(new Card(131, "Arab Spring Fallout", US, 2,
      NoRemove, NoMarker, NoLapsing, NoAutoTrigger,
      (_: Role) => game hasMuslim canArabSpringFallout
      ,
      (_: Role) => {
        val candidates = countryNames(game.muslims filter canArabSpringFallout)
        val targets = if (candidates.size <= 2)
          candidates
        else if (game.humanRole == US) {
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
      (_: Role) => {
        val candidates = countryNames(game.muslims filter (_.civilWar))
        val target = if (game.humanRole == US)
          askCountry("Select civil war country: ", candidates)
        else {
          log("!!! Bot event not yet implemented !!!")
          candidates.head
        }
        val (active, sleeper) = if (game.humanRole == US)
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
      (_: Role) => {
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
      (_: Role) => {
        val sunnis = countryNames(game.muslims filter (m=> m.isSunni && m.canTakeAwakeningOrReactionMarker))
        if (sunnis.nonEmpty) {  // This should never happen, but let's be defensive
          val sunniTarget = if (game.humanRole == US)
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
      (_: Role) => {
        if (game.humanRole == US) {
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
      (_: Role) => {
        val candidates = countryNames(game.muslims filter (m => !m.isIslamistRule && m.totalCells > 0))
        if (candidates.nonEmpty) {
          val target = if (game.humanRole == US)
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
      (_: Role) => {
        val candidates = countryNames(game.muslims filter (m => m.isAlly && !m.isGood))
        val target = if (game.humanRole == US)
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
      (role: Role) => game.humanRole == US  // The bot treats this as unplayable
      ,
      (_: Role) => {
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
      (_: Role) => {
        val die = dieRoll
        log(s"Die roll: $die")
        println()
        decreaseFunding((die + 1) / 2)  // Half rounded up
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(140, "Maersk Alabama", US, 2,
      Remove, GlobalMarker, NoLapsing, NoAutoTrigger, AlwaysPlayable,
      (_: Role) => {
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
      (_: Role) => {
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
      (_: Role) => {
        val candidates = countryNames(game.muslims filter (m => m.civilWar || m.inRegimeChange))
        val target = if (game.humanRole == US)
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
      (_: Role) => {
        if (game.humanRole == US) {
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
      (_: Role) => {
        val candidates = countryNames(game.muslims filter (_.troops > 0))
        val target = if (game.humanRole == US)
          askCountry("Replace trroops in which country: ", candidates)
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
      (_: Role) => {
        val candidates = countryNames(game.muslims filter (_.civilWar))
        val target = if (game.humanRole == US)
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
      (_: Role) => {
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
        val target = if (game.humanRole == US)
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
      (_: Role) => game hasMuslim canTakeStrikeEagle
      ,
      (_: Role) => {
        val candidates = countryNames(game.muslims filter canTakeStrikeEagle)
        val target = if (game.humanRole == US)
          askCountry("Select country: ", candidates)
        else {
          log("!!! Bot event not yet implemented !!!")
          candidates.head
        }
        println()
        removeCachedWMD(target)
        if (game.cellsAvailable(ignoreFunding = true) > 0)
          addSleeperCellsToCountry(Israel, 1, ignoreFunding = true)
        increaseFunding(1)
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(148, "Tahrir Square", US, 2,
      NoRemove, NoMarker, NoLapsing, NoAutoTrigger, AlwaysPlayable,
      (_: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(149, "UN Nation Building", US, 2,
      NoRemove, NoMarker, NoLapsing, NoAutoTrigger, AlwaysPlayable,
      (_: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(150, "UNSCR 1973", US, 2,
      NoRemove, CountryMarker, NoLapsing, NoAutoTrigger, AlwaysPlayable,
      (_: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(151, "UNSCR 2118", US, 2,
      Remove, NoMarker, NoLapsing, NoAutoTrigger, AlwaysPlayable,
      (_: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(152, "Congress Acts", US, 3,
      NoRemove, NoMarker, NoLapsing, NoAutoTrigger, AlwaysPlayable,
      (_: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(153, "Facebook", US, 3,
      NoRemove, NoMarker, NoLapsing, NoAutoTrigger,
      (_: Role) => globalEventInPlay("Smartphones")
      ,
      (_: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(154, "Facebook", US, 3,
      NoRemove, NoMarker, NoLapsing, NoAutoTrigger,
      (_: Role) => globalEventInPlay("Smartphones")
      ,
      (_: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(155, "Fracking", US, 3,
      NoRemove, GlobalMarker, NoLapsing, NoAutoTrigger, AlwaysPlayable,
      (_: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(156, "Gulf Union", US, 3,
      NoRemove, NoMarker, NoLapsing, NoAutoTrigger, AlwaysPlayable,
      (_: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(157, "Limited Deployment", US, 3,
      NoRemove, NoMarker, NoLapsing, NoAutoTrigger, AlwaysPlayable,
      (_: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(158, "Mass Turnout", US, 3,
      NoRemove, NoMarker, NoLapsing, NoAutoTrigger, AlwaysPlayable,
      (_: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(159, "NATO", US, 3,
      NoRemove, CountryMarker, NoLapsing, NoAutoTrigger, AlwaysPlayable,
      (_: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(160, "Operation Neptune Spear", US, 3,
      NoRemove, NoMarker, NoLapsing, NoAutoTrigger, AlwaysPlayable,
      (_: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(161, "PRISM", US, 3,
      NoRemove, NoMarker, NoLapsing, NoAutoTrigger, AlwaysPlayable,
      (_: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(162, "SCAF", US, 3,
      NoRemove, NoMarker, NoLapsing, NoAutoTrigger, AlwaysPlayable,
      (_: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(163, "Status Quo", US, 3,
      NoRemove, NoMarker, NoLapsing, NoAutoTrigger, AlwaysPlayable,
      (_: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(164, "Bloody Thursday", Jihadist, 1,
      NoRemove, NoMarker, NoLapsing, NoAutoTrigger, AlwaysPlayable,
      (_: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(165, "Coup", Jihadist, 1,
      NoRemove, NoMarker, NoLapsing, NoAutoTrigger, AlwaysPlayable,
      (_: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(166, "Ferguson", Jihadist, 1,
      NoRemove, NoMarker, Lapsing, NoAutoTrigger, AlwaysPlayable,
      (_: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(167, "Houthi Rebels", Jihadist, 1,
      Remove, NoMarker, NoLapsing, NoAutoTrigger, AlwaysPlayable,
      (_: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(168, "IEDs", Jihadist, 1,
      NoRemove, NoMarker, NoLapsing, NoAutoTrigger, AlwaysPlayable,
      (_: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(169, "Islamic Maghreb", Jihadist, 1,
      NoRemove, NoMarker, Lapsing, NoAutoTrigger, AlwaysPlayable,
      (_: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(170, "Theft of State", Jihadist, 1,
      NoRemove, NoMarker, NoLapsing, NoAutoTrigger, AlwaysPlayable,
      (_: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(171, "Abu Ghraib Jail Break", Jihadist, 2,
      Remove, NoMarker, NoLapsing, NoAutoTrigger, AlwaysPlayable,
      (_: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(172, "Al-Shabaab", Jihadist, 2,
      NoRemove, NoMarker, NoLapsing, NoAutoTrigger, AlwaysPlayable,
      (_: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(173, "Arab Winter", Jihadist, 2,
      NoRemove, NoMarker, Lapsing, NoAutoTrigger, AlwaysPlayable,
      (_: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(174, "Boston Marathon", Jihadist, 2,
      Remove, NoMarker, NoLapsing, NoAutoTrigger, AlwaysPlayable,
      (_: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(175, "Censorship", Jihadist, 2,
      NoRemove, NoMarker, NoLapsing, NoAutoTrigger, AlwaysPlayable,
      (_: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(176, "Change of State", Jihadist, 2,
      NoRemove, NoMarker, NoLapsing, NoAutoTrigger, AlwaysPlayable,
      (_: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(177, "Gaza Rockets", Jihadist, 2,
      NoRemove, NoMarker, NoLapsing, NoAutoTrigger, AlwaysPlayable,
      (_: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(178, "Ghost Soldiers", Jihadist, 2,
      NoRemove, NoMarker, NoLapsing, NoAutoTrigger, AlwaysPlayable,
      (_: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(179, "Korean Crisis", Jihadist, 2,
      NoRemove, NoMarker, Lapsing, NoAutoTrigger, AlwaysPlayable,
      (_: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(180, "Mosul Central Bank", Jihadist, 2,
      Remove, NoMarker, NoLapsing, NoAutoTrigger, AlwaysPlayable,
      (_: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(181, "NPT Safeguards Ignored", Jihadist, 2,
      NoRemove, NoMarker, NoLapsing, NoAutoTrigger, AlwaysPlayable,  
      (_: Role) => {
        // Note: Remove on die roll of 1-3
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(182, "Paris Attacks", Jihadist, 2,
      NoRemove, NoMarker, NoLapsing, NoAutoTrigger, AlwaysPlayable,
      (_: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(183, "Pirates", Jihadist, 2,
      Remove, GlobalMarker, NoLapsing, NoAutoTrigger,
      (_: Role) => globalEventNotInPlay("Maersk Alabama")
      ,
      (_: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(184, "Sequestration", Jihadist, 2,
      Remove, GlobalMarker, NoLapsing, NoAutoTrigger, AlwaysPlayable,
      (_: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(185, "al-Maliki", Jihadist, 3,
      Remove, NoMarker, NoLapsing, NoAutoTrigger, AlwaysPlayable,
      (_: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(186, "Boko Haram", Jihadist, 3,
      NoRemove, NoMarker, NoLapsing, NoAutoTrigger, AlwaysPlayable,
      (_: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(187, "Foreign Fighters", Jihadist, 3,
      NoRemove, NoMarker, NoLapsing, NoAutoTrigger, AlwaysPlayable,
      (_: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(188, "ISIL", Jihadist, 3,
      Remove, NoMarker, NoLapsing, NoAutoTrigger, AlwaysPlayable,
      (_: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(189, "Jihadist Videos", Jihadist, 3,
      NoRemove, NoMarker, NoLapsing, NoAutoTrigger, AlwaysPlayable,
      (_: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(190, "Martyrdom Operation", Jihadist, 3,
      NoRemove, NoMarker, NoLapsing, NoAutoTrigger, AlwaysPlayable,
      (_: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(191, "Muslim Brotherhood", Jihadist, 3,
      Remove, NoMarker, NoLapsing, NoAutoTrigger, AlwaysPlayable,
      (_: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(192, "Quagmire", Jihadist, 3,
      NoRemove, NoMarker, NoLapsing, NoAutoTrigger, AlwaysPlayable,
      (_: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(193, "Regional al-Qaeda", Jihadist, 3,
      NoRemove, NoMarker, NoLapsing, NoAutoTrigger, AlwaysPlayable,
      (_: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(194, "Snowden", Jihadist, 3,
      Remove, NoMarker, NoLapsing, NoAutoTrigger, AlwaysPlayable,
      (_: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(195, "Taliban Resurgent", Jihadist, 3,
      NoRemove, NoMarker, NoLapsing, NoAutoTrigger, AlwaysPlayable,
      (_: Role) => ()
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
      (_: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(198, "US Atrocities", Jihadist, 3,
      NoRemove, NoMarker, NoLapsing, NoAutoTrigger, AlwaysPlayable,
      (_: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(199, "US Consulate Attacked", Jihadist, 3,
      NoRemove, NoMarker, Lapsing, NoAutoTrigger, AlwaysPlayable,
      (_: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(200, "Critical Middle", Unassociated, 1,
      NoRemove, NoMarker, NoLapsing, NoAutoTrigger, AlwaysPlayable,
      (_: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(201, "Cross Border Support", Unassociated, 1,
      NoRemove, NoMarker, NoLapsing, NoAutoTrigger, AlwaysPlayable,
      (_: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(202, "Cyber Warfare", Unassociated, 1,
      NoRemove, NoMarker, NoLapsing, NoAutoTrigger, AlwaysPlayable,
      (_: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(203, "Day of Rage", Unassociated, 1,
      Remove, NoMarker, NoLapsing, NoAutoTrigger, AlwaysPlayable,
      (_: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(205, "Erdoğan Effect", Unassociated, 1,
      NoRemove, NoMarker, NoLapsing, NoAutoTrigger, AlwaysPlayable,
      (_: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(204, "Ebola Scare", Unassociated, 1,
      Remove, NoMarker, USLapsing, NoAutoTrigger, AlwaysPlayable,
      (_: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(206, "Friday of Anger", Unassociated, 1,
      NoRemove, NoMarker, NoLapsing, NoAutoTrigger, AlwaysPlayable,
      (_: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(207, "JV / Copycat", Unassociated, 1,
      NoRemove, NoMarker, NoLapsing, NoAutoTrigger, AlwaysPlayable,
      (_: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(208, "Kinder – Gentler", Unassociated, 1,
      NoRemove, NoMarker, NoLapsing, NoAutoTrigger, AlwaysPlayable,
      (_: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(209, "Quds Force", Unassociated, 1,
      NoRemove, NoMarker, NoLapsing, NoAutoTrigger, AlwaysPlayable,
      (_: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(210, "Sectarian Violence", Unassociated, 1,
      NoRemove, NoMarker, NoLapsing, NoAutoTrigger, AlwaysPlayable,
      (_: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(211, "Smartphones", Unassociated, 1,
      NoRemove, GlobalMarker, NoLapsing, NoAutoTrigger, AlwaysPlayable,
      (_: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(212, "Smartphones", Unassociated, 1,
      NoRemove, GlobalMarker, NoLapsing, NoAutoTrigger, AlwaysPlayable,
      (_: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(213, "Smartphones", Unassociated, 1,
      NoRemove, GlobalMarker, NoLapsing, NoAutoTrigger, AlwaysPlayable,
      (_: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(214, "3 Cups of Tea", Unassociated, 2,
      NoRemove, GlobalMarker, NoLapsing, NoAutoTrigger, AlwaysPlayable,
      (_: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(215, "Abu Bakr al-Baghdadi", Unassociated, 2,
      USRemove, NoMarker, NoLapsing, NoAutoTrigger, AlwaysPlayable,
      (_: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(216, "Abu Sayyaf (ISIL)", Unassociated, 2,
      USRemove, NoMarker, NoLapsing, NoAutoTrigger, AlwaysPlayable,
      (_: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(217, "Agitators", Unassociated, 2,
      NoRemove, NoMarker, NoLapsing, NoAutoTrigger, AlwaysPlayable,
      (_: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(218, "Al-Nusra Front", Unassociated, 2,
      NoRemove, NoMarker, NoLapsing, NoAutoTrigger, AlwaysPlayable,
      (_: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(219, "Ayman al-Zawahiri", Unassociated, 2,
      USRemove, NoMarker, NoLapsing, NoAutoTrigger, AlwaysPlayable,
      (_: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(220, "Daraa", Unassociated, 2,
      Remove, NoMarker, NoLapsing, NoAutoTrigger, AlwaysPlayable,
      (_: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(221, "FlyPaper", Unassociated, 2,
      NoRemove, NoMarker, NoLapsing, NoAutoTrigger, AlwaysPlayable,
      (_: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(222, "Hagel", Unassociated, 2,
      Remove, NoMarker, NoLapsing, NoAutoTrigger, AlwaysPlayable,
      (_: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(223, "Iranian Elections", Unassociated, 2,
      Remove, NoMarker, NoLapsing, NoAutoTrigger, AlwaysPlayable,
      (_: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(224, "Je Suis Charlie", Unassociated, 2,
      NoRemove, NoMarker, NoLapsing, NoAutoTrigger, AlwaysPlayable,
      (_: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(225, "Jihadi John", Unassociated, 2,
      USRemove, NoMarker, NoLapsing, NoAutoTrigger, AlwaysPlayable,
      (_: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(226, "Operation Serval", Unassociated, 2,
      NoRemove, GlobalMarker, NoLapsing, NoAutoTrigger, AlwaysPlayable,
      (_: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(227, "Popular Support", Unassociated, 2,
      NoRemove, NoMarker, NoLapsing, NoAutoTrigger, AlwaysPlayable,
      (_: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(228, "Popular Support", Unassociated, 2,
      NoRemove, NoMarker, NoLapsing, NoAutoTrigger, AlwaysPlayable,
      (_: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(229, "Prisoner Exchange", Unassociated, 2,
      NoRemove, NoMarker, NoLapsing, NoAutoTrigger, AlwaysPlayable,
      (_: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(230, "Sellout", Unassociated, 2,
      NoRemove, NoMarker, NoLapsing, NoAutoTrigger, AlwaysPlayable,
      (_: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(231, "Siege of Kobanigrad", Unassociated, 2,
      NoRemove, NoMarker, NoLapsing, NoAutoTrigger, AlwaysPlayable,
      (_: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(232, "Trade Embargo", Unassociated, 2,
      USRemove, GlobalMarker, NoLapsing, NoAutoTrigger, AlwaysPlayable,
      (_: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(233, "UN Ceasefire", Unassociated, 2,
      NoRemove, NoMarker, NoLapsing, NoAutoTrigger, AlwaysPlayable,
      (_: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(234, "Free Syrian Army", Unassociated, 3,
      Remove, NoMarker, NoLapsing, NoAutoTrigger, AlwaysPlayable,
      (_: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(235, "Qadhafi", Unassociated, 3,
      NoRemove, NoMarker, NoLapsing, NoAutoTrigger, AlwaysPlayable,
      (_: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(236, "Oil Price Spike", Unassociated, 3,
      NoRemove, NoMarker, Lapsing, NoAutoTrigger, AlwaysPlayable,
      (_: Role) => {
        // Removes "Fracking" marker
      }
      
    )),
    // ------------------------------------------------------------------------
    entry(new Card(237, "Osama bin Ladin", Unassociated, 3,
      USRemove, NoMarker, NoLapsing, NoAutoTrigger, AlwaysPlayable,
      (_: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(238, "Revolution", Unassociated, 3,
      NoRemove, NoMarker, NoLapsing, NoAutoTrigger, AlwaysPlayable,
      (_: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(239, "Truce", Unassociated, 3,
      NoRemove, NoMarker, Lapsing, NoAutoTrigger, AlwaysPlayable,
      (_: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(240, "US Election", Unassociated, 3,
      NoRemove, NoMarker, NoLapsing, AutoTrigger,
      (_: Role) => false  // No directly playable, but will always auto trigger
      ,
      (_: Role) => ()
    ))
  )
}