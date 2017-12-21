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
import LabyrinthAwakening._
import USBot.PlotInCountry

object AwakeningCards {
  
  // Various tests used by the card events
  val advisorsCandidate = (m: MuslimCountry) => !m.isAdversary && m.civilWar && m.totalTroops == 0 && !m.hasMarker(Advisors)
  val humanitarianAidCandidate = (m: MuslimCountry) => m.canTakeAidMarker && m.totalCells > 0
  val peshmergaCandidate = (m: MuslimCountry) => (m.name == Iraq || m.name == Syria) && 
                                     m.totalCells > 0 && !m.isGood && !m.isIslamistRule
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
    (m.hasMarker(UNSCR_1973) && m.totalCells > 0) || (m.civilWar && !m.hasMarker(UNSCR_1973))
  }
  val backlashCandidate = (m: MuslimCountry) =>
    (m.plots exists (p => !p.backlashed)) && !game.isCaliphateMember(m.name)
  val unNationBuildingCandidate = (m: MuslimCountry) => 
    (m.inRegimeChange || m.civilWar) && !m.isAdversary && !(m.isGood && m.isAlly)

  val massTurnoutCandidate = (m: MuslimCountry) => 
    m.inRegimeChange && m.awakening > 0 && !game.isCaliphateMember(m.name)
  val scafCandidate = (m: MuslimCountry) => 
    m.name != Iran && m.name != Syria && m.awakening > 0 && m.reaction > 0 &&
    (!m.isAlly || !m.isFair || m.totalCells > 0)
  val statusQuoCandidate = (m: MuslimCountry) => 
    m.regimeChange == TanRegimeChange && 
    (m.totalTroopsAndMilitia / 2) >= m.totalCells &&
    !game.isCaliphateMember(m.name)
  
  val coupCandidate = (m: MuslimCountry) => 
    m.resources == 1 && m.totalCells >= 2 && !(m.civilWar && m.besiegedRegime)
    
  val islamicMaghrebCountry = Set(AlgeriaTunisia, Libya, Mali, Morocco, Nigeria)
  val islamicMaghrebCandidate = (m: MuslimCountry) => islamicMaghrebCountry(m.name) && m.isPoor
  val theftOfStateCandidate = (m: MuslimCountry) => m.isPoor && m.awakening > 0
  val changeOfStateCandidate = (m: MuslimCountry) => {
    (m.isShiaMix || m.name == Jordan || m.name == Morocco) && !m.isUntested &&
    !(m.isGood || m.isIslamistRule || m.civilWar || m.inRegimeChange)
  }
  val botChangeOfStateCandidate = (m: MuslimCountry) => {
    (m.isShiaMix || m.name == Jordan || m.name == Morocco) && m.isFair && m.isAlly &&
    !(m.isGood || m.isIslamistRule || m.civilWar || m.inRegimeChange)
  }
  val ghostSoldiersCandidate = (m: MuslimCountry) => m.militia > 0 && (m.civilWar || m.inRegimeChange)
  val martyrdomCandidate = (c: Country) => c.totalCells > 0 && 
    !(game.isMuslim(c.name) && game.getMuslim(c.name).isIslamistRule)
  val regionalAlQaedaCandidate = (m: MuslimCountry) => m.name != Iran && m.isUntested
  val talibanResurgentCandidate = (m: MuslimCountry) => (m.civilWar || m.inRegimeChange) && m.totalCells >= 3
  val usAtrocitiesCandidate = (m: MuslimCountry) => m.totalTroops > 0 && (m.civilWar || m.inRegimeChange)
  val smartPhonesCandidate = (m: MuslimCountry) => m.canTakeAwakeningOrReactionMarker &&
                                          (game.targetsThisPhase.wasOpsOrEventTarget(m.name) ||
                                           game.targetsLastPhase.wasOpsOrEventTarget(m.name))

  val alNusraFrontCandidate = (m: MuslimCountry) => (m.civilWar || m.inRegimeChange) &&
                                                     m.totalCells > 0 &&
                                                     m.militia > 0
  val selloutCandidate = (m: MuslimCountry) => (m.civilWar || m.inRegimeChange) && 
                                               m.totalCells > 0 &&
                                               !game.isCaliphateMember(m.name)
  val unCeasfireCandidate = (m: MuslimCountry) => m.civilWar && !game.isCaliphateMember(m.name)
  val opNewDawnCandidate = (m: MuslimCountry) => m.troops > 0 && m.canTakeMilitia
  def parisAttacksPossible: Boolean = {
    val list = UnitedStates :: Canada :: UnitedKingdom :: Benelux :: France :: Schengen
    (game.cellsAvailable > 0 || game.availablePlots.nonEmpty) &&
    (list exists (name => game.getNonMuslim(name).isHard))
  }
  def flyPaperCandidates: List[String] = {
    val caliphate = game.caliphateCapital map (c => game.caliphateDaisyChain(c).toSet) getOrElse Set.empty
    countryNames(game.muslims filter (m => caliphate(m.name) || m.civilWar || m.inRegimeChange))
  }
  // Countries with cells that are not within two countries with troops/advisor
  def specialForcesCandidates: List[String] = {
    // First find all muslim countries with troops or "Advisors"
    val withForces = countryNames(game.countries filter (c => c.totalTroops > 0 || c.hasMarker(Advisors)))
    // Next get all countries that contain any cells and filter out the ones are are not 
    // within two of a country with forces.
    countryNames(game.countries filter { country =>
      country.totalCells > 0 && (withForces exists (forces => distance(forces, country.name) <= 2))
    })
  }
  
  // Countries with an awakening maker or adjacent to a country with an awakening marker.
  def faceBookCandidates: List[String] = countryNames(game.muslims filter { m =>
    m.canTakeAwakeningOrReactionMarker &&
    (m.awakening > 0 || (game.adjacentMuslims(m.name) exists (_.awakening > 0)))
  })
  
  val GulfUnionCountries = List(GulfStates, SaudiArabia, Yemen, Jordan, Morocco).sorted
  
  def gulfUnionCandidates: Set[String] = {
    GulfUnionCountries.foldLeft(Set.empty[String]) { (candidates, name) =>
      val gulf = game getMuslim name
      val adj = (getAdjacent(name) filter (x => game.isMuslim(x) && x != Iran)) map game.getMuslim
      candidates ++ ((gulf :: adj) filter (_.canTakeMilitia) map (_.name))
    }
  }
  
  val criticalMiddleUSCandidate = (m: MuslimCountry) => m.isFair && m.resources > 1 &&
                                     (!m.isAlly || m.canTakeAwakeningOrReactionMarker)
  val criticalMiddleJihadistCandidate = (m: MuslimCountry) => m.isPoor && m.resources < 3
  
  def crossBorderSupportPlayable(role: Role, forTrigger: Boolean) = {
    val cellsOK   = game.cellsAvailable > 0
    val militiaOK = game.militiaAvailable > 0 && (game.getCountries(African) exists (_.canTakeMilitia))
    (role == game.humanRole && (cellsOK || militiaOK)) ||
    (role == Jihadist && cellsOK) ||
    (role == US       && militiaOK)
  }
  
  def cyberWarfarePlayable(role: Role, forTrigger: Boolean): Boolean = role == game.humanRole || {
    val postureUS = (game.getNonMuslims(China::Russia::India::Nil) 
                        exists (c => c.isUntested || c.posture !=  game.usPosture))
    val postureJ  = (game.getNonMuslims(China::Russia::India::Nil) 
                        exists (c => c.isUntested || c.posture == game.usPosture))
    (role == US      ) && (postureUS || game.reserves.jihadist > 0) ||
    (role == Jihadist) && (postureJ  || game.reserves.us       > 0)
  }
  
  def qudsForcePlayable(role: Role, forTrigger: Boolean): Boolean = 
    (role == Jihadist &&
     ((game isNonMuslim Iran) || !(game getMuslim Iran).isAlly) &&
     (game hasMuslim (_.militia > 0))) ||
    (role == US &&
     ((game isNonMuslim Iran) || !(game getMuslim Iran).isIslamistRule) &&
     (game hasMuslim (_.totalCells > 0)))
  
  def servalCandidates = {
    val names = List(Morocco, AlgeriaTunisia, Libya, Sudan, Somalia, Mali)
    countryNames(game getMuslims names filter (_.isPoor))
  }
  
  def servalPlayable(role: Role, forTrigger: Boolean): Boolean = {
    if (role == game.humanRole)
      servalCandidates.nonEmpty
    else if (role == US) {  // USBot
      val canPlaceMarker = (USBot.servalTarget(servalCandidates) map 
                 (name => !game.getMuslim(name).hasMarker(OperationServal)) 
                 getOrElse false)
      game.militiaAvailable > 0 || canPlaceMarker
    }
    else { // JihadistBot
      val canStartCivilWar = (JihadistBot.minorJihadTarget(servalCandidates) map
                      (name => !game.getMuslim(name).civilWar) 
                      getOrElse false)
      game.cellsAvailable > 0 || canStartCivilWar
    }
  }
  
  def oilPriceSpikePlayable(role: Role, forTrigger: Boolean): Boolean = {
    if (role == game.humanRole)
      true
    else if (role == Jihadist) {
      // Unplayable if it would cause Jihadist instant victory
      val usOilExporters = game.muslims count (m => m.isGood && m.oilExporter)
      game.goodResources + usOilExporters < 12
    }
    else {
      // Unplayable if it would cause US instant victory
      val jihadOilExporters = game.muslims count (m => m.isIslamistRule && m.oilExporter)
      game.islamistResources + jihadOilExporters < 6 || !game.islamistAdjacency
    }
  }
  // Convenience method for adding a card to the deck.
  private def entry(card: Card) = (card.number -> card)
  
  val deckMap: Map[Int, Card] = Map(
    // ------------------------------------------------------------------------
    entry(new Card(121, "Advisors", US, 1,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (_: Role, _: Boolean) => (game.muslims count (_.hasMarker(Advisors))) < 3 && (game hasMuslim advisorsCandidate)
      ,
      (role: Role) => {
        val candidates = countryNames(game.muslims filter advisorsCandidate)
        val target = if (role == game.humanRole)
          askCountry(s"Advisors in which country: ", candidates)
        else 
          USBot.deployToPriority(candidates).get
        println()
        addEventTarget(target)
        testCountry(target)
        addEventMarkersToCountry(target, Advisors)
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(122, "Backlash", US, 1,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (role: Role, forTrigger: Boolean) => 
        (game.funding > 1 || role != game.botRole) && (game hasMuslim backlashCandidate)
      ,
      (role: Role) => {
        val candidates = countryNames(game.muslims filter backlashCandidate)
        if (role == game.humanRole) {
          val target = askCountry(s"Backlash in which country: ", candidates)
          // Pick a random plot in the country
          addEventTarget(target)
          val m = game.getMuslim(target)
          val plot :: remaining = shuffle(m.plots)
          val newPlots = plot.copy(backlashed = true) :: remaining
          game = game.updateCountry(m.copy(plots = newPlots))
          println()
          log(s"Backlash applied to a plot in $target")
        }
        else {
          val plots = for {
            name <- candidates
            m    = game.getMuslim(name)
            plot <- m.plots
            if !plot.backlashed
          } yield PlotInCountry(plot, m)

          // Pick the highest priority plot among the countries
          val PlotInCountry(plotOnMap, country) = USBot.priorityPlot(plots)
          val (matching, other) = country.plots partition (_ == plotOnMap)
          val newPlots = matching.head.copy(backlashed = true) :: matching.tail ::: other
          addEventTarget(country.name)
          val m = country.asInstanceOf[MuslimCountry]
          game = game.updateCountry(m.copy(plots = newPlots))
          println()
          log(s"Backlash applied to a $plotOnMap in ${country.name}")
        }
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(123, "Humanitarian Aid", US, 1,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (role: Role, forTrigger: Boolean) => game hasMuslim humanitarianAidCandidate
      ,
      (role: Role) => {
        val candidates = countryNames(game.muslims filter humanitarianAidCandidate)
        val target = if (role == game.humanRole)
          askCountry(s"Humanitarian Aid in which country: ", candidates)
        else
          USBot.markerAlignGovTarget(candidates).get

        println()
        addEventTarget(target)
        addAidMarker(target)
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(124, "Pearl Roundabout", US, 1,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (role: Role, forTrigger: Boolean) => globalEventNotInPlay(BloodyThursday) && lapsingEventNotInPlay(ArabWinter)
      ,
      (role: Role) => {
        println()
        addEventTarget(GulfStates)
        testCountry(GulfStates)
        addAwakeningMarker(GulfStates)
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(125, "Peshmerga", US, 1,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (role: Role, forTrigger: Boolean) => (game hasMuslim peshmergaCandidate) && game.militiaAvailable > 0
      ,
      (role: Role) => {
        val candidates = countryNames(game.muslims filter peshmergaCandidate)
        val target = if (role == game.humanRole)
          askCountry(s"Select country: ", candidates)
        else 
          USBot.deployToPriority(candidates).get
        
        addEventTarget(target)        
        println()
        addMilitiaToCountry(target, 2 min game.militiaAvailable)
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(126, "Reaper", US, 1,
      NoRemove, NoLapsing,  NoAutoTrigger, DoesNotAlertPlot, AlwaysPlayable,
      (role: Role) => {
        if (role == game.humanRole) {
          if (game.muslims exists (_.totalCells > 0)) {
            val choices = List(
              "remove"  -> "Remove up to 2 cells in one Muslim country",
              "discard" -> "Randomly discard 1 card from the Jihadist hand")
            println("Choose one:")
            askMenu(choices).head match {
              case "discard" => log("Discard the top card in the Jihadist hand")
              case _ =>
              val candidates = game.muslims filter (_.totalCells > 0)
              val target = askCountry("Remove 2 cells in which country? ", countryNames(candidates))
              val (actives, sleepers, sadr) = askCells(target, 2, sleeperFocus = true)
              addEventTarget(target)
              println()
              removeCellsFromCountry(target, actives, sleepers, sadr, addCadre = true)
            }
          }
          else
            log("Discard the top card in the Jihadist hand")
        }
        else {
          val nonIRWith5Cells = countryNames(game.muslims filter (m => !m.isIslamistRule && m.totalCells >= 5))
          val withCells       = countryNames(game.muslims filter (_.totalCells > 0))
          if (nonIRWith5Cells.isEmpty && askYorN(s"Do you ($Jihadist) have any cards in hand (y/n)? ")) {
            println()
            log(s"You ($Jihadist) must discard one random card")
          }
          else {
            val target = if (nonIRWith5Cells.nonEmpty)
              USBot.disruptPriority(nonIRWith5Cells).get
            else
              USBot.disruptPriority(withCells).get
            addEventTarget(target)
            val (actives, sleepers, sadr) = USBot.chooseCellsToRemove(target, 2)
            println()
            removeCellsFromCountry(target, actives, sleepers, sadr, addCadre = true)
          }
        }
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(127, "Reaper", US, 1,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (role: Role, forTrigger: Boolean) => deck(126).eventConditions(role, forTrigger),
      (role: Role) => deck(126).executeEvent(role)
    )),
    // ------------------------------------------------------------------------
    entry(new Card(128, "Reaper", US, 1,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (role: Role, forTrigger: Boolean) => deck(126).eventConditions(role, forTrigger),
      (role: Role) => deck(126).executeEvent(role)
    )),
    // ------------------------------------------------------------------------
    entry(new Card(129, "Special Forces", US, 1,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (role : Role, forTrigger: Boolean) => specialForcesCandidates.nonEmpty
      ,
      (role: Role) => {
        val (target, (actives, sleepers, sadr)) = if (role == game.humanRole) {
          val target = askCountry("Remove cell in which country: ", specialForcesCandidates)
          (target, askCells(target, 1, sleeperFocus = true))
        }
        else {
          val target = USBot.disruptPriority(specialForcesCandidates).get
          (target, USBot.chooseCellsToRemove(target, 1))
        }
        println()
        addEventTarget(target)
        removeCellsFromCountry(target, actives, sleepers, sadr, addCadre = true)
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(130, "Special Forces", US, 1,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (role: Role, forTrigger: Boolean) => deck(129).eventConditions(role, forTrigger),
      (role: Role) => deck(129).executeEvent(role)
    )),
    // ------------------------------------------------------------------------
    entry(new Card(131, "Arab Spring Fallout", US, 2,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (role: Role, forTrigger: Boolean) => lapsingEventNotInPlay(ArabWinter) && (game hasMuslim arabSpringFalloutCandidate)
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
        else
          USBot.multipleTargets(2, candidates, USBot.markerAlignGovTarget)
        
        println()
        targets foreach { target =>
          addEventTarget(target)
          testCountry(target)
          addAwakeningMarker(target)
        }
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(132, "Battle of Sirte", US, 2,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (role: Role, forTrigger: Boolean) => game hasMuslim (_.civilWar)
      ,
      (role: Role) => {
        val candidates = countryNames(game.muslims filter (_.civilWar))
        val (target, (actives, sleepers, sadr)) = if (role == game.humanRole) {
          val target = askCountry("Select civil war country: ", candidates)
          (target, askCells(target, 1, sleeperFocus = true))
        }
        else {
          // Bot chooses the candidate with the where (totalCells - TandM) is highest.
          val best = USBot.highestCellsMinusTandM(candidates)
          val target = shuffle(candidates).head
          (target, USBot.chooseCellsToRemove(target, 1))
        }

        println()
        addEventTarget(target)
        removeCellsFromCountry(target, actives, sleepers, sadr, addCadre = true)
        addMilitiaToCountry(target, game.militiaAvailable min 2)
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(133, "Benghazi Falls", US, 2,
      Remove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (role: Role, forTrigger: Boolean) => {
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
        addEventTarget(Libya)
        startCivilWar(Libya)
        addMilitiaToCountry(Libya, game.militiaAvailable min 2)
        setCountryPosture(Italy, Hard)
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(134, "Civil Resistance", US, 2,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (role: Role, forTrigger: Boolean) => lapsingEventNotInPlay(ArabWinter)
      ,
      (role: Role) => {
        val sunnis = countryNames(game.muslims filter (m=> m.isSunni && m.canTakeAwakeningOrReactionMarker))
        if (sunnis.nonEmpty) {  // This should never happen, but let's be defensive
          val sunniTarget = if (role == game.humanRole)
            askCountry("Select a Sunni country: ", sunnis)
          else 
            USBot.markerAlignGovTarget(sunnis).get

          println()
          addEventTarget(sunniTarget)
          testCountry(sunniTarget)
          addAwakeningMarker(sunniTarget)
        }
        
        if (randomShiaMixList exists (_.canTakeAwakeningOrReactionMarker)) {
          def doRandomShiaMix: Unit = {
            val shiaMix = randomShiaMixCountry
            if (shiaMix.canTakeAwakeningOrReactionMarker) {
              println()
              addEventTarget(shiaMix.name)
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
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (role: Role, forTrigger: Boolean) => 
        (role == game.humanRole || (game.availablePlots contains PlotWMD) ||
        cacheQuestion(askYorN(s"Do you ($Jihadist) have any cards in hand (y/n)? ")))
      ,
      (role: Role) => {
        if (role == game.humanRole) {
          val choices = List(
            "reveal" -> "Reveal all WMD plots and remove one",
            "draw"  -> "Randomly draw 1 card from the Jihadist hand")
          println("Choose one:")
          askMenu(choices).head match {
            case "draw" => log("Draw the top card in the Jihadist Bot's hand")
            case _ =>
              val wmdOnMap = for (c <- game.countries; PlotOnMap(PlotWMD, _) <- c.plots)
                yield c.name
              val wmdAvailable = game.availablePlots.sorted takeWhile (_ == PlotWMD)
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
              
                if (target == "available")
                  removeAvailableWMD(1)
                else {
                  addEventTarget(target)
                  removePlacedWMD(target, 1)
                }
              }
          }
        }
        else {
          // See Event Instructions table
          // If there are any WMD plots in the available box, the bot
          // will remove one. Otherwise take a US players random card.
          if (game.availablePlots contains PlotWMD) {
            removeAvailableWMD(1)
          }
          else
            log(s"You ($Jihadist) must place one random card on top of the $US hand")
        }
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(136, "Factional Infighting", US, 2,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot, AlwaysPlayable,
      (role: Role) => {
        val candidates = countryNames(game.muslims filter (m => !m.isIslamistRule && m.totalCells > 0))
        if (candidates.nonEmpty) {
          val target = if (role == game.humanRole)
            askCountry("Select a country: ", candidates)
          else 
            USBot.disruptPriority(USBot.highestCellsMinusTandM(candidates)).get

          val m = game.getMuslim(target)
          val (actives, sleepers, sadr) = (m.activeCells, m.sleeperCells, m.hasSadr) match {
            case (0, 0, true)  => (0, 0, true)
            case (0, s, true)  => (0, 1, true)
            case (a, _, true)  => (1, 0, true)
            case (a, s, false) =>
              val actives  = a min 2
              val sleepers = s min (2 - a)
              (actives, sleepers, false)
          }
          println()
          addEventTarget(target)
          removeCellsFromCountry(target, actives, sleepers, sadr, addCadre = true)
          if (game.getMuslim(target).sleeperCells > 0)
            flipSleeperCells(target, 1)
        }
        // Also, decrease funding by 1 even if no cells affected.
        decreaseFunding(1)
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(137, "FMS", US, 2,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      // Note: No militia allowed in Good countries!
      (role: Role, forTrigger: Boolean) => game.militiaAvailable > 0 && (game hasMuslim (m => m.isAlly && !m.isGood))
      ,
      (role: Role) => {
        val candidates = countryNames(game.muslims filter (m => m.isAlly && !m.isGood))
        val target = if (role == game.humanRole)
          askCountry("Select country: ", candidates)
        else 
          USBot.deployToPriority(candidates).get

        println()
        addEventTarget(target)
        addMilitiaToCountry(target, game.militiaAvailable min 3)
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(138, "Intel Community", US, 2,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (role: Role, forTrigger: Boolean) => role == game.humanRole  // The bot treats this as unplayable
      ,
      (role: Role) => {
        // See Event Instructions table
        log("US player does not inspect the Jihadist hand in the solo game.")
        val cadres = countryNames(game.countries filter (_.hasCadre))
        if (cadres.isEmpty)
          log("No cadres on the map to remove")
        else {
          val target = askCountry("Select country with cadre: ", cadres)
          addEventTarget(target)
          removeCadreFromCountry(target)
        }
        
        // US player conducts a 1 Op operations.
        println()
        log("US player conducts an operation with 1 Op")
        humanExecuteOperation(1)
        println()
        if (askYorN("Do you wish to play an extra card now during this action phase? (y/n) "))
          usCardPlay(None, additional = true)
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(139, "Int'l Banking Regime", US, 2,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot, AlwaysPlayable,
      (role: Role) => {
        val die = getDieRoll(role)
        log(s"Die roll: $die")
        println()
        decreaseFunding((die + 1) / 2)  // Half rounded up
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(140, "Maersk Alabama", US, 2,
      Remove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot, AlwaysPlayable,
      (role: Role) => {
        if (game.reserves.us < 2) {
          game = game.copy(reserves = game.reserves.copy(us = game.reserves.us + 1))
          log(s"Add 1 Ops value to US reserves")
        }
        else
          log(s"US reserves are already at max of 2 Ops")
        println()
        increasePrestige(1)
        addGlobalEventMarker(MaerskAlabama)
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(141, "Malala Yousafzai", US, 2,
      Remove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (role: Role, forTrigger: Boolean) => 
        globalEventNotInPlay(ThreeCupsOfTea) &&  // If not blocked and can have at least one effect
        (game.funding > 1 || game.prestige < 12 || game.getMuslim(Pakistan).canTakeAwakeningOrReactionMarker)
      ,
      (role: Role) => {
        println()
        increasePrestige(1)
        decreaseFunding(1)
        addEventTarget(Pakistan)
        testCountry(Pakistan)
        addAwakeningMarker(Pakistan)
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(142, "Militia", US, 2,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (role: Role, forTrigger: Boolean) => game hasMuslim (m => (m.civilWar || m.inRegimeChange) && game.militiaAvailable > 0)
      ,
      (role: Role) => {
        val candidates = countryNames(game.muslims filter (m => m.civilWar || m.inRegimeChange))
        val target = if (role == game.humanRole)
          askCountry("Place militia in which country: ", candidates)
        else 
          USBot.deployToPriority(candidates).get

        println()
        addEventTarget(target)
        addMilitiaToCountry(target, game.getMuslim(target).resources min game.militiaAvailable)
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(143, "Obama Doctrine", US, 2,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (role: Role, forTrigger: Boolean) =>
        if (game.params.scenarioName == "Mitt's Turn") game.usPosture == Hard
        else game.usPosture == Soft
      ,
      (role: Role) => {
        val numActions = if (game.params.scenarioName == "Mitt's Turn") 3 else 2
        if (role == game.humanRole) {
          val canAwakening = (game hasMuslim (_.canTakeAwakeningOrReactionMarker)) && lapsingEventNotInPlay(ArabWinter)
          val canAid       = game hasMuslim (_.canTakeAidMarker)
          val choices = List(
            choice(canAwakening,       "awakening", "Place 1 Awakening marker"),
            choice(canAid,             "aid",       "Place 1 Aid marker"),
            choice(game.prestige < 12, "prestige",  "+1 Prestige"),
            choice(game.funding > 1,   "funding",   "-1 Funding"),
            choice(true,               "posture",   "Select posture of 1 Schengen country"),
            choice(true,               "draw",      "Select Reaper, Operation New Dawn, or Advisors from discard pile.")
          ).flatten 
          println(s"Do any $numActions of the following:")
          askMenu(choices, numActions, repeatsOK = false) foreach { action =>
            println()
            action match {
              case "awakening" =>
                val target = askCountry("Place awakening marker in which country: ",
                             countryNames(game.muslims filter (_.canTakeAwakeningOrReactionMarker)))
                addEventTarget(target)
                testCountry(target)
                addAwakeningMarker(target)
              case "aid" =>
                val target = askCountry("Place aid marker in which country: ",
                             countryNames(game.muslims filter (_.canTakeAidMarker)))
                addEventTarget(target)
                testCountry(target)
                addAidMarker(target)
              case "prestige" => increasePrestige(1)
              case "funding"  => decreaseFunding(1)
              case "posture" =>
                val target  = askCountry("Select posture of which Schengen country: ", Schengen)
                val posture = askOneOf("New posture (Soft or Hard): ", Seq(Soft, Hard)).get
                addEventTarget(target)
                setCountryPosture(target, posture)
              case _ =>
                log("Select Reaper, Operation New Dawn, or Advisors from discard pile")
            }
          }
        }
        else {
          // See Event Instructions table
          val canAwakening = lapsingEventNotInPlay(ArabWinter) &&
                 (game hasMuslim (_.canTakeAwakeningOrReactionMarker))
          var actions = List(
            if (game.prestige < 12) Some("prestige") else None,
            if (game.funding  >  1) Some("funding") else None,
            if (canAwakening      ) Some("awakening") else None,
            if (game hasMuslim (_.canTakeAidMarker)) Some("aid") else None
          ).flatten take numActions
          actions foreach {
            case "prestige" => increasePrestige(1)
            case "funding"  => decreaseFunding(1)
            case "awakening" => 
              val candidates = countryNames(game.muslims filter (_.canTakeAwakeningOrReactionMarker))
              val target = USBot.markerAlignGovTarget(candidates).get
              addEventTarget(target)
              testCountry(target)
              addAwakeningMarker(target)
            case "aid" =>
              val candidates = countryNames(game.muslims filter (_.canTakeAidMarker))
              val target = USBot.markerAlignGovTarget(candidates).get
              addEventTarget(target)
              testCountry(target)
              addAidMarker(target)
          }  
        }
      }
    ) {
      // For "Mitt's Turn" scenario this card has 3 ops.
      override def ops: Int = if (game.params.scenarioName == "Mitt's Turn") 3 else 2
    }),
    // ------------------------------------------------------------------------
    entry(new Card(144, "Operation New Dawn", US, 2,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (role: Role, forTrigger: Boolean) => game.militiaAvailable > 0 && (game hasMuslim opNewDawnCandidate)
      ,
      (role: Role) => {
        val candidates = countryNames(game.muslims filter opNewDawnCandidate)
        val target = if (role == game.humanRole)
          askCountry("Replace troops in which country: ", candidates)
        else {
          val withTroops = game.muslims filter (_.troops > 0)
          val candidates = if (withTroops exists (_.inRegimeChange))
            withTroops filter (_.inRegimeChange)
          else if (withTroops exists (_.hasPlots))
            withTroops filter (_.hasPlots)
          else
            withTroops
          shuffle(countryNames(withTroops)).head
        }
        val numTroops  = 2 min game.getMuslim(target).troops
        val numMilitia = numTroops min game.militiaAvailable
        println()
        addEventTarget(target)
        moveTroops(target, "track", numTroops)
        addMilitiaToCountry(target, numMilitia)      
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(145, "Russian Aid", US, 2,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (role: Role, forTrigger: Boolean) => game hasMuslim (_.civilWar)
      ,
      (role: Role) => {
        val candidates = countryNames(game.muslims filter (_.civilWar))
        val target = if (role == game.humanRole)
          askCountry("Place militia and aid in which country: ", candidates)
        else
          USBot.deployToPriority(candidates).get

        println()
        addEventTarget(target)
        addMilitiaToCountry(target, 1 min game.militiaAvailable)
        addAidMarker(target)
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(146, "Sharia", US, 2,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (role: Role, forTrigger: Boolean) => game hasMuslim (m => m.besiegedRegime || m.canTakeAwakeningOrReactionMarker)
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
        else
          USBot.markerAlignGovTarget(candidates).get

        println()
        addEventTarget(target)
        testCountry(target)
        removeBesiegedRegimeMarker(target)
        addAwakeningMarker(target)
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(147, "Strike Eagle", US, 2,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (role: Role, forTrigger: Boolean) => game hasMuslim strikeEagleCandidate
      ,
      (role: Role) => {
        val candidates = countryNames(game.muslims filter strikeEagleCandidate)
        val target = if (role == game.humanRole)
          askCountry("Select country: ", candidates)
        else
          USBot.disruptPriority(candidates).get

        println()
        addEventTarget(target)
        removeCachedWMD(target, 1)
        if (game.cellsAvailable > 0)
          addSleeperCellsToCountry(Israel, 1)
        increaseFunding(1)
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(148, "Tahrir Square", US, 2,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (role: Role, forTrigger: Boolean) => lapsingEventNotInPlay(ArabWinter) &&
           ((game getMuslim Egypt).canTakeAwakeningOrReactionMarker || (game hasMuslim tahrirCandidate))
      ,
      (role: Role) => {
        val candidates = countryNames(game.muslims filter tahrirCandidate)
        
        addEventTarget(Egypt)
        testCountry(Egypt)
        addAwakeningMarker(Egypt, 2)
        addReactionMarker(Egypt)
        
        if (candidates.nonEmpty) {
          val target = if (role == game.humanRole)
            askCountry("Place 1 awakening marker in which country: ", candidates)
          else
            USBot.markerAlignGovTarget(candidates).get

          addEventTarget(target)
          testCountry(target)
          addAwakeningMarker(target)
        }
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(149, "UN Nation Building", US, 2,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (role: Role, forTrigger: Boolean) => game hasMuslim unNationBuildingCandidate
      ,
      (role: Role) => {
        val candidates = countryNames(game.muslims filter unNationBuildingCandidate)
        val target = if (role == game.humanRole)
          askCountry("Select country: ", candidates)
        else 
          USBot.markerAlignGovTarget(candidates).get
        
        addEventTarget(target)
        addAidMarker(target)
        performWarOfIdeas(target, 3, ignoreGwotPenalty = true)
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(150, "UNSCR 1973", US, 2,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (role: Role, forTrigger: Boolean) => game hasMuslim unscr1973Candidate
      ,
      (role: Role) => {
        val candidates = countryNames(game.muslims filter unscr1973Candidate)
        val target = if (role == game.humanRole)
          askCountry("Place UNSCR 1973 in which country: ", candidates)
        else 
          USBot.unscr1973Target(countryNames(game.muslims filter unscr1973Candidate)).get
        
        addEventTarget(target)
        val m = game.getMuslim(target)
        // If the target already contains the marker, then
        // we only remove a cell.
        val sameCountry = m.hasMarker(UNSCR_1973)

        if (!sameCountry) {
          // If marker is already on the map, remove it first.
          game.muslims find (_.hasMarker(UNSCR_1973)) foreach { c =>
            removeEventMarkersFromCountry(c.name, UNSCR_1973)
          }
        }

        if (m.totalCells > 0) {
          val (actives, sleepers, sadr) = if (role == game.humanRole)
            askCells(target, 1, sleeperFocus = true)
          else 
            USBot.chooseCellsToRemove(target, 1)
          removeCellsFromCountry(target, actives, sleepers, sadr, addCadre = true)
        }

        if (!sameCountry)
          addEventMarkersToCountry(target, UNSCR_1973)
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(151, "UNSCR 2118", US, 2,
      Remove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (role: Role, forTrigger: Boolean) => {
        val syria = game.getMuslim(Syria)
        !syria.isUntested && syria.wmdCache > 0
      }
      ,
      (role: Role) => {
        addEventTarget(Syria)
        val syria = game.getMuslim(Syria)
        removeCachedWMD(Syria, if (syria.isAlly) syria.wmdCache else 1)
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(152, "Congress Acts", US, 3,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (role: Role, forTrigger: Boolean) => 
        if (role == game.humanRole) {
          globalEventInPlay(Sequestration) || (
             game.troopsAvailable >= 6 && (game hasMuslim (m => m.civilWar)))
        }
        else {
          game.numIslamistRule > 2 && game.usPosture == Soft && 
            (game hasMuslim (m => m.civilWar && m.totalCells > m.totalTroopsAndMilitia))
        }
      ,
      (role: Role) => {
        removeGlobalEventMarker(Sequestration)
        returnSequestrationTroopsToAvailable()
        
        if (role == game.humanRole) {
          val candidates = countryNames(game.muslims filter (m => m.civilWar))
          if (game.troopsAvailable >= 6 && candidates.nonEmpty) {
            val target = askCountry("Select country: ", candidates)
            val numTroops = askInt("Deploy how many troops from the track", 6, game.troopsAvailable, Some(6))
            addEventTarget(target)
            performRegimeChange("track", target, numTroops)
          }
        }
        else {
          val candidates = countryNames(game.muslims filter (m => m.civilWar && m.totalCells > m.totalTroopsAndMilitia))
          val target = shuffle(USBot.highestCellsMinusTandM(candidates)).head
          addEventTarget(target)
          performRegimeChange("track", target, 6)  // Bot always uses exatly 6 troops for regime change
        }
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(153, "Facebook", US, 3,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (role: Role, forTrigger: Boolean) => globalEventInPlay(Smartphones) &&
                                           lapsingEventNotInPlay(ArabWinter) &&
                                           faceBookCandidates.nonEmpty
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
        else
          USBot.multipleTargets(3, candidates, USBot.markerAlignGovTarget)

        targets foreach { target => 
          addEventTarget(target)
          testCountry(target)
          addAwakeningMarker(target)
        }
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(154, "Facebook", US, 3,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (role: Role, forTrigger: Boolean) => deck(153).eventConditions(role, forTrigger),
      (role: Role) => deck(153).executeEvent(role)
    )),
    // ------------------------------------------------------------------------
    entry(new Card(155, "Fracking", US, 3,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot, AlwaysPlayable,
      (role: Role) => {
        addGlobalEventMarker(Fracking)
        rollPrestige()
        log("US player draws a card")
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(156, "Gulf Union", US, 3,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (role: Role, forTrigger: Boolean) => if (role == game.humanRole) {
        game.militiaAvailable > 0 || 
             (GulfUnionCountries exists (name => game.getMuslim(name).militia > 0))
      }
      else {
        game.militiaAvailable > 0 &&
        ((gulfUnionCandidates map game.getMuslim) exists (m => m.totalCells > m.totalTroopsAndMilitia))
      }
      ,
      (role: Role) => {
        val maxMilitia = 4 min game.militiaAvailable
        if (role == game.humanRole) {
          // Card allow placing militia or respositioning militia in the Gulf Union countries.
          val existingMilitia = (GulfUnionCountries exists (name => game.getMuslim(name).militia > 0))
          val actions = List(
            if (game.militiaAvailable > 0) Some("place")      else None,
            if (existingMilitia)           Some("reposition") else None
          ).flatten
          val choices = List(
            "place"      -> "Place up to 4 militia in one Gulf Union (or adjacent) country",
            "reposition" -> "Repositon militia in Gulf Union countries")
          val placeCandidates = gulfUnionCandidates.toList.sorted
          val action = if (actions.size == 1) actions.head 
          else {
            println("Choose one:")
            askMenu(choices).head
          }
          action match {
            case "place" =>
              val target = askCountry("Place militia in which country: ", placeCandidates)
              val num    = askInt(s"Place how many militia in $target", 1, maxMilitia, Some(maxMilitia))
              addEventTarget(target)
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
                  addEventTarget(p.name)
                  testCountry(p.name)
                  val m = game.getMuslim(p.name) // Get fresh copy after testCountry()
                  game = game.updateCountry(m.copy(militia = p.newMilitia))
                  log(s"Remove ${p.muslim.militia - p.newMilitia} militia from ${p.name}")
                }
                for (p <- placements; if !p.hasLess) {
                  addEventTarget(p.name)
                  testCountry(p.name)
                  val m = game.getMuslim(p.name) // Get fresh copy after testCountry()
                  game = game.updateCountry(m.copy(militia = p.newMilitia))
                  log(s"Add ${p.newMilitia - p.muslim.militia} militia to ${p.name}")
                }
              }
          } 
        }
        else {
          // See Event Instructions table
          val candidates = ((gulfUnionCandidates map game.getMuslim) filter (m => m.totalCells > m.totalTroopsAndMilitia))
          val target = USBot.deployToPriority(USBot.highestCellsMinusTandM(countryNames(candidates.toList))).get
          addEventTarget(target)
          testCountry(target)
          addMilitiaToCountry(target, maxMilitia)
        }
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(157, "Limited Deployment", US, 3,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (role: Role, forTrigger: Boolean) => game.usPosture == Hard && (game hasMuslim (_.civilWar))
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
          val t = USBot.deployToPriority(USBot.highestCellsMinusTandM(candidates)).get
          val a = (getAdjacentMuslims(t) filter (n => game.getMuslim(n).canTakeAwakeningOrReactionMarker))
          val bestAdjacent = USBot.markerAlignGovTarget(a)
          (t, bestAdjacent)
        }
        
        addEventTarget(target)
        moveTroops("track", target, 2 min game.troopsAvailable)
        addAidMarker(target)
        if (lapsingEventInPlay(ArabWinter))
          log("Awakening markers cannot be placed because \"Arab Winter\" is in effect")
        else
          adjacent foreach { m => addAwakeningMarker(m) }
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(158, "Mass Turnout", US, 3,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (role: Role, forTrigger: Boolean) => game hasMuslim massTurnoutCandidate
      ,
      (role: Role) => {
        val candidates = countryNames(game.muslims filter massTurnoutCandidate)
        val target = if (role == game.humanRole)
          askCountry("Select regime change country: ", candidates)
        else
          USBot.markerAlignGovTarget(candidates).get
        
        addEventTarget(target)
        improveGovernance(target, 1, canShiftToGood = true)
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(159, "NATO", US, 3,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (role: Role, forTrigger: Boolean) => game.gwotPenalty == 0 && (game hasMuslim (m => m.inRegimeChange || m.civilWar))
      ,
      (role: Role) => {
        val candidates = countryNames(game.muslims filter (m => m.inRegimeChange || m.civilWar))
        val target = if (role == game.humanRole)
          askCountry("Select country for NATO: ", candidates)
        else
          USBot.deployToPriority(candidates).get
        
        addEventTarget(target)
        addAidMarker(target)
        (game.muslims find (_.hasMarker(NATO)) map (_.name)) match {
          case Some(`target`) =>
            log(s"NATO marker remains in $target")
          case Some(current) =>
            removeEventMarkersFromCountry(current, NATO)
            addEventMarkersToCountry(target, NATO)
          case None =>
            addEventMarkersToCountry(target, NATO)
        }
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(160, "Operation Neptune Spear", US, 3,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (role: Role, forTrigger: Boolean) =>
        role == game.humanRole || cacheQuestion(askYorN("Is one of the indicated cards in the discard pile (y/n)? "))
      ,
      (role: Role) => {
        val cards = List("Ayman al-Zawahiri", "Abu Bakr al-Baghdadi", "Abu Sayyaf (ISIL)",
                         "Jihadi John", "Osama bin Ladin")
        log("US player takes one of the following cards from the discard pile:")
        log(orList(cards))
        // See Event Instructions table
        if (role == game.botRole)
          log("The Bot selects the card nearest the bottom of the discard pile")
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(161, "PRISM", US, 3,
      NoRemove, NoLapsing, NoAutoTrigger, 
      (_, _) => game hasCountry (_.hasPlots)  // Alerts all plots on the map
      ,
      (role: Role, forTrigger: Boolean) => role == game.humanRole         || 
                                           (game hasCountry (_.hasPlots)) ||
                                           game.sleeperCellsOnMap >= 5
      ,
      (role: Role) => {
        val actions = List(
          if (game.sleeperCellsOnMap > 0) Some("activate") else None,
          if (game.alertTargets.nonEmpty) Some("alert") else None
        ).flatten
        val choices = List(
          "activate" -> "Activate half (rounded up) of all sleeper cells on the map",
          "alert"    -> "Alert all plots on the map"
        )
          
        if (role == game.humanRole) {
          if (actions.nonEmpty) {
            val action = if (actions.size == 1)
              actions.head
            else {
              println("Choose one:")
              askMenu(choices).head
            }

            if (action == "activate") {
              val numToFlip = (game.sleeperCellsOnMap + 1) / 2  // half rounded up
              // Ask which cells to activate
              val withSleepers = for (c <- game.countries; if c.sleeperCells > 0)
                yield MapItem(c.name, c.sleeperCells)
              println(s"Activate a total of ${amountOf(numToFlip, "sleeper cell")}")
              val toFlip = askMapItems(withSleepers.sortBy(_.country), numToFlip, "sleeper cell")
              
              // It is possible that the same country was chosen multiple times
              // Consolidate them so we do 1 flip per country.
              def flipNextBatch(remaining: List[MapItem]): Unit = {
                if (remaining.nonEmpty) {
                  val name = remaining.head.country
                  val (batch, next) = remaining.partition(_.country == name)
                  val total = batch.foldLeft(0) { (sum, x) => sum + x.num }
                  addEventTarget(name)
                  flipSleeperCells(name, total)
                  flipNextBatch(next)
                }
              }
              flipNextBatch(toFlip)
            }
            else
              for (c <- game.countries; p <- c.plots)  {// Alert all plots on the map
                addEventTarget(c.name)
                performAlert(c.name, humanPickPlotToAlert(c.name))
              }
          }
          // Even of neither action above was possible.
          decreasePrestige(1)
        }
        else {
          // See Event Instructions table
          if (game hasCountry (_.hasPlots))
            for (c <- game.countries; p <- c.plots)  {// Alert all plots on the map
              addEventTarget(c.name)
              performAlert(c.name, humanPickPlotToAlert(c.name))
            }
          else {
            val candidates = countryNames(game.countries filter (_.sleeperCells > 0))
            def flipNext(numLeft: Int, targets: List[String]): Unit = {
              if (numLeft > 0 && targets.nonEmpty) {
                var name = USBot.disruptPriority(targets).get
                val c = game getCountry name
                addEventTarget(name)
                val num = numLeft min c.sleeperCells
                flipSleeperCells(name, num)
                flipNext(numLeft - num, targets filterNot (_ == name))
              }
            }
            
            flipNext((game.sleeperCellsOnMap + 1) / 2, candidates)
          }
          decreasePrestige(1)
        }
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(162, "SCAF", US, 3,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (role: Role, forTrigger: Boolean) =>
        (role == game.humanRole && (game hasMuslim scafCandidate)) ||
        (role == game.botRole && (game hasMuslim (m => !m.isAlly && scafCandidate(m))))
      ,
      (role: Role) => {
        val candidates = countryNames(game.muslims filter scafCandidate)
        val target = if (role == game.humanRole)
          askCountry("Select country: ", candidates)
        else 
          USBot.scafTarget(candidates).get

        addEventTarget(target)
        val m = game.getMuslim(target)
        setAlignment(target, Ally)
        if (m.isPoor)
          improveGovernance(target, 1, canShiftToGood = false)
        removeCellsFromCountry(target, m.activeCells, m.sleeperCells, m.hasSadr, addCadre = true)
        addReactionMarker(target, m.totalCells)
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(163, "Status Quo", US, 3,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (role: Role, forTrigger: Boolean) =>
        (role == game.humanRole && (game hasMuslim statusQuoCandidate)) ||
        (role == game.botRole && (game hasMuslim (m => !m.isAlly && statusQuoCandidate(m))))
      ,  
      (role: Role) => {
        val candidates = countryNames(game.muslims filter statusQuoCandidate)
        val target = if (role == game.humanRole)
          askCountry("Select country: ", candidates)
        else 
          USBot.statusQuoTarget(candidates).get  // See Event Instructions table
        
        addEventTarget(target)
        endRegimeChange(target)
        setAlignment(target, Ally)
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(164, "Bloody Thursday", Jihadist, 1,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (role: Role, forTrigger: Boolean) => globalEventNotInPlay(BloodyThursday) || (game hasMuslim (_.awakening > 0))
      ,
      (role: Role) => {
        addGlobalEventMarker(BloodyThursday)
        val candidates = countryNames(game.muslims filter (_.awakening > 0))
        if (candidates.nonEmpty) {
          val target = if (role == game.humanRole)
            askCountry("Select country with awakening marker: ", candidates)
          else
            JihadistBot.markerAlignGovTarget(candidates).get

          addEventTarget(target)
          removeAwakeningMarker(target)
        }
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(165, "Coup", Jihadist, 1,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (role: Role, forTrigger: Boolean) =>
        (role == game.humanRole && (game hasMuslim coupCandidate)) ||
        (role == game.botRole && (game hasMuslim (m => !m.isIslamistRule && coupCandidate(m))))
      ,
      (role: Role) => {
        val target = if (role == game.humanRole) {
          val candidates = countryNames(game.muslims filter coupCandidate)
          askCountry("Select country: ", candidates)
        }
        else {
          val candidates = countryNames(game.muslims filter (m => !m.isIslamistRule && coupCandidate(m)))
          JihadistBot.goodPriority(candidates).get
        }
        
        addEventTarget(target)
        startCivilWar(target)
        addBesiegedRegimeMarker(target)
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(166, "Ferguson", Jihadist, 1,
      NoRemove, Lapsing, NoAutoTrigger, DoesNotAlertPlot, AlwaysPlayable,
      (role: Role) => {
        log("Jihadist player my block 1 US associated event played later this turn.")
        if (role == game.botRole)
          log("The Jihadist Bot will cancel the NEXT US associated event played by the US")
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(167, "Houthi Rebels", Jihadist, 1,
      Remove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (role: Role, forTrigger: Boolean) => {
        game.getMuslim(Yemen).isPoor &&
        (game.isNonMuslim(Iran) || !game.getMuslim(Iran).isAlly) &&
        !(game.getMuslim(Yemen).civilWar && game.cellsAvailable == 0)
      }
      ,
      (role: Role) => {
        addEventTarget(Yemen)
        addSleeperCellsToCountry(Yemen, 2 min game.cellsAvailable)
        startCivilWar(Yemen)
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(168, "IEDs", Jihadist, 1,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (role: Role, forTrigger: Boolean) => game hasMuslim (m =>
        (m.inRegimeChange || m.civilWar) && m.totalCells > 0 && m.totalTroops > 0
      )
      ,  
      (role: Role) => {
        log("US player randomly discards one card")
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(169, "Islamic Maghreb", Jihadist, 1,
      NoRemove, Lapsing, NoAutoTrigger, DoesNotAlertPlot,
      (role: Role, forTrigger: Boolean) => if (role == game.botRole)
        (game hasMuslim islamicMaghrebCandidate) && (game.funding < 8 || game.cellsAvailable > 0)
      else  
        (game hasMuslim islamicMaghrebCandidate) && (game.funding < 9 || game.cellsAvailable > 0)
      ,  
      (role: Role) => {
        val (target, action) = if (role == game.humanRole) {
          val candidates = countryNames(game.muslims filter islamicMaghrebCandidate)
          val t = askCountry("Select country: ", candidates)
          val two = (game.getMuslim(t).civilWar || game.isCaliphateMember(t))
          val choices = List(
            "cells" -> (if (two) "Place cells" else "Place a cell"),
            "funding" -> "Increase funding")
          (t, askMenu(choices).head)
        }
        else {
          val maghrebs = game.muslims filter islamicMaghrebCandidate
          val better = maghrebs filter (m => m.civilWar || game.isCaliphateMember(m.name))
          val candidates = countryNames(if (better.nonEmpty) better else maghrebs)
          val t = JihadistBot.travelToTarget(candidates).get
          val action = if (game.funding < 8) "funding" else "cells"
          log(s"Jihadist Bot chooses $t")
          (t, action)
        }
        
        addEventTarget(target)
        val num = if (game.getMuslim(target).civilWar || game.isCaliphateMember(target)) 2 else 1
        if (action == "funding")
          increaseFunding(num)
        else
          addSleeperCellsToCountry(target, num min game.cellsAvailable)
        rollCountryPosture(Serbia)
        log("Travel to/within Schengen countries requires a roll for the rest this turn")
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(170, "Theft of State", Jihadist, 1,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (role: Role, forTrigger: Boolean) => game hasMuslim theftOfStateCandidate
      ,
      (role: Role) => {
        val candidates = countryNames(game.muslims filter theftOfStateCandidate)
        val target = if (role == game.humanRole)
          askCountry("Select country: ", candidates)
        else
          JihadistBot.markerAlignGovTarget(candidates).get

        addEventTarget(target)
        removeAwakeningMarker(target)
        addReactionMarker(target)
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(171, "Abu Ghraib Jail Break", Jihadist, 2,
      Remove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot, AlwaysPlayable,
      (role: Role) => {
        addEventTarget(Iraq)
        testCountry(Iraq)
        addActiveCellsToCountry(Iraq, 1 min game.cellsAvailable)
        addReactionMarker(Iraq)
        decreasePrestige(1)
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(172, "Al-Shabaab", Jihadist, 2,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot, AlwaysPlayable,
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
          val canReaction = reactionCandidates.nonEmpty && lapsingEventNotInPlay(ArabWinter)
          val canCell     = game.cellsAvailable > 0
          val canPlot1    = game.availablePlots contains Plot1
          val canPlot2    = game.availablePlots contains Plot2
          val canBesiege  = besiegeCandidates.nonEmpty
          val choices = List(
            choice(canReaction,"reaction", "Place 1 Reaction marker"),
            choice(canCell,    "cell",     "Place 1 cell"),
            choice(canPlot1,   "plot1",    "Place a level 1 plot"),
            choice(canPlot2,   "plot2",    "Place a level 2 plot"),
            choice(canBesiege, "besiege",  "Place a besieged regime marker"),
            choice(true,       "draw",     "Select Pirates, Boko Haram, or Islamic Maghreb from discard pile")
          ).flatten 
          println("Do any 2 of the following:")
          askMenu(choices, 2, repeatsOK = false) foreach { action =>
            println()
            action match {
              case "reaction" =>
                val target = askCountry("Place reaction marker in which country: ", reactionCandidates)
                addEventTarget(target)
                testCountry(target)
                addReactionMarker(target)
              case "cell" =>
                val target = askCountry("Place a cell in which country: ", candidates)
                addEventTarget(target)
                testCountry(target)
                addSleeperCellsToCountry(target, 1)
              case "plot1" =>
                val target = askCountry("Place a level 1 plot in which country: ", candidates)
                addEventTarget(target)
                testCountry(target)
                addAvailablePlotToCountry(target, Plot1)
              case "plot2" =>
                val target = askCountry("Place a level 2 plot in which country: ", candidates)
                addEventTarget(target)
                testCountry(target)
                addAvailablePlotToCountry(target, Plot2)
              case "besiege"  =>
                val target = askCountry("Place besieged regime marker in which country: ", besiegeCandidates)
                addEventTarget(target)
                testCountry(target)
                addBesiegedRegimeMarker(target)
              case _ =>
                log("Select Pirates, Boko Haram, or Islamic Maghreb from discard pile")
            }
          }
        }
        else {
          // See Event Instructions table
          val candidates = Somalia::Sudan::Yemen::Nil
          val muslims    = candidates map game.getMuslim filter (!_.isIslamistRule)
          val besiegeTarget  = countryNames(muslims filter (!_.besiegedRegime)).headOption
          val reactionTarget = if (lapsingEventNotInPlay(ArabWinter))
            None
          else
            countryNames(muslims filter (_.canTakeAwakeningOrReactionMarker)).headOption
          val cellTarget     = if (game.cellsAvailable > 0)
            muslims.headOption map (_.name) orElse Some(KenyaTanzania)
          else
            None
          val plotTarget = if (game.availablePlots exists (p => p == Plot1 || p == Plot2))
            muslims.headOption map (_.name) orElse Some(KenyaTanzania)
          else
            None
          val actions = List(
            besiegeTarget  map (_ => "besiege"),
            cellTarget     map (_ => "cell"),
            reactionTarget map (_ => "reaction"),
            plotTarget     map (_ => "plot"),
            Some("draw")
          ).flatten take 2
          actions foreach { action =>
            println()
            action match {
              case "besiege"  =>
                addEventTarget(besiegeTarget.get)
                testCountry(besiegeTarget.get)
                addBesiegedRegimeMarker(besiegeTarget.get)
              case "cell" =>
                addEventTarget(cellTarget.get)
                testCountry(cellTarget.get)
                addSleeperCellsToCountry(cellTarget.get, 1)
              case "reaction" =>
                addEventTarget(reactionTarget.get)
                testCountry(reactionTarget.get)
                addReactionMarker(reactionTarget.get)
              case "plot" =>
                val plot = (game.availablePlots.sorted dropWhile (p => p != Plot1 && p != Plot2)).head
                addEventTarget(plotTarget.get)
                testCountry(plotTarget.get)
                addAvailablePlotToCountry(plotTarget.get, plot)
              case _ =>
                log("Select Pirates, Boko Haram, or Islamic Maghreb from discard pile")
                log("and place it on top of the the Jihadist's hand of cards")
            }
          }
        }
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(173, "Arab Winter", Jihadist, 2,
      NoRemove, Lapsing, NoAutoTrigger, DoesNotAlertPlot, AlwaysPlayable,
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
          else
            JihadistBot.multipleTargets(2, candidates, JihadistBot.markerAlignGovTarget)

          addEventTarget(targets:_*)
          targets foreach (removeAwakeningMarker(_))
          log("No more Awakening/Reaction markers may be placed this turn.")
        }
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(174, "Boston Marathon", Jihadist, 2,
      Remove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (role: Role, forTrigger: Boolean) => {
        // The Bot will not play this event if the GWOT penalty is > 0 for fear
        // that the US posture may change and the GWOT penalty would be lost.
        // This restriction does not apply when the event is being triggered upon
        // the US card play.
        (role == game.humanRole || game.gwotPenalty == 0 || forTrigger)
      }
      ,
      (role: Role) => {
        // See Event Instructions table
        rollUSPosture()
        increaseFunding(1)
        addEventTarget(Caucasus)
        rollCountryPosture(Caucasus)
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(175, "Censorship", Jihadist, 2,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot, AlwaysPlayable,
      (role: Role) => {
        val candidates = countryNames(game.muslims filter (_.awakening > 0))
        if (candidates.isEmpty)
          log(s"There are no reaction markers on the map")
        else {
          val target = if (role == game.humanRole) 
            askCountry("Select country: ", candidates)
          else
            JihadistBot.markerAlignGovTarget(candidates).get

          addEventTarget(target)
          val m = game.getMuslim(target)
          removeAwakeningMarker(target, 2 min m.awakening)
        }
        
        removeGlobalEventMarker(Smartphones)
        addGlobalEventMarker(Censorship)
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(176, "Change of State", Jihadist, 2,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (role: Role, forTrigger: Boolean) => if (role == game.botRole)
        game hasMuslim botChangeOfStateCandidate
      else
        game hasMuslim changeOfStateCandidate
      ,
      (role: Role) => {
        val candidates = if (role == game.botRole)
          countryNames(game.muslims filter botChangeOfStateCandidate)
        else
          countryNames(game.muslims filter changeOfStateCandidate)
        
        val target = if (role == game.humanRole) 
          askCountry("Select country: ", candidates)
        else
          JihadistBot.changeOfStateTarget(candidates).get

        addEventTarget(target)
        // Strip the country of all markers and make it Untested
        setCountryToUntested(target)
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(177, "Gaza Rockets", Jihadist, 2,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (role: Role, forTrigger: Boolean) =>
        (game.availablePlots contains Plot1) &&
        (role == game.humanRole || game.funding < 9) // Bot unplayable if funding == 9
      ,
      (role: Role) => {
        // See Event Instructions table
        val num = 2 min (game.availablePlots count (_ == Plot1))
        addEventTarget(Israel)
        for (i <- 1 to num)
          addAvailablePlotToCountry(Israel, Plot1)
        decreaseFunding(1)
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(178, "Ghost Soldiers", Jihadist, 2,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (role: Role, forTrigger: Boolean) => game hasMuslim ghostSoldiersCandidate
      ,
      (role: Role) => {
        val candidates = countryNames(game.muslims filter ghostSoldiersCandidate)
        val target = if (role == game.humanRole) 
          askCountry("Select country: ", candidates)
        else 
          JihadistBot.troopsMilitiaTarget(candidates).get

        addEventTarget(target)
        val m = game.getMuslim(target)
        removeMilitiaFromCountry(target, (m.militia + 1) / 2) // Half rounded up
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(179, "Korean Crisis", Jihadist, 2,
      NoRemove, Lapsing, NoAutoTrigger, DoesNotAlertPlot,
      (role: Role, forTrigger: Boolean) => (game.troopsAvailable + game.troopsOnMap) > 0
      ,
      (role: Role) => {
        import JihadistBot.troopsToTakeOffMap
        // Take troops from available if possible, otherwise we must 
        // ask the user where to take them from.
        val numToRemove  = 2 min (game.troopsAvailable + game.troopsOnMap)
        val numFromTrack = numToRemove min game.troopsAvailable
        val numFromMap   = numToRemove - numFromTrack
        
        val countries = if (numFromMap == 0)
          Nil
        else if (role == game.humanRole) {
          val targets = game.muslims filter (_.troops > 0) map (m => MapItem(m.name, m.troops))
          println(s"Select ${amountOf(numFromMap, "troop")} from the map to remove")
          askMapItems(targets.sortBy(_.country), numFromMap, "troop")
        }
        else
          troopsToTakeOffMap(numFromMap, countryNames(game.muslims filter (_.troops > 0)))
        
        for (MapItem(name, num) <- MapItem("track", numFromTrack) :: countries; if num > 0) {
          if (name != "track")
            addEventTarget(name)
          takeTroopsOffMap(name, num)
        }
        addEventTarget(China)
        setCountryPosture(China, oppositePosture(game.usPosture))
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(180, "Mosul Central Bank", Jihadist, 2,
      Remove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (role: Role, forTrigger: Boolean) => game hasMuslim (m => m.civilWar && m.totalCells > 0)
      ,
      (role: Role) => increaseFunding(2)
    )),
    // ------------------------------------------------------------------------
    entry(new Card(181, "NPT Safeguards Ignored", Jihadist, 2,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,  
      (role: Role, forTrigger: Boolean) =>
        game.getCountry(Iran).wmdCache   > 0 &&
        game.getCountry(Iran).totalCells > 0 &&
        countryEventNotInPlay(Iran, TradeEmbargoUS)
      ,
      (role: Role) => {
        addEventTarget(Iran)
        val die = getDieRoll(role)
        val success = die < 4
        log(s"Die roll: $die")
        if (success) {
          log("Success")
          moveWMDCacheToAvailable(Iran)
          // Card only removed if die roll was successful
          removeCardFromGame(181)
        }
        else {
          log("Failure")
          val c = game.getCountry(Iran)
          if (c.activeCells > 0)
            removeCellsFromCountry(Iran, 1, 0, false, addCadre = true)
          else
            removeCellsFromCountry(Iran, 0, 1, false, addCadre = true)
        }
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(182, "Paris Attacks", Jihadist, 2,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (role: Role, forTrigger: Boolean) => parisAttacksPossible
      ,
      (role: Role) => {
        // Note: There is a slight chance that the Bot could execute this event,
        // and get a black die roll for only plots or only cells and there are 
        // plot/cells available.
        val countries = List(UnitedStates, Canada, UnitedKingdom, Benelux, France)
        val validSchengen = for (s <- Schengen; if game.getNonMuslim(s).isHard) yield s
        println()
        val tanDie     = getDieRoll(role, "Enter tan die: ")
        val blackDie   = getDieRoll(role, "Enter black die: ")
        val (plots, activeCells) = blackDie match {
          case 1 => (Plot1::Plot2::Nil, 0)
          case 2 => (Plot3::Nil, 2)
          case 3 => (Plot2::Nil, 1)
          case 4 => (Plot2::Nil, 0)
          case 5 => (Plot1::Nil, 1)
          case _ => (Plot1::Nil, 0)
        }
        def isHard(name: String) = game.getNonMuslim(name).isHard
        def getTarget(list: List[String]): Option[String] = {
          list.drop(tanDie - 1) match {
            case Nil if validSchengen.nonEmpty => Some("Schengen")
            case Nil                           => None
            case x::xs if isHard(x)            => Some(x)
            case x::xs                         => getTarget(xs)
          }
        }
        def placePlots(name: String, plotList: List[Plot]): Unit = plotList match {
          case Nil =>
          case p::ps =>
            if (game.availablePlots contains p)
              addAvailablePlotToCountry(name, p)
            placePlots(name, ps)
        }
        
        log(s"Tan die: $tanDie,  Black die: $blackDie")
        val target = getTarget(countries drop (tanDie - 1)) map {
          case "Schengen" if role == game.humanRole =>
            askCountry("Choose a Hard Schengen country: ", validSchengen)
          case "Schengen" => JihadistBot.plotTarget(validSchengen).get
          case name => name
        }
        
        target match {
          case None =>
            log("No Hard country was selected for the Paris Attacks event")
          case Some(name) =>
            addEventTarget(name)
            log(s"$name is the target of the Paris Attacks event")
            placePlots(name, plots)
            addActiveCellsToCountry(name, activeCells min game.cellsAvailable)
        }
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(183, "Pirates", Jihadist, 2,
      Remove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (role: Role, forTrigger: Boolean) => globalEventNotInPlay(MaerskAlabama) && pirates2ConditionsInEffect
      ,
      (role: Role) => addGlobalEventMarker(Pirates2)
    )),
    // ------------------------------------------------------------------------
    entry(new Card(184, "Sequestration", Jihadist, 2,
      Remove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (role: Role, forTrigger: Boolean) => game.usPosture == Soft
      ,
      (role: Role) => {
        import JihadistBot.troopsToTakeOffMap
        // Take troops from available if possible, otherwise we must 
        // ask the user where to take them from.
        val numToRemove  = 3 min (game.troopsAvailable + game.troopsOnMap)
        val numFromTrack = numToRemove min game.troopsAvailable
        val numFromMap   = numToRemove - numFromTrack
        val countries = if (numFromMap == 0)
          Nil
        else if (role == game.humanRole) {
          val targets = game.muslims filter (_.troops > 0) map (m => MapItem(m.name, m.troops))
          println(s"Select ${amountOf(numFromMap, "troop")} from the map to remove")
          askMapItems(targets.sortBy(_.country), numFromMap, "troop")
        }
        else
          troopsToTakeOffMap(numFromMap, countryNames(game.muslims filter (_.troops > 0)))
        
        log("US player draws one card from the discard pile")
        log("Must draw \"Obama Doctrine\" if it is available")
        // See Event Instructions table
        if (game.botRole == US) {
          log("US Bot will draw \"Obama Doctrine\", or \"Congress Acts\", or")
          log("randomly among the US associated cards with the highest Ops")
        }
        
        for (MapItem(name, num) <- MapItem("track", numFromTrack) :: countries) {
          if (name != "track")
            addEventTarget(name)
          takeTroopsOffMap(name, num)
        }
        game = game.copy(params = game.params.copy(sequestrationTroops = true))
        addGlobalEventMarker(Sequestration)
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(185, "al-Maliki", Jihadist, 3,
      Remove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (role: Role, forTrigger: Boolean) => game hasCountry (_.totalTroops > 0)
      ,
      (role: Role) => {
        // The only non-muslim country that may contain troops is the Philippines
        // if (abu Sayyaf is in effect)
        val candidates = countryNames(game.countries filter (_.totalTroops > 0))
        val target = if (role == game.humanRole)
          askCountry("Select country: ", candidates)
        else {
          // See Event Instructions table
          // Only choose the Philippines if it is the only candidate
          val eligibleMuslims = game.muslims filter (_.totalTroops > 0)
            if (eligibleMuslims.isEmpty)
              (game.countries filter (_.totalTroops > 0) map (_.name)).head
            else {
              val candidates = {
                countryNames(if (eligibleMuslims exists (_.isAlly))
                  eligibleMuslims filter (_.isAlly)
                else if (eligibleMuslims exists (_.isNeutral))
                  eligibleMuslims filter (_.isNeutral)
                else
                  eligibleMuslims)
            }
            JihadistBot.troopsMilitiaTarget(candidates).get
          }
        }

        addEventTarget(target)
        if (game isNonMuslim target)
          moveTroops(target, "track", game.getCountry(target).troops)
        else {
          val m = game.getMuslim(target)
          moveTroops(target, "track", m.troops)
          removeAllTroopsMarkers(target)
          setAlignment(target, Neutral)
          addAidMarker(target)
          endRegimeChange(target)
        }
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(186, "Boko Haram", Jihadist, 3,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (role: Role, forTrigger: Boolean) =>
        (game.availablePlots exists (p => p == Plot2 || p == Plot3)) || game.cellsAvailable > 0
      ,
      (role: Role) => {
        addEventTarget(Nigeria)
        testCountry(Nigeria)
        if (role == game.humanRole) {
          val havePlots = game.availablePlots exists (p => p == Plot2 || p == Plot3)
          val haveCells = game.cellsAvailable > 0
          val choices = List(
            choice(havePlots, "plot",  "Place a level 2 or level 3 Plot in Nigeria"),
            choice(haveCells, "cells", "Place up to 3 cells in Nigeria")
          ).flatten
          if (choices.size > 1)
            println("Choose one of:")
          askMenu(choices).head match {
            case "plot" =>
              val options = Seq(Plot2, Plot3) filter game.availablePlots.contains map {
                case Plot2 => "2"
                case Plot3 => "3"
              }
              val plotNum = if (options.size == 1)
                options.head
              else
                askOneOf("What level plot? (2 or 3) ", options).get
              plotNum match {
                case "2" => addAvailablePlotToCountry(Nigeria, Plot2)
                case "3" => addAvailablePlotToCountry(Nigeria, Plot3)
              }
              
            case "cells" =>
              val maxCells = 3 min game.cellsAvailable
              val num = askInt("Place how many cells", 1, maxCells, Some(maxCells))
              addSleeperCellsToCountry(Nigeria, num)
              if (num == 3 && canDeclareCaliphate(Nigeria) && askDeclareCaliphate(Nigeria))
                declareCaliphate(Nigeria)
          }
          log()
          log("Jihadist player may return Boko Haram to hand by")
          log("discarding a non-US associated 3 Ops card")
        }
        else {
          // See Event Instructions table
          // We know that either cells are available or a Plot2/3 is available from
          // the eventConditions().
          val plot = (game.availablePlots.sorted filter (p => p == Plot2 || p == Plot3)).headOption
          val placeCells = if (plot.isEmpty) true
          else if (game.cellsAvailable == 0) false
          else if (game isMuslim Nigeria) true  // Muslim priority to cells
          else false                            // non-Muslim priority to plot
          if (placeCells)  {
            val numCells = 3 min game.cellsAvailable
            addSleeperCellsToCountry(Nigeria, numCells)
            if (numCells == 3 && JihadistBot.willDeclareCaliphate(Nigeria))
              declareCaliphate(Nigeria)
          }
          else
            addAvailablePlotToCountry(Nigeria, plot.get)
        }
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(187, "Foreign Fighters", Jihadist, 3,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (role: Role, forTrigger: Boolean) => {
        val candidates = game.muslims filter (m => m.inRegimeChange || m.civilWar)
        candidates.nonEmpty && 
        (game.cellsAvailable > 0 || (candidates exists (m => m.aidMarkers > 0 || !m.besiegedRegime)))
      }
      ,
      (role: Role) => {
        val candidates = if (game.cellsAvailable > 0)
          countryNames(game.muslims filter (m => m.inRegimeChange || m.civilWar))
        else
          countryNames(game.muslims filter (m => (m.inRegimeChange || m.civilWar) &&
                                                  (m.aidMarkers > 0 || !m.besiegedRegime)))
        val target = if (role == game.humanRole)
          askCountry("Select country: ", candidates)
        else
          JihadistBot.recruitTravelToPriority(candidates).get

        addEventTarget(target)
        val m = game.getMuslim(target)
        val numCells = 5 min game.cellsAvailable
        addSleeperCellsToCountry(target, numCells)
        if (m.aidMarkers > 0)
          removeAidMarker(target, 1)
        else if (!m.besiegedRegime)
          addBesiegedRegimeMarker(target)
        
        if (numCells >= 3 &&
            canDeclareCaliphate(target) &&
            ((role == game.humanRole && askDeclareCaliphate(target)) ||
             (role == game.botRole && JihadistBot.willDeclareCaliphate(target))))
          declareCaliphate(target)
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(188, "ISIL", Jihadist, 3,
      Remove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot, AlwaysPlayable,
      (role: Role) => {
        def canCivilWar(m: MuslimCountry) =
          !m.civilWar       && 
          !m.isIslamistRule &&
          !m.isGood         &&
          m.totalTroopsAndMilitia <= m.totalCells
          
        for (name <- Seq(Syria, Iraq); if canCivilWar(game.getMuslim(name))) {
          addEventTarget(name)
          testCountry(name)
          startCivilWar(name)
        }
        decreasePrestige(1)
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(189, "Jihadist Videos", Jihadist, 3,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (role: Role, forTrigger: Boolean) => game.cellsAvailable > 0 // Ignores funding
      ,
      (role: Role) => {
        var targets = if (role == game.humanRole)
          askCountries(3, countryNames(game.countries filter (_.totalCells == 0)))
        else {
          // See Event Instructions table
          val eligible = countryNames(game.countries filter (m => m.totalCells == 0 && !m.isIslamistRule))
          
          if (eligible contains UnitedStates)
            UnitedStates :: JihadistBot.multipleTargets(2, eligible filterNot (_ == UnitedStates), 
                                                JihadistBot.recruitTravelToPriority)
          else 
            JihadistBot.multipleTargets(3, eligible, JihadistBot.recruitTravelToPriority)
        }
        
        val numCells = if (role == game.botRole && game.jihadistIdeology(Potent)) {
          log(s"$Jihadist Bot with Potent Ideology places two cells for each recruit success")
          2
        }
        else
          1
        // Process all of the targets
        for (target <- targets) {
          addEventTarget(target)
          testCountry(target)
          
          val c = game.getCountry(target)
          if (game.cellsAvailable > 0) {
            val cells = numCells min game.cellsAvailable
            if (c.autoRecruit) {
              log(s"Recruit in $target succeeds automatically")
              addSleeperCellsToCountry(target, cells)
            }
            else {
              val die = getDieRoll(role)
              log(s"Die roll: $die")
              if (c.recruitSucceeds(die)) {
                log(s"Recruit in $target succeeds with a die roll of $die")
                addSleeperCellsToCountry(target, cells)
              }
              else {
                log(s"Recruit in $target fails with a die roll of $die")
                addCadreToCountry(target)
              }
            }
          }
          else
            addCadreToCountry(target)
        }
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(190, "Martyrdom Operation", Jihadist, 3,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (role: Role, forTrigger: Boolean) => game.availablePlots.nonEmpty && (game hasCountry martyrdomCandidate)
      ,
      (role: Role) => {
        val candidates = countryNames(game.countries filter martyrdomCandidate)
        val (target, (active, sleeper, sadr), plots) = if (role == game.humanRole) {
          val target = askCountry("Select country: ", candidates)
          val cell = askCells(target, 1, sleeperFocus = false)
          (target, cell, askAvailablePlots(2, ops = 3))
        }
        else {
          // See Event Instructions table
          val target = JihadistBot.plotTarget(candidates).get
          addEventTarget(target)
          val c = game getCountry target
          val cell = if (c.activeCells > 0) (1, 0, false)
                     else if (c.hasSadr)    (0, 0, true)
                      else                  (0, 1, false)
          (target, cell, shuffle(game.availablePlots) take 2)
        }
        
        addEventTarget(target)
        removeCellsFromCountry(target, active, sleeper, sadr, addCadre = false)
        for (plot <- plots)
          addAvailablePlotToCountry(target, plot)
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(191, "Muslim Brotherhood", Jihadist, 3,
      Remove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (role: Role, forTrigger: Boolean) => lapsingEventNotInPlay(ArabWinter)
      ,
      (role: Role) => {
        val candidates = countryNames(game.muslims filter (_.canTakeAwakeningOrReactionMarker))
        
        val other2 = if (role == game.humanRole)
          askCountries(2, candidates)
        else 
          JihadistBot.multipleTargets(2, candidates, JihadistBot.markerAlignGovTarget)
        
        val targets = if (game.getMuslim(Egypt).canTakeAwakeningOrReactionMarker)
          Egypt :: other2
        else {
          log("Egypt cannot currently take a reaction marker.")
          other2
        }
        for (target <- targets) {
          addEventTarget(target)
          testCountry(target)
          addReactionMarker(target)
        }
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(192, "Quagmire", Jihadist, 3,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (role: Role, forTrigger: Boolean) =>
        (game.prestigeLevel == Medium || game.prestigeLevel == Low) &&
        (game hasMuslim (m => m.inRegimeChange && m.totalCells > 0))
      ,
      (role: Role) => {
        if (role == game.botRole) {
          log(s"You ($US) must randomly discard two cards")
          log("Playable Jihadist events on the discards are triggered")
          
          def nextDiscard(num: Int): List[Int] = {
            if (num > 2)
              Nil
            else {
              val prompt = s"Card # of the ${ordinal(num)} discard (or blank if none) "
              askCardNumber(prompt) match {
                case None         => Nil
                case Some(cardNo) =>  cardNo :: nextDiscard(num + 1)
              }
            }
          }
        
          for (n <- nextDiscard(1); card = deck(n))
            if (card.eventWillTrigger(Jihadist))
              performCardEvent(card, Jihadist, triggered = true)
        }
        else
          log(s"Discard the top two cards of the $Jihadist Bot's hand")
        
        setUSPosture(Soft)
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(193, "Regional al-Qaeda", Jihadist, 3,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (role: Role, forTrigger: Boolean) => game.cellsAvailable > 0 && (game.muslims count regionalAlQaedaCandidate) >= 2
      ,
      (role: Role) => {
        val candidates = countryNames(game.muslims filter regionalAlQaedaCandidate)
        val maxPer = if (game.numIslamistRule > 0) 2 else 1
        case class Target(name: String, cells: Int)
        val targets = if (role == game.humanRole) {
          if (game.cellsAvailable == 1) {
            val name = askCountry("Select country: ", candidates)
            Target(name, 1)::Nil
          }
          else if (maxPer == 2 && game.cellsAvailable < 4) {
            println(s"There are only ${game.cellsAvailable} available cells")
            val name1 = askCountry("Select 1st unmarked country: ", candidates)
            val num1  = askInt(s"Place how many cells in $name1", 1, 2)
            val remain = game.cellsAvailable - num1
            if (remain == 0)
              Target(name1, num1)::Nil
            else {
              val name2 = askCountry("Select 2nd unmarked country: ", candidates filterNot (_ == name1))
              Target(name1, num1):: Target(name2, remain)::Nil
            }
          }
          else {
            val name1 = askCountry("Select 1st unmarked country: ", candidates)
            val name2 = askCountry("Select 2nd unmarked country: ", candidates filterNot (_ == name1))
            Target(name1, maxPer):: Target(name2, maxPer)::Nil
          }
        }
        else {
          def nextTarget(available: Int, targets: List[String]): List[Target] = targets match {
            case Nil => Nil
            case t::ts => 
              val n = maxPer min available
              Target(t, n) :: nextTarget(available - n, ts)
          }
          var names = JihadistBot.multipleTargets(2, candidates, JihadistBot.recruitTravelToPriority)
          nextTarget(game.cellsAvailable, names)  
        }
        
        for (Target(name, num) <- targets) {
          addEventTarget(name)
          testCountry(name)
          addSleeperCellsToCountry(name, num)
        }
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(194, "Snowden", Jihadist, 3,
      Remove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (role: Role, forTrigger: Boolean) =>
        game.prestige > 1                                    ||
        game.usPosture != game.getNonMuslim(Russia).posture  ||
        game.usPosture != game.getNonMuslim(Germany).posture
      ,
      (role: Role) => {
        decreasePrestige(2)
        addEventTarget(Russia)
        addEventTarget(Germany)
        setCountryPosture(Russia, oppositePosture(game.usPosture))
        setCountryPosture(Germany, oppositePosture(game.usPosture))
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(195, "Taliban Resurgent", Jihadist, 3,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (role: Role, forTrigger: Boolean) => if (role == game.humanRole)
          game hasMuslim talibanResurgentCandidate
      else {
        val candidates = countryNames(game.muslims filter talibanResurgentCandidate)
        JihadistBot.talibanResurgentTarget(candidates).nonEmpty
      }
      ,
      (role: Role) => {
        // See Event Instructions table
        val candidates = countryNames(game.muslims filter talibanResurgentCandidate)
        val (target, plots, (actives, sleepers, sadr)) = if (role == game.humanRole) {
          val name = askCountry("Select country: ", candidates)
          val plots = askPlots(game.availablePlots filterNot (_ == PlotWMD), 2)
          println("Choose cells to remove:")
          (name, plots, askCells(name, 3, sleeperFocus = false))
        }
        else {
          val name = JihadistBot.talibanResurgentTarget(candidates).get
          val plots = (game.availablePlots.sorted filterNot (_ == PlotWMD)) take 2
          (name, plots, JihadistBot.chooseCellsToRemove(name, 3))
        }
        addEventTarget(target)
        removeCellsFromCountry(target, actives, sleepers, sadr, addCadre = false)
        plots foreach { p => addAvailablePlotToCountry(target, p) }
        performJihads(JihadTarget(target, 2, 0, false, major = false)::Nil, ignoreFailures = true)
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(196, "Training Camps", Jihadist, 3,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (role: Role, forTrigger: Boolean) => {
        if (role == game.humanRole)
          game hasMuslim (m => !(m.isUntested || m.isGood))
        else // See Event Instructions table
          game hasMuslim (m => !(m.isUntested || m.isGood) && !m.autoRecruit)
      }
      ,
      (role: Role) => {
        val target = if (role == game.humanRole) {
          val candidates = countryNames(game.muslims filter (m => !(m.isUntested || m.isGood)))
          askCountry("Place Training Camps in which country: ", candidates)
        }
        else {
          // See Event Instructions table
          val candidates = 
            countryNames(game.muslims filter (m => !(m.isUntested || m.isGood && !m.autoRecruit)))
          JihadistBot.recruitTravelToPriority(candidates).get
        }
        println()
        val priorCapacity = game.trainingCampCapacity
        game.trainingCamp foreach { name => 
          removeEventMarkersFromCountry(name, TrainingCamps)
        }
        addEventTarget(target)
        addEventMarkersToCountry(target, TrainingCamps)
        updateTrainingCampCapacity(priorCapacity)
        val cellsToAdd = game.cellsAvailable min 2
        addSleeperCellsToCountry(target, cellsToAdd)
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(197, "Unconfirmed", Jihadist, 3,
      Remove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot, AlwaysPlayable,
      (role: Role) => {
        // See Event Instructions table
        if (role == game.humanRole)
          log("Draw one of the indicated cards from the discard pile.")
        else
          log(s"The $Jihadist Bot draws the candidate card nearest the top of the dicard pile")
        decreasePrestige(1)
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(198, "US Atrocities", Jihadist, 3,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (role: Role, forTrigger: Boolean) => game hasMuslim usAtrocitiesCandidate
      ,
      (role: Role) => {
        val alignCandidates = countryNames(game.muslims filter usAtrocitiesCandidate)
        val postureCandidates = countryNames(game.nonMuslims filter (n => n.isUntested && !n.isSchengen))
        var (alignTarget, postureTarget) = if (role == game.humanRole) {
          val at = askCountry(s"Select country for alignment shift: ", alignCandidates)
          val pt = if (postureCandidates.isEmpty)
            None
          else {
            val name = askCountry(s"Select posture of which country: ", postureCandidates)
            val pos = askOneOf(s"New posture for $name (Soft or Hard): ", Seq(Soft, Hard)).get
            
            Some(name, pos)
          }
          (at, pt)
        }
        else {
          val at = JihadistBot.markerAlignGovTarget(alignCandidates).get
          val pt = if (postureCandidates.isEmpty)
            None
          else
            Some(shuffle(postureCandidates).head, oppositePosture(game.usPosture))
          (at, pt)
        }
        
        addEventTarget(alignTarget)
        shiftAlignmentRight(alignTarget)
        postureTarget foreach {
          case (name, posture) => 
            addEventTarget(name)
            setCountryPosture(name, posture)
        }
        decreasePrestige(1)
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(199, "US Consulate Attacked", Jihadist, 3,
      NoRemove, Lapsing, NoAutoTrigger, DoesNotAlertPlot, AlwaysPlayable,
      (role: Role) => {
        decreasePrestige(2)
        if (role == game.humanRole)
          log(s"Discard the top card of the $US hand")
        else
          log(s"You ($US) must discard one random card")
        log("If US Elections is played later this turn, US Posture switches automatically")
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(200, "Critical Middle", Unassociated, 1,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (role: Role, forTrigger: Boolean) => role match {
        case US       => game hasMuslim criticalMiddleUSCandidate
        case Jihadist => game hasMuslim criticalMiddleJihadistCandidate
      }
      ,
      (role: Role) => {
        // See Event Instructions table
        val usCandidates = countryNames(game.muslims filter criticalMiddleUSCandidate)
        val jiCandidates = countryNames(game.muslims filter criticalMiddleJihadistCandidate)
        val (target, action, from) = (role, role == game.humanRole) match {
          case (US, true) =>
            val target = askCountry("Select country: ", usCandidates)
            val m = game getMuslim target
            val choices = List(
              choice(m.canTakeAwakeningOrReactionMarker, "awakening", "Place an awakening marker"),
              choice(!m.isAlly,                          "shiftLeft", "Shift alignment towards Ally")
            ).flatten
            val action = if (choices.isEmpty) None
            else askMenu(choices).headOption
            (target, action, Nil)
              
          case (Jihadist, true) =>
            val target = askCountry("Select country: ", jiCandidates)
            val m = game getMuslim target
            val choices = List(
              choice(game.cellsAvailable > 0, "cells",      "Place cells"),
              choice(!m.isAdversary,          "shiftRight", "Shift alignment towards Adversary")
            ).flatten
            val action = if (choices.isEmpty) None
            else askMenu(choices).headOption
            val from = if (action == Some("cells")) {
              val sources = countryNames(game.countries filter (c => c.name != target && c.totalCells > 0))
              askCellsFromAnywhere(2, true, sources, sleeperFocus = false)
            }
            else
              Nil
              (target, action, from)
            
          case (US, false) =>
            USBot.criticalMiddleShiftPossibilities(usCandidates) match {
              case Nil => (USBot.markerAlignGovTarget(usCandidates).get, Some("awakening"), Nil)
              case xs  => (USBot.markerAlignGovTarget(xs).get, Some("shiftLeft"), Nil)
            }
          case (Jihadist, false) =>
            JihadistBot.criticalMiddleShiftPossibilities(jiCandidates) match {
              case Nil =>
                val target = JihadistBot.travelToTarget(jiCandidates).get
                def nextFrom(countries: List[String], remaining: Int): List[CellsItem] =
                  if (remaining == 0)
                    Nil
                  else if (game.cellsAvailable > 0) {
                    val n = remaining min game.cellsAvailable
                    CellsItem("track", 0, n)::nextFrom(countries, remaining - n)
                  }
                  else if (countries.isEmpty)
                    Nil
                  else {
                    JihadistBot.travelFromTarget(target, countries) match {
                      case Some(name) =>
                        val c = (game getCountry name)
                        val n = remaining min c.totalCells
                        val a = n min c.activeCells
                        val s = n - a
                        CellsItem(name, a, s)::nextFrom(countries filterNot (_ == name), remaining - n)
                      case None => Nil
                    }
                  }

                val fromCandidates = countryNames(game.countries filter (c => c.name != target && JihadistBot.hasCellForTravel(c)))
                (target, Some("cells"), nextFrom(fromCandidates, 2))
                
              case xs  => (JihadistBot.markerAlignGovTarget(xs).get, Some("shiftRight"), Nil)
            }
        }
        
        addEventTarget(target)
        val m = game getMuslim target
        action match {
          case None               => log(s"$target is already and Ally and cannot take an awakening marker")
          case Some("awakening")  => addAwakeningMarker(target)
          case Some("shiftLeft")  => shiftAlignmentLeft(target)
          case Some("shiftRight") => shiftAlignmentRight(target)
          case _ => // place cells
            from foreach {
              case CellsItem("track", _, n) => addSleeperCellsToCountry(target, n)
              case CellsItem(name, a, s)    =>
                moveCellsBetweenCountries(name, target, a, true)
                moveCellsBetweenCountries(name, target, s, false)
            }
        }
        log()
        log("IMPORTANT!")
        log("Place the \"Critical Middle\" card in the approximate middle of the draw pile")
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(201, "Cross Border Support", Unassociated, 1,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (role: Role, forTrigger: Boolean) => crossBorderSupportPlayable(role, forTrigger)
      ,
      (role: Role) => {
        // See Event Instructions table
        val cellsOK   = game.cellsAvailable > 0
        val militiaOK = game.militiaAvailable > 0 && (game.getCountries(African) exists (_.canTakeMilitia))
        val (target, action) = if (role == game.humanRole) {
          val choices = List(
            choice(militiaOK, "militia", "Place militia"),
            choice(cellsOK,   "cells",   "Place cells")
          ).flatten
          println("What would you like to do:")
          val action = askMenu(choices).head
          val candidates = if (action == "militia")
            countryNames(game.getCountries(African) filter (_.canTakeMilitia))
          else
            countryNames(game.getCountries(African))
          val name = askCountry("Select African country: ", candidates)
          (name, action)
        }
        else if (role == US) {
          val candidates = USBot.highestCellsMinusTandM(countryNames(game.getCountries(African) filter (_.canTakeMilitia)))
          val target = shuffle(candidates).head
          (target, "militia")
        }
        else {
          val target = JihadistBot.recruitTravelToPriority(African).get
          (target, "cells")
        }
        
        addEventTarget(target)
        testCountry(target)
        val bump = (target == Mali || target == Nigeria)
        if (action == "militia") {
          val num = (if (bump) 2 else 1) min game.militiaAvailable
          addMilitiaToCountry(target, num)
        }
        else {
          // Can possibly declare Caliphate (only Mali or Muslim Nigeria)
          val num = (if (bump) 3 else 2) min game.cellsAvailable
          addSleeperCellsToCountry(target, num)
          if (num == 3 &&
              canDeclareCaliphate(target) &&
              ((role == game.humanRole && askDeclareCaliphate(target)) ||
               (role == game.botRole && JihadistBot.willDeclareCaliphate(target))))
            declareCaliphate(target)
        }
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(202, "Cyber Warfare", Unassociated, 1,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (role: Role, forTrigger: Boolean) => cyberWarfarePlayable(role, forTrigger)
      ,
      (role: Role) => {
        val COUNTRIES = List(China, Russia, India)
        val opRes = if (role == US) game.reserves.jihadist else game.reserves.us
        val cadres = game hasCountry (_.hasCadre)
        // See Event Instructions table
        if (role == game.humanRole) {
          val choices = List(
            choice(opRes > 0, "reserves", s"Steal opponent's ${amountOf(opRes,"reserve Op")}"),
            choice(true,      "posture",  "Set the posture of China, Russia, or India"),
            choice(true,      "place",    "Place a cadre"),
            choice(cadres,    "remove",   "Remove a cadre")
          ).flatten
          println("Choose 1 option:")
          askMenu(choices).head match {
            case "reserves" =>
              clearReserves(if (role == US) Jihadist else US)
              addToReserves(role, opRes)
            case "posture"  =>
              val name    = askCountry("Set the posture of which country? ", COUNTRIES)
              val posture = askOneOf(s"New posture for $name (Soft or Hard): ", Seq(Soft, Hard)).get
              addEventTarget(name)
              setCountryPosture(name, posture)
            case "place"    =>
              val candidates = countryNames(game.countries filter (c => !c.hasCadre && c.totalCells == 0))
              val name = askCountry("Place a cadre in which country? ", candidates)
              addEventTarget(name)
              testCountry(name)
              addCadreToCountry(name)
            case _          => 
              val candidates = countryNames(game.countries filter (c => c.hasCadre ))
              val name = askCountry("Remove cadre from which country? ", candidates)
              addEventTarget(name)
              removeCadreFromCountry(name)
          }
        }
        else if (role == US) {
          countryNames(game.getNonMuslims(COUNTRIES) filter (c => c.isUntested || c.posture != game.usPosture)) match {
            case Nil =>
              clearReserves(Jihadist)
              addToReserves(role, opRes)
            case xs =>
              val name = USBot.markerAlignGovTarget(xs).get
              addEventTarget(name)
              testCountry(name)
              setCountryPosture(name, game.usPosture)
          }
        }
        else {
          countryNames(game.getNonMuslims(COUNTRIES) filter (c => c.isUntested || c.posture == game.usPosture)) match {
            case Nil =>
              clearReserves(US)
              addToReserves(role, opRes)
            case xs =>
              val name = JihadistBot.markerAlignGovTarget(xs).get
              addEventTarget(name)
              testCountry(name)
              setCountryPosture(name, oppositePosture(game.usPosture))
          }
        }
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(203, "Day of Rage", Unassociated, 1,
      Remove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (role: Role, forTrigger: Boolean) => (game getMuslim Yemen).canTakeAwakeningOrReactionMarker
      ,
      (role: Role) => {
        addEventTarget(Yemen)
        testCountry(Yemen)
        if (role == US)
          addAwakeningMarker(Yemen)
        else
          addReactionMarker(Yemen)
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(204, "Ebola Scare", Unassociated, 1,
      Remove, USLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (role: Role, forTrigger: Boolean) =>
        role == game.humanRole ||
        role == US ||
        cacheQuestion(askYorN(s"Does the $US player have any cards in hand (y/n) ?"))
      ,  
      (role: Role) => {
        // See Event Instructions table
        if (role == Jihadist) {
          if (role == game.humanRole)
            log(s"Discard the top card of the $US hand")
          else
            log(s"You ($US) must discard one random card")
        }
        else {
          val withTroops = countryNames(game.muslims filter (_.troops > 0))
          val source = if (game.troopsAvailable > 0)
            "track"
          else if (role == game.humanRole)
            askCountry("Remove troop from which country? ", withTroops)
          else {
            USBot.ebolaScareTarget(withTroops).get
          }
          takeTroopsOffMap(source, 1)
          increasePrestige(2)
        }
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(205, "Erdogan Effect", Unassociated, 1,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (role: Role, forTrigger: Boolean) => role == game.humanRole || {
        val mm = game getMuslims (Turkey::Iraq::Syria::Nil) 
        (role == Jihadist && 
          (game.cellsAvailable > 0 || 
          ((mm exists (_.canTakeAwakeningOrReactionMarker)) && lapsingEventNotInPlay(ArabWinter)))) ||
        (role == US && (mm exists (m => m.reaction > 0 || m.totalCells > 0)))
      }
      ,
      (role: Role) => {
        // See Event Instructions table
        val candidates = List(Turkey, Serbia, Iraq, Caucasus, Syria, Iran)
        if (role == game.humanRole) {
          val name = askCountry("Select country: ", candidates)
          val choices = if (game isMuslim name) {
            val m = game getMuslim name
            val canAwake = m.canTakeAwakeningOrReactionMarker && lapsingEventNotInPlay(ArabWinter)
            val canBesiege = !(m.besiegedRegime || m.isGood || m.isIslamistRule)
            val canMilitia = game.militiaAvailable > 0 && !(m.besiegedRegime || m.isGood || m.isIslamistRule)
            List(
              choice(m.canTakeAidMarker,      "+aid", "Place aid marker"),
              choice(m.aidMarkers > 0,        "-aid", "Remove aid marker"),
              choice(canBesiege,              "+bsg", "Place besiged regime marker"),
              choice(m.besiegedRegime,        "-bsh", "Remove besieged regime marker"),
              choice(canAwake,                "+awa", "Place awakening marker"),
              choice(m.awakening > 0,         "-awa", "Remove awakening marker"),
              choice(canAwake,                "+rea", "Place reaction marker"),
              choice(m.reaction > 0,          "-rea", "Remove reaction marker"),
              choice(canMilitia,              "+mil", "Place 2 milita"),
              choice(m.militia > 0,           "-mil", "Remove 2 militia"),
              choice(game.cellsAvailable > 0, "+cel", "Place 2 cells"),
              choice(m.totalCells > 0,        "-cel", "Remove 2 cells")
            ).flatten
          }
          else {
            val n = game getNonMuslim name
            List(
              choice(n.isUntested && n.canChangePosture,  "+pos", "Place posture marker"),
              choice(!n.isUntested && n.canChangePosture, "-pos", "Remove posture marker"),
              choice(game.cellsAvailable > 0,             "+cel", "Place 2 cells"),
              choice(n.totalCells > 0,                    "-cel", "Remove 2 cells")
            ).flatten
          }
          if (choices.isEmpty)
            log(s"There are no valid action that can be taken in $name")
          else {
            addEventTarget(name)
            testCountry(name)
            println("Choose 1: ")
            askMenu(choices).head match {
              case "+aid" => addAidMarker(name)
              case "-aid" => removeAidMarker(name)
              case "+bsg" => addBesiegedRegimeMarker(name)
              case "-bsh" => removeBesiegedRegimeMarker(name)
              case "+awa" => addAwakeningMarker(name)
              case "-awa" => removeAwakeningMarker(name)
              case "+rea" => addReactionMarker(name)
              case "-rea" => removeReactionMarker(name)
              case "+mil" => addMilitiaToCountry(name, 2 min game.militiaAvailable)
              case "-mil" => removeMilitiaFromCountry(name, 2 min (game getMuslim name).militia)
              case "+cel" => addSleeperCellsToCountry(name, 2 min game.cellsAvailable)
              case "-cel" =>
                val (actives, sleepers, sadr) = askCells(name, 2, role == US)
                removeCellsFromCountry(name, actives, sleepers, sadr, addCadre = true)
              case "+pos" =>
                val posture = askOneOf(s"New posture for $name (Soft or Hard): ", Seq(Soft, Hard)).get
                setCountryPosture(name, posture) 
              case "-pos" => setCountryPosture(name, PostureUntested) 
            }
          }
        }
        else if (role == Jihadist ) {
          // Bot only affects cells or reaction markers.
          if (game.cellsAvailable > 0) {
            val name = JihadistBot.recruitTravelToPriority(Turkey::Iraq::Syria::Nil).get
            addEventTarget(name)
            testCountry(name)
            addSleeperCellsToCountry(name, 2 min game.cellsAvailable)
          }
          else {
            val mm = game getMuslims (Turkey::Iraq::Syria::Nil) 
            val candidates = countryNames(mm filter (_.canTakeAwakeningOrReactionMarker))
            val name = JihadistBot.markerAlignGovTarget(candidates).get
            addEventTarget(name)
            testCountry(name)
            addReactionMarker(name)
          }
        }
        else {  // US Bot
          val mm = game getMuslims (Turkey::Iraq::Syria::Nil) 
          if (mm exists (_.totalCells > 0)) {
            val candidates = countryNames(mm filter (_.totalCells > 0))
            val name = USBot.disruptPriority(candidates).get
            addEventTarget(name)
            val (actives, sleepers, sadr) = USBot.chooseCellsToRemove(name, 2)
            removeCellsFromCountry(name, actives, sleepers, sadr, addCadre = true)
          }
          else {
            val candidates = countryNames(mm filter (_.reaction > 0))
            val name = USBot.markerAlignGovTarget(candidates).get
            addEventTarget(name)
            removeReactionMarker(name)
          }
        }
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(206, "Friday of Anger", Unassociated, 1,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (role: Role, forTrigger: Boolean) =>
        (role == Jihadist && (game hasMuslim (_.awakening > 0))) ||
        (role == US       && (game hasMuslim (_.reaction > 0)))
      ,
      (role: Role) => {
        if (role == Jihadist) {
           val candidates = countryNames(game.muslims filter (_.awakening > 0))
           val name = if (role == game.humanRole)
             askCountry("Place reaction marker in which country? ", candidates)
           else
             JihadistBot.markerAlignGovTarget(candidates).get
           addEventTarget(name)
           addReactionMarker(name)
        }
        else {
          val candidates = countryNames(game.muslims filter (_.reaction > 0))
          val name = if (role == game.humanRole)
            askCountry("Place awakening marker in which country? ", candidates)
          else
            USBot.markerAlignGovTarget(candidates).get
          addEventTarget(name)
          addAwakeningMarker(name)
        }
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(207, "JV / Copycat", Unassociated, 1,
      NoRemove, NoLapsing, NoAutoTrigger,
      (_, _) => {
        val us = game getNonMuslim UnitedStates
        us.hasPlots || (us.totalCells == 0 && !us.hasCadre && (game hasNonMuslim (_.hasPlots)))
      }
      ,
      (role: Role, forTrigger: Boolean) => 
        (role == Jihadist && (game.cellsAvailable > 0 || (game.availablePlots contains Plot1))) ||
        (role == US && (game hasNonMuslim (n => n.hasCadre || n.totalCells > 0 || n.hasPlots)))
      ,
      (role: Role) => {
        // See Event Instructions table
        if (role == Jihadist) {
          if (role == game.humanRole) {
            val candidates = countryNames(game.nonMuslims)
            val name = askCountry("Select country: ", countryNames(game.nonMuslims))
            addEventTarget(name)
            testCountry(name)
            val choices = List(
              choice(game.cellsAvailable > 0,            "cell", "Place a cell"),
              choice(game.availablePlots contains Plot1, "plot", "Place a Plot 1")
            ).flatten
            println("Select 1:")
            askMenu(choices).head match {
              case "cell" => addSleeperCellsToCountry(name, 1)
              case "plot" => addAvailablePlotToCountry(name, Plot1)
            }
          }
          else {  // Jihadist Bot
            addEventTarget(UnitedStates)
            if (game.cellsAvailable > 0 && 
                  ((game.availablePlots contains PlotWMD) || 
                    game.funding == 9 ||
                    !(game.availablePlots contains Plot1))) {
              addSleeperCellsToCountry(UnitedStates, 1)          
            }
            else
              addAvailablePlotToCountry(UnitedStates, Plot1)
          }
        }
        else {  // US 
          if (role == game.humanRole) {
            val candidates = 
              countryNames(game.nonMuslims filter (n => n.hasCadre || n.totalCells > 0 || n.hasPlots))
            val name = askCountry("Select country: ", countryNames(game.nonMuslims))
            val n = game getNonMuslim name
            addEventTarget(name)
            testCountry(name)
            val choices = List(
              choice(n.totalCells > 0, "cell" , "Remve a cell"),
              choice(n.hasCadre,       "cadre", "Remove cadre"),
              choice(n.hasPlots,       "plot" , "Alert a plot")
            ).flatten
            println("Select 1:")
            askMenu(choices).head match {
              case "cell"  => 
                // Use the Bot routine to pick a sleeper first
                val (actives, sleepers, sadr) = USBot.chooseCellsToRemove(name, 1)
                removeCellsFromCountry(name, actives, sleepers, sadr, addCadre = true)
              case "cadre" => removeCadreFromCountry(name)
              case "plot"  => performAlert(name, humanPickPlotToAlert(name))
            }
          }
          else {  // US Bot
            val criteria = (n: NonMuslimCountry) => n.hasCadre || n.totalCells > 0 || n.hasPlots
            val name = if (criteria(game getNonMuslim UnitedStates))
              UnitedStates
            else {
              val candidates = countryNames(game.nonMuslims filter criteria)
              USBot.disruptPriority(candidates).get
            }
            addEventTarget(name)
            val n = game getNonMuslim name
            if (n.hasPlots)
              performAlert(name, USBot.selectPriorityPlot(name::Nil).onMap)
            else if (n.totalCells > 0) {
              val (actives, sleepers, sadr) = USBot.chooseCellsToRemove(name, 1)
              removeCellsFromCountry(name, actives, sleepers, sadr, addCadre = true)
            }
            else
              removeCadreFromCountry(name)
          }
        }
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(208, "Kinder - Gentler", Unassociated, 1,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (role: Role, forTrigger: Boolean) => role == game.humanRole ||
        (role == Jihadist && game.troopCommitment == Overstretch) ||
        (role == US       && game.troopCommitment == LowIntensity)
      ,
      (role: Role) => {
        // See Event Instructions table
        if (game.troopCommitment == Overstretch) {
          increaseFunding(1)
          decreasePrestige(1)
        }
        else if (game.troopCommitment == LowIntensity) {
          decreaseFunding(1)
          increasePrestige(1)
        }
        else
          log("The event has no effect because the troop comittment is War")
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(209, "Quds Force", Unassociated, 1,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (role: Role, forTrigger: Boolean) => qudsForcePlayable(role, forTrigger)
      ,
      (role: Role) => if (role == Jihadist) { // See Event Instructions table
        val candidates = countryNames(game.muslims filter (_.militia > 0))
        val name = if (role == game.humanRole)
          askCountry("Select country with militia: ", candidates)
        else
          JihadistBot.minorJihadTarget(candidates).get
        
        addEventTarget(name)
        val m = game getMuslim name
        val numMilitia = (if (m.isSunni) 1 else 2) min m.militia
        removeMilitiaFromCountry(name, numMilitia)
      }
      else {  // US
        val candidates = countryNames(game.muslims filter (_.totalCells > 0))
        val name = if (role == game.humanRole)
          askCountry("Select country with cells: ", candidates)
        else
          USBot.disruptPriority(candidates).get
        
        addEventTarget(name)
        val m = game getMuslim name
        val numCells = if (m.isSunni) 1 else 2
        val (actives, sleepers, sadr) = if (role == game.humanRole)
          askCells(name, numCells, sleeperFocus = true)
        else
          USBot.chooseCellsToRemove(name, numCells)
        removeCellsFromCountry(name, actives, sleepers, sadr, addCadre = true)
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(210, "Sectarian Violence", Unassociated, 1,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (role: Role, forTrigger: Boolean) =>
        (role == Jihadist && (game hasMuslim (_.awakening > 0))) ||
        (role == US       && (game hasMuslim (_.reaction > 0)))
      ,
      (role: Role) => if (role == Jihadist) {
        val candidates = countryNames(game.muslims filter (_.awakening > 0))
        val name = if (role == game.humanRole)
          askCountry("Remove awakening marker from which country: ", candidates)
        else
          JihadistBot.markerAlignGovTarget(candidates).get
        addEventTarget(name)
        removeAwakeningMarker(name)
      }
      else {  // US
        val candidates = countryNames(game.muslims filter (_.reaction > 0))
        val name = if (role == game.humanRole)
          askCountry("Remove reaction marker from which country: ", candidates)
        else
          USBot.markerAlignGovTarget(candidates).get
        addEventTarget(name)
        removeReactionMarker(name)
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(211, "Smartphones", Unassociated, 1,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (role: Role, forTrigger: Boolean) => game hasMuslim smartPhonesCandidate
      ,
      (role: Role) => {
        val candidates = countryNames(game.muslims filter smartPhonesCandidate)
        val name = if (role == game.humanRole)
          askCountry("Select country: ", candidates)
        else if (role == Jihadist)
          JihadistBot.markerAlignGovTarget(candidates).get
        else
          USBot.markerAlignGovTarget(candidates).get
        
        addEventTarget(name)
        testCountry(name)
        if (role == Jihadist)
          addReactionMarker(name)
        else
          addAwakeningMarker(name)
        removeGlobalEventMarker(Censorship)
        addGlobalEventMarker(Smartphones)
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(212, "Smartphones", Unassociated, 1,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (role: Role, forTrigger: Boolean) => deck(211).eventConditions(role, forTrigger),
      (role: Role) => deck(211).executeEvent(role)
    )),
    // ------------------------------------------------------------------------
    entry(new Card(213, "Smartphones", Unassociated, 1,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (role: Role, forTrigger: Boolean) => deck(211).eventConditions(role, forTrigger),
      (role: Role) => deck(211).executeEvent(role)
    )),
    // ------------------------------------------------------------------------
    entry(new Card(214, "3 Cups of Tea", Unassociated, 2,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (role: Role, forTrigger: Boolean) =>
        (game hasMuslim (m => m.isUntested)) ||
        (role == Jihadist && globalEventNotInPlay(ThreeCupsOfTea))
      ,
      (role: Role) => {
        val candidates = countryNames(game.muslims filter (m => m.isUntested))
        if (candidates.nonEmpty) {
          val name = if (role == game.humanRole)
            askCountry("Select unmarked Muslim country: ", candidates)
          else if (role == Jihadist)
            JihadistBot.markerAlignGovTarget(candidates).get
          else
            USBot.markerAlignGovTarget(candidates).get
          
          addEventTarget(name)
          testCountry(name)
          if (role == Jihadist)
            addReactionMarker(name)
          else
            addAwakeningMarker(name, 2)
          
        }
        if (role == Jihadist) {
          addGlobalEventMarker(ThreeCupsOfTea)
        }
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(215, "Abu Bakr al-Baghdadi", Unassociated, 2,
      USRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (role: Role, forTrigger: Boolean) =>
        (role == US && game.caliphateDeclared) ||
        (role == Jihadist && game.cellsAvailable > 0)
      ,
      (role: Role) => if (role == US) {
        increasePrestige(3)
      }
      else {
        // See Event Instructions table
        // Can possibly declare Caliphate, only in Syria or Iraq
        if (role == game.humanRole) {
          println("Place 3 cells in Syria and/or Iraq")
          val inSyria = askInt("How many do you wish to place in Syria? ", 0, 3)
          val inIraq = 3 - inSyria
          val targets = List((Syria, inSyria),(Iraq, inIraq)) filterNot (_._2 == 0)
          for ((target, num) <- targets) {
            addEventTarget(target)
            testCountry(target)
            val withCells = countryNames(game.countries filter (c => c.name != target && c.totalCells > 0))
            println()
            println(s"Choose ${amountOf(num, "cell")} to place in $target")
            val sources = askCellsFromAnywhere(num, true, withCells, sleeperFocus = false) 
            println()
            sources foreach {
              case CellsItem("track", _ , n) => addSleeperCellsToCountry(target, n)
              case CellsItem(name, a, s)     =>
                moveCellsBetweenCountries(name, target, a, true)
                moveCellsBetweenCountries(name, target, s, false)
            }
            // Count how many were actually placed (based on availability)
            val totalPlaced = (sources map (_.total)).sum
            if (totalPlaced == 3 && canDeclareCaliphate(target) && askDeclareCaliphate(target))
              declareCaliphate(target)
          }
          
          val action = if (game.cellsAvailable == 0) "draw"
          else {
            val choices = List(
              "cell" -> "Place a cell from the track in a random Schengen country",
              "draw" -> "Draw \"Paris Attacks\" or \"Training Camps\" from the discard pile")
            println()
            println("Choose 1:")
            askMenu(choices).head match {
              case "draw" => log("Jihadist draws \"Paris Attacks\" or \"Training Camps\" from the discard pile")
              case "cell" => 
                val schengen = randomSchengenCountry
                addEventTarget(schengen.name)
                testCountry(schengen.name)
                addSleeperCellsToCountry(schengen.name, 1)
            }
          }
        }
        else {
          val target = JihadistBot.recruitTravelToPriority(Syria::Iraq::Nil).get
          addEventTarget(target)
          testCountry(target)
          val num = 3 min game.cellsAvailable
          addSleeperCellsToCountry(target, num)
          if (num == 3 && canDeclareCaliphate(target) && JihadistBot.willDeclareCaliphate(target))
            declareCaliphate(target)
          
          if (askYorN("Is \"Paris Attacks\" or \"Training Camps\" in the discard pile (y/n)? "))
            log("Jihadist draws the card nearest the bottom of the discard pile")
          else if (game.cellsAvailable > 0) {
            val schengen = randomSchengenCountry
            addEventTarget(schengen.name)
            testCountry(schengen.name)
            addSleeperCellsToCountry(schengen.name, 1)
          }
        }
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(216, "Abu Sayyaf (ISIL)", Unassociated, 2,
      USRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (role: Role, forTrigger: Boolean) =>
        game hasMuslim (m => m.oilExporter && 
        (m.isIslamistRule || m.inRegimeChange || m.civilWar))
      ,
      (role: Role) => if (role == US) {
        increasePrestige(2)
        decreaseFunding(1)
      }
      else { // Jihadist
        increaseFunding(if (game.caliphateDeclared) 3 else 2)
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(217, "Agitators", Unassociated, 2,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (role: Role, forTrigger: Boolean) => (role == game.humanRole) || {
        val cards = List("Coup", "ISIL", "Free Syrian Army", "Benghazi Falls", "Operation Serval",
                     "Revolution", "Houthi Rebels", "Congress Acts")
        println("The following cards cause Civil War or Regime Change")
        cards foreach println
        cacheQuestion(askYorN("Is at least one of these cards in the discard pile (y/n)? "))
      }
      ,
      (role: Role) => {
        // See Event Instructions table
        if (role == game.botRole)
          log(s"$role takes the candidate card nearest the bottom of the discard pile")
        else
          log(s"$role takes one of the candidate cards from the discard pile")
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(218, "Al-Nusra Front", Unassociated, 2,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (role: Role, forTrigger: Boolean) =>
        (role == game.humanRole && (game hasMuslim alNusraFrontCandidate)) ||
        (role == Jihadist && (game hasMuslim (m => alNusraFrontCandidate(m) && m.militia > m.totalCells))) ||
        (role == US       && (game hasMuslim (m => alNusraFrontCandidate(m) && m.totalCells > m.militia))) 
      ,
      (role: Role) => {
        // See Event Instructions table
        val candidates = countryNames(game.muslims filter alNusraFrontCandidate)
        val target = if (role == game.humanRole)
          askCountry("Select country: ", candidates)
        else if (role == Jihadist) {
          val candidates = countryNames(game.muslims filter (m => alNusraFrontCandidate(m) && m.militia > m.totalCells))
          JihadistBot.minorJihadTarget(candidates).get
        }
        else {
          val candidates = countryNames(game.muslims filter (m => alNusraFrontCandidate(m) && m.totalCells > m.militia))
          USBot.disruptPriority(USBot.highestCellsMinusTandM(candidates)).get
        }
        
        addEventTarget(target)
        val m = game getMuslim target
        if (m.totalCells > m.militia) {
          val (actives, sleepers, sadr) = USBot.chooseCellsToRemove(target, m.totalCells - m.militia)
          removeCellsFromCountry(target, actives, sleepers, sadr, addCadre = true)
        }
        else if (m.militia > m.totalCells)
          removeMilitiaFromCountry(target, m.militia - m.totalCells)
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(219, "Ayman al-Zawahiri", Unassociated, 2,
      USRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (role: Role, forTrigger: Boolean) =>
        (role == US && (game.funding > 1 || game.prestige < 12)) ||
        (role == Jihadist && game.prestige > 1 && (game hasMuslim (_.reaction > 0)))
      ,
      (role: Role) => {
        if (role == US) {
          // Values adjusted if the #237 Osama Bin Laden card has been rmoved 
          val num = if (game.cardRemoved(237)) 2 else 1
          decreaseFunding(1)
          increasePrestige(num)
        }
        else {
          val totalReactionMarkers = (game.muslims map (_.reaction)).sum
          // Prestige is decrease 1 for every 4 reaction markers on the map (rounded up)
          val num = (totalReactionMarkers + 3) / 4
          decreasePrestige(num)
        }
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(220, "Daraa", Unassociated, 2,
      Remove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (role: Role, forTrigger: Boolean) => {
        val syria = game getMuslim Syria
        val canAwakeReact = syria.canTakeAwakeningOrReactionMarker && lapsingEventNotInPlay(ArabWinter)
        !syria.isIslamistRule && (
          (role == US && canAwakeReact) ||
          (role == Jihadist && (!syria.isPoor || canAwakeReact))
        )
      }
      ,
      (role: Role) => {
        val syria = game getMuslim Syria
        addEventTarget(Syria)
        testCountry(Syria)
        if (role == US) {
          improveGovernance(Syria, 1, canShiftToGood = false)
          addAwakeningMarker(Syria)
        }
        else {  // Jihadist
          degradeGovernance(Syria, 1, canShiftToIR = false)
          addReactionMarker(Syria)
        }
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(221, "FlyPaper", Unassociated, 2,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (role: Role, forTrigger: Boolean) => {
        val candidates = flyPaperCandidates
        if (role == game.humanRole)
          candidates.nonEmpty
        else if (role == Jihadist && candidates.nonEmpty) {
          // Bot will only remove cells.  Make sure there are cell in a country
          // other than the target country.
          val target = JihadistBot.recruitTravelToPriority(candidates).get
          game hasCountry (c => c.name != target && c.totalCells > 0)
        }
        else if (role == US && candidates.nonEmpty) {
          val lowResource = (game getMuslims candidates map (_.resources)).min
          val target = shuffle(countryNames(game getMuslims candidates filter (_.resources == lowResource))).head
          (game hasMuslim (m => m.name != target && m.reaction > 0)) ||
          (game hasCountry (c => c.name != target && c.totalCells > 0))
        }
        else
          false
      }
      ,
      (role: Role) => {
        val candidates = flyPaperCandidates
        // Can possibly declare Caliphate, by either player
        // See Event Instructions table
        if (role == game.humanRole) {
          val totalReaction = game.muslims count (_.reaction > 0)
          val totalCells    = game.countries count (_.totalCells > 0)
          val numReaction = if (totalReaction == 0) 0
                            else askInt("Remove how many reaction markers? ", 0, 3 min totalReaction)
          
          val reactionCountries = if (numReaction == 0)
            Nil
          else {
            val withReaction = countryNames(game.muslims filter (_.reaction > 0))
            val names = if (numReaction == 1)
              askCountry("Remove reaction marker from which country? ", withReaction)::Nil
            else {
              println("Remove reaction markers from which countries?")
              askCountries(numReaction, withReaction)
            }
            for (name <- names)
              removeReactionMarker(name)
            names
          }
          
          val numCells = if (totalCells == 0 || numReaction == 3) 0
                         else askInt("Remove how many cells? ", 0, (3 - numReaction) min totalCells)
          if (numCells > 0) {
            val withCells = countryNames(game.countries filter 
                          (c => !(reactionCountries contains c.name) && c.totalCells > 0))
            val names = if (numCells == 1)
              askCountry("Remove a cell from which country? ", withCells)::Nil
            else {
              println("Remove cells from which countries?")
              askCountries(numCells, withCells)
            }
            
            case class Cells(name: String, cells: (Int, Int, Boolean))
            
            val cells = for (name <- names; c = askCells(name, 1, sleeperFocus = role == US))
              yield Cells(name, c)
            
            for (Cells(name, (a, s, sadr)) <- cells)
              removeCellsFromCountry(name, a, s, sadr, addCadre = true)
          }
          
          val cellsToPlace = (numReaction + numCells) min game.cellsAvailable
          if (cellsToPlace > 0) {
            val name = askCountry(s"Select country to place ${amountOf(cellsToPlace, "cell")}: ", candidates)
            addEventTarget(name)
            addSleeperCellsToCountry(name, cellsToPlace)
            if (cellsToPlace == 3 && canDeclareCaliphate(name) && askDeclareCaliphate(name))
              declareCaliphate(name)
          }                                  
        }
        else if (role == Jihadist) {
          // Bot removes only cells
          val target = JihadistBot.recruitTravelToPriority(candidates).get
          val withCells = countryNames(game.countries filter (c => c.name != target && c.totalCells > 0))
          val countries = if (withCells.size <= 3)
            withCells
          else {
            def nextCountry(num: Int, candidates: List[String]): List[String] =
              if (num > 3 || candidates.isEmpty)
                Nil
              else {
                val country = JihadistBot.travelFromPriorities(target, candidates).get
                country :: nextCountry(num + 1, candidates filterNot (_ == country))
              }
            nextCountry(1, withCells)
          }
          
          for (name <- countries) {
            val (a, s, sadr) = JihadistBot.chooseCellsToRemove(name, 1)
            removeCellsFromCountry(name, a, s, sadr, addCadre = true)
          }
          
          addEventTarget(target)
          addSleeperCellsToCountry(target, countries.size)
          if (countries.size == 3 && canDeclareCaliphate(target) && JihadistBot.willDeclareCaliphate(target))
            declareCaliphate(target)
        }
        else {  // US Bot
          // Will remove reaction markers before cells, 2 max
          val lowResource = (game getMuslims candidates map (_.resources)).min
          val target = shuffle(countryNames(game getMuslims candidates filter (_.resources == lowResource))).head
          val withReaction = countryNames(game.muslims filter (_.reaction > 0))
          def nextReaction(remaining: Int, countries: List[String]): List[String] = {
            if (remaining == 0 || countries.isEmpty)
              Nil
            else {
              val name = USBot.disruptPriority(countries).get
              name :: nextReaction(remaining - 1, countries filterNot (_ == name))
            }
          }
          val reactions = nextReaction(2, withReaction)
          for (name <- reactions)
            removeReactionMarker(name)
            
          val numCells = if (reactions.size == 2)
            0
          else {
            val withCells = countryNames(game.countries filter 
              (c => c.name != target && !(reactions contains c.name) && c.totalCells > 0))
            def nextCell(remaining: Int, countries: List[String]): Int = {
              if (remaining == 0 || countries.isEmpty)
                0
              else {
                val name = USBot.disruptPriority(countries).get
                val (a, s, sadr) = USBot.chooseCellsToRemove(name, 1)
                removeCellsFromCountry(name, a, s, sadr, addCadre = true)
                1 + nextCell(remaining - 1, countries filterNot (_ == name))
              }
            }
            nextCell(2 - reactions.size, withCells)
          }
          
          val toPlace = reactions.size + numCells
          addEventTarget(target)
          addSleeperCellsToCountry(target, toPlace)
        }
      }  
    )),
    // ------------------------------------------------------------------------
    entry(new Card(222, "Hagel", Unassociated, 2,
      Remove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (role: Role, forTrigger: Boolean) =>
        role == game.humanRole ||
        (role == Jihadist && game.usPosture == Hard) ||
        (role == US && game.usPosture == Soft && (game.islamistResources > 1 || !(game.gwot == (Soft, 2) || game.gwot == (Soft, 3))))
      , // See Event Instructions table
      (role: Role) => setUSPosture(if (role == US) Hard else Soft)
    )),
    // ------------------------------------------------------------------------
    entry(new Card(223, "Iranian Elections", Unassociated, 2,
      Remove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot, AlwaysPlayable,
      (role: Role) => {
        // See Event Instructions table
        log("Flip Iran country mat to its Shia-Mix Muslim side")
        log("Set Iran to Fair Adversary")
        val iran = game.getNonMuslim(Iran)
        game = game.updateCountry(DefaultMuslimIran.copy(
          sleeperCells = iran.sleeperCells,
          activeCells  = iran.activeCells,
          hasCadre     = iran.hasCadre,
          plots        = iran.plots,
          markers      = iran.markers,
          wmdCache     = iran.wmdCache
        ))
        addEventTarget(Iran)
        if (role == US) {
          addAwakeningMarker(Iran, 2)
          addReactionMarker(Iran)
        }
        else {
          addReactionMarker(Iran, 2)
          addAwakeningMarker(Iran)
        }
        decreaseFunding(1)
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(224, "Je Suis Charlie", Unassociated, 2,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (role: Role, forTrigger: Boolean) => {
        val plotOK = game.resolvedPlotTargets exists (name => game isNonMuslim name)
        val nonMuslimPostureChange = game hasNonMuslim (n => n.canChangePosture && n.posture != game.usPosture)
        if (role == game.humanRole)
          plotOK &&
          (role == US ||
           (role == Jihadist && (game.usPosture != game.worldPosture && game.worldPosture != Even) || game.prestige > 1))
        else
          plotOK &&
          ((role == US && nonMuslimPostureChange) ||
           (role == Jihadist && (game.usPosture == Hard && game.worldPosture == Hard) || game.prestige > 1))
      }
      ,
      (role: Role) => {
        // See Event Instructions table
        if (role == US) {
          val nonSchengen = countryNames(game.nonMuslims filter (n => n.canChangePosture && !(Schengen contains n.name)))
          if (role == game.humanRole) {
            val schengen  = askCountry("Select posture of which Schengen country: ", Schengen)
            val posture = askOneOf("New posture (Soft or Hard): ", Seq(Soft, Hard)).get
            addEventTarget(schengen)
            setCountryPosture(schengen, posture)
            
            val other  = askCountry("Select posture of which non-Schengen country: ", nonSchengen)
            val posture2 = askOneOf("New posture (Soft or Hard): ", Seq(Soft, Hard)).get
            addEventTarget(other)
            setCountryPosture(other, posture2)
          }
          else {
            val shengens = game.nonMuslims filter (n => (Schengen contains n.name) && n.canChangePosture && n.posture != game.usPosture)
            val others = game.nonMuslims filter (n => !(Schengen contains n.name) && n.canChangePosture && n.posture != game.usPosture)
            // Either list could be empty...
            USBot.posturePriority(countryNames(shengens)) foreach { name =>
              addEventTarget(name)
              setCountryPosture(name, game.usPosture)
            } 
            USBot.posturePriority(countryNames(others)) foreach { name =>
              addEventTarget(name)
              setCountryPosture(name, game.usPosture)
            } 
          }
        }
        else { // Jihadist
          if (role == game.humanRole) {
            val canPosture = game.usPosture == game.worldPosture && game.worldPosture != Even
            val choices = List(
              choice(canPosture,        "posture" , "Set US posture to opposite of World"),
              choice(game.prestige > 1, "prestige", "Reduce US prestige by 1/2 die roll (rounded up)")
            ).flatten
            if (choices.isEmpty)
              log("The event has no effect")
            else {
              println("Choose 1:")
              askMenu(choices).head match {
                case "posture"  => setUSPosture(oppositePosture(game.worldPosture))
                case "prestige" =>
                  val die = getDieRoll(role)
                  log(s"Die roll: $die")
                  decreasePrestige((die + 1)/ 2)
              }
            }
          }
          else { // Jihadist Bot
            if (game.usPosture == Hard && game.worldPosture == Hard)
              setUSPosture(Soft)
            else {
              val die = getDieRoll(role)
              log(s"Die roll: $die")
              decreasePrestige((die + 1)/ 2)
            }
          }
        }
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(225, "Jihadi John", Unassociated, 2,
      USRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (role: Role, forTrigger: Boolean) =>
        role == game.humanRole ||
        (role == US && game.prestige < 12) ||
        (role == Jihadist && 
         (game.funding < 9 || (game.getNonMuslims(Schengen) exists (n => n.isUntested || n.posture == game.usPosture)))) 
      ,
      (role: Role) => if (role == US) {
        increasePrestige(if (game.caliphateDeclared) 2 else 1)
      }
      else {
        val (name, posture) = if (role == game.humanRole)
          (askCountry("Select Schengen country: ", Schengen),
          askOneOf("New posture (Soft or Hard): ", Seq(Soft, Hard)).get)
        else
          (JihadistBot.posturePriority(Schengen).get, oppositePosture(game.usPosture))
        
        addEventTarget(name)
        setCountryPosture(name, posture)
        increaseFunding(if (game.caliphateDeclared) 2 else 1)
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(226, "Operation Serval", Unassociated, 2,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (role: Role, forTrigger: Boolean) => servalPlayable(role, forTrigger)
      ,
      (role: Role) => if (role == US) {  // See Event Instructions table
        val name = if (role == game.humanRole)
          askCountry("Select country for Serval marker: ", servalCandidates)
        else 
          USBot.servalTarget(servalCandidates).get
        val sameCountry = (game getMuslim name).hasMarker(OperationServal)

        if (!sameCountry) {
          // If marker is already on the map, remove it first.
          game.muslims find (_.hasMarker(OperationServal)) foreach { c =>
            removeEventMarkersFromCountry(c.name, OperationServal)
          }
        }
        addEventTarget(name)
        if (!sameCountry)
          addEventMarkersToCountry(name, OperationServal)
        addMilitiaToCountry(name, 1 min game.militiaAvailable)
      }
      else {  // Jihadist
        val name = if (role == game.humanRole)
          askCountry("Select country: ", servalCandidates)
        else
          JihadistBot.minorJihadTarget(servalCandidates).get
        
        addEventTarget(name)
        startCivilWar(name)
        val numCells = if (name == Mali) 2 else 1
        addSleeperCellsToCountry(name, numCells min game.cellsAvailable)
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(227, "Popular Support", Unassociated, 2,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (role: Role, forTrigger: Boolean) =>
        lapsingEventNotInPlay(ArabWinter) && (
          (role == US && (game hasMuslim (_.awakening > 0))) ||
          (role == Jihadist && (game hasMuslim (_.reaction > 0)))
        )
      ,
      (role: Role) => if (role == US) {
        val candidates = countryNames(game.muslims filter (_.awakening > 0))
        val (name, adjacent) = if (role == game.humanRole) {
          val name = askCountry("Select country with awakening marker: ", candidates)
          val adjCandidates = getAdjacentMuslims(name) filter (x => game.getMuslim(x).canTakeAwakeningOrReactionMarker)
          val adjacent = if (adjCandidates.nonEmpty)
            Some(askCountry("Select an adjacent Muslim country: ", adjCandidates))
          else {
            println("No adjacent countries can take an awakening marker")
            None
          }
          (name, adjacent)
        }
        else {
          val name = USBot.markerAlignGovTarget(candidates).get
          val adjCandidates = getAdjacentMuslims(name) filter (x => game.getMuslim(x).canTakeAwakeningOrReactionMarker)
          val adjacent = USBot.markerAlignGovTarget(adjCandidates)
          (name, adjacent)
        }
        
        addEventTarget(name)
        addAwakeningMarker(name)
        adjacent foreach { adj =>
          addEventTarget(adj)
          testCountry(adj)
          addAwakeningMarker(adj)
        }
      }
      else {
        val candidates = countryNames(game.muslims filter (_.reaction > 0))
        val (name, adjacent) = if (role == game.humanRole) {
          val name = askCountry("Select country with reaction marker: ", candidates)
          val adjCandidates = getAdjacentMuslims(name) filter (x => game.getMuslim(x).canTakeAwakeningOrReactionMarker)
          val adjacent = if (adjCandidates.nonEmpty)
            Some(askCountry("Select an adjacent Muslim country: ", adjCandidates))
          else {
            println("No adjacent countries can take a reaction marker")
            None
          }
          (name, adjacent)
        }
        else {
          val name = JihadistBot.markerAlignGovTarget(candidates).get
          val adjCandidates = getAdjacentMuslims(name) filter (x => game.getMuslim(x).canTakeAwakeningOrReactionMarker)
          val adjacent = JihadistBot.markerAlignGovTarget(adjCandidates)
          (name, adjacent)
        }
        
        addEventTarget(name)
        addReactionMarker(name)
        adjacent foreach { adj =>
          addEventTarget(adj)
          testCountry(adj)
          addReactionMarker(adj)
        }
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(228, "Popular Support", Unassociated, 2,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (role: Role, forTrigger: Boolean) => deck(227).eventConditions(role, forTrigger),
      (role: Role) => deck(227).executeEvent(role)
    )),
    // ------------------------------------------------------------------------
    entry(new Card(229, "Prisoner Exchange", Unassociated, 2,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (role: Role, forTrigger: Boolean) => false   // Not playable in the solo game
      ,
      (role: Role) => ()      // See Event Instructions table
    )),
    // ------------------------------------------------------------------------
    entry(new Card(230, "Sellout", Unassociated, 2,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (role: Role, forTrigger: Boolean) =>
        (role == game.humanRole && (game hasMuslim selloutCandidate)) ||
        (role == Jihadist && (game hasMuslim selloutCandidate))
      ,       // USBot treats as unplayable
      (role: Role) => {
        val (name, (actives, sleepers, sadr), action) = if (role == game.humanRole) {
          val candidates = countryNames(game.muslims filter selloutCandidate)
          val name = askCountry("Select country: ", candidates)
          val m = game getMuslim name
          val numCells = m.totalCells - 1
          println(s"Remove ${amountOf(numCells, "cell")} from $name")
          val cells = askCells(name, numCells, sleeperFocus = role == US)
          val choices = List(
            choice(!m.isPoor,      "gov",   "Worsen governance 1 level"),
            choice(!m.isAdversary, "align", "Shift alignment 1 box toward Adversary")
          ).flatten
          val action = choices match {
            case Nil => None
            case (action, _) :: Nil => Some(action)
            case _ =>
              println("Choose one: ")
              Some(askMenu(choices).head)
          }
          (name, cells, action)
        }
        else {  // JihadistBot
          val candidates = game.muslims filter selloutCandidate
          val most = (candidates map (_.totalCells)).max
          val m = shuffle(candidates filter (_.totalCells == most)).head
          val cells = JihadistBot.chooseCellsToRemove(m.name, m.totalCells - 1)
          val action = if (!m.isAdversary) Some("align")
                       else if (!m.isPoor) Some("gov")
                       else None
          (m.name, cells, action)
        }
        
        addEventTarget(name)
        removeCellsFromCountry(name, actives, sleepers, sadr, addCadre = true)
        increaseFunding((actives + sleepers + 1) / 2)  // half of removed cells rounded up
        action match {
          case Some("gov")   => degradeGovernance(name, 1, canShiftToIR = false)
          case Some("align") => shiftAlignmentRight(name)
          case _             => log(s"$name is already Poor Adversary")
        }
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(231, "Siege of Kobanigrad", Unassociated, 2,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (role: Role, forTrigger: Boolean) =>
        (role == US && (game hasMuslim (m => m.civilWar && m.totalCells > 0))) ||
        (role == Jihadist && (game hasMuslim (m => m.civilWar && m.militia > 0)))
      ,
      (role: Role) => if (role == US) {
        val candidates = countryNames(game.muslims filter (m => m.civilWar && m.totalCells > 0))
        val (name, (actives, sleepers, sadr)) = if (role == game.humanRole) {
          val name = askCountry("Select country: ", candidates)
          val cells = askCells(name, 3 min (game getMuslim name).totalCells, sleeperFocus = true)
          (name, cells)
        }
        else {
          val name = USBot.disruptPriority(candidates).get
          val cells = USBot.chooseCellsToRemove(name, 3 min (game getMuslim name).totalCells)
          (name, cells)
        }
        addEventTarget(name)
        removeCellsFromCountry(name, actives, sleepers, sadr, addCadre = true)
      }
      else {
        val candidates = countryNames(game.muslims filter (m => m.civilWar && m.militia > 0))
        val name = if (role == game.humanRole)
          askCountry("Select country: ", candidates)
        else
          JihadistBot.minorJihadTarget(candidates).get
        
        addEventTarget(name)
        removeMilitiaFromCountry(name, 2 min (game getMuslim name).militia)
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(232, "Trade Embargo", Unassociated, 2,
      USRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot, AlwaysPlayable,
      (role: Role) => if (role == US) {
        val iran = game getCountry Iran
        if (iran.wmdCache > 0) {
          log("Remove the Iraninan WMD from the game")
          increasePrestige(iran.wmdCache)
          iran match {
            case m: MuslimCountry    => game = game.updateCountry(m.copy(wmdCache = 0))
            case n: NonMuslimCountry => game = game.updateCountry(n.copy(wmdCache = 0))
          }
        }
        addEventTarget(Iran)
        if (iran.hasMarker(TradeEmbargoJihadist)) {
          removeEventMarkersFromCountry(Iran, TradeEmbargoJihadist)
          log("Iran may resume oil exports")
          addEventMarkersToCountry(Iran, TradeEmbargoUS)
        }
      }
      else { // Jihadist
        val candidates = countryNames(game.muslims filter (m => m.isShiaMix && m.canTakeAwakeningOrReactionMarker))
        val shiaMix = if (candidates.isEmpty || lapsingEventInPlay(ArabWinter))
          None
        else if (role == game.humanRole)
          Some(askCountry("Select Shia-Mix country to place reaction marker: ", candidates))
        else
          JihadistBot.markerAlignGovTarget(candidates)
        
        addEventTarget(Iran)
        decreasePrestige(1)
        shiaMix foreach { name =>
          addEventTarget(name)
          addReactionMarker(name)
        }
        if (!game.getCountry(Iran).hasMarker(TradeEmbargoJihadist)) {
          // If Iran becomes Neutral or Ally, remove any Trade Embargo marker. [11.3.3.1]
          addEventMarkersToCountry(Iran, TradeEmbargoJihadist)
          log("Iran is no longer an oil exporter")
        }
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(233, "UN Ceasefire", Unassociated, 2,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (role: Role, forTrigger: Boolean) => game hasMuslim unCeasfireCandidate
      ,
      (role: Role) => {
        // See Event Instructions table
        // Not playable in a caliphate member
        val candidates = countryNames(game.muslims filter unCeasfireCandidate)
        val name = if (role == game.humanRole)
          askCountry("Select country: ", candidates)
        else if (role == Jihadist) 
          JihadistBot.unCeasefireTarget(candidates).get
        else 
          USBot.unCeasefireTarget(candidates).get
        
        addEventTarget(name)
        val m = game getMuslim name
        endCivilWar(name)  // This will remove the militia
        addAwakeningMarker(name, m.militia)
        moveTroops(name, "track", m.troops)
        removeCellsFromCountry(name, m.activeCells, m.sleeperCells, m.hasSadr, addCadre = false)
        addReactionMarker(name, m.totalCells)
        setAlignment(name, Neutral)
        rollGovernance(name)
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(234, "Free Syrian Army", Unassociated, 3,
      Remove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (role: Role, forTrigger: Boolean) => {
        val syria = game getMuslim Syria
        !syria.isUntested && (
          role == game.humanRole ||
          (role == Jihadist && !syria.isIslamistRule && syria.reaction <= syria.awakening) ||
          (role == US && !syria.isGood && syria.awakening <= syria.reaction)
        )
      }
      ,
      (role: Role) => {
        addEventTarget(Syria)
        startCivilWar(Syria)
        val (cells, militia) = if (role == game.humanRole) {
          val choices = List("cells"   -> "Place 2 cells and 1 milita in Syria",
                             "militia" -> "Place 2 militia and 1 cell in Syria")
          println("Choose one: ")
          if (askMenu(choices).head == "cells") (2, 1) else (1, 2)
        }
        else if (role == Jihadist) (2, 1)
        else (1, 2)
        addSleeperCellsToCountry(Syria, cells min game.cellsAvailable)
        addMilitiaToCountry(Syria, militia min game.militiaAvailable)
        addEventTarget(Turkey)
        testCountry(Turkey)
        addAidMarker(Turkey)
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(235, "Qadhafi", Unassociated, 3,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (role: Role, forTrigger: Boolean) => game hasMuslim (_.civilWar)
      ,
      (role: Role) => {
        val candidates = countryNames(game.muslims filter (_.civilWar))
        val name = if (role == game.humanRole)
          askCountry("Select country in Civil War: ", candidates)
        else if (role == Jihadist)
          JihadistBot.qadhafiTarget(candidates).get
        else
          USBot.qadhafiTarget(candidates).get
        
        val m = game getMuslim name
        addEventTarget(name)
        endCivilWar(name)
        if (m.totalCells > m.totalTroopsAndMilitia)
          setGovernance(name, Poor, Some(Adversary))
        else if (m.totalTroopsAndMilitia > m.totalCells)
          setGovernance(name, Fair, Some(Ally))
        else
          log(s"The governance and alignment of $name does not change")
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(236, "Oil Price Spike", Unassociated, 3,
      NoRemove, Lapsing, NoAutoTrigger, DoesNotAlertPlot,
      (role: Role, forTrigger: Boolean) => oilPriceSpikePlayable(role, forTrigger)
      ,
      (role: Role) => {
        // See Event Instructions table
        removeGlobalEventMarker(Fracking) // Cancels effects of "Fracking" marker
        if (role == game.humanRole)
          log(s"$role player draws a card other than Oil Price Spike from the discad pile")
        else if (role == US)
          log(s"$US Bot draws highest Ops US associated card (at random) from the discard pile ")
        else
          log(s"$Jihadist Bot draws highest Ops Jihadist associated card (at random) from the discard pile ")
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(237, "Osama bin Ladin", Unassociated, 3,
      USRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (role: Role, forTrigger: Boolean) =>
        (role == Jihadist  && 
         game.prestige > 1 && 
         (game hasMuslim (m => m.isIslamistRule || m.civilWar))) ||
        (role == US && 
         (game.funding > 1 || game.prestige < 12) &&
         !(game.getMuslim(Pakistan).isIslamistRule && game.getMuslim(Afghanistan).isIslamistRule))
      ,
      (role: Role) => if (role == US) {
        decreaseFunding(2)
        increasePrestige(3)
      }
      else
        decreasePrestige(game.muslims count (m => m.isIslamistRule || m.civilWar))
    )),
    // ------------------------------------------------------------------------
    entry(new Card(238, "Revolution", Unassociated, 3,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (role: Role, forTrigger: Boolean) => game hasMuslim (m => m.isPoor && m.awakening > 0 && m.reaction > 0)
      ,
      (role: Role) => {
        // See Event Instructions table
        val candidates = countryNames(game.muslims filter (m => m.isPoor && m.awakening > 0 && m.reaction > 0))
        val name = if (role == game.humanRole)
          askCountry("Select country: ", candidates)
        else if (role == Jihadist)
          JihadistBot.revolutionTarget(candidates).get
        else
          USBot.revolutionTarget(candidates).get
        
        addEventTarget(name)
        startCivilWar(name)
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(239, "Truce", Unassociated, 3,
      NoRemove, Lapsing, NoAutoTrigger, DoesNotAlertPlot,
      (role: Role, forTrigger: Boolean) => false   // Not playable in the solo game
      ,
      (role: Role) => ()      // See Event Instructions table
    )),
    // ------------------------------------------------------------------------
    entry(new Card(240, "US Election", Unassociated, 3,
      NoRemove, NoLapsing, AutoTrigger, DoesNotAlertPlot,
      (role: Role, forTrigger: Boolean) => false  // Not directly playable, but will always auto trigger
      ,
      (role: Role) => {
        if (lapsingEventInPlay(USConsulateAttacked)) {
          log("US Consulate Attacked event is lapsing")
          setUSPosture(oppositePosture(game.usPosture))
        }
        else
          rollUSPosture()
        
        logWorldPosture()
        if (game.gwotPenalty == 0)
          increasePrestige(1)
        else
          decreasePrestige(1)
      }
    ))
  )
}