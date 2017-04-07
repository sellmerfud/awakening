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
// original Labyrinth game.

import scala.util.Random.shuffle
import LabyrinthAwakening._
import USBot.PlotInCountry

object LabyrinthCards {
  
  // Various tests used by the card events
  val backlashCandidate = (m: MuslimCountry) =>
    (m.plots exists (p => !p.backlashed)) && !game.isCaliphateMember(m.name)
  val unNationBuildingCandidate = (m: MuslimCountry) =>
    (m.inRegimeChange || m.civilWar) &&
    !game.isCaliphateMember(m.name)
  val massTurnoutCandidate = (m: MuslimCountry) => 
    m.inRegimeChange && !game.isCaliphateMember(m.name)
    
  def specialForcesCandidates: List[String] = {
    // First find all muslim countries with troops or "Advisors"
    val withTroops = countryNames(game.countries filter (c => c.totalTroops > 0))
    // Next get all countries that contain any cells and filter out the ones are are not 
    // within two of a country with forces.
    countryNames(game.countries filter { country =>
      country.totalCells > 0 && (withTroops exists (forces => distance(forces, country.name) <= 1))
    })
  }
  
  def letsRollCandidates: List[String] = countryNames(game.countries filter { c =>
    c.plots.nonEmpty &&
    (c.isGood || ((game isMuslim c.name) && (game getMuslim c.name).isAlly))
  })
    
  def schengenVisasPlayable(role: Role): Boolean = {
    if (role == game.humanRole)
      game.cellsOnMap > 0
    else {
      var numAutoRecruits = countryNames(game.countries filter (c => c.autoRecruit && c.totalCells > 0)).size
      game.cellsOnMap > 0 &&
      ((game hasMuslim (m => m.cells > 1 || (m.cells == 1 && (!m.autoRecruit || numAutoRecruits > 1)))) ||
      (game hasNonMuslim (n => n.cells > 1 || (n.cells == 1 && n.posture != game.usPosture))))
    }
  }
  
  def cleanOperativesPlayable(role: Role): Boolean = {
    if (role == game.humanRole)
      game.cellsOnMap > 0
    else {
      var numAutoRecruits = countryNames(game.countries filter (c => c.autoRecruit && c.totalCells > 0)).size
      game.cellsOnMap > 0 &&
      ((game hasMuslim (m => m.cells > 1 || (m.cells == 1 && (!m.autoRecruit || numAutoRecruits > 1)))) ||
      (game hasNonMuslim (n => n.name != UnitedStates && n.cells > 1)))
    }
  }
  
  def alJazeeraCandidites: List[String] = {
    val possibles = SaudiArabia::getAdjacentMuslims(SaudiArabia)
    countryNames(possibles map game.getMuslim filter (m => m.totalTroops > 0 && !m.isAdversary))
  }

  // Convenience method for adding a card to the deck.
  private def entry(card: Card) = (card.number -> card)
  
  val deckMap: Map[Int, Card] = Map(
    // ------------------------------------------------------------------------
    entry(new Card(1, "Backlash", US, 1,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (role: Role) => (game.funding > 1 || role != game.botRole) && 
                      (game hasMuslim backlashCandidate)
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
            m = game.getMuslim(name)
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
    entry(new Card(2, "Biometrics", US, 1,
      NoRemove, Lapsing, NoAutoTrigger, DoesNotAlertPlot, AlwaysPlayable,
      (role: Role) => {
        log("For the rest of the turn travel to adjacent Good countries must roll to succeed")
        log("and no non-adjacent travel allowed")
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(3, "CTR", US, 1,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (role: Role) => game.usPosture == Soft && {
        val russia = game getNonMuslim Russia
        val cAsia  = game getMuslim CentralAsia
        !russia.hasMarker(CTR) || (!cAsia.hasMarker(CTR) && !cAsia.isAdversary)
      }
      ,
      (role: Role) => {
        val cAsia  = game getMuslim CentralAsia
        addEventTarget(Russia)
        addEventMarkersToCountry(Russia, CTR)
        if (!cAsia.isAdversary) {
          addEventTarget(CentralAsia)
          addEventMarkersToCountry(CentralAsia, CTR)
        }
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(4, "Moro Talks", US, 1,
      Remove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot, AlwaysPlayable,
      (role: Role) => {
        addEventTarget(Philippines)
        testCountry(Philippines)
        decreaseFunding(1)
        removeEventMarkersFromCountry(Philippines, AbuSayyaf)
        addEventMarkersToCountry(Philippines, MoroTalks)
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(5, "NEST", US, 1,
      Remove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (role: Role) => role == game.humanRole
      ,
      (role: Role) => {
        addEventMarkersToCountry(UnitedStates, NEST)
        log("Add plots in the US are now placed face up")
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(6, "Sanctions", US, 1,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (role: Role) => (game getCountry UnitedStates).hasMarker(PatriotAct)
      ,
      (role: Role) => {
        decreaseFunding(2)
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(7, "Sanctions", US, 1,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (role: Role) => deck(6).eventConditions(role),
      (role: Role) => deck(6).executeEvent(role)
    )),
    // ------------------------------------------------------------------------
    entry(new Card(8, "Special Forces", US, 1,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (role: Role) => specialForcesCandidates.nonEmpty
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
    entry(new Card(9, "Special Forces", US, 1,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (role: Role) => deck(8).eventConditions(role),
      (role: Role) => deck(8).executeEvent(role)
    )),
    // ------------------------------------------------------------------------
    entry(new Card(10, "Special Forces", US, 1,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (role: Role) => deck(8).eventConditions(role),
      (role: Role) => deck(8).executeEvent(role)
    )),
    // ------------------------------------------------------------------------
    entry(new Card(11, "Abbas", US, 2,
      Remove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot, AlwaysPlayable,
      (role: Role) => {
        addGlobalEventMarker(Abbas)
        if (game.troopCommitment != Overstretch && !game.adjacentToIslamistRule(Israel)) {
          increasePrestige(1)
          decreaseFunding(2)
        }
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(12, "Al-Azhar", US, 2,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (role: Role) => !(game getMuslim Egypt).isUntested || game.funding > 1
      ,
      (role: Role) => {
        testCountry(Egypt)
        if (game.numIslamistRule > 0)
          decreaseFunding(2)
        else
          decreaseFunding(4)
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(13, "Anbar Awakening", US, 2,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (role: Role) => List(Iraq, Syria) map game.getMuslim exists (_.totalTroops > 0)
      ,
      (role: Role) => {
        val candidates = countryNames(List(Iraq, Syria) map game.getMuslim filter (_.totalTroops > 0))
        val name = if (role == game.humanRole)
          askCountry("Select country: ", candidates)
        else
          USBot.markerAlignGovTarget(candidates).get
        
        addEventTarget(name)
        addAidMarker(name)
        increasePrestige(1)
        addGlobalEventMarker(AnbarAwakening)
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(14, "Covert Action", US, 2,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (role: Role) => game.muslims exists (_.isAdversary)
      ,
      (role: Role) => {
        val candidates = countryNames(game.muslims filter (_.isAdversary))
        val name = if (role == game.humanRole)
          askCountry("Select country: ", candidates)
        else
          USBot.markerAlignGovTarget(candidates).get
        addEventTarget(name)
        val die = getDieRoll(role)
        val success = die > 3
        log(s"Die roll: $die")
        if (success) {
          log("Success")
          shiftAlignmentLeft(name)
        }
        else
          log("Failure")
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(15, "Ethiopia Strikes", US, 2,
      Remove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (role: Role) => List(Somalia, Sudan) map game.getMuslim exists (_.isIslamistRule)
      ,
      (role: Role) => {
        val candidates = countryNames(List(Somalia, Sudan) map game.getMuslim filter (_.isIslamistRule))
        val name = if (role == game.humanRole)
          askCountry("Select country: ", candidates)
        else
          USBot.markerAlignGovTarget(candidates).get
        addEventTarget(name)
        setGovernance(name, Poor, Some(Neutral))
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(16, "Euro-Islam", US, 2,
      Remove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (role: Role) => role == game.humanRole || 
                      (game getNonMuslim Benelux).posture != game.usPosture ||
                      game.funding > 1
      ,
      (role: Role) => {
        val posture = if (role == game.humanRole)
          askOneOf("New posture (Soft or Hard): ", Seq(Soft, Hard)).get
        else
          game.usPosture
        addEventTarget(Benelux)
        setCountryPosture(Benelux, posture)
        decreaseFunding(1)
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(17, "FSB", US, 2,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (role: Role) => List(Russia, CentralAsia) map game.getCountry exists (_.totalCells > 0)
      ,
      (role: Role) => {
        // In the solo game, the discard option of the event it ignored.
        val candidates = countryNames(List(Russia, CentralAsia) map game.getCountry filter (_.totalCells > 0))
        val (name, (active, sleeper, sadr)) = if (role == game.humanRole) {
          val target = askCountry("Select country: ", candidates)
          (target, askCells(target, 1, sleeperFocus = true))
        }
        else {
          var target = USBot.disruptPriority(candidates).get
          (target, USBot.chooseCellsToRemove(target, 1))
        }
        addEventTarget(name)
        removeCellsFromCountry(name, active, sleeper, sadr, addCadre = true)
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(18, "Intel Community", US, 2,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (role: Role) => role == game.humanRole  // The bot treats this as unplayable
      ,
      (role: Role) => {
        // See Event Instructions table
        log("US player does not inspect the Jihadist hand in the solo game.")
        val cadres = countryNames(game.countries filter (_.hasCadre))
        if (cadres.isEmpty)
          log("No cadres on the map to remove")
        else {
          val target =askCountry("Select country with cadre: ", cadres)
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
    entry(new Card(19, "Kemalist Republic", US, 2,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (role: Role) => role == game.humanRole || !(game getMuslim Turkey).isGood
      ,
      (role: Role) => {
        addEventTarget(Turkey)
        setGovernance(Turkey, Fair, Some(Neutral))
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(20, "King Abdullah", US, 2,
      Remove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (role: Role) => {
        val jordan = game getMuslim Jordan
        !(jordan.isFair && jordan.isAlly) || game.prestige < 12 || game.funding > 1
      }
      ,
      (role: Role) => {
        addEventTarget(Jordan)
        setGovernance(Jordan, Fair, Some(Ally))
        increasePrestige(1)
        decreaseFunding(1)
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(21, "“Let’s Roll!”", US, 2,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (role: Role) => letsRollCandidates.nonEmpty
      ,
      (role: Role) => {
        val plotCandidates    = letsRollCandidates
        val postureCandidates = countryNames(game.nonMuslims filter (_.canChangePosture))
        if (role == game.humanRole) {
          val name = askCountry("Select country with plots: ", plotCandidates)
          val plot = humanPickPlotToAlert(name)
          addEventTarget(name)
          removePlotFromCountry(name, plot)
          log(s"$US draws one card and adds it to their hand")
          val postureName = askCountry("Select posture of which country: ", postureCandidates)
          val newPosture = askOneOf(s"New posture for $postureName (Soft or Hard): ", Seq(Soft, Hard)).get
          addEventTarget(postureName)
          setCountryPosture(postureName, newPosture)
        }
        else {
          val PlotInCountry(plot, c) = USBot.selectPriorityPlot(plotCandidates)
          addEventTarget(c.name)
          removePlotFromCountry(c.name, plot)
          log(s"Add one card to the top of the $US Bot hand")
          val postureName = USBot.posturePriority(postureCandidates).get
          addEventTarget(postureName)
          setCountryPosture(postureName, game.usPosture)
        }
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(22, "Mossad & Shin Bet", US, 2,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (role: Role) => List(Israel, Jordan, Lebanon) map game.getCountry exists (_.totalCells > 0)
      ,
      (role: Role) => {
        for (c <- List(Israel, Jordan, Lebanon) map game.getCountry filter (_.totalCells > 0)) {
          addEventTarget(c.name)
          removeCellsFromCountry(c.name, c.activeCells, c.sleeperCells, c.hasSadr, addCadre = true)
        }
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(23, "Predator", US, 2,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (role: Role) => game.hasMuslim (m => m.name != Iran && m.totalCells > 0)
      ,
      (role: Role) => {
        val candidates = countryNames(game.muslims filter (m => m.name != Iran && m.totalCells > 0))
        val (name, (active, sleeper, sadr)) = if (role == game.humanRole) {
          val target = askCountry("Select country with a cell: ", candidates)
          (target, askCells(target, 1, sleeperFocus = true))
        }
        else {
          var target = USBot.disruptPriority(candidates).get
          (target, USBot.chooseCellsToRemove(target, 1))
        }
        addEventTarget(name)
        removeCellsFromCountry(name, active, sleeper, sadr, addCadre = true)
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(24, "Predator", US, 2,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (role: Role) => deck(23).eventConditions(role),
      (role: Role) => deck(23).executeEvent(role)
    )),
    // ------------------------------------------------------------------------
    entry(new Card(25, "Predator", US, 2,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (role: Role) => deck(23).eventConditions(role),
      (role: Role) => deck(23).executeEvent(role)
    )),
    // ------------------------------------------------------------------------
    entry(new Card(26, "Quartet", US, 2,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (role: Role) => {
        val conditions = globalEventInPlay(Abbas) &&
                         game.troopCommitment != Overstretch &&
                         !game.adjacentToIslamistRule(Israel)
        conditions && (role == game.humanRole || game.prestige < 12 || game.funding > 1)
      }
      ,
      (role: Role) => {
        increasePrestige(2)
        decreaseFunding(3)
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(27, "Saddam Captured", US, 2,
      Remove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (role: Role) => (game getMuslim Iraq).totalTroops > 0
      ,
      (role: Role) => {
        addAidMarker(Iraq)
        increasePrestige(1)
        addGlobalEventMarker(SaddamCaptured)
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(28, "Sharia", US, 2,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (role: Role) => game.hasMuslim (_.besiegedRegime)
      ,
      (role: Role) => {
        val candidates = countryNames(game.muslims filter (_.besiegedRegime))
        val name = if (role == game.humanRole)
          askCountry("Select country with besieged regime: ", candidates)
        else
          USBot.markerAlignGovTarget(candidates).get
        addEventTarget(name)
        removeBesiegedRegimeMarker(name)
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(29, "Tony Blair", US, 2,
      Remove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot, AlwaysPlayable,
      (role: Role) => {
        setCountryPosture(UnitedKingdom, game.usPosture)
        val schengens = if (role == game.humanRole) {
          val num = askInt("Roll War of Ideas in how many Schengen countries", 0, 3)
          askCountries(num, Schengen)
        }
        else 
          USBot.multipleTargets(3, Schengen, USBot.woiNonMuslimPriority)

        for (name <- schengens) {
          val die = getDieRoll(role, s"Enter die roll for War of Ideas in $name: ")
          performWarOfIdeas(name, 3)
        }
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(30, "UN Nation Building", US, 2,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (role: Role) => globalEventNotInPlay(VieiraDeMelloSlain) && 
                      (game hasMuslim unNationBuildingCandidate)
      ,
      (role: Role) => {
        val candidates = countryNames(game.muslims filter unNationBuildingCandidate)
        val target = if (role == game.humanRole)
          askCountry("Select country: ", candidates)
        else 
          USBot.markerAlignGovTarget(candidates).get
        
        val die = getDieRoll(role, "Enter War of Ideas die roll: ")
        addEventTarget(target)
        addAidMarker(target)
        performWarOfIdeas(target, die, ignoreGwotPenalty = true)
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(31, "Wiretapping", US, 2,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (role: Role) => globalEventNotInPlay(LeakWiretapping) && (
        List(UnitedStates, UnitedKingdom, Canada) map game.getCountry
          exists (c => c.hasCadre || c.totalCells > 0 || c.plots.nonEmpty)
      )
      ,
      (role: Role) => {

        for (name <- List(UnitedStates, UnitedKingdom, Canada); c = game getCountry name) {
          addEventTarget(name)
          removeCadreFromCountry(name)
          removeCellsFromCountry(name, c.activeCells, c.sleeperCells, c.hasSadr, addCadre = false)
          for (plot <- c.plots)
            removePlotFromCountry(name, plot)
        }
        
        if (role == game.humanRole)
          log(s"$US draws one card and adds it to their hand")
        else
          log(s"Add one card to the top of the $US Bot hand")
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(32, "Back Channel", US, 3,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (role: Role) => role == game.humanRole && {  // Unplayable by the Bot
        val neededOps = (game.muslims filter (_.isAdversary) map (_.resources)).distinct.sorted
        askYorN(s"Do you have a card in hand with and Ops value of ${orList(neededOps)}? ")
      }
      ,
      (role: Role) => {
        val candidates = countryNames(game.muslims filter (_.isAdversary))
        val name = askCountry("Select adversary country: ", candidates)
        val ops = (game getMuslim name).resources
        log(s"You must discard card with Ops value: $ops")
        addEventTarget(name)
        setAlignment(name, Neutral)
        addAidMarker(name)
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(33, "Benazir Bhutto", US, 3,
      Remove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (role: Role) => {
        val pakistan = (game getMuslim Pakistan)
        !(pakistan.hasMarker(BhuttoShot) ||
          pakistan.isIslamistRule           ||
          game.adjacentToIslamistRule(Pakistan))
      }
      ,
      (role: Role) => {
        addEventTarget(Pakistan)
        if ((game getMuslim Pakistan).isPoor)
          setGovernance(Pakistan, Fair)
        addEventMarkersToCountry(Pakistan, BenazirBhutto)
        log("No jihad allowed in Pakistan while \"Benazir Bhutto is in effect\"")
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(34, "Enhanced Measures", US, 3,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (role: Role) => globalEventNotInPlay(LeakEnhancedMeasures) &&
                      game.usPosture == Hard &&
                      game.disruptTargets(3).nonEmpty
      ,
      (role: Role) => if (role == game.humanRole) {
        humanDisrupt(3)
        log(s"Take the top card of the $Jihadist Bot's hand")
        addGlobalEventMarker(EnhancedMeasures)
      }
      else {
        val target  = USBot.disruptTarget(game disruptTargets 3).get
        log(s"$US performs a Disrupt operation in $target")
        performDisrupt(target)
        log(s"$US (you) must put a random card from your hand on top card of the $Jihadist Bot's hand")
        addGlobalEventMarker(EnhancedMeasures)
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(35, "Hijab", US, 3,
      Remove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (role: Role) => game.numIslamistRule == 0 &&
                      (role == game.humanRole         ||
                      !(game getMuslim Turkey).isGood ||
                      (game getNonMuslim France).posture != game.usPosture ||
                      game.funding > 1)
      ,
      (role: Role) => {
        addEventTarget(Turkey)
        testCountry(Turkey)
        if (!(game getMuslim Turkey).isGood)
          improveGovernance(Turkey, 1, canShiftToGood = true)
        val newPosture = if (role == game.humanRole)
          askOneOf("New posture for France (Soft or Hard): ", Seq(Soft, Hard)).get
        else
          game.usPosture
        addEventTarget(France)
        setCountryPosture(France, newPosture)
        decreaseFunding(2)
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(36, "Indo-Pakistani Talks", US, 3,
      Remove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (role: Role) => {
        val pakistan = game getMuslim Pakistan
        (pakistan.isGood || pakistan.isFair) &&
        (role == game.humanRole ||
         !pakistan.isAlly       ||
         (game getNonMuslim India).posture != game.usPosture)
      }
      ,
      (role: Role) => {
        addEventTarget(Pakistan)
        setAlignment(Pakistan, Ally)
        val newPosture = if (role == game.humanRole)
          askOneOf("New posture for India (Soft or Hard): ", Seq(Soft, Hard)).get
        else
          game.usPosture
        addEventTarget(India)
        setCountryPosture(India, newPosture)
        addEventMarkersToCountry(Pakistan, Indo_PakistaniTalks)
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(37, "Iraqi WMD", US, 3,
      Remove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (role: Role) => role == game.humanRole &&  // Unplayable by the Bot
                      game.usPosture == Hard &&
                      (game getMuslim Iraq).isAdversary
      ,
      (role: Role) => {
        addEventTarget(Iraq)
        addEventMarkersToCountry(Iraq, IraqiWMD)
        val sources = game.regimeChangeSources(3) filterNot (_ == Iraq)
        if (sources.isEmpty) {
          log("You cannot perform a Regime Change in Iraq now, because")
          log("there are not 6 troops available for the operation")
        }
        else if (askYorN("Do you wish to perform a Regime Change in Iraq now? (y/n) ")) {
          log()
          log(s"$US performs a Regime Change operation")
          log(separator())
          val source    = askCountry("Deploy troops from: ", sources)
          val maxTroops = if (source == "track") game.troopsAvailable
                          else game.getCountry(source).maxDeployFrom(3)
          val numTroops = askInt("How many troops: ", 6, maxTroops)
          performRegimeChange(source, Iraq, numTroops)
        }
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(38, "Libyan Deal", US, 3,
      Remove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (role: Role) => ((game getMuslim Iraq).isAlly || (game getMuslim Syria).isAlly) &&
                       (game getMuslim Libya).isPoor
      ,
      (role: Role) => {
        addEventTarget(Libya)
        setAlignment(Libya, Ally)
        increasePrestige(1)
        val schengens = if (role == game.humanRole) {
          println("Select 2 Shegen countries' posture")
          askCountries(2, Schengen) map { name =>
            (name, askOneOf(s"New posture for $name (Soft or Hard): ", Seq(Soft, Hard)).get)
          }
        }
        else
          USBot.multipleTargets(2, Schengen, USBot.posturePriority) map (n => (n, game.usPosture))
        
        for ((name, posture) <- schengens) {
          addEventTarget(name)
          setCountryPosture(name, posture)
        }
        removeEventMarkersFromCountry(Libya, LibyanWMD)
        addEventMarkersToCountry(Libya, LibyanDeal)
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(39, "Libyan WMD", US, 3,
      Remove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (role: Role) => role == game.humanRole                      && // Bot treats as unplayable
                      countryEventNotInPlay(Libya, "Libyan Deal") &&
                      (game getMuslim Libya).isAdversary          &&
                      game.usPosture == Hard
      ,
      (role: Role) => {
        addEventTarget(Libya)
        addEventMarkersToCountry(Libya, LibyanWMD)
        val sources = game.regimeChangeSources(3) filterNot (_ == Libya)
        if (sources.isEmpty) {
          log("You cannot perform a Regime Change in Libya now, because")
          log("there are not 6 troops available for the operation")
        }
        else if (askYorN("Do you wish to perform a Regime Change in Libya now? (y/n) ")) {
          log()
          log(s"$US performs a Regime Change operation")
          log(separator())
          val source    = askCountry("Deploy troops from: ", sources)
          val maxTroops = if (source == "track") game.troopsAvailable
                          else game.getCountry(source).maxDeployFrom(3)
          val numTroops = askInt("How many troops: ", 6, maxTroops)
          performRegimeChange(source, Libya, numTroops)
        }
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(40, "Mass Turnout", US, 3,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (role: Role) => game hasMuslim massTurnoutCandidate
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
    entry(new Card(41, "NATO", US, 3,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (role: Role) => game.gwotPenalty == 0 && (game hasMuslim (m => m.inRegimeChange || m.civilWar))
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
    entry(new Card(42, "Pakistani Offensive", US, 3,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (role: Role) => (game getMuslim Pakistan).isAlly &&
                      (game getMuslim Pakistan).hasMarker(FATA)
      ,
      (role: Role) => {
        removeEventMarkersFromCountry(Pakistan, FATA)
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(43, "Patriot Act", US, 3,
      Remove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot, AlwaysPlayable,
      (role: Role) => {
        addEventMarkersToCountry(UnitedStates, PatriotAct)
        log("The United State is not adjacent to any country except Canada")
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(44, "Renditions", US, 3,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (role: Role) => globalEventNotInPlay(LeakRenditions) &&
                      game.usPosture == Hard
      ,
      (role: Role) => if (role == game.humanRole) {
        if (game.disruptTargets(3).nonEmpty)
          humanDisrupt(3)
        log(s"Discard the top card of the $Jihadist Bot's hand")
        addGlobalEventMarker(Renditions)
      }
      else {
        val target  = USBot.disruptTarget(game disruptTargets 3) foreach { target =>
          log(s"$US performs a Disrupt operation in $target")
          performDisrupt(target)
        }
        log(s"$US (you) must discard a random card from your hand")
        addGlobalEventMarker(Renditions)
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(45, "Safer Now", US, 3,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (role: Role) => game.numIslamistRule == 0 && 
                      !(game hasCountry (c => c.isGood && (c.totalCells > 0 || c.plots.nonEmpty)))
      ,
      (role: Role) => {
        rollUSPosture()
        increasePrestige(3)
        val candidates = countryNames(game.nonMuslims filter (n => n.isSchengen && n.canChangePosture))
        val (name, posture) = if (role == game.humanRole) {
          val target = askCountry("Select posture of which country: ", candidates)
          (target, askOneOf(s"New posture for $target (Soft or Hard): ", Seq(Soft, Hard)).get)
        }
        else
          (USBot.posturePriority(candidates).get, game.usPosture)
        
        addEventTarget(name)
        setCountryPosture(name, posture)
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(46, "Sistani", US, 3,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (role: Role) => game hasMuslim (m => m.isShiaMix && m.inRegimeChange && m.totalCells > 0)
      ,
      (role: Role) => {
        val candidates = countryNames(game.muslims filter 
                 (m => m.isShiaMix && m.inRegimeChange && m.totalCells > 0))
        val name = if (role == game.humanRole)
          askCountry("Select Shia-Mix regime change country: ", candidates)
        else
          USBot.markerAlignGovTarget(candidates).get
        
        improveGovernance(name, 1, canShiftToGood = true)
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(47, "The door of Ijtihad was closed", US, 3,
      NoRemove, Lapsing, NoAutoTrigger, DoesNotAlertPlot, AlwaysPlayable,
      (role: Role) => if (role == game.humanRole)
        log(s"During $Jihadist Bot's action phases, US events will not trigger")
      else {
        log(s"You ($Jihadist) must select cards to play randomly from your hand")
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(48, "Adam Gadahn", Jihadist, 1,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (role: Role) => firstCardOfPhase(Jihadist) &&
                      game.cellsToRecruit > 0    &&
                      askYorN(s"Does the $Jihadist player have another card in hand? (y/n) ")
      ,
      (role: Role) => {
        val  prompt = if (role == game.humanRole)
          "Enter card # of card you wish to use for recruit in the US: "
        else
          s"Enter card # of the next card in the $Jihadist Bot's hand: "
        
        val card = deck(askCardNumber(prompt, allowNone = false).get)
        // Add the card to the list of plays for the turn.
        game = game.copy(plays = PlayedCard(Jihadist, card.number) :: game.plays)
        logCardPlay(Jihadist, card, playable = false, triggered = false)
        log()
        log(s"$Jihadist performs a Recruit operation in the US with ${opsString(card.ops)}")
        log(separator())
        if (game.cellsToRecruit == 1)
          log(s"There is 1 cell available for recruitment")
        else
          log(s"There are ${game.cellsToRecruit} cells available for recruitment")
        
        def nextRecruit(completed: Int): Unit = 
          if (completed < card.ops && game.cellsToRecruit > 0) {
            val ord = ordinal(completed + 1)
            val die     = getDieRoll(role, s"Die roll for $ord recruit: ")
            val success = die < 3
            val result  = if (success) "succeeds" else "fails"
            log(s"$ord recruit $result with a roll of $die")
            if (success) {
              val numCells = if (game.jihadistIdeology(Potent)) {
                log(s"$Jihadist Bot with Potent Ideology places two cells for each success")
                2
              }
              else
                1
              addSleeperCellsToCountry(UnitedStates, numCells min game.cellsToRecruit)
            }
            nextRecruit(completed + 1)
          }

        addEventTarget(UnitedStates)
        nextRecruit(0)
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(49, "Al-Ittihad al-Islami", Jihadist, 1,
      Remove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (role: Role) => game.cellsAvailable > 0
      ,
      (role: Role) => {
        addEventTarget(Somalia)
        testCountry(Somalia)
        addSleeperCellsToCountry(Somalia, 1)
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(50, "Ansar al-Islam", Jihadist, 1,
      Remove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (role: Role) => game.cellsAvailable > 0 &&
                      !((game getMuslim Iraq).isUntested || (game getMuslim Iraq).isGood)
      ,
      (role: Role) => {
        val candidates = List(Iraq, Iran)
        val name = if (role == game.humanRole)
          askCountry("Place cell in which country: ", candidates)
        else
          JihadistBot.recruitTravelToPriority(candidates).get
        
        addEventTarget(name)
        testCountry(name)
        addSleeperCellsToCountry(name, 1)
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(51, "FREs", Jihadist, 1,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (role: Role) => (game getMuslim Iraq).totalTroops > 0 && game.cellsAvailable > 0
      ,
      (role: Role) => {
        // Can create Caliphate (only if globalEventNotInPlay(SaddamCaptured))
        val num = if (globalEventInPlay(SaddamCaptured))
          2 min game.cellsAvailable
        else
          4 min game.cellsAvailable
        addEventTarget(Iraq)
        addSleeperCellsToCountry(Iraq, num)
        if (num >= 3 && canDeclareCaliphate(Iraq) &&
          ((role == game.humanRole && askDeclareCaliphate(Iraq)) ||
           (role == game.botRole   && JihadistBot.willDeclareCaliphate(Iraq)))) {
          declareCaliphate(Iraq)
        }
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(52, "IEDs", Jihadist, 1,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (role: Role) => game hasMuslim (m => m.inRegimeChange && m.totalCells > 0)
      ,
      (role: Role) => {
        if (role == game.humanRole)
          log(s"Discard the top card of the $US Bot's hand")
        else
          log(s"You ($US) must randomly discard one card")
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(53, "Madrassas", Jihadist, 1,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (role: Role) => firstCardOfPhase(Jihadist) &&
                      game.cellsAvailable > 0    && // Ignore funding
                      askYorN(s"Does the $Jihadist player have another card in hand? (y/n) ")
      ,
      (role: Role) => {
        val  prompt = if (role == game.humanRole)
          "Enter card # of card you wish to use for recruit: "
        else
          s"Enter card # of the next card in the $Jihadist Bot's hand: "
        
        val card = deck(askCardNumber(prompt, allowNone = false).get)
        // Add the card to the list of plays for the turn.
        game = game.copy(plays = PlayedCard(Jihadist, card.number) :: game.plays)
        logCardPlay(Jihadist, card, playable = false, triggered = false)
        val totalOps = card.ops + 1 // Add 1 for the Ops on the Madrassas card.
        
        if (role == game.humanRole)
          humanRecruit(totalOps, ignoreFunding = true, madrassas = true)
        else {
          log()
          log(s"$Jihadist performs a Recruit operation ${opsString(totalOps)}")
          log(separator())
          JihadistBot.performRecruit(totalOps, ignoreFunding = true, madrassas = true)
        }
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(54, "Moqtada al-Sadr", Jihadist, 1,
      Remove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (role: Role) => (game getMuslim Iraq).totalTroops > 0
      ,
      (role: Role) => addEventMarkersToCountry(Sadr)
    )),
    // ------------------------------------------------------------------------
    entry(new Card(55, "Uyghur Jihad", Jihadist, 1,
      Remove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (role: Role) => game.cellsAvailable > 0
      ,
      (role: Role) => {
        addEventTarget(China)
        testCountry(China)
        if ((game getNonMuslim China).isSoft)
          addSleeperCellsToCountry(China, 1)
        else {
          addEventTarget(CentralAsia)
          testCountry(CentralAsia)
          addSleeperCellsToCountry(CentralAsia, 1)
        }
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(56, "Vieira de Mello Slain", Jihadist, 1,
      Remove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (role: Role) => game hasMuslim (m => m.inRegimeChange && m.totalCells > 0)
      ,
      (role: Role) => {
        decreasePrestige(1)
        addGlobalEventMarker(VieiraDeMelloSlain)
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(57, "Abu Sayyaf", Jihadist, 2,
      Remove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (role: Role) => globalEventNotInPlay(MoroTalks)
      ,
      (role: Role) => {
        addEventTarget(Philippines)
        testCountry(Philippines)
        if (game.cellsAvailable > 0)
          addSleeperCellsToCountry(Philippines, 1)
        addEventMarkersToCountry(Philippines, AbuSayyaf)
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(58, "Al-Anbar", Jihadist, 2,
      Remove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (role: Role) => globalEventNotInPlay(AnbarAwakening)
      ,
      (role: Role) => {
        addEventTarget(Iraq)
        testCountry(Iraq)
        if (game.cellsAvailable > 0)
          addSleeperCellsToCountry(Iraq, 1)
        addGlobalEventMarker(AlAnbar)
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(59, "Amerithrax", Jihadist, 2,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (role: Role) => role == game.botRole // human player cannot play against US Bot!
      ,
      (role: Role) => {
        log(s"You ($US) must discard your highest-value US associated card (if any)")
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(60, "Bhutto Shot", Jihadist, 2,
      Remove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (role: Role) => (game getMuslim Pakistan).totalCells > 0
      ,
      (role: Role) => {
        removeEventMarkersFromCountry(Pakistan, BenazirBhutto)
        addEventMarkersToCountry(Pakistan, BhuttoShot)
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(61, "Detainee Release", Jihadist, 2,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (role: Role) => lapsingEventNotInPlay(GTMO)      &&
                      globalEventNotInPlay(Renditions) &&
                      (game.targetsThisPhase.disrupted.nonEmpty ||
                      game.targetsLastPhase.disrupted.nonEmpty)
      ,
      (role: Role) => {
        if (game.cellsAvailable > 0) {
          val candidates = (game.targetsThisPhase.disrupted ++ 
                            game.targetsLastPhase.disrupted).toList.sorted
          val name = if (role == game.humanRole)
            askCountry("Select country where disrupt occurred: ", candidates)
          else
            JihadistBot.recruitTravelToPriority(candidates).get
          addEventTarget(name)
          addSleeperCellsToCountry(name, 1)
        }
        if (role == game.humanRole)
          log(s"You ($Jihadist) may draw a card")
        else
          log(s"Add a card to the top of the $Jihadist Bot's hand")
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(62, "Ex-KGB", Jihadist, 2,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (role: Role) => countryEventInPlay(Russia, CTR) ||
                      (game getNonMuslim Caucasus).posture == game.usPosture ||
                      !(game getMuslim CentralAsia).isAdversary
      ,
      (role: Role) => {
        val canShift   = !(game getMuslim CentralAsia).isAdversary
        val canPosture = (game getNonMuslim Caucasus).posture != game.usPosture
        val target = if (countryEventInPlay(Russia, CTR))
          Russia
        else if (role == game.humanRole) {
          val choices = List(
            choice(canShift,   CentralAsia, "Shift Central Asia 1 box toward Adversary"),
            choice(canPosture, Caucasus,    "Set Caucasus to opposite posture of the US")
          ).flatten
          println("Choose one:")
          askMenu(choices).head
        }
        else if (canShift) 
          CentralAsia
        else
          Caucasus
        
        addEventTarget(target)
        target match {
          case Russia   => removeEventMarkersFromCountry(Russia, CTR)
          case Caucasus => setCountryPosture(Caucasus, oppositePosture(game.usPosture))
          case _        => shiftAlignmentRight(CentralAsia)
        }
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(63, "Gaza War", Jihadist, 2,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot, AlwaysPlayable,
      (role: Role) => {
        if (role == game.humanRole)
          log(s"Discard the top card of the $US Bot's hand")
        else
          log(s"You ($US) must randomly discard one card")
        increaseFunding(1)
        decreasePrestige(1)
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(64, "Hariri Killed", Jihadist, 2,
      Remove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (role: Role) => (game getMuslim Lebanon).isUntested || {
        val syria = game getMuslim Syria
        syria.isAdversary || syria.isGood || syria.isFair
      }
      ,
      (role: Role) => {
        addEventTarget(Lebanon)
        testCountry(Lebanon)
        addEventTarget(Syria)
        testCountry(Syria)
        setAlignment(Syria, Adversary)
        if (!(game getMuslim Syria).isIslamistRule)
          degradeGovernance(Syria, 1, canShiftToIR = false)
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(65, "HEU", Jihadist, 2,
      Remove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (role: Role) => (List(Russia, CentralAsia) map game.getCountry 
            exists (c => c.totalCells > 0 && !c.hasMarker(CTR)))
      ,      
      (role: Role) => {
        val candidates = List(Russia, CentralAsia) filter { name =>
          val c = game getCountry name
          c.totalCells > 0 && !c.hasMarker(CTR)
        } 
        val name = if (role == game.humanRole)
          askCountry("Select country: ", candidates)
        else if (candidates.size == 1)
          candidates.head
        else {
          val casia = game getMuslim CentralAsia
          if (casia.isPoor || casia.isIslamistRule)
            CentralAsia
          else
            Russia
        }
        
        addEventTarget(name)
        val die = getDieRoll(role)
        val success = die <= (game getCountry name).governance
        log(s"Die roll: $die  (${if (success) "Success" else "Failure"})")
        if (success) {
          log(s"Move the HEU WMD plot marker to the available plots box")
          val updatedPlots = game.plotData.copy(availablePlots = PlotWMD :: game.availablePlots)
          game = game.copy(plotData = updatedPlots)
        }
        else {
          val (active, sleeper, sadr) = if (role == game.humanRole) {
            println("You must remove a cell")
            askCells(name, 1, sleeperFocus = false)
          }
          else
            JihadistBot.chooseCellsToRemove(name, 1)
          removeCellsFromCountry(name, active, sleeper, sadr, addCadre = true)
        }
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(66, "Homegrown", Jihadist, 2,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (role: Role) => game.cellsAvailable > 0
      ,
      (role: Role) => {
        addEventTarget(UnitedKingdom)
        testCountry(UnitedKingdom)
        addSleeperCellsToCountry(UnitedKingdom, 1)
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(67, "Islamic Jihad Union", Jihadist, 2,
      Remove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (role: Role) => game.cellsAvailable > 0
      ,
      (role: Role) => {
        val candidates = List(CentralAsia, Afghanistan)
        val targets = if (game.cellsAvailable > 1)
          candidates
        else if (role == game.botRole)
          JihadistBot.recruitTravelToPriority(candidates).toList
        else {
          println("There is only one cell available to be placed")
          askCountry("Select country for cell: ", candidates)::Nil
        }
        for (name <- targets) {
          addEventTarget(name)
          testCountry(name)
          addSleeperCellsToCountry(name, 1)
        }
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(68, "Jemaah Islamiya", Jihadist, 2,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (role: Role) => game.cellsAvailable > 0
      ,
      (role: Role) => {
        addEventTarget(IndonesiaMalaysia)
        testCountry(IndonesiaMalaysia)
        addSleeperCellsToCountry(IndonesiaMalaysia, 2 min game.cellsAvailable)
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(69, "Kazakh Strain", Jihadist, 2,
      Remove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (role: Role) =>
        (game getCountry CentralAsia).totalCells > 0 && countryEventNotInPlay(CentralAsia, CTR)
      ,      
      (role: Role) => {
        addEventTarget(CentralAsia)
        val die = getDieRoll(role)
        val success = die <= (game getCountry CentralAsia).governance
        log(s"Die roll: $die  (${if (success) "Success" else "Failure"})")
        if (success) {
          log(s"Move the Kazakh Strain WMD plot marker to the available plots box")
          val updatedPlots = game.plotData.copy(availablePlots = PlotWMD :: game.availablePlots)
          game = game.copy(plotData = updatedPlots)
        }
        else {
          val (active, sleeper, sadr) = if (role == game.humanRole) {
            println("You must remove a cell")
            askCells(CentralAsia, 1, sleeperFocus = false)
          }
          else
            JihadistBot.chooseCellsToRemove(CentralAsia, 1)
          removeCellsFromCountry(CentralAsia, active, sleeper, sadr, addCadre = true)
        }
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(70, "Lashkar-e-Tayyiba", Jihadist, 2,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (role: Role) => countryEventNotInPlay(Pakistan, Indo_PakistaniTalks) &&
                      game.cellsAvailable > 0
      ,
      (role: Role) => {
        val candidates = List(Pakistan, India)
        val targets = if (game.cellsAvailable > 1)
          candidates
        else if (role == game.botRole)
          JihadistBot.recruitTravelToPriority(candidates).toList
        else {
          println("There is only one cell available to be placed")
          askCountry("Select country for cell: ", candidates)::Nil
        }
        for (name <- targets) {
          addEventTarget(name)
          testCountry(name)
          addSleeperCellsToCountry(name, 1)
        }
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(71, "Loose Nuke", Jihadist, 2,
      Remove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (role: Role) =>
        (game getCountry Russia).totalCells > 0 && countryEventNotInPlay(Russia, CTR)
      ,      
      (role: Role) => {
        addEventTarget(Russia)
        val die = getDieRoll(role)
        val success = die <= (game getCountry Russia).governance
        log(s"Die roll: $die  (${if (success) "Success" else "Failure"})")
        if (success) {
          log(s"Move the Loose Nuke WMD plot marker to the available plots box")
          val updatedPlots = game.plotData.copy(availablePlots = PlotWMD :: game.availablePlots)
          game = game.copy(plotData = updatedPlots)
        }
        else {
          val (active, sleeper, sadr) = if (role == game.humanRole) {
            println("You must remove a cell")
            askCells(Russia, 1, sleeperFocus = false)
          }
          else
            JihadistBot.chooseCellsToRemove(Russia, 1)
          removeCellsFromCountry(Russia, active, sleeper, sadr, addCadre = true)
        }
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(72, "Opium", Jihadist, 2,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (role: Role) => (game getCountry Afghanistan).totalCells > 0 &&
                      game.cellsAvailable > 0
      ,
      (role: Role) => {
        // Can create Caliphate (only in Afghanistan)
        val num = if ((game getMuslim Afghanistan).isIslamistRule)
          game.cellsAvailable
        else
          3 min game.cellsAvailable
        addEventTarget(Afghanistan)
        addSleeperCellsToCountry(Afghanistan, num)
        if (num >= 3 && canDeclareCaliphate(Afghanistan) &&
          ((role == game.humanRole && askDeclareCaliphate(Afghanistan)) ||
           (role == game.botRole   && JihadistBot.willDeclareCaliphate(Afghanistan)))) {
          declareCaliphate(Afghanistan)
        }
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(73, "Pirates", Jihadist, 2,
      Remove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (role: Role) => globalEventNotInPlay(MaerskAlabama) && pirates1ConditionsInEffect
      ,
      (role: Role) => addGlobalEventMarker(Pirates2)
    )),
    // ------------------------------------------------------------------------
    entry(new Card(74, "Schengen Visas", Jihadist, 2,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (role: Role) => schengenVisasPlayable(role)
      ,
      (role: Role) => if (role == game.humanRole) {
        val num = 2 min game.cellsOnMap
        val travellers = if (num == 1) {
          for (c <- game.countries; if c.cells > 0)
            yield CellsItem(c.name, c.activeCells, c.sleeperCells)
        }
        else {
          println(s"Select 2 cells to travel to Schengen countries: ")
          askCellsFromAnywhere(num, trackOK = false, countryNames(game.countries), sleeperFocus = false)
        }
        var i = 1
        for (CellsItem(from, actives, sleepers) <- travellers) {
          for (a <- 1 to actives) {
            val to = askCountry(s"Select ${ordinal(i)} destination country: ", Schengen)
            testCountry(to)
            moveCellsBetweenCountries(from, to, 1, true)
            i += 1
          }
          for (a <- 1 to sleepers) {
            val to = askCountry(s"Select ${ordinal(i)} destination country: ", Schengen)
            testCountry(to)
            moveCellsBetweenCountries(from, to, 1, false)
            i += 1
          }
        }
      }
      else {
        var alreadyTravelled = Map.empty[String, Int].withDefaultValue(0)
        def nextTravel(num: Int): Unit = {
          var autoRecruits = countryNames(game.countries filter (c => c.autoRecruit && c.totalCells > 0)).toSet
          val hasTraveler = (c: Country) => {
            val eligible = c.activeCells + (c.sleeperCells - alreadyTravelled(c.name))
            if (game isMuslim c.name)
              (eligible > 1 || (eligible == 1 && (!autoRecruits(c.name) || autoRecruits.size > 1)))
            else
              (eligible > 1 || eligible == 1 && (game getNonMuslim c.name).posture != game.usPosture)
          }
          val travellers = game.countries filter hasTraveler map (c => (c.name, c.activeCells > 0))
          if (num <= 2 && travellers.nonEmpty) {
            val to   = JihadistBot.posturePriority(Schengen).get
            val from = JihadistBot.travelFromTarget(to, travellers map (_._1) filterNot (_ == to)).get
            val (_, active) = (travellers find (_._1 == from)).get
            testCountry(to)
            moveCellsBetweenCountries(from, to, 1, active)
            alreadyTravelled += to -> (alreadyTravelled(to) + 1)
            nextTravel(num + 1)
          }
        }
        nextTravel(1)
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(75, "Schroeder & Chirac", Jihadist, 2,
      Remove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (role: Role) => game.usPosture == Hard && (
                        role == game.humanRole ||
                        !(game getNonMuslim Germany).isSoft ||
                        !(game getNonMuslim France).isSoft  ||
                        game.prestige > 1
                      )
      ,
      (role: Role) => {
        addEventTarget(Germany)
        addEventTarget(France)
        setCountryPosture(Germany, Soft)
        setCountryPosture(France, Soft)
        decreasePrestige(1)
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(76, "Abu Ghurayb", Jihadist, 3,
      Remove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (role: Role) => game hasMuslim (m => m.inRegimeChange && m.totalCells > 0)
      ,
      (role: Role) => {
        if (role == game.humanRole)
          log(s"You ($Jihadist) may draw two cards")
        else
          log(s"Draw 2 cards and place them on top of the $Jihadist Bot's hand")
        decreasePrestige(2)
        val candidates = countryNames(game.muslims filter (_.isAlly))
        if (candidates.isEmpty)
          log("There are no Ally Muslim countries. No shift possible.")
        val name = if (role == game.humanRole)
          askCountry("Select Ally Muslim country: ", candidates)
        else
          JihadistBot.markerAlignGovTarget(candidates).get
        
        addEventTarget(name)
        shiftAlignmentRight(name)
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(77, "Al-Jazeera", Jihadist, 3,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (role: Role) => alJazeeraCandidites.nonEmpty
      ,
      (role: Role) => {
        val name = if (role == game.humanRole)
          askCountry("Select country with troops: ", alJazeeraCandidites)
        else
          JihadistBot.markerAlignGovTarget(alJazeeraCandidites).get
        
        addEventTarget(name)
        shiftAlignmentRight(name)
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(78, "Axis of Evil", Jihadist, 3,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot, AlwaysPlayable,
      (role: Role) => {
        if (role == game.humanRole)
          log(s"The $US Bot does NOT dicard any cards")
        else
          log(s"You ($US) must discard any of Iran, Hizballah, or Jaysh al-Mahdi")
        setUSPosture(Hard)
        rollPrestige()
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(79, "Clean Operatives", Jihadist, 3,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (role: Role) => cleanOperativesPlayable(role)
      ,
      (role: Role) => if (role == game.humanRole) {
        val allCountries = countryNames(game.countries)
        val num = 2 min game.cellsOnMap
        val travellers = if (num == 1) {
          for (c <- game.countries; if c.cells > 0)
            yield CellsItem(c.name, c.activeCells, c.sleeperCells)
        }
        else {
          println(s"Select 2 cells to travel anywhere: ")
          askCellsFromAnywhere(num, trackOK = false, allCountries, sleeperFocus = false)
        }
        var i = 1
        for (CellsItem(from, actives, sleepers) <- travellers) {
          for (a <- 1 to actives) {
            val to = askCountry(s"Select ${ordinal(i)} destination country: ", allCountries)
            testCountry(to)
            moveCellsBetweenCountries(from, to, 1, true)
            i += 1
          }
          for (a <- 1 to sleepers) {
            val to = askCountry(s"Select ${ordinal(i)} destination country: ", allCountries)
            testCountry(to)
            moveCellsBetweenCountries(from, to, 1, false)
            i += 1
          }
        }
      }
      else {
        def nextTravel(num: Int): Unit = {
          var autoRecruits = countryNames(game.countries filter (c => c.autoRecruit && c.totalCells > 0)).toSet
          val hasTraveler = (c: Country) => {
            if (game isMuslim c.name)
              (c.cells > 1 || (c.cells == 1 && (!autoRecruits(c.name) || autoRecruits.size > 1)))
            else
              c.cells > 0
          }
          val travellers = (game.countries filterNot (_.name == UnitedStates)
                                           filter hasTraveler
                                           map (c => (c.name, c.activeCells > 0)))
          if (num <= 2 && travellers.nonEmpty) {
            val from = JihadistBot.travelFromTarget(UnitedStates, travellers map (_._1)).get
            val (_, active) = (travellers find (_._1 == from)).get
            moveCellsBetweenCountries(from, UnitedStates, 1, active)
            nextTravel(num + 1)
          }
        }
        nextTravel(1)
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(80, "FATA", Jihadist, 3,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot, AlwaysPlayable,
      (role: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(81, "Foreign Fighters", Jihadist, 3,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot, AlwaysPlayable,
      // Can create Caliphate
      (role: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(82, "Jihadist Videos", Jihadist, 3,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot, AlwaysPlayable,
      (role: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(83, "Kashmir", Jihadist, 3,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot, AlwaysPlayable,
      (role: Role) => ()
        // Blocked: countryEventNotInPlay(Pakistan, "Indo-Pakistani Talks")
    )),
    // ------------------------------------------------------------------------
    entry(new Card(84, "Leak", Jihadist, 3,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot, AlwaysPlayable,
      (role: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(85, "Leak", Jihadist, 3,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot, AlwaysPlayable,
      (role: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(86, "Lebanon War", Jihadist, 3,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot, AlwaysPlayable,
      (role: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(87, "Martyrdom Operation", Jihadist, 3,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot, AlwaysPlayable,
      (role: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(88, "Martyrdom Operation", Jihadist, 3,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot, AlwaysPlayable,
      (role: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(89, "Martyrdom Operation", Jihadist, 3,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot, AlwaysPlayable,
      (role: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(90, "Quagmire", Jihadist, 3,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot, AlwaysPlayable,
      (role: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(91, "Regional al-Qaeda", Jihadist, 3,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot, AlwaysPlayable,
      (role: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(92, "Saddam", Jihadist, 3,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot, AlwaysPlayable,
      (role: Role) => () // globalEventNotInPlay(SaddamCaptured)
    )),
    // ------------------------------------------------------------------------
    entry(new Card(93, "Taliban", Jihadist, 3,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot, AlwaysPlayable,
      (role: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(94, "The door of Ijtihad was closed", Jihadist, 3,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot, AlwaysPlayable,
      (role: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(95, "Wahhabism", Jihadist, 3,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot, AlwaysPlayable,
      (role: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(96, "Danish Cartoons", Unassociated, 1,
      Remove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot, AlwaysPlayable,
      (role: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(97, "Fatwa", Unassociated, 1,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot, AlwaysPlayable,
      (role: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(98, "Gaza Withdrawal", Unassociated, 1,
      Remove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot, AlwaysPlayable,
      (role: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(99, "HAMAS Elected", Unassociated, 1,
      Remove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot, AlwaysPlayable,
      (role: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(100, "Hizb Ut-Tahrir", Unassociated, 1,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot, AlwaysPlayable,
      (role: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(101, "Kosovo", Unassociated, 1,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot, AlwaysPlayable,
      (role: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(102, "Former Soviet Union", Unassociated, 2,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot, AlwaysPlayable,
      (role: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(103, "Hizballah", Unassociated, 2,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot, AlwaysPlayable,
      (role: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(104, "Iran", Unassociated, 2,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot, AlwaysPlayable,
      (role: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(105, "Iran", Unassociated, 2,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot, AlwaysPlayable,
      (role: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(106, "Jaysh al-Mahdi", Unassociated, 2,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot, AlwaysPlayable,
      (role: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(107, "Kurdistan", Unassociated, 2,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot, AlwaysPlayable,
      (role: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(108, "Musharraf", Unassociated, 2,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot, AlwaysPlayable,
      (role: Role) => ()  // Make sure there is no marker
                          // If there is it must be removed when Benazir Bhutto is played
    )),
    // ------------------------------------------------------------------------
    entry(new Card(109, "Tora Bora", Unassociated, 2,
      Remove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot, AlwaysPlayable,
      (role: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(110, "Zarqawi", Unassociated, 2,
      Remove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot, AlwaysPlayable,
      // Can create Caliphate (onlyinIraq,Syria,Lebanon,orJordan)
      (role: Role) => () // Remove is conditional
    )),
    // ------------------------------------------------------------------------
    entry(new Card(111, "Zawahiri", Unassociated, 2,
      Remove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot, AlwaysPlayable,
      // US play blocked by AlAnbar
      (role: Role) => () // Remove is conditional
    )),
    // ------------------------------------------------------------------------
    entry(new Card(112, "Bin Ladin", Unassociated, 3,
      Remove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot, AlwaysPlayable,
      // Us play blocked by AlAnbar
      (role: Role) => () // Remove is conditional
    )),
    // ------------------------------------------------------------------------
    entry(new Card(113, "Darfur", Unassociated, 3,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot, AlwaysPlayable,
      (role: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(114, "GTMO", Unassociated, 3,
      NoRemove, Lapsing, NoAutoTrigger, DoesNotAlertPlot, AlwaysPlayable,
      (role: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(115, "Hambali", Unassociated, 3,
      Remove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot, AlwaysPlayable,
      (role: Role) => () // Remove is conditional
    )),
    // ------------------------------------------------------------------------
    entry(new Card(116, "KSM", Unassociated, 3,
      Remove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot, AlwaysPlayable,
      (role: Role) => () // Remove is conditional
    )),
    // ------------------------------------------------------------------------
    entry(new Card(117, "Oil Price Spike", Unassociated, 3,
      NoRemove, Lapsing, NoAutoTrigger, DoesNotAlertPlot, AlwaysPlayable,
      (role: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(118, "Oil Price Spike", Unassociated, 3,
      NoRemove, Lapsing, NoAutoTrigger, DoesNotAlertPlot,
      (role: Role) => deck(117).eventConditions(role),
      (role: Role) => deck(117).executeEvent(role)
    )),
    // ------------------------------------------------------------------------
    entry(new Card(119, "Saleh", Unassociated, 3,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot, AlwaysPlayable,
      (role: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(120, "US Election", Unassociated, 3,
      NoRemove, NoLapsing, AutoTrigger, DoesNotAlertPlot,
      (role: Role) => false  // No directly playable, but will always auto trigger
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

