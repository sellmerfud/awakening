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
  val martyrdomCandidate = (c: Country) => c.totalCells > 0 && 
    !(game.isMuslim(c.name) && game.getMuslim(c.name).isIslamistRule)
  val regionalAlQaedaCandidate = (m: MuslimCountry) => m.name != Iran && m.isUntested
    
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
    
  def schengenVisasPlayable(role: Role, forTrigger: Boolean): Boolean = {
    if (role == game.humanRole)
      game.cellsOnMap > 0
    else {
      var numAutoRecruits = countryNames(game.countries filter (c => c.autoRecruit && c.totalCells > 0)).size
      game.cellsOnMap > 0 &&
      ((game hasMuslim (m => m.cells > 1 || (m.cells == 1 && (!m.autoRecruit || numAutoRecruits > 1)))) ||
      (game hasNonMuslim (n => n.cells > 1 || (n.cells == 1 && n.posture != game.usPosture))))
    }
  }
  
  def cleanOperativesPlayable(role: Role, forTrigger: Boolean): Boolean = {
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
    val possibles = game.getMuslim(SaudiArabia)::game.adjacentMuslims(SaudiArabia)
    countryNames(possibles filter (m => m.totalTroops > 0 && !m.isAdversary))
  }

  def danishCartoonPlots: List[Plot] = if (game.numIslamistRule == 0)
    (game.availablePlots find (_ == Plot1)).toList
  else
    game.availablePlots
  
  def hizballahCanddidates = countryNames(game.muslims filter { m =>
    m.isShiaMix && m.totalCells > 0 && distance(m.name, Lebanon) <= 3
  })
  def hizballahPlayable(role: Role, forTrigger: Boolean) = role match {
    case US       => hizballahCanddidates.nonEmpty
    case Jihadist =>
      val leb = game getMuslim Lebanon
      !(leb.isPoor && leb.isNeutral) &&
      (role == game.humanRole || !(leb.isAdversary || leb.isIslamistRule))
  }
  
  def iranCandidates(role: Role): List[String] = {
    val iran = if ((game getCountry Iran).totalCells > 0) List(Iran) else Nil
    val usHumanTest = (m: MuslimCountry) => m.isShiaMix && (m.isUntested || m.totalCells > 0) 
    val usBotTest   = (m: MuslimCountry) => m.isShiaMix && m.totalCells > 0 // Bot only cares if cell can be removed
    val jihadTest   = (m: MuslimCountry) =>
      m.isShiaMix && (m.isUntested || m.isGood || m.isFair || (m.isPoor && m.aidMarkers > 0))
    
    role match {
      case US if role == game.humanRole => iran ::: countryNames(game.muslims filter usHumanTest)
      case US                           => iran ::: countryNames(game.muslims filter usBotTest)
      case Jihadist                     => countryNames(game.muslims filter jihadTest)
    }    
  }
  
  def jayshAlMahdiCandidates: List[String] = countryNames(
    (game.muslims filter (m => m.isShiaMix && m.totalCells > 0 && m.totalTroops > 0)))
    
  def zarqawiCandidates: List[String] = {
    val names = List(Iraq, Syria, Lebanon, Jordan)
    countryNames(names map game.getMuslim filter (_.totalTroops > 0))
  } 
  
  def hambaliCandidates: List[String] = {
    val possibles = IndonesiaMalaysia :: getAdjacent(IndonesiaMalaysia)
    countryNames(possibles map game.getCountry filter {
      case m: MuslimCountry    => m.totalCells > 0 && m.isAlly
      case n: NonMuslimCountry => n.totalCells > 0 && n.isHard
    })
  }
  
  def ksmUSCandidates: List[String] = {
    val possibles = game.nonMuslims ::: (game.muslims filter (_.isAlly))
    countryNames(possibles filter (_.plots.nonEmpty))
  }
  def ksmJihadistCandidates: List[String] = {
    val possibles = game.nonMuslims ::: (game.muslims filter (!_.isIslamistRule))
    countryNames(possibles filter (_.totalCells > 0))
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
  
  def fsbCandidates = countryNames(game.getCountries(Russia::CentralAsia::Nil) filter (_.totalCells > 0))

  def mossadShinBetCandidates = countryNames(
    game.getCountries(Israel::Jordan::Lebanon::Nil) filter (_.totalCells > 0)
  )

  def predatorCandidates = countryNames(
    game.muslims filter (m => m.name != Iran && m.totalCells > 0)
  )

  def wireTappingCandidates = countryNames(
    game.getCountries(UnitedStates::UnitedKingdom::Canada::Nil) filter { c =>
      c.hasCadre || c.totalCells > 0 || c.plots.nonEmpty
    }
  )

  def toraBoraCandidates = countryNames(
    game.muslims filter (m => m.inRegimeChange && m.totalCells > 1)
  )

  // Convenience method for adding a card to the deck.
  private def entry(card: Card) = (card.number -> card)
  
  val deckMap: Map[Int, Card] = Map(
    // ------------------------------------------------------------------------
    entry(new Card(1, "Backlash", US, 1,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot, CannotNotRemoveLastCell,
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
      NoRemove, Lapsing, NoAutoTrigger, DoesNotAlertPlot,  CannotNotRemoveLastCell,
      AlwaysPlayable,
      (role: Role) => {
        log("For the rest of the turn travel to adjacent Good countries must roll to succeed")
        log("and no non-adjacent travel allowed")
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(3, "CTR", US, 1,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot, CannotNotRemoveLastCell,
      (role: Role, forTrigger: Boolean) => game.usPosture == Soft && {
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
      Remove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,  CannotNotRemoveLastCell,
      AlwaysPlayable,
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
      Remove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot, CannotNotRemoveLastCell,
      (role: Role, forTrigger: Boolean) => role == game.humanRole
      ,
      (role: Role) => {
        addEventMarkersToCountry(UnitedStates, NEST)
        log("Plots in the US are now placed face up")
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(6, "Sanctions", US, 1,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,  CannotNotRemoveLastCell,
      (role: Role, forTrigger: Boolean) => (game getCountry UnitedStates).hasMarker(PatriotAct)
      ,
      (role: Role) => {
        decreaseFunding(2)
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(7, "Sanctions", US, 1,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot, CannotNotRemoveLastCell,
      (role: Role, forTrigger: Boolean) => deck(6).eventConditions(role, forTrigger),
      (role: Role) => deck(6).executeEvent(role)
    )),
    // ------------------------------------------------------------------------
    entry(new Card(8, "Special Forces", US, 1,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      () => {
        // Can we remove the last cell on the board?
        specialForcesCandidates exists (name => USBot.wouldRemoveLastCell(name, 1))
      }
      ,
      (role: Role, forTrigger: Boolean) => specialForcesCandidates.nonEmpty
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
      () => deck(8).eventRemovesLastCell(),
      (role: Role, forTrigger: Boolean) => deck(8).eventConditions(role, forTrigger),
      (role: Role) => deck(8).executeEvent(role)
    )),
    // ------------------------------------------------------------------------
    entry(new Card(10, "Special Forces", US, 1,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      () => deck(8).eventRemovesLastCell(),
      (role: Role, forTrigger: Boolean) => deck(8).eventConditions(role, forTrigger),
      (role: Role) => deck(8).executeEvent(role)
    )),
    // ------------------------------------------------------------------------
    entry(new Card(11, "Abbas", US, 2,
      Remove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,  CannotNotRemoveLastCell,
      AlwaysPlayable,
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
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot, CannotNotRemoveLastCell,
      (role: Role, forTrigger: Boolean) => !(game getMuslim Egypt).isUntested || game.funding > 1
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
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot, CannotNotRemoveLastCell,
      (role: Role, forTrigger: Boolean) => List(Iraq, Syria) map game.getMuslim exists (_.totalTroops > 0)
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
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot, CannotNotRemoveLastCell,
      (role: Role, forTrigger: Boolean) => game.muslims exists (_.isAdversary)
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
      Remove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot, CannotNotRemoveLastCell,
      (role: Role, forTrigger: Boolean) => List(Somalia, Sudan) map game.getMuslim exists (_.isIslamistRule)
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
      Remove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot, CannotNotRemoveLastCell,
      (role: Role, forTrigger: Boolean) =>
        role == game.humanRole || 
        (game getNonMuslim Benelux).posture != game.usPosture ||
        (game.numIslamistRule == 0 && game.funding > 1)
      ,
      (role: Role) => {
        val posture = if (role == game.humanRole)
          askOneOf("New posture (Soft or Hard): ", Seq(Soft, Hard)).get
        else
          game.usPosture
        addEventTarget(Benelux)
        setCountryPosture(Benelux, posture)
        if (game.numIslamistRule == 0)
          decreaseFunding(1)
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(17, "FSB", US, 2,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      () => {
        // Can we remove the last cell on the board?
        fsbCandidates exists (name => USBot.wouldRemoveLastCell(name, 1))
      }
      ,
      (role: Role, forTrigger: Boolean) => fsbCandidates.nonEmpty
      ,
      (role: Role) => {
        // In the solo game, the discard option of the event it ignored.
        val (name, (active, sleeper, sadr)) = if (role == game.humanRole) {
          val target = askCountry("Select country: ", fsbCandidates)
          (target, askCells(target, 1, sleeperFocus = true))
        }
        else {
          var target = USBot.disruptPriority(fsbCandidates).get
          (target, USBot.chooseCellsToRemove(target, 1))
        }
        addEventTarget(name)
        removeCellsFromCountry(name, active, sleeper, sadr, addCadre = true)
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(18, "Intel Community", US, 2,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot, CannotNotRemoveLastCell,
      (role: Role, forTrigger: Boolean) => role == game.humanRole  // The bot treats this as unplayable
      ,
      (role: Role) => {
        // See Event Instructions table
        log("US player does not inspect the Jihadist hand in the solo game.")

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
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot, CannotNotRemoveLastCell,
      (role: Role, forTrigger: Boolean) => role == game.humanRole || {
         val turkey = game getMuslim Turkey
         !(turkey.isGood || (turkey.isFair && turkey.isAlly))
      }
      ,
      (role: Role) => {
        addEventTarget(Turkey)
        setGovernance(Turkey, Fair, Some(Ally))
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(20, "King Abdullah", US, 2,
      Remove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot, CannotNotRemoveLastCell,
      (role: Role, forTrigger: Boolean ) => {
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
    entry(new Card(21, "Let’s Roll!", US, 2,
      NoRemove, NoLapsing, NoAutoTrigger,
      (name: String, _: Plot)  => letsRollCandidates contains name
      ,
      CannotNotRemoveLastCell,
      (role: Role, forTrigger: Boolean) => letsRollCandidates.nonEmpty
      ,
      (role: Role) => {
        val plotCandidates    = letsRollCandidates
        val postureCandidates = countryNames(game.nonMuslims filter (_.canChangePosture))
        if (role == game.humanRole) {
          val name = askCountry("Select country with plots: ", plotCandidates)
          val plot = humanPickPlotToAlert(name)
          addEventTarget(name)
          performAlert(name, plot)
          log(s"$US draws one card and adds it to their hand")
          askCardsDrawn(1)
          val postureName = askCountry("Select posture of which country: ", postureCandidates)
          val newPosture = askOneOf(s"New posture for $postureName (Soft or Hard): ", Seq(Soft, Hard)).get
          addEventTarget(postureName)
          setCountryPosture(postureName, newPosture)
        }
        else {
          val PlotInCountry(plot, c) = USBot.selectPriorityPlot(plotCandidates)
          addEventTarget(c.name)
          performAlert(c.name, plot)
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
      () => {
        // Can we remove the last cell on the board?
        val totalCells = (mossadShinBetCandidates map (name => game.getCountry(name).totalCells)).sum
        totalCells > 0 && totalCells == game.totalCellsOnMap
      }
      ,
      (role: Role, forTrigger: Boolean) => mossadShinBetCandidates.nonEmpty
      ,
      (role: Role) => {
        for (name <- mossadShinBetCandidates; c = game.getCountry(name)) {
          addEventTarget(name)
          removeCellsFromCountry(name, c.activeCells, c.sleeperCells, c.hasSadr, addCadre = true)
        }
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(23, "Predator", US, 2,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      () => {
        // Can we remove the last cell on the board?
        predatorCandidates exists (name => USBot.wouldRemoveLastCell(name, 1))
      }
      ,
      (role: Role, forTrigger: Boolean) => predatorCandidates.nonEmpty
      ,
      (role: Role) => {
        val (name, (active, sleeper, sadr)) = if (role == game.humanRole) {
          val target = askCountry("Select country with a cell: ", predatorCandidates)
          (target, askCells(target, 1, sleeperFocus = true))
        }
        else {
          var target = USBot.disruptPriority(predatorCandidates).get
          (target, USBot.chooseCellsToRemove(target, 1))
        }
        addEventTarget(name)
        removeCellsFromCountry(name, active, sleeper, sadr, addCadre = true)
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(24, "Predator", US, 2,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      () => deck(23).eventRemovesLastCell(),
      (role: Role, forTrigger: Boolean) => deck(23).eventConditions(role, forTrigger),
      (role: Role) => deck(23).executeEvent(role)
    )),
    // ------------------------------------------------------------------------
    entry(new Card(25, "Predator", US, 2,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      () => deck(23).eventRemovesLastCell(),      
      (role: Role, forTrigger: Boolean) => deck(23).eventConditions(role, forTrigger),
      (role: Role) => deck(23).executeEvent(role)
    )),
    // ------------------------------------------------------------------------
    entry(new Card(26, "Quartet", US, 2,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot, CannotNotRemoveLastCell,
      (role: Role, forTrigger: Boolean) => {
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
      Remove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot, CannotNotRemoveLastCell,
      (role: Role, forTrigger: Boolean) => (game getMuslim Iraq).totalTroops > 0
      ,
      (role: Role) => {
        addAidMarker(Iraq)
        increasePrestige(1)
        addGlobalEventMarker(SaddamCaptured)
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(28, "Sharia", US, 2,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot, CannotNotRemoveLastCell,
      (role: Role, forTrigger: Boolean) => game.hasMuslim (_.besiegedRegime)
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
      Remove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,  CannotNotRemoveLastCell,
      AlwaysPlayable,
      (role: Role) => {
        setCountryPosture(UnitedKingdom, game.usPosture)
        val schengens = if (role == game.humanRole) {
          val num = askInt("Roll War of Ideas in how many Schengen countries", 0, 3)
          askCountries(num, Schengen)
        }
        else 
          USBot.multipleTargets(3, Schengen, USBot.woiNonMuslimPriority)

        for (name <- schengens)
          performWarOfIdeas(name, 3)
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(30, "UN Nation Building", US, 2,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot, CannotNotRemoveLastCell,
      (role: Role, forTrigger: Boolean) =>
        globalEventNotInPlay(VieiraDeMelloSlain) && 
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
      NoRemove, NoLapsing, NoAutoTrigger,
      (name: String, _: Plot) => List(UnitedStates, UnitedKingdom, Canada) contains name
      ,
      () => {
        // Can we remove the last cell on the board?
        val totalCells = (wireTappingCandidates map (name => game.getCountry(name).totalCells)).sum
        totalCells > 0 && totalCells == game.totalCellsOnMap
      }
      ,
      (role: Role, forTrigger: Boolean) => globalEventNotInPlay(LeakWiretapping) &&
                                           wireTappingCandidates.nonEmpty
      ,
      (role: Role) => {
        for (name <- wireTappingCandidates; c = game getCountry name) {
          addEventTarget(name)
          removeCadreFromCountry(name)
          removeCellsFromCountry(name, c.activeCells, c.sleeperCells, c.hasSadr, addCadre = false)
          for (plot <- c.plots)
            performAlert(name, plot)
        }
        
        if (role == game.humanRole) {
          log(s"$US draws one card and adds it to their hand")
          askCardsDrawn(1)
        }
        else
          log(s"Add one card to the top of the $US Bot hand")
        addGlobalEventMarker(Wiretapping)
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(32, "Back Channel", US, 3,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot, CannotNotRemoveLastCell,
      (role: Role, forTrigger: Boolean) => game.usPosture == Soft && role == game.humanRole && {  // Unplayable by the Bot
        //  Oil price spike can make 3 resource countries unplayable this turn
        val neededOps = (game.muslims filter (m => m.isAdversary && m.resourceValue < 4) map (_.resourceValue)).distinct.sorted
        cacheYesOrNo(s"Do you have a card in hand with and Ops value of ${orList(neededOps)}? ")
      }
      ,
      (role: Role) => {
        val candidates = countryNames(game.muslims filter (m => m.isAdversary && m.resourceValue < 4))
        val name = askCountry("Select adversary country: ", candidates)
        val ops = (game getMuslim name).resourceValue
        log(s"You must discard card with Ops value: $ops")
        askCardsDiscarded(1)
        addEventTarget(name)
        setAlignment(name, Neutral)
        addAidMarker(name)
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(33, "Benazir Bhutto", US, 3,
      Remove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot, CannotNotRemoveLastCell,
      (role: Role, forTrigger: Boolean) => {
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
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot, CannotNotRemoveLastCell,
      (role: Role, forTrigger: Boolean) =>
        globalEventNotInPlay(LeakEnhancedMeasures) &&
        game.usPosture == Hard &&
        game.disruptTargets(3).nonEmpty
      ,
      (role: Role) => if (role == game.humanRole) {
        humanDisrupt(3)
        addGlobalEventMarker(EnhancedMeasures)
        log(s"Take the top card of the $Jihadist Bot's hand")
        askCardsDrawn(1)
      }
      else {
        val target  = USBot.disruptTarget(game disruptTargets 3).get
        log(s"$US performs a Disrupt operation in $target")
        performDisrupt(target)
        addGlobalEventMarker(EnhancedMeasures)
        log(s"$Jihadist (you) must put a random card from your hand on top card of the $US Bot's hand")
        askCardsDrawn(1)
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(35, "Hijab", US, 3,
      Remove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot, CannotNotRemoveLastCell,
      (role: Role, forTrigger: Boolean) =>
        game.numIslamistRule == 0 &&
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
      Remove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot, CannotNotRemoveLastCell,
      (role: Role, forTrigger: Boolean) => {
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
      Remove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot, CannotNotRemoveLastCell,
      (role: Role, forTrigger: Boolean) =>
        role == game.humanRole &&  // Unplayable by the Bot
        game.usPosture == Hard &&
        (game getMuslim Iraq).isAdversary
      ,
      (role: Role) => {
        addEventTarget(Iraq)
        addEventMarkersToCountry(Iraq, IraqiWMD)
        val sources = game.regimeChangeSourcesFor(Iraq)
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
                          else game.getCountry(source).maxDeployFrom
          val numTroops = askInt("How many troops: ", 6, maxTroops)
          performRegimeChange(source, Iraq, numTroops)
        }
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(38, "Libyan Deal", US, 3,
      Remove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot, CannotNotRemoveLastCell,
      (role: Role, forTrigger: Boolean) =>
        ((game getMuslim Iraq).isAlly || (game getMuslim Syria).isAlly) && (game getMuslim Libya).isPoor
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
      Remove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot, CannotNotRemoveLastCell,
      (role: Role, forTrigger: Boolean) =>
        role == game.humanRole                      && // Bot treats as unplayable
        countryEventNotInPlay(Libya, "Libyan Deal") &&
        (game getMuslim Libya).isAdversary          &&
        game.usPosture == Hard
      ,
      (role: Role) => {
        addEventTarget(Libya)
        addEventMarkersToCountry(Libya, LibyanWMD)
        val sources = game.regimeChangeSourcesFor(Libya)
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
                          else game.getCountry(source).maxDeployFrom
          val numTroops = askInt("How many troops: ", 6, maxTroops)
          performRegimeChange(source, Libya, numTroops)
        }
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(40, "Mass Turnout", US, 3,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot, CannotNotRemoveLastCell,
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
    entry(new Card(41, "NATO", US, 3,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot, CannotNotRemoveLastCell,
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
    entry(new Card(42, "Pakistani Offensive", US, 3,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot, CannotNotRemoveLastCell,
      (role: Role, forTrigger: Boolean) =>
        countryEventInPlay(Pakistan, FATA) && (game getMuslim Pakistan).isAlly
      ,
      (role: Role) => {
        removeEventMarkersFromCountry(Pakistan, FATA)
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(43, "Patriot Act", US, 3,
      Remove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,  CannotNotRemoveLastCell,
      AlwaysPlayable,
      (role: Role) => {
        addEventMarkersToCountry(UnitedStates, PatriotAct)
        log("The United State is not adjacent to any country except Canada")
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(44, "Renditions", US, 3,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot, CannotNotRemoveLastCell,
      (role: Role, forTrigger: Boolean) =>
        globalEventNotInPlay(LeakRenditions) && game.usPosture == Hard
      ,
      (role: Role) => if (role == game.humanRole) {
        if (game.disruptTargets(3).nonEmpty)
          humanDisrupt(3)
        addGlobalEventMarker(Renditions)
        log(s"Discard the top card of the $Jihadist Bot's hand")
        askCardsDiscarded(1)
      }
      else {
        val target  = USBot.disruptTarget(game disruptTargets 3) foreach { target =>
          log(s"$US performs a Disrupt operation in $target")
          performDisrupt(target)
        }
        addGlobalEventMarker(Renditions)
        log(s"$US (you) must discard a random card from your hand")
        askCardsDiscarded(1)
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(45, "Safer Now", US, 3,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot, CannotNotRemoveLastCell,
      (role: Role, forTrigger: Boolean) =>
        game.numIslamistRule == 0 && !(game hasCountry (c => c.isGood && (c.totalCells > 0 || c.plots.nonEmpty)))
      ,
      (role: Role) => {
        rollUSPosture()
        increasePrestige(3)
        val candidates = countryNames(game.nonMuslims filter (n => n.name != UnitedStates && n.canChangePosture))
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
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot, CannotNotRemoveLastCell,
      (role: Role, forTrigger: Boolean) => game hasMuslim (m => m.isShiaMix && m.inRegimeChange && m.totalCells > 0)
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
    entry(new Card(47, "The door of Itjihad was closed", US, 3,
      NoRemove, Lapsing, NoAutoTrigger, DoesNotAlertPlot, CannotNotRemoveLastCell,
      AlwaysPlayable,
      (role: Role) => if (role == game.humanRole)
        log(s"On $Jihadist action phases this turn, non-$US events are not playable.")
      else {
        log(s"You ($Jihadist) must select cards to play randomly from your hand")
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(48, "Adam Gadahn", Jihadist, 1,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot, CannotNotRemoveLastCell,
      (role: Role, forTrigger: Boolean) =>
        firstCardOfPhase(Jihadist) &&
        game.cellsToRecruit > 0    &&
        cacheYesOrNo(s"Does the $Jihadist player have another card in hand? (y/n) ")
      ,
      (role: Role) => {
        val  prompt = if (role == game.humanRole)
          "Enter card # of card you wish to use for recruit in the US: "
        else
          s"Enter card # of the next card in the $Jihadist Bot's hand: "
        
        val card = deck(askCardNumber(prompt, allowNone = false).get)
        // Add the card to the list of plays for the turn.
        game = game.copy(plays = PlayedCard(Jihadist, card.number) :: game.plays)
        logCardPlay(Jihadist, card, playable = false)
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
      Remove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot, CannotNotRemoveLastCell,
      (role: Role, forTrigger: Boolean) => forTrigger || game.cellsAvailable > 0
      ,
      (role: Role) => {
        if (game.cellsAvailable > 0) {
          addEventTarget(Somalia)
          testCountry(Somalia)
          addSleeperCellsToCountry(Somalia, 1)
        }
        else {
          addEventTarget(Somalia)
          log(s"There are no cells available to place in $Somalia.")
        }
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(50, "Ansar al-Islam", Jihadist, 1,
      Remove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot, CannotNotRemoveLastCell,
      (role: Role, forTrigger: Boolean) =>
        !((game getMuslim Iraq).isUntested || (game getMuslim Iraq).isGood) &&
        (forTrigger || game.cellsAvailable > 0)
      ,
      (role: Role) => {
        // If no cells available, the event was trigger simply so the 
        // card can be removed.
        val candidates = List(Iraq, Iran)
        val name = if (role == game.humanRole)
          askCountry("Place cell in which country: ", candidates)
        else
          JihadistBot.recruitTravelToPriority(candidates).get
        
        addEventTarget(name)
        if (game.cellsAvailable > 0) {
          testCountry(name)
          addSleeperCellsToCountry(name, 1)
        }
        else
          log(s"There are no cells available to place in $name.")
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(51, "FREs", Jihadist, 1,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot, CannotNotRemoveLastCell,
      (role: Role, forTrigger: Boolean) => (game getMuslim Iraq).totalTroops > 0 && game.cellsAvailable > 0
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
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot, CannotNotRemoveLastCell,
      (role: Role, forTrigger: Boolean) => game hasMuslim (m => m.inRegimeChange && m.totalCells > 0)
      ,
      (role: Role) => {
        if (role == game.humanRole)
          log(s"Discard the top card of the $US Bot's hand")
        else
          log(s"You ($US) must randomly discard one card")
        askCardsDiscarded(1)
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(53, "Madrassas", Jihadist, 1,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot, CannotNotRemoveLastCell,
      (role: Role, forTrigger: Boolean) => firstCardOfPhase(Jihadist) &&
                      game.cellsAvailable > 0    && // Ignore funding
                      cacheYesOrNo(s"Does the $Jihadist player have another card in hand? (y/n) ")
      ,
      (role: Role) => {
        val  prompt = if (role == game.humanRole)
          "Enter card # of card you wish to use for recruit: "
        else
          s"Enter card # of the next card in the $Jihadist Bot's hand: "
        
        val card = deck(askCardNumber(prompt, allowNone = false).get)
        // Add the card to the list of plays for the turn.
        game = game.copy(plays = PlayedCard(Jihadist, card.number) :: game.plays)
        logCardPlay(Jihadist, card, playable = false)
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
      Remove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot, CannotNotRemoveLastCell,
      (role: Role, forTrigger: Boolean) => (game getMuslim Iraq).totalTroops > 0
      ,
      (role: Role) => addEventMarkersToCountry(Iraq, Sadr)
    )),
    // ------------------------------------------------------------------------
    entry(new Card(55, "Uyghur Jihad", Jihadist, 1,
      Remove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot, CannotNotRemoveLastCell,
      (role: Role, forTrigger: Boolean) => forTrigger || game.cellsAvailable > 0 || (game getNonMuslim China).isUntested
      ,
      (role: Role) => {
        addEventTarget(China)
        testCountry(China)
        if (game.cellsAvailable > 0) {
          if ((game getNonMuslim China).isSoft) 
            addSleeperCellsToCountry(China, 1)
          else {
            addEventTarget(CentralAsia)
            testCountry(CentralAsia)
            addSleeperCellsToCountry(CentralAsia, 1)
          }          
        }
        else {
          val target = if ((game getNonMuslim China).isSoft) China else CentralAsia

          log(s"There are no cells available to place in $target.")
        }
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(56, "Vieira de Mello Slain", Jihadist, 1,
      Remove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot, CannotNotRemoveLastCell,
      (role: Role, forTrigger: Boolean) => game hasMuslim (m => m.inRegimeChange && m.totalCells > 0)
      ,
      (role: Role) => {
        decreasePrestige(1)
        addGlobalEventMarker(VieiraDeMelloSlain)
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(57, "Abu Sayyaf", Jihadist, 2,
      Remove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot, CannotNotRemoveLastCell,
      (role: Role, forTrigger: Boolean) => countryEventNotInPlay(Philippines, MoroTalks)
      ,
      (role: Role) => {
        addEventTarget(Philippines)
        testCountry(Philippines)
        if (game.cellsAvailable > 0)
          addSleeperCellsToCountry(Philippines, 1)
        else
          log(s"There are no cells available to place in $Philippines")
        addEventMarkersToCountry(Philippines, AbuSayyaf)
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(58, "Al-Anbar", Jihadist, 2,
      Remove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot, CannotNotRemoveLastCell,
      (role: Role, forTrigger: Boolean) => globalEventNotInPlay(AnbarAwakening)
      ,
      (role: Role) => {
        addEventTarget(Iraq)
        testCountry(Iraq)
        if (game.cellsAvailable > 0)
          addSleeperCellsToCountry(Iraq, 1)
        else
          log(s"There are no cells available to place in $Iraq")
        addGlobalEventMarker(AlAnbar)
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(59, "Amerithrax", Jihadist, 2,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot, CannotNotRemoveLastCell,
      (role: Role, forTrigger: Boolean) => role == game.botRole // human player cannot play against US Bot!
      ,
      (role: Role) => {
        log(s"You ($US) must discard your highest-value US associated card (if any)")
        askCardsDiscarded(1)
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(60, "Bhutto Shot", Jihadist, 2,
      Remove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot, CannotNotRemoveLastCell,
      (role: Role, forTrigger: Boolean) => (game getMuslim Pakistan).totalCells > 0
      ,
      (role: Role) => {
        removeEventMarkersFromCountry(Pakistan, BenazirBhutto)
        addEventMarkersToCountry(Pakistan, BhuttoShot)
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(61, "Detainee Release", Jihadist, 2,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot, CannotNotRemoveLastCell,
      (role: Role, forTrigger: Boolean) => lapsingEventNotInPlay(GTMO)      &&
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
        if (role == game.humanRole) {
          log(s"Draw a card and add it to your hand")
          askCardsDrawn(1)
        }
        else
          log(s"Add a card to the top of the $Jihadist Bot's hand")
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(62, "Ex-KGB", Jihadist, 2,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot, CannotNotRemoveLastCell,
      (role: Role, forTrigger: Boolean) =>
        countryEventInPlay(Russia, CTR) ||
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
          askMenu("\nChoose one:", choices).head
        }
        else if (canShift) 
          CentralAsia
        else
          Caucasus
        
        addEventTarget(target)
        target match {
          case Russia   => 
            removeEventMarkersFromCountry(Russia, CTR)
          case Caucasus => 
            setCountryPosture(Caucasus, oppositePosture(game.usPosture))
          case _        => 
            testCountry(CentralAsia)
            shiftAlignmentRight(CentralAsia)
        }
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(63, "Gaza War", Jihadist, 2,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot, CannotNotRemoveLastCell,
      AlwaysPlayable,
      (role: Role) => {
        increaseFunding(1)
        decreasePrestige(1)
        if (role == game.humanRole)
          log(s"Discard the top card of the $US Bot's hand")
        else
          log(s"You ($US) must randomly discard one card")
        askCardsDiscarded(1)
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(64, "Hariri Killed", Jihadist, 2,
      Remove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot, CannotNotRemoveLastCell,
      (role: Role, forTrigger: Boolean) => (game getMuslim Lebanon).isUntested || {
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
          worsenGovernance(Syria, 1, canShiftToIR = false)
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(65, "HEU", Jihadist, 2,
      Remove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot, CannotNotRemoveLastCell,
      (role: Role, forTrigger: Boolean) =>
        (List(Russia, CentralAsia) map game.getCountry exists (c => c.totalCells > 0 && !c.hasMarker(CTR)))
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
        val success = if (name == CentralAsia && game.getMuslim(name).isIslamistRule)
          true
        else {
          val ok = die <= game.getCountry(name).governance
          log(s"Die roll: $die  (${if (ok) "Success" else "Failure"})")
          ok
        }
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
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot, CannotNotRemoveLastCell,
      (role: Role, forTrigger: Boolean) => game.cellsAvailable > 0
      ,
      (role: Role) => {
        addEventTarget(UnitedKingdom)
        testCountry(UnitedKingdom)
        addSleeperCellsToCountry(UnitedKingdom, 1)
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(67, "Islamic Jihad Union", Jihadist, 2,
      Remove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot, CannotNotRemoveLastCell,
      (role: Role, forTrigger: Boolean) => forTrigger || game.cellsAvailable > 0
      ,
      (role: Role) => {
        val candidates = List(CentralAsia, Afghanistan)
        val targets = if (game.cellsAvailable == 0)
          Nil
        else if (game.cellsAvailable > 1)
          candidates
        else if (role == game.botRole)
          JihadistBot.recruitTravelToPriority(candidates).toList
        else {
          println("There is only one cell available to be placed")
          askCountry("Select country for cell: ", candidates)::Nil
        }

        addEventTarget(CentralAsia)
        addEventTarget(Afghanistan)

        for (name <- targets) {
          testCountry(name)
          addSleeperCellsToCountry(name, 1)
        }
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(68, "Jemaah Islamiya", Jihadist, 2,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot, CannotNotRemoveLastCell,
      (role: Role, forTrigger: Boolean) => game.cellsAvailable > 0
      ,
      (role: Role) => {
        addEventTarget(IndonesiaMalaysia)
        testCountry(IndonesiaMalaysia)
        addSleeperCellsToCountry(IndonesiaMalaysia, 2 min game.cellsAvailable)
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(69, "Kazakh Strain", Jihadist, 2,
      Remove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot, CannotNotRemoveLastCell,
      (role: Role, forTrigger: Boolean) =>
        (game getCountry CentralAsia).totalCells > 0 && countryEventNotInPlay(CentralAsia, CTR)
      ,      
      (role: Role) => {
        addEventTarget(CentralAsia)
        val die = getDieRoll(role)
        val success = if (game.getMuslim(CentralAsia).isIslamistRule)
          true
        else {
          val ok = die <= game.getCountry(CentralAsia).governance
          log(s"Die roll: $die  (${if (ok) "Success" else "Failure"})")
          ok
        }
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
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot, CannotNotRemoveLastCell,
      (role: Role, forTrigger: Boolean) =>
        countryEventNotInPlay(Pakistan, Indo_PakistaniTalks) && game.cellsAvailable > 0
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
      Remove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot, CannotNotRemoveLastCell,
      (role: Role, forTrigger: Boolean) =>
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
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot, CannotNotRemoveLastCell,
      (role: Role, forTrigger: Boolean) =>
        (game getCountry Afghanistan).totalCells > 0 && game.cellsAvailable > 0
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
      Remove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot, CannotNotRemoveLastCell,
      (role: Role, forTrigger: Boolean) => globalEventNotInPlay(MaerskAlabama) && pirates1ConditionsInEffect
      ,
      (role: Role) => addGlobalEventMarker(Pirates2)
    )),
    // ------------------------------------------------------------------------
    entry(new Card(74, "Schengen Visas", Jihadist, 2,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot, CannotNotRemoveLastCell,
      (role: Role, forTrigger: Boolean) => schengenVisasPlayable(role, forTrigger)
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
            addEventTarget(to)
            testCountry(to)
            moveCellsBetweenCountries(from, to, 1, true, forTravel = true)
            i += 1
          }
          for (a <- 1 to sleepers) {
            val to = askCountry(s"Select ${ordinal(i)} destination country: ", Schengen)
            addEventTarget(to)
            testCountry(to)
            moveCellsBetweenCountries(from, to, 1, false, forTravel = true)
            i += 1
          }
        }
      }
      else {
        def nextTravel(destinations: List[String]): Unit = {
          val canTravelFrom = (c: Country) => JihadistBot.hasCellForTravel(c)
          val travellers = game.countries filter canTravelFrom map (c => (c.name, JihadistBot.activeCells(c) > 0))
          if (destinations.size < 2 && travellers.nonEmpty) {
            val canidates = Schengen.filterNot(destinations.contains)
            val to   = JihadistBot.posturePriority(canidates).get
            val from = JihadistBot.travelFromTarget(to, travellers map (_._1) filterNot (_ == to)).get
            val (_, active) = (travellers find (_._1 == from)).get
            addEventTarget(to)
            testCountry(to)
            moveCellsBetweenCountries(from, to, 1, active, forTravel = true)
            JihadistBot.usedCells(to).addSleepers(1)
            nextTravel(to :: destinations)
          }
        }
        nextTravel(Nil)
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(75, "Schroeder & Chirac", Jihadist, 2,
      Remove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot, CannotNotRemoveLastCell,
      (role: Role, forTrigger: Boolean) =>
        game.usPosture == Hard && (
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
      Remove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot, CannotNotRemoveLastCell,
      (role: Role, forTrigger: Boolean) => game hasMuslim (m => m.inRegimeChange && m.totalCells > 0)
      ,
      (role: Role) => {
        if (role == game.humanRole) {
          log("Draw two cards and add them to your hand")
          askCardsDrawn(2)
        }
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
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot, CannotNotRemoveLastCell,
      (role: Role, forTrigger: Boolean) => alJazeeraCandidites.nonEmpty
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
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot, CannotNotRemoveLastCell,
      AlwaysPlayable,
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
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot, CannotNotRemoveLastCell,
      (role: Role, forTrigger: Boolean) => cleanOperativesPlayable(role, forTrigger)
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
            addEventTarget(to)
            testCountry(to)
            moveCellsBetweenCountries(from, to, 1, true, forTravel = true)
            i += 1
          }
          for (a <- 1 to sleepers) {
            val to = askCountry(s"Select ${ordinal(i)} destination country: ", allCountries)
            addEventTarget(to)
            testCountry(to)
            moveCellsBetweenCountries(from, to, 1, false, forTravel = true)
            i += 1
          }
        }
      }
      else {
        def nextTravel(num: Int): Unit = {
          val canTravelFrom = (c: Country) => JihadistBot.hasCellForTravel(c)
          addEventTarget(UnitedStates)
          val travellers = (game.countries filterNot (_.name == UnitedStates)
                                           filter canTravelFrom
                                           map (c => (c.name, JihadistBot.activeCells(c) > 0)))
          if (num <= 2 && travellers.nonEmpty) {
            val from = JihadistBot.travelFromTarget(UnitedStates, travellers map (_._1)).get
            val (_, active) = (travellers find (_._1 == from)).get
            moveCellsBetweenCountries(from, UnitedStates, 1, active, forTravel = true)
            nextTravel(num + 1)
          }
        }
        nextTravel(1)
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(80, "FATA", Jihadist, 3,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot, CannotNotRemoveLastCell,
      (role: Role, forTrigger: Boolean) =>
        countryEventNotInPlay(Pakistan, FATA) || game.cellsAvailable > 0
      ,
      (role: Role) => {
        addEventTarget(Pakistan)
        if (game.cellsAvailable > 0)
          addSleeperCellsToCountry(Pakistan, 1)
        addEventMarkersToCountry(Pakistan, FATA)
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(81, "Foreign Fighters", Jihadist, 3,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot, CannotNotRemoveLastCell,
      (role: Role, forTrigger: Boolean) => {
        val candidates = game.muslims filter (_.inRegimeChange)
        candidates.nonEmpty && 
        (game.cellsAvailable > 0 || (candidates exists (m => m.aidMarkers > 0 || !m.besiegedRegime)))
      }
      ,
      (role: Role) => {
        val candidates = if (game.cellsAvailable > 0)
          countryNames(game.muslims filter (_.inRegimeChange))
        else
          countryNames(game.muslims filter (m => m.inRegimeChange &&
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
    entry(new Card(82, "Jihadist Videos", Jihadist, 3,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot, CannotNotRemoveLastCell,
      (role: Role, forTrigger: Boolean) => game.humanRole == Jihadist || game.cellsAvailable > 0 // Ignores funding
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
    entry(new Card(83, "Kashmir", Jihadist, 3,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot, CannotNotRemoveLastCell,
      (role: Role, forTrigger: Boolean) =>
        countryEventNotInPlay(Pakistan, Indo_PakistaniTalks) && (game.cellsAvailable > 0 || !(game getMuslim Pakistan).isAdversary)
      ,
      (role: Role) => {
        addEventTarget(Pakistan)
        testCountry(Pakistan)
        shiftAlignmentRight(Pakistan)
        if (game.cellsAvailable > 0)
          addSleeperCellsToCountry(Pakistan, 1)
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(84, "Leak", Jihadist, 3,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot, CannotNotRemoveLastCell,
      (role: Role, forTrigger: Boolean) => List(EnhancedMeasures, Renditions, Wiretapping) exists globalEventInPlay
      ,
      (role: Role) => {
        val markers = List(EnhancedMeasures, Renditions, Wiretapping) filter globalEventInPlay
        val marker = if (markers.size == 1)
          markers.head
        else if (role == game.humanRole)
          askOneOf(s"Block which marker (${orList(markers)})? ", markers).get
        else
          shuffle(markers).head
        log()
        removeGlobalEventMarker(marker)
        val leakMarker = marker match {
          case EnhancedMeasures => LeakEnhancedMeasures
          case Renditions       => LeakRenditions
          case Wiretapping      => LeakWiretapping
        }
        addGlobalEventMarker(leakMarker)
        log()
        rollUSPosture()
        log()
        rollPrestige()
        log()
        val candidates = countryNames(game.muslims filter (_.isAlly))
        if (candidates.isEmpty)
          log("There are no Ally Muslim countries.  Shift no possible")
        else {
          val name = if (role == game.humanRole)
            askCountry("Select Ally Muslim country: ", candidates)
          else
            JihadistBot.markerAlignGovTarget(candidates).get
          addEventTarget(name)
          shiftAlignmentRight(name)
        }
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(85, "Leak", Jihadist, 3,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot, CannotNotRemoveLastCell,
      (role: Role, forTrigger: Boolean) => deck(84).eventConditions(role, forTrigger),
      (role: Role) => deck(84).executeEvent(role)
    )),
    // ------------------------------------------------------------------------
    entry(new Card(86, "Lebanon War", Jihadist, 3,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot, CannotNotRemoveLastCell,
      AlwaysPlayable,
      (role: Role) => {
        if (role == game.humanRole)
          log(s"Discard the top card of the $US Bot's hand")
        else
          log(s"You ($US) must randomly discard one card")
        askCardsDiscarded(1)
        decreasePrestige(1)
        if (game.cellsAvailable > 0) {
          val candidates = countryNames(game.muslims filter (_.isShiaMix))
          val name = if (role == game.humanRole)
            askCountry("Select a Shia-Mix country: ", candidates)
          else
            JihadistBot.recruitTravelToPriority(candidates).get
          
          addEventTarget(name)
          testCountry(name)
          addSleeperCellsToCountry(name, 1)
        }
        else
          log("No cells available to place in a Shix-Mix country")
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(87, "Martyrdom Operation", Jihadist, 3,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot, CannotNotRemoveLastCell,
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
        removeCellsFromCountry(target, active, sleeper, sadr, addCadre = true)
        for (plot <- plots)
          addAvailablePlotToCountry(target, plot)
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(88, "Martyrdom Operation", Jihadist, 3,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot, CannotNotRemoveLastCell,
      (role: Role, forTrigger: Boolean) => deck(87).eventConditions(role, forTrigger),
      (role: Role) => deck(87).executeEvent(role)
    )),
    // ------------------------------------------------------------------------
    entry(new Card(89, "Martyrdom Operation", Jihadist, 3,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot, CannotNotRemoveLastCell,
      (role: Role, forTrigger: Boolean) => deck(87).eventConditions(role, forTrigger),
      (role: Role) => deck(87).executeEvent(role)
    )),
    // ------------------------------------------------------------------------
    entry(new Card(90, "Quagmire", Jihadist, 3,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot, CannotNotRemoveLastCell,
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
                case None =>
                  Nil
                case Some(cardNo) =>
                  cardNo :: nextDiscard(num + 1)
              }
            }
          }
              
          for (n <- nextDiscard(1); card = deck(n)) {
            if (n == AvengerCard)
                avengerCardDrawn(discarded = false)
            else if (card.autoTrigger)
              autoTriggerCardDiscarded(n)
            else if (card.eventWillTrigger(Jihadist)) {
              log()
              log(s"""The "${card.name}" event is triggered.""")
              performCardEvent(card, Jihadist, triggered = true)
            }
            else{
              log()
              log(s"""The "${card.name}" event does not trigger.""")
              if (n == CriticalMiddle)
                criticalMiddleReminder()
            }
          }
        }
        else {
          log(s"Discard the top two cards of the $Jihadist Bot's hand")
          askCardsDiscarded(2)
        }
        
        if (game.usPosture != Soft) {
          log(s"\nThe Quagmire event affects the $US posture.", Color.Event)
          setUSPosture(Soft)
        }
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(91, "Regional al-Qaeda", Jihadist, 3,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot, CannotNotRemoveLastCell,
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
    entry(new Card(92, "Saddam", Jihadist, 3,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot, CannotNotRemoveLastCell,
      (role: Role, forTrigger: Boolean) =>
        globalEventNotInPlay(SaddamCaptured) &&
        (game.getMuslim(Iraq)).isPoor &&
        (game.getMuslim(Iraq)).isAdversary &&
        game.funding < 9
      ,
      (role: Role) => {
        log("Set funding to 9")
        game = game.copy(funding = 9)
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(93, "Taliban", Jihadist, 3,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot, CannotNotRemoveLastCell,
      (role: Role, forTrigger: Boolean) =>
        game.cellsAvailable > 0 || 
        game.prestige > 1       ||
        !(game getMuslim Afghanistan).besiegedRegime
      ,
      (role: Role) => {
        val afghanistan = game getMuslim Afghanistan
        val pakistan    = game getMuslim Pakistan
        addEventTarget(Afghanistan)
        addEventTarget(Pakistan)
        testCountry(Afghanistan)
        testCountry(Pakistan)
        if (!afghanistan.besiegedRegime)
          addBesiegedRegimeMarker(Afghanistan)
        if (game.cellsAvailable == 0)
          log("No cells available for placement")
        else if (game.cellsAvailable > 1) {
          addSleeperCellsToCountry(Afghanistan, 1)
          addSleeperCellsToCountry(Pakistan, 1)
        }
        else if (role == game.humanRole) {
          println("There is only 1 cell available for placement")
          val name = askCountry("Place a cell in which country: ", Afghanistan::Pakistan::Nil)
          addSleeperCellsToCountry(name, 1)
        }
        else {
          val name = JihadistBot.recruitTravelToPriority(Afghanistan::Pakistan::Nil).get
          addSleeperCellsToCountry(name, 1)
        }
        
        val delta = if (afghanistan.isIslamistRule || pakistan.isIslamistRule) 3 else 1
        decreasePrestige(delta)
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(94, "The door of Itjihad was closed", Jihadist, 3,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot, CannotNotRemoveLastCell,
      (role: Role, forTrigger: Boolean) =>
        game.targetsThisPhase.testedOrImprovedToFairOrGood.nonEmpty ||
        game.targetsLastPhase.testedOrImprovedToFairOrGood.nonEmpty
      ,
      (role: Role) => {
        val candidates = (game.targetsThisPhase.testedOrImprovedToFairOrGood ++
                         game.targetsLastPhase.testedOrImprovedToFairOrGood).toList
        val name = if (role == game.humanRole)
          askCountry("Select country: ", candidates)
        else
          JihadistBot.markerAlignGovTarget(candidates).get
        
        addEventTarget(name)
        worsenGovernance(name, 1, canShiftToIR = false)
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(95, "Wahhabism", Jihadist, 3,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot, CannotNotRemoveLastCell,
      (role: Role, forTrigger: Boolean) => game.funding < 9 && !(game getMuslim SaudiArabia).isUntested
      ,
      (role: Role) => {
        val saudi = game getMuslim SaudiArabia
        if (saudi.isIslamistRule) {
          log("Set funding to 9")
          game = game.copy(funding = 9)
        }
        else
          increaseFunding(saudi.governance)
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(96, "Danish Cartoons", Unassociated, 1,
      Remove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot, CannotNotRemoveLastCell,
      (role: Role, forTrigger: Boolean) =>
        role == game.humanRole ||
        (role == Jihadist && danishCartoonPlots.nonEmpty) ||
        (role == US && danishCartoonPlots.isEmpty)
      ,                       
      (role: Role) => {
        val posture = if (game.humanRole == US)
          askOneOf(s"New posture for Scandinavia (Soft or Hard): ", Seq(Soft, Hard)).get
        else
          game.usPosture
        
        addEventTarget(Scandinavia)
        setCountryPosture(Scandinavia, posture)
        
        if (danishCartoonPlots.nonEmpty) {
          log()
          val plot = danishCartoonPlots match {
            case p::Nil                            => p
            case plots if game.botRole == Jihadist => shuffle(plots).head
            case plots                             => askPlots(plots, 1).head
          }
          val candidates = countryNames(game.muslims filter (!_.isIslamistRule))
          val name = if (game.humanRole == Jihadist)
            askCountry(s"Place $plot in which country: ", candidates)
          else
            JihadistBot.plotPriority(candidates).get
          
          addEventTarget(name)
          testCountry(name)
          addAvailablePlotToCountry(name, plot)
        }
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(97, "Fatwa", Unassociated, 1,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot, CannotNotRemoveLastCell,
      (role: Role, forTrigger: Boolean) => cacheYesOrNo("Do both players have at least one card in hand? (y/n) ")
      ,
      (role: Role) => {
        log(s"\nTake the top card of the ${game.botRole} Bot's hand.")
        askCardsDrawn(1)


        log("\nPut a random card from your hand (not including the one you just took)")
        log(s"on top card of the ${game.botRole} Bot's hand.")
        askCardsDrawn(1)
        
        // Create a one Op card to satisfy the card play functions.
        val card = new Card(0, "n/a", Unassociated, 1, 
               NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,  CannotNotRemoveLastCell,
               NeverPlayable, (_: Role) =>())
        (role, game.humanRole) match {
          case (US, US)             => humanUsCardPlay(card, false)
          case (US, _)              => USBot.cardPlay(card, false)
          case (Jihadist, Jihadist) => humanJihadistCardPlay(card, false)
          case (Jihadist, _)        => JihadistBot.cardPlay(card, false)
        }
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(98, "Gaza Withdrawal", Unassociated, 1,
      Remove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot, CannotNotRemoveLastCell,
      (role: Role, forTrigger: Boolean) =>
        role == game.humanRole ||
        (role == US && game.funding > 1) ||
        (role == Jihadist && game.cellsAvailable > 0)
      ,
      (role: Role) => if (role == US) {
        if (game.funding > 0)
          decreaseFunding(1)
        else
          log("Funding cannot be decreased, it is already a zero.")
      }
      else {
        addEventTarget(Israel)
        if (game.cellsAvailable > 0)
          addSleeperCellsToCountry(Israel, 1)
        else
          log(s"There are no cells available to place in $Israel")
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(99, "HAMAS Elected", Unassociated, 1,
      Remove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot, CannotNotRemoveLastCell,
      (role: Role, forTrigger: Boolean) =>
        role == game.humanRole ||
        forTrigger ||
        (role != US && (game.prestige > 1 || cacheYesOrNo(s"Do you ($US) have a card in hand? (y/n) ")))
      ,      
      (role: Role) => {
        if (game.humanRole == US)
          log(s"You ($US) must select and discard one card if you have any")
        else
          log(s"Discard the top card of the $US Bot's hand")
        askCardsDiscarded(1)
        decreasePrestige(1)
        decreaseFunding(1)
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(100, "Hizb Ut-Tahrir", Unassociated, 1,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot, CannotNotRemoveLastCell,
      (role: Role, forTrigger: Boolean) => 
        (role == game.humanRole && game.troopCommitment != War) ||
        (role == Jihadist && game.troopCommitment == Overstretch  && game.funding < 9) ||
        (role == US       && game.troopCommitment == LowIntensity && game.funding > 1)
      ,      
      (role: Role) => if (game.troopCommitment == Overstretch)
        increaseFunding(2)
      else if (game.troopCommitment == LowIntensity)
        decreaseFunding(2)
    )),
    // ------------------------------------------------------------------------
    entry(new Card(101, "Kosovo", Unassociated, 1,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot, CannotNotRemoveLastCell,
      (role: Role, forTrigger: Boolean) =>
        role == game.humanRole || role == US || !(game getNonMuslim Serbia).isOppositeUsPosture
      ,
      (role: Role) => {
        increasePrestige(1)
        addEventTarget(Serbia)
        setCountryPosture(Serbia, oppositePosture(game.usPosture))
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(102, "Former Soviet Union", Unassociated, 2,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot, CannotNotRemoveLastCell,
      (role: Role, forTrigger: Boolean) => role == game.humanRole || {
        val ca = game getMuslim CentralAsia
        val jihadOk = !(ca.isAdversary || ca.isIslamistRule || (ca.isPoor && ca.isNeutral))
        val usOk    = !(ca.isAlly || ca.isGood || ca.isUntested || (ca.isFair && ca.isNeutral))
        (role == Jihadist && jihadOk) || (role == US && usOk)
      }
      ,
      (role: Role) => {
        addEventTarget(CentralAsia)
        setAlignment(CentralAsia, Neutral)
        rollGovernance(CentralAsia)
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(103, "Hizballah", Unassociated, 2,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      () => {
        // Can we remove the last cell on the board?
        hizballahCanddidates exists (name => USBot.wouldRemoveLastCell(name, 1))
      }
      ,
      (role: Role, forTrigger: Boolean) => hizballahPlayable(role, forTrigger)
      ,
      (role: Role) => if (role == US) {
        val (name, (active, sleeper, sadr)) = if (role == game.humanRole) {
          val name = askCountry("Remove a cell from which country: ", hizballahCanddidates)
          (name, askCells(name, 1, sleeperFocus = true))
        }
        else {
          val name = USBot.disruptPriority(hizballahCanddidates).get
          (name, USBot.chooseCellsToRemove(name, 1))
        }
        addEventTarget(name)
        removeCellsFromCountry(name, active, sleeper, sadr, addCadre = true)
      }
      else { // Jihadist
        addEventTarget(Lebanon)
        setGovernance(Lebanon, Poor, Some(Neutral))
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(104, "Iran", Unassociated, 2,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      () => {
        // Can we remove the last cell on the board?
        iranCandidates(US) exists (name => USBot.wouldRemoveLastCell(name, 1))
      }
      ,      
      (role: Role, forTrigger: Boolean) => iranCandidates(role).nonEmpty
      ,
      (role: Role) => if (role == US) {
        if (role == game.humanRole) {
          val candidates = iranCandidates(role)
          val prompt = if (candidates contains Iran)
            "Select a Shia-Mix country or Iran: "
          else
            "Select a Shia-Mix country: "
          val name = askCountry(prompt, candidates)
          addEventTarget(name)
          testCountry(name)
          if ((game getCountry name).totalCells > 0) {
            val (active, sleeper, sadr) = askCells(name, 1, sleeperFocus = true)
            removeCellsFromCountry(name, active, sleeper, sadr, addCadre = true)
          }
        }
        else {
          // Candidates for the Bot always contain at least 1 cell!
          val name = USBot.disruptPriority(iranCandidates(role)).get
          val (active, sleeper, sadr) = USBot.chooseCellsToRemove(name, 1)
          addEventTarget(name)
          removeCellsFromCountry(name, active, sleeper, sadr, addCadre = true)
        }
      }
      else {  // Jihadist
        val name = if (role == game.humanRole)
          askCountry("Select a Shix-Mix country: ", iranCandidates(role))
        else
          JihadistBot.iranTarget(iranCandidates(role)).get
        
        addEventTarget(name)
        testCountry(name)
        performJihads(JihadTarget(name, 2, 0, false, major = false)::Nil, ignoreFailures = true)
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(105, "Iran", Unassociated, 2,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      () => deck(104).eventRemovesLastCell(),
      (role: Role, forTrigger: Boolean) => deck(104).eventConditions(role, forTrigger),
      (role: Role) => deck(104).executeEvent(role)
    )),
    // ------------------------------------------------------------------------
    entry(new Card(106, "Jaysh al-Mahdi", Unassociated, 2,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      () => {
        // Can we remove the last cell on the board?
        jayshAlMahdiCandidates exists (name => USBot.wouldRemoveLastCell(name, 2))
      }
      ,
      (role: Role, forTrigger: Boolean) => (role == US && jayshAlMahdiCandidates.nonEmpty) || 
       (role == Jihadist && {
           // Do not include Poor or Islamist Rule since they cannot be degraded
         jayshAlMahdiCandidates map game.getMuslim exists (m => m.isGood || m.isFair) 
       })
      ,      
      (role: Role) => if (role == US) {
        val (name, (actives, sleepers, sadr)) = if (role == game.humanRole) {
          val name = askCountry("Select a Shia-Mix country with troops and cells: ", jayshAlMahdiCandidates)
          val num = 2 min (game getMuslim name).totalCells
          (name, askCells(name, num, sleeperFocus = true))
        }
        else {
          val name = USBot.disruptPriority(jayshAlMahdiCandidates).get
          val num = 2 min (game getMuslim name).totalCells
          (name, USBot.chooseCellsToRemove(name, num))
        }
        addEventTarget(name)
        removeCellsFromCountry(name, actives, sleepers, sadr, addCadre = true)
      }
      else {  // Jihadist
        val candidates = countryNames(jayshAlMahdiCandidates 
                                        map game.getMuslim
                                        filter (m => m.isGood || m.isFair))
        
        val name = if (role == game.humanRole)
          askCountry("Select a Shia-Mix country with troops and cells: ", candidates)
        else
          JihadistBot.markerAlignGovTarget(candidates).get
        
        addEventTarget(name)
        worsenGovernance(name, 1, canShiftToIR = false)
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(107, "Kurdistan", Unassociated, 2,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot, CannotNotRemoveLastCell,
      (role: Role, forTrigger: Boolean) => role == US || 
        (List(Turkey, Iraq) map game.getMuslim exists (m => !(m.isPoor || m.isIslamistRule)))
      ,
      (role: Role) => if (role == US) {
        addEventTarget(Iraq)
        testCountry(Iraq)
        addAidMarker(Iraq)
      }
      else { // Jihadist
        addEventTarget(Turkey)
        testCountry(Turkey)
        val candidates = countryNames(List(Turkey, Iraq) 
                             map game.getMuslim
                             filter (m => !(m.isPoor || m.isIslamistRule)))
        // candidates could be empty if Iraq is Poor and Turkey just tested to Poor
        if (candidates.nonEmpty) {
          val name = if (role == game.humanRole)
            askCountry("Select country degrade governance: ", candidates)
          else
            JihadistBot.markerAlignGovTarget(candidates).get
          
          addEventTarget(name)
          worsenGovernance(name, 1, canShiftToIR = false)
        }
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(108, "Musharraf", Unassociated, 2,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      () => {
        // Can we remove the last cell on the board?
        USBot.wouldRemoveLastCell(Pakistan, 1)
      }
      ,
      (role: Role, forTrigger: Boolean) => countryEventNotInPlay(Pakistan, BenazirBhutto) && {
        val pak = game getMuslim Pakistan
        pak.totalCells > 0 &&
        (role == game.humanRole ||
         (role == Jihadist && !(pak.isAdversary || pak.isNeutral || pak.isPoor || pak.isIslamistRule)) ||
         (role == US       && !(pak.isAlly || (pak.isFair && pak.isNeutral) || pak.isGood)))
      }
      ,
      (role: Role) => {
        val (active, sleeper, sadr) = if (role == game.humanRole)
          askCells(Pakistan, 1, sleeperFocus = role == US)
        else if (role == US)
          USBot.chooseCellsToRemove(Pakistan, 1)
        else
          JihadistBot.chooseCellsToRemove(Pakistan, 1)
        addEventTarget(Pakistan)
        removeCellsFromCountry(Pakistan, active, sleeper, sadr, addCadre = true)
        setGovernance(Pakistan, Poor, Some(Ally))
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(109, "Tora Bora", Unassociated, 2,
      Remove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      () => {
        // Can we remove the last cell on the board?
        toraBoraCandidates exists (name => USBot.wouldRemoveLastCell(name, 2))
      }
      ,
      (role: Role, forTrigger: Boolean) => toraBoraCandidates.nonEmpty
      ,
      (role: Role) => {
        val (name, (actives, sleepers, sadr)) = if (role == game.humanRole) {
          val name = askCountry("Select country: ", toraBoraCandidates)
          (name, askCells(name, 2, sleeperFocus = role == US))
        }
        else if (role == US) {
          val name = USBot.disruptPriority(toraBoraCandidates).get
          (name, USBot.chooseCellsToRemove(name, 2))
        }
        else {
          val name = shuffle(toraBoraCandidates).head
          (name, JihadistBot.chooseCellsToRemove(name, 2))
        }
        
        addEventTarget(name)
        removeCellsFromCountry(name, actives, sleepers, sadr, addCadre = true)
        rollPrestige()
        if (role == game.humanRole)
          log(s"draw a card and add it to your hand")
        else
          log(s"Draw a card and place it on top of the $role Bot's hand")
        askCardsDrawn(1)
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(110, "Zarqawi", Unassociated, 2,
      USRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot, CannotNotRemoveLastCell,
      (role: Role, forTrigger: Boolean) => zarqawiCandidates.nonEmpty &&
        ((role == US && game.prestige < 12) ||
         (role == Jihadist && (game.cellsAvailable > 0 || (game.availablePlots contains Plot2))))
      ,
      (role: Role) => if (role == US) {
        increasePrestige(3)
      }
      else { // Jihadist
        // Can create Caliphate (onlyinIraq,Syria,Lebanon,orJordan)
        val name = if (role == game.humanRole)
          askCountry("Select country: ", zarqawiCandidates)
        else if (game.availablePlots contains Plot2)
          JihadistBot.plotPriority(zarqawiCandidates).get
        else
          JihadistBot.recruitTravelToPriority(zarqawiCandidates).get
        
        addEventTarget(name)
        val num = 3 min game.cellsAvailable
        addSleeperCellsToCountry(name, num)
        if (num == 3 && canDeclareCaliphate(name) &&
          ((role == game.humanRole && askDeclareCaliphate(name)) ||
           (role == game.botRole   && JihadistBot.willDeclareCaliphate(name)))) {
          declareCaliphate(name)
        }
        
        if (game.availablePlots contains Plot2)
          addAvailablePlotToCountry(name, Plot2, visible = true)
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(111, "Zawahiri", Unassociated, 2,
      USRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot, CannotNotRemoveLastCell,
      (role: Role, forTrigger: Boolean) => (role == Jihadist && game.prestige > 1) ||
          (role == US &&
           game.funding > 1 &&
           game.numIslamistRule == 0 &&
           globalEventNotInPlay(AlAnbar) &&
           countryEventNotInPlay(Pakistan, FATA))
      ,
      (role: Role) => if (role == US)
        decreaseFunding(2)
      else   // Jihadist
        decreasePrestige(if (game.numIslamistRule > 0) 3 else 1)
    )),
    // ------------------------------------------------------------------------
    entry(new Card(112, "Bin Ladin", Unassociated, 3,
      USRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot, CannotNotRemoveLastCell,
      (role: Role, forTrigger: Boolean) => (role == Jihadist && game.prestige > 1) ||
          (role == US &&
           (game.funding > 1 || game.prestige < 12) &&
           game.numIslamistRule == 0 &&
           globalEventNotInPlay(AlAnbar) &&
           countryEventNotInPlay(Pakistan, FATA))
      ,
      (role: Role) => if (role == US) {
        decreaseFunding(4)
        increasePrestige(1)
      }
      else   // Jihadist
        decreasePrestige(if (game.numIslamistRule > 0) 4 else 2)
    )),
    // ------------------------------------------------------------------------
    entry(new Card(113, "Darfur", Unassociated, 3,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot, CannotNotRemoveLastCell,
      (role: Role, forTrigger: Boolean) => if (role == game.humanRole )
        true
      else if (role == US)
        (game.prestigeLevel == High || game.prestigeLevel == VeryHigh)
      else {
        val sudan = game getMuslim Sudan
        (game.prestigeLevel == Low || game.prestigeLevel == Medium) &&
        !(sudan.isAdversary && sudan.besiegedRegime)
      }
      ,
      (role: Role) => {
        addEventTarget(Sudan)
        testCountry(Sudan)
        if (game.prestigeLevel == High || game.prestigeLevel == VeryHigh) {
          addAidMarker(Sudan)
          shiftAlignmentLeft(Sudan)
        }
        else {
          addBesiegedRegimeMarker(Sudan)
          shiftAlignmentRight(Sudan)
        }
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(114, "GTMO", Unassociated, 3,
      NoRemove, Lapsing, NoAutoTrigger, DoesNotAlertPlot, CannotNotRemoveLastCell,
      (role: Role, forTrigger: Boolean) => role == game.humanRole ||
        (role == US && game.prestigeLevel != High && game.prestigeLevel != VeryHigh) ||
        (role == Jihadist && game.prestigeLevel != Low)
      ,
      (role: Role) => {
        rollPrestige()
        log("No recruit operations or Detainee Release for the rest of this turn")
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(115, "Hambali", Unassociated, 3,
      USRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      () => {
        // Can we remove the last cell on the board?
        hambaliCandidates exists (name => USBot.wouldRemoveLastCell(name, 1))
      }
      ,
      (role: Role, forTrigger: Boolean) => hambaliCandidates.nonEmpty && (role == US || game.availablePlots.nonEmpty)
      ,
      (role: Role) => if (role == US) {
        val (name, (active, sleeper, sadr)) = if (role == game.humanRole) {
          val name = askCountry("Select country: ", hambaliCandidates)
          (name, askCells(name, 1, sleeperFocus = true))
        }
        else {
          val name = USBot.disruptPriority(hambaliCandidates).get
          (name, USBot.chooseCellsToRemove(name, 1))
        }
        addEventTarget(name)
        removeCellsFromCountry(name, active, sleeper, sadr, addCadre = true)
        log(s"$US player draw 2 cards")
        askCardsDrawn(2)
      }
      else {  // Jihadist
        val (name, plot) = if (role == game.humanRole) {
          val name = askCountry("Select country: ", hambaliCandidates)
          (name, askAvailablePlots(1, ops = 3).head)
        }
        else {
          val name = JihadistBot.plotPriority(hambaliCandidates).get
          (name, shuffle(game.availablePlots).head)
        }
        
        addEventTarget(name)
        addAvailablePlotToCountry(name, plot)
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(116, "KSM", Unassociated, 3,
      USRemove, NoLapsing, NoAutoTrigger,
      (name: String, _: Plot) => ksmUSCandidates contains name
      ,
      CannotNotRemoveLastCell,
      (role: Role, forTrigger: Boolean) =>
        (role == US && ksmUSCandidates.nonEmpty) ||
        (role == Jihadist && ksmJihadistCandidates.nonEmpty && game.availablePlots.nonEmpty)
      ,
      (role: Role) => if (role == US) {
        for (name <- ksmUSCandidates; c = game getCountry name) {
          addEventTarget(name)
          for (plot <- c.plots)
            performAlert(name, plot)
        }
        log("The US player draws 2 cards")
        askCardsDrawn(2)
      }
      else {  // Jihadist
        val (name, plot) = if (role == game.humanRole) {
          val name = askCountry("Select country: ", ksmJihadistCandidates)
          (name, askAvailablePlots(1, ops = 3).head)
        }
        else if (ksmJihadistCandidates contains UnitedStates)
          (UnitedStates, shuffle(game.availablePlots).head)
        else {
          val name = JihadistBot.plotPriority(ksmJihadistCandidates).get
          (name, shuffle(game.availablePlots).head)
        }
          
        addEventTarget(name)
        addAvailablePlotToCountry(name, plot)
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(117, "Oil Price Spike", Unassociated, 3,
      NoRemove, Lapsing, NoAutoTrigger, DoesNotAlertPlot, CannotNotRemoveLastCell,
      (role: Role, forTrigger: Boolean) => oilPriceSpikePlayable(role, forTrigger)
      ,
      (role: Role) => {
        if (role == game.humanRole)
          log(s"$role player draws a card other than Oil Price Spike from the discad pile")
        else if (role == US)
          log(s"$US Bot draws highest Ops US associated card (at random) from the discard pile ")
        else
          log(s"$Jihadist Bot draws highest Ops Jihadist associated card (at random) from the discard pile ")
        askCardsDrawn(1)
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(118, "Oil Price Spike", Unassociated, 3,
      NoRemove, Lapsing, NoAutoTrigger, DoesNotAlertPlot, CannotNotRemoveLastCell,
      (role: Role, forTrigger: Boolean) => deck(117).eventConditions(role, forTrigger),
      (role: Role) => deck(117).executeEvent(role)
    )),
    // ------------------------------------------------------------------------
    entry(new Card(119, "Saleh", Unassociated, 3,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot, CannotNotRemoveLastCell,
      (role: Role, forTrigger: Boolean) => {
        val yemen = game getMuslim Yemen
        yemen.isUntested ||
        (role == Jihadist && !(yemen.isAdversary && yemen.besiegedRegime) ) || 
        (role == US && !yemen.isIslamistRule)
      }
      ,
      (role: Role) => {
        addEventTarget(Yemen)
        testCountry(Yemen)
        if (role == US) {
          if (!(game getMuslim Yemen).isIslamistRule) {
            setAlignment(Yemen, Ally)
            addAidMarker(Yemen)
          }
        }
        else {
          shiftAlignmentRight(Yemen)
          addBesiegedRegimeMarker(Yemen)
        }
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(120, "US Election", Unassociated, 3,
      NoRemove, NoLapsing, AutoTrigger, DoesNotAlertPlot, CannotNotRemoveLastCell,
      NeverPlayable,  // No directly playable, but will always auto trigger
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

