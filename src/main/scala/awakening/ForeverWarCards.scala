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
// Awakening expansion.

import scala.util.Random.shuffle
import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

import LabyrinthAwakening._
import USBot.PlotInCountry

object ForeverWarCards {
  
  // Various tests used by the card events
  val abdelFattahCandidate = (m: MuslimCountry) => 
    m.canTakeAwakeningOrReactionMarker && (m.name == Egypt || m.militia > 0)
  
  val backlashCandidate = (m: MuslimCountry) =>
    (m.plots exists (!_.backlashed)) && !game.isCaliphateMember(m.name)

  val foreignInternalDefenseCandidate = (m: MuslimCountry) => m.alignment != Adversary && m.totalCells > 0
  
  val nadiaMuradCandidate = (m: MuslimCountry) => (m.name == Iraq || areAdjacent(m.name, Iraq)) && 
                                                  m.canTakeAwakeningOrReactionMarker            &&
                                                  !game.isCaliphateMember(m.name)
                                                  
  val patrioticArabDemocraciesCandidate = (m: MuslimCountry) => m.alignment != Adversary &&
                                                                m.canTakeAwakeningOrReactionMarker &&
                                                                !game.isCaliphateMember(m.name)
  val saudiAirStrikesCandidate = (m: MuslimCountry) => 
    m.totalCells > 0 && (m.name == SaudiArabia || 
      (areAdjacent(m.name, SaudiArabia) && (m.civilWar || m.inRegimeChange || m.isIslamistRule)))
    
  // Countries with cells and with troops/advisors
  def specialForcesCandidates: List[String] =
    countryNames(game.countries filter (c => c.totalCells > 0 && (c.totalTroops > 0 || c.numAdvisors > 0)))
  
  def arabNatoCandidates: List[String] = {
    val list = game.getMuslim(GulfStates) :: (game.muslims filter (m => m.isSunni || game.adjacentToSunni(m.name)))
    countryNames(list filter (_.canTakeMilitia))
  }
  
  def moabCandidates: List[String] = countryNames(
    game.countries filter (c => c.totalCells > 0 && (c.totalTroops > 0 || c.hasMarker(Advisors)))
  )
  
  def personalSecContractorsCandidates: List[String] = countryNames(
    game.muslims filter (m => m.canTakeMilitia && (m.civilWar || m.inRegimeChange || m.totalCells > m.governance))
  )
  
  def popularMobilForcesCandidates: List[String] = countryNames(
    game.muslims filter (m => m.canTakeMilitia && m.civilWar && m.totalCells > m.totalTroopsAndMilitia)
  )
  
  def trumpTripAlignmentCandidates = countryNames(game.muslims filter (m => !m.isIslamistRule && !m.isAlly))
  def trumpTripPostureCandidates   = countryNames(game.nonMuslims filter (n => n.canChangePosture && n.posture != game.usPosture))
  
  def airAmericaCaliphateCandidates = countryNames(
    game.muslims filter (m => m.totalCells > 0 && game.isCaliphateMember(m.name))
  )
  def airAmericaNonCaliphateCandidates = countryNames(
    game.muslims filter (m => m.totalCells > 0 && (m.civilWar || m.inRegimeChange))
  )
  
  def deepStateCandidates = countryNames(
    game.getMuslims(Egypt::Syria::Pakistan::Turkey::Nil) filter (m => !(m.isUntested || m.isIslamistRule || m.civilWar || m.isGood))
  )
  
  def sfabCandidates = countryNames(
    game.muslims filter (m => m.civilWar && m.alignment != Adversary && m.totalTroops == 0)
  )
  
  //  Candidates are any Muslim country will cells that is adjacents
  //  to another Muslim country of the opposite type (Sunni/Shia Mix)
  def sunniShiaRiftCandidates = {
    val adjOther = (m: MuslimCountry) => (m.isSunni && game.adjacentToShiaMix(m.name)) ||
                                         (m.isShiaMix && game.adjacentToSunni(m.name))
    countryNames(game.muslims filter (m => m.totalCells > 0 && adjOther(m)))
  }
  
  def mohamedMorsiCandidates = countryNames(
    game.muslims filter (m => (m.name == Egypt || m.militia > 0) && m.canTakeAwakeningOrReactionMarker)
  )
  
  def palestinianPeaceCandidates = countryNames(
    game.adjacentMuslims(Israel) filter (_.canTakeAwakeningOrReactionMarker)
  )
  
   def sayyedHassanReactionCandidates = countryNames(
     game.muslims filter (m => m.canTakeAwakeningOrReactionMarker && distance(Lebanon, m.name) <= 2)
   )
   
   def sayyedHassanBesiegedRegimeCandidates = countryNames(
     game.muslims filter (m => (m.canTakeBesiegedRegimeMarker) && distance(Lebanon, m.name) <= 2)
   )
   
   def sayyedHassanCellCandidates = countryNames(
     game.countries filter (c => distance(Lebanon, c.name) <= 2)
   )
  
  def vehicleRammingCandidates = countryNames(
    game.getNonMuslims(Schengen) filter (_.isHard)
    
  )
  
  def amaqNewsAgencyCandidates =  countryNames(
    game.countries filter (c => c.totalCells == 0 && c.hasCadre == false)
  )
  
  def attemptedCoupCandidates = countryNames(
    game.muslims filter (m => m.totalCells >= 2 && !m.civilWar &&
                         ((m.isAlly && m.isPoor) || (game.adjacentToCivilWar(m.name) && !m.isGood)))
  )
  
  def earlyExitCandidates = countryNames(
    game.countries filter (c => (c.hasCadre || c.totalCells > 0) && (c.totalTroops > 0 || c.numAdvisors > 0))
  )
  
  def goingUnderGroundBotMajorCandidates = countryNames(
    game.getMuslims(game.majorJihadTargets(2)) filter (m => m.isPoor && JihadistBot.jihadSuccessPossible(m, true))
  )
  
  def goingUnderGroundBotMinorCandidates = countryNames(
    game.getMuslims(game.jihadTargets) filter (m => (m.isFair || m.isGood) && JihadistBot.jihadSuccessPossible(m, false))
  )
  
  // Convenience method for adding a card to the deck.
  private def entry(card: Card) = (card.number -> card)
  
  val deckMap: Map[Int, Card] = Map(
    // ------------------------------------------------------------------------
    entry(new Card(241, "Abdel Fattah el-Sisi", US, 1,
      Remove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (_: Role, _: Boolean) => globalEventNotInPlay(PoliticalIslamismJihadist) && (game hasMuslim abdelFattahCandidate)
      ,
      (role: Role) => {
        val egypt = if (game.getMuslim(Egypt).canTakeAwakeningOrReactionMarker) List(Egypt) else Nil
        val candidates = countryNames(game.muslims filter abdelFattahCandidate)
        val target = if (role == game.humanRole)
          askCountry(s"Place an awakening marker in which country: ", candidates)
        else 
          USBot.markerAlignGovTarget(candidates).get
        println()
        addEventTarget(target)
        testCountry(target)
        addAwakeningMarker(target)
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(242, "Avenger", US, 1,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (role: Role, _: Boolean) => 
        (role == game.humanRole && game.hasMuslim(_.totalCells > 0)) ||
        (role == game.botRole && game.hasMuslim(m => m.totalCells - m.totalTroopsAndMilitia > 4)) ||
        cacheQuestion(askYorN(s"Does the $Jihadist player have any cards in hand? (y/n) "))
      ,
      (role: Role) => {
        if (role == game.humanRole) {
          val candidates = countryNames(game.muslims filter (_.totalCells > 0))
          val canDiscard = cacheQuestion(askYorN(s"Does the $Jihadist player have any cards in hand? (y/n) "))
          val choices = List(
            choice(candidates.nonEmpty, "remove", "Remove up to 2 cells in any one Muslim country"),
            choice(canDiscard,          "discard", s"Discard top card of the $Jihadist hand")
          ).flatten
          askMenu("\nChoose one:", choices).head match {
            case "discard" => 
              println()
              log(s"Discard top card of the $Jihadist hand")
            case _ =>
              val target = askCountry("Remove cells from which country: ", candidates)
              val num = game.getMuslim(target).totalCells min 2
              val (actives, sleepers, sadr) = askCells(target, num, true)
              addEventTarget(target)
              removeCellsFromCountry(target, actives, sleepers, sadr, addCadre = true)
          }
        }
        else {
          // Bot
          val candidates = countryNames(game.muslims.filter(m => m.totalCells - m.totalTroopsAndMilitia > 4))
          if (candidates.nonEmpty)
          {
            val target = USBot.disruptPriority(candidates).get
            val (actives, sleepers, sadr) = USBot.chooseCellsToRemove(target, 2)
            addEventTarget(target)
            removeCellsFromCountry(target, actives, sleepers, sadr, addCadre = true)
          }
          else
            println()
            log(s"You ($Jihadist) must discard one random card")
        }
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(243, "Backlash", US, 1,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (_: Role, _: Boolean) => false
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
    entry(new Card(244, "Foreign Internal Defense", US, 1,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (_: Role, _: Boolean) => game.militiaAvailable > 0 && (game hasMuslim foreignInternalDefenseCandidate)
      ,
      (role: Role) => {
        val candidates = countryNames(game.muslims filter foreignInternalDefenseCandidate)
        val target = if (role == game.humanRole)
          askCountry(s"Select country: ", candidates)
        else 
          USBot.deployToPriority(candidates).get
        
        addEventTarget(target)
        testCountry(target)
        println()
        addMilitiaToCountry(target, 1)
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(245, "Green Movement 2.0", US, 1,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (_: Role, _: Boolean) => {
        game.getCountry(Iran) match {
          case n: NonMuslimCountry => false
          case m: MuslimCountry    => !(m.isIslamistRule || game.isCaliphateMember(Iran)) && m.canTakeAwakeningOrReactionMarker
        }
      }
      ,
      (role: Role) => {
        println()
        addEventTarget(Iran)
        testCountry(Iran)
        addAwakeningMarker(Iran)
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(246, "Holiday Surprise", US, 1,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (_: Role, _: Boolean) => 
        // Check countries here because Philippines can sometimes contain troops
        (game hasCountry (c => (c.totalTroops > 0 || c.numAdvisors > 0) && c.totalCells > 1)) ||
        (game hasMuslim  (m => (m.totalTroops > 0 || m.numAdvisors > 0) && (m.inRegimeChange || m.civilWar)))
      ,
      (role: Role) => {
        increasePrestige(1)
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(247, "Nadia Murad", US, 1,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (_: Role, _: Boolean) => {
        val iraq = game.getMuslim(Iraq)
        (iraq.civilWar || iraq.isIslamistRule || iraq.besiegedRegime || game.isCaliphateMember(Iraq)) &&
        (game hasMuslim nadiaMuradCandidate)
      }
      ,
      (role: Role) => {
        val candidates = countryNames(game.muslims filter nadiaMuradCandidate)
        val target = if (role == game.humanRole)
          askCountry(s"Place an awakening marker in which country: ", candidates)
        else 
          USBot.markerAlignGovTarget(candidates).get
        println()
        addEventTarget(target)
        testCountry(target)
        addAwakeningMarker(target)
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(248, "Patriotic Arab Democracies Movement", US, 1,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (_: Role, _: Boolean) => game hasMuslim patrioticArabDemocraciesCandidate
      ,
      (role: Role) => {
        val candidates = countryNames(game.muslims filter patrioticArabDemocraciesCandidate)
        val target = if (role == game.humanRole)
          askCountry(s"Place an awakening marker in which country: ", candidates)
        else 
          USBot.markerAlignGovTarget(candidates).get
        println()
        addEventTarget(target)
        testCountry(target)
        addAwakeningMarker(target)
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(249, "Saudi Air Strikes", US, 1,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (_: Role, _: Boolean) => (game.getMuslim(SaudiArabia).isNeutral ||
                               game.getMuslim(SaudiArabia).isAlly)    &&
                               (game hasMuslim saudiAirStrikesCandidate)
      ,
      (role: Role) => {
        val candidates = countryNames(game.muslims filter saudiAirStrikesCandidate)
        val target = if (role == game.humanRole)
          askCountry(s"Remove cell(s) from which country: ", candidates)
        else 
          USBot.disruptPriority(candidates).get

        val c = game.getMuslim(target)
        val num = if (c.name == Yemen) c.totalCells min 2 else 1
          
        val (actives, sleepers, sadr) = if (role == game.humanRole)
          askCells(target, num, true)
        else
          USBot.chooseCellsToRemove(target, num)
        
        addEventTarget(target)
        removeCellsFromCountry(target, actives, sleepers, sadr, addCadre = true)
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(250, "Special Forces", US, 1,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (_: Role, _: Boolean) => specialForcesCandidates.nonEmpty
      ,
      (role: Role) => {
        val target = if (role == game.humanRole)
          askCountry("Remove cell in which country: ", specialForcesCandidates)
        else
          USBot.disruptPriority(specialForcesCandidates).get
        
        val (actives, sleepers, sadr) = if (role == game.humanRole)
          askCells(target, 1, sleeperFocus = true)
        else
          USBot.chooseCellsToRemove(target, 1)
  
        println()
        addEventTarget(target)
        removeCellsFromCountry(target, actives, sleepers, sadr, addCadre = true)
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(251, "Trump Tweets", US, 1,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot, AlwaysPlayable,
      (role: Role) => {
        def logNotZero(value: Int, msg: String): Unit = if (value != 0) log(f"$value%+2d $msg")
          
        def removeAidTarget: Option[String] = {
          if (role == game.botRole)
            USBot.removeAidTarget
          else
            countryNames(game.muslims filter (_.aidMarkers > 0)) match {
              case Nil => None
              case candidates =>
                Some(askCountry("Remove an aid marker from which country: ", candidates))
            }
        }
        
        def addAidTarget: Option[String] = {
          val candidates = countryNames(game.muslims filter (_.canTakeAidMarker))
          if (candidates.isEmpty)
            None
          else if (role == game.botRole) 
            USBot.markerAlignGovTarget(candidates)
          else
            Some(askCountry("Place an aid marker in which country: ", candidates))
        }
        
        //  Note: if scenario is HillaryWins then we add a +1 modifer to the die roll
        val hillaryMod  = if (game.scenarioName == scenarios.HillaryWins.name) 1 else 0
        val prestigeMod = game.prestigeModifier
        val die = getDieRoll(role)
        val modRoll = die + hillaryMod + prestigeMod
        
        log(s"Die roll: $die")
        logNotZero(prestigeMod, "Prestige")
        logNotZero(hillaryMod,  s"${scenarios.HillaryWins.name} scenario rule")
        if (modRoll != die)
          log(s"Modified roll: $modRoll")
        println()
        modRoll match {
          case 0 => 
            val target = randomConvergenceTarget.name
            addEventTarget(target)
            testCountry(target)
            addReactionMarker(target)
          case 1 => decreasePrestige(1)
          case 2 =>
            removeAidTarget foreach { target =>
              addEventTarget(target)
              removeAidMarker(target)
            }
          case 3 => increaseFunding(1)
          case 4 => decreaseFunding(1)
          case 5 => 
            addAidTarget foreach { target =>
              addEventTarget(target)
              testCountry(target)
              addAidMarker(target)
            }            
          case 6 => increasePrestige(1)
          case _ =>
            val target = randomConvergenceTarget.name
            addEventTarget(target)
            testCountry(target)
            addAwakeningMarker(target)
        }
        
        setTrumpTweetsON()
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(252, "Trump Tweets", US, 1,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (role: Role, forTrigger: Boolean) => deck(251).eventConditions(role, forTrigger),
      (role: Role) => deck(251).executeEvent(role)
    )),
    // ------------------------------------------------------------------------
    entry(new Card(253, "Trump Tweets", US, 1,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (role: Role, forTrigger: Boolean) => deck(251).eventConditions(role, forTrigger),
      (role: Role) => deck(251).executeEvent(role)
    )),
    // ------------------------------------------------------------------------
    entry(new Card(254, "US Embassy to Jerusalem", US, 1,
      Remove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (role: Role, _: Boolean) => game.usPosture == Hard && trumpTweetsON &&
                                  (role == game.humanRole || game.prestigeLevel == Low)
      ,
      (role: Role) => {
        rollPrestige()
        increaseFunding(1)
        setTrumpTweetsOFF()
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(255, "Western Arms Sales", US, 1,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (role: Role, _: Boolean) => role == game.humanRole && (game hasMuslim (_.isAlly))
      ,
      (role: Role) => {
        val numToAdd = if (game.getMuslim(SaudiArabia).isAlly) 2 else 1
        addToReserves(US, numToAdd)
        
        val opsInReserve = game.reserves.us
        println()
        println(s"You have ${opsString(opsInReserve)} in reserve.")
        val ops = if (askYorN(s"Do you wish to add them for this operation? (y/n) ")) {
          println()
          log(s"$US player expends their reserves of ${opsString(opsInReserve)}")
          game = game.copy(reserves = game.reserves.copy(us = 0))
          opsInReserve + 1
        }
        else
          1
        humanExecuteOperation(ops)
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(256, "White Helmets", US, 1,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (_: Role, _: Boolean) => (game hasMuslim (m => m.militia > 0 && m.civilWar)) &&
                               (game.prestige < 12 || game.funding > 1)
      ,
      (role: Role) => {
        val action = if (role == game.humanRole) {
          val choices = List(
            choice(game.prestige < 12, "prestige", "+1 Prestige"),
            choice(game.funding > 1,   "funding", "-1 Funding")
          ).flatten
          askMenu("\nChoose one:", choices).head
        }
        else {
          // Bot
          if (game.prestige < 12) "prestige" else "funding"
        }
        action match {
          case "prestige" => increasePrestige(1)
          case _          => decreaseFunding(1)
        }
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(257, "Women's Rights Activism", US, 1,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (_: Role, _: Boolean) => game hasMuslim (_.reaction > 0)
      ,
      (role: Role) => {
        val candidates = countryNames(game.muslims filter (_.reaction > 0))
        val target = if (role == game.humanRole)
          askCountry("Remove reaction marker from which country: ", candidates)
        else
          USBot.markerAlignGovTarget(candidates).get
        
        addEventTarget(target)
        removeReactionMarker(target)
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(258, "75th Ranger Regiment", US, 2,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (_: Role, _: Boolean) => game hasMuslim (m => m.civilWar && m.totalCells > 0)
      ,
      (role: Role) => {
        val candidates = countryNames(game.muslims filter (m => m.civilWar && m.totalCells > 0))
        val target = if (role == game.humanRole)
          askCountry("Remove cells from which Civil War country: ", candidates)
        else
          USBot.disruptPriority(candidates).get
        
        val num = game.getMuslim(target).totalCells min 2
        val (actives, sleepers, sadr) = if (role == game.humanRole)
          askCells(target, num, true)
        else
          USBot.chooseCellsToRemove(target, num)
        
        addEventTarget(target)
        removeCellsFromCountry(target, actives, sleepers, sadr, addCadre = true)
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(259, "Arab NATO", US, 2,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (role: Role, _: Boolean) => (game.getMuslim(SaudiArabia).governance == Good ||
                                   game.getMuslim(GulfStates).governance  == Good) &&
                                  (role == game.humanRole || (game.militiaAvailable > 0 && arabNatoCandidates.nonEmpty))
      ,
      (role: Role) => {
        if (role == game.humanRole) {
          val reposCandidates = {
            val xs = game.getMuslims(SaudiArabia::GulfStates::Turkey::Nil) ::: game.adjacentMuslims(SaudiArabia) :::
                     game.adjacentMuslims(GulfStates) ::: game.adjacentMuslims(Turkey)
            countryNames(xs filter (_.canTakeMilitia))
          }
          
          val reposMilitia = reposCandidates.foldLeft(0) { (sum, name) => sum + game.getMuslim(name).militia }
          
          val choices = List(
            choice(game.militiaAvailable > 0 && arabNatoCandidates.nonEmpty, "place", "Place militia"),
            choice(reposMilitia > 1,                                         "repos", "Reposition militia")
          ).flatten

          askMenu("\nChoose one:", choices).head match {
            case "place" =>
              val numMilita  = game.militiaAvailable min 2
              val candidates = arabNatoCandidates
            
              @tailrec def placeMilitia(numLeft: Int): Unit = {
                numLeft match {
                  case 0 =>
                  case x =>
                    println()
                    val target = askCountry("Place militia in which country: ", candidates)
                    val num    = askInt(s"Place how many militia in $target", 1, numLeft, Some(numLeft))
                    addEventTarget(target)
                    testCountry(target)
                    addMilitiaToCountry(target, num)
                    placeMilitia(numLeft - num)
                }
              }
            
              placeMilitia(numMilita)
                
            case _ =>  // Reposition
              println("\nThe target countries are:")
              println(separator())
              wrap("", reposCandidates) foreach (println(_))
              println()
              println(s"There are a total of $reposMilitia militia in the target countries.")
              println("Assume that we start by taking all of those militia off of the map.")
              println("You will be prompted with the name of each country one by one.")
              println("Specify the number of militia that you would like in each country:")
              case class MilitiaCountry(name: String, newMilitia: Int) {
                val muslim   = game.getMuslim(name)
                def hasLess  = newMilitia < muslim.militia
                def noChange = newMilitia == muslim.militia
              }
              def nextCountry(members: List[String], remaining: Int): List[MilitiaCountry] = {
                members match {
                  case Nil                     => Nil
                  case x::Nil                  => MilitiaCountry(x, remaining) :: Nil
                  case x::xs if remaining == 0 => MilitiaCountry(x, 0) :: nextCountry(xs, 0)
                  case x::xs                   =>
                    val num = askInt(s"How many militia in $x", 0, remaining)
                    MilitiaCountry(x, num) :: nextCountry(xs, remaining - num)
                }
              }
              val placements = nextCountry(reposCandidates, reposMilitia) filterNot (_.noChange)
              if (placements.isEmpty)
                log("No change to the position of the existing militia")
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
          // Bot
          val numMilita = game.militiaAvailable min 2
          
          val candidates = arabNatoCandidates
          //  The militia do not have to be place in the same country
          //  so add them one at a time.
          for (x <- 1 to numMilita) {
            val target = USBot.deployToPriority(candidates).get
            addEventTarget(target)
            testCountry(target)
            addMilitiaToCountry(target, 1)
          }
        }
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(260, "Imran Khan", US, 2,
      Remove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (_: Role, _: Boolean) => {
        val pakistan = game.getMuslim(Pakistan)
        !pakistan.isIslamistRule && pakistan.totalCells == 0 && !game.isCaliphateMember(Pakistan)
      }
      ,
      (role: Role) => {
        testCountry(Pakistan)
        improveGovernance(Pakistan, 1, true)
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(261, "Intel Community", US, 2,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (role: Role, _: Boolean) => role == game.humanRole  // The bot treats this as unplayable
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
    entry(new Card(262, "MOAB", US, 2,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (_: Role, _: Boolean) => moabCandidates.nonEmpty
      ,
      (role: Role) => {
        val target = if (role == game.humanRole)
          askCountry("Remove a cell from which country: ", moabCandidates)
        else
          USBot.disruptPriority(moabCandidates).get
        
        val (actives, sleepers, sadr) = if (role == game.humanRole)
          askCells(target, 1, true)
        else
          USBot.chooseCellsToRemove(target, 1)
        
        addEventTarget(target)
        removeCellsFromCountry(target, actives, sleepers, sadr, addCadre = true)
        increasePrestige(1)
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(263, "Operation Tidal Wave II", US, 2,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (_: Role, _: Boolean) => game.caliphateDeclared && game.funding > 0
      ,
      (role: Role) => {
        decreaseFunding(2)
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(264, "Personal Security Contractors", US, 2,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (_: Role, _: Boolean) => personalSecContractorsCandidates.nonEmpty && game.militiaAvailable > 0
      ,
      (role: Role) => {
        val target = if (role == game.humanRole)
          askCountry("Place militia in which country: ", personalSecContractorsCandidates)
        else
          USBot.deployToPriority(personalSecContractorsCandidates).get
        
        val num = game.getMuslim(target).resourceValue min game.militiaAvailable
        addEventTarget(target)
        testCountry(target)
        addMilitiaToCountry(target, num)
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(265, "Popular Mobilization Forces", US, 2,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (_: Role, _: Boolean) => popularMobilForcesCandidates.nonEmpty && game.militiaAvailable > 0
      ,
      (role: Role) => {
        val target = if (role == game.humanRole)
          askCountry("Place militia in which country: ", popularMobilForcesCandidates)
        else
          USBot.deployToPriority(personalSecContractorsCandidates).get
          
          val num = game.getMuslim(target).governance min game.militiaAvailable
          addEventTarget(target)
          testCountry(target)
          addMilitiaToCountry(target, num)
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(266, "Presidential Reality Show", US, 2,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (role: Role, _: Boolean) => {
        val prereq = !(game hasMuslim (m => m.totalTroops > 0 && (m.civilWar || m.inRegimeChange)))
        val cardExists = cacheQuestion(askYorN("""Is there a "Trump Tweets" card or any card with prerequisite "Trump Tweets ON" in the discard pile? (y/n) """))
        prereq && cardExists
      }
      ,
      (role: Role) => {
        if (role == game.humanRole) {
          log()
          log(s"""$role player draws from the discard pile either "Trump Tweets" or""")
          log("""a card with the prerequisite "Trump Tweets ON".""")
        }
        else {
          // Bot
          println()
          val getTrumpTweets = if (!trumpTweetsON)
            askYorN("Is there a \"Trump Tweets\" card in the discard pile? (y/n) ")
          else
            false
          
          if (getTrumpTweets) {
            log("""Place the "Trump Tweets" card closest to the bottom of the""")
            log(s"discard pile on top of the $US hand.")            
          }
          else {
            log("Place the card closest to the bottom of the discard pile")
            log(s"""with a prerequisite of "Trump Tweets ON" on top of the $US hand.""")
          }
        }
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(267, "Third Offset Strategy", US, 2,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (_: Role, _: Boolean) => cacheQuestion(askYorN(s"Does the $Jihadist player have more than one card in hand? (y/n) "))
      ,
      (role: Role) => {
        if (role == game.humanRole)
          log(s"Discard the top card of the $Jihadist hand.")
        else
          log(s"You ($Jihadist) must randomly discard one card.")
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(268, "Trump Trip", US, 2,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (_: Role, _: Boolean) => trumpTripAlignmentCandidates.nonEmpty || trumpTripPostureCandidates.nonEmpty
      ,
      (role: Role) => {
        val action = if (role == game.humanRole) {
          val choices = List(
            choice(trumpTripAlignmentCandidates.nonEmpty, "align",   "Shift alignment of 1 country"),
            choice(trumpTripPostureCandidates.nonEmpty,   "posture", "Set posture of 1 country")
          ).flatten
          askMenu("\nChoose one:", choices).head
        }
        else if (trumpTripAlignmentCandidates.nonEmpty)  // Bot
          "align"
        else
          "posture"
        
        val target = if (role == game.humanRole)
          askCountry("\nWhich country: ", if (action == "align") trumpTripAlignmentCandidates else trumpTripPostureCandidates)
        else if (action == "align")
            USBot.markerAlignGovTarget(trumpTripAlignmentCandidates).get
        else
          USBot.posturePriority(trumpTripPostureCandidates).get
        
        addEventTarget(target)
        
        action match {
          case "align" =>
            testCountry(target)
            shiftAlignmentLeft(target)
            
          case _ =>
            // No need to test before setting posture
            setCountryPosture(target, game.usPosture)
        }
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(269, "Air America", US, 3,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (_: Role, _: Boolean) => airAmericaCaliphateCandidates.nonEmpty || airAmericaNonCaliphateCandidates.nonEmpty
      ,
      (role: Role) => {
        //  Allows US player to remove up to 4 cells from any calipate members
        //  or up to 3 cells from any Civil War/Regime Change countries
        if (role == game.humanRole) {
          val choices = List(
            choice(airAmericaNonCaliphateCandidates.nonEmpty, "non-cal", "Remove up to 3 cells in Civil War/Regime Change countries"),
            choice(airAmericaCaliphateCandidates.nonEmpty,    "cal",     "Remove up to 4 cells in Caliphate countries")
          ).flatten

          val (candidates, maxCells) = askMenu("\nChoose one:", choices).head match {
            case "non-cal" => (airAmericaNonCaliphateCandidates, 3)
            case _         => (airAmericaCaliphateCandidates, 4)
          }
          
          println()
          val removed = askToRemoveCells(maxCells, candidates, sleeperFocus = true)
          for (CellsToRemove(name, (actives, sleepers, sadr)) <- removed) {
            addEventTarget(name)
            removeCellsFromCountry(name, actives, sleepers, sadr, addCadre = true)
          }
        }
        else {
          // Bot will remove cells from caliphate countries only if it can remove
          // four cells (or there are no Civil War/Regime change countries)
          // Otherwise it will remove the max it can from Civil War/Regime change countries
          val caliphateNum = airAmericaCaliphateCandidates.foldLeft(0) { (sum, name) => game.getMuslim(name).totalCells }
          val (candidates, maxCells) = if (caliphateNum > 3 || airAmericaNonCaliphateCandidates.isEmpty)
            (airAmericaCaliphateCandidates, 4)
          else
            (airAmericaNonCaliphateCandidates, 3)
          
          // We will select the cells one at a time, because
          // removal of a cell could change the Bots priorities
          @tailrec def nextRemoval(numLeft: Int): Unit = {
            val withCells = candidates filter (name => game.getMuslim(name).totalCells > 0)
            if (numLeft > 0 && withCells.nonEmpty) {
              val target = USBot.disruptPriority(withCells).get
              val (actives, sleepers, sadr) = USBot.chooseCellsToRemove(target, 1)
        
              addEventTarget(target)
              removeCellsFromCountry(target, actives, sleepers, sadr, addCadre = true)
              nextRemoval(numLeft - 1)
            }
          }
          
          nextRemoval(maxCells)
        }
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(270, "Deep State", US, 3,
      Remove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (role: Role, _: Boolean) => {
        val candidates = deepStateCandidates
        if (role == game.humanRole)
          candidates.nonEmpty
        else {
          if (candidates.isEmpty)
            false
          else if (game.usPosture == Hard)
            true
          else {
            // If US posture is Soft, Bot will only play event if priority country is already Adversary
            val target = USBot.markerAlignGovTarget(candidates).get
            game.getMuslim(target).isAdversary
          }
        }
      }
      ,
      (role: Role) => {
        val target = if (role == game.humanRole) {
          askCountry("Which country: ", deepStateCandidates)
        }
        else
          USBot.markerAlignGovTarget(deepStateCandidates).get // Bot
        
        println()
        addEventTarget(target)
        testCountry(target)
        improveGovernance(target, 1, true)
        if (game.usPosture == Soft)
          shiftAlignmentRight(target)
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(271, "Expanded ROE", US, 3,
      NoRemove, Lapsing, NoAutoTrigger, DoesNotAlertPlot,
      (_: Role, _: Boolean) => true
      ,
      (role: Role) => {
        log("During Attrition at the end of this turn, Add +1 to the")
        log("number of cells to be removed for each hit secured by the US player.")
        decreasePrestige(1)
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(272, "Fire and Fury", US, 3,
      Remove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (role: Role, _: Boolean) => role == game.humanRole && // Unplayable by Bot
                                  game.troopsAvailable >= 6 &&
                                  game.usPosture == Hard &&
                                  trumpTweetsON
      ,
      (role: Role) => {
        val candidates = countryNames(
          game.muslims filter (m => m.name != Iran && (m.civilWar || m.isAdversary))
        )
        
        if (candidates.nonEmpty && askYorN("Do you wish to perform a Regime Change Operation? (y/n) ")) {
          log()
          log(s"$US performs a Regime Change operation")
          log(separator())
          val dest      = askCountry("Regime change in which country: ", candidates)
          val source    = askCountry("Deploy troops from: ", game.regimeChangeSourcesFor(dest))
          val maxTroops = if (source == "track") game.troopsAvailable
                          else game.getCountry(source).maxDeployFrom
          val numTroops = askInt("Deploy how many troops: ", 6, maxTroops)
          addEventTarget(dest)
          testCountry(dest)
          performRegimeChange(source, dest, numTroops)
        }
        setTrumpTweetsOFF()
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(273, "Fully Resourced COIN", US, 3,
      NoRemove, Lapsing, NoAutoTrigger, DoesNotAlertPlot,
      (_: Role, _: Boolean) => game.usPosture == Hard && (game hasMuslim (_.inRegimeChange))
      ,
      (role: Role) => {
        log("US player will draw 2 additional cards next turn.")
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(274, "Government of National Accord", US, 3,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (_: Role, _: Boolean) => (game hasMuslim (_.civilWar))
      ,
      (role: Role) => {
        val numMilitia = game.militiaAvailable min 2
        val die = dieRoll
        val candidates = countryNames(game.muslims filter (_.civilWar))        
        val target = if (role == game.humanRole)
          askCountry("Which Civil War country: ", candidates)
        else
          USBot.deployToPriority(candidates).get
        
        println()
        log(s"$US chooses to target $target")
        if (numMilitia > 0)
          addMilitiaToCountry(target, numMilitia)
        
        val m = game.getMuslim(target)
        (m.totalTroopsAndMilitia - m.totalCells) match {
          case x if x < 1 =>
            log("There are not more Troops and Militia than Cells, no die roll necessary.")
          case x if x < die =>
            log(s"Troops + Militia - Cells = $x")
            log(s"Die roll = $die, failure")
            log("The Civil War does not end.")
          
          case x =>
            log(s"Troops + Militia - Cells = $x")
            log(s"Die roll = $die, success!")
            log("The Civil War ends.")
            endCivilWar(target)
            increasePrestige(1)
        }
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(275, "Operation Inherent Resolve", US, 3,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (_: Role, _: Boolean) => (game.getMuslims(Iraq::Syria::Nil) exists (_.civilWar)) && globalEventNotInPlay(EarlyExit)
      ,
      (role: Role) => {
        val candidates = countryNames(game.getMuslims(Iraq::Syria::Nil) filter (_.civilWar))
        val target = if (role == game.humanRole)
          askCountry("Place militia and Advisors in which country: ", candidates)
        else
          USBot.deployToPriority(candidates).get
        
        addEventTarget(target)
        testCountry(target)
        
        println()
        if (game.militiaAvailable > 0)
          addMilitiaToCountry(target, 1)
        
        addAdvisorsToCountry(target)
        
        val removeCandidates = countryNames(game.getMuslims(Iraq::Syria::Nil) filter (_.totalCells > 0))
        
        if (role == game.humanRole) {
          println()
          if (removeCandidates.isEmpty)
            log("There are no cells in Iraq or Syria.")
          else {
            val removed = askToRemoveCells(3, removeCandidates, sleeperFocus = true)
            for (CellsToRemove(name, (actives, sleepers, sadr)) <- removed) {
              addEventTarget(name)
              removeCellsFromCountry(name, actives, sleepers, sadr, addCadre = true)
            }
          }
        }
        else {
          // Bot
          // Bot will remove cells from caliphate countries only if it can remove
          // four cells (or there are no Civil War/Regime change countries)
          // Otherwise it will remove the max it can from Civil War/Regime change countries
          val caliphateNum = airAmericaCaliphateCandidates.foldLeft(0) { (sum, name) => game.getMuslim(name).totalCells }
          
          // We will select the cells one at a time, because
          // removal of a cell could change the Bots priorities
          @tailrec def nextRemoval(numLeft: Int): Unit = {
            val withCells = removeCandidates filter (name => game.getMuslim(name).totalCells > 0)
            if (numLeft > 0 && withCells.nonEmpty) {
              val target = USBot.disruptPriority(withCells).get
              val (actives, sleepers, sadr) = USBot.chooseCellsToRemove(target, 1)
        
              addEventTarget(target)
              removeCellsFromCountry(target, actives, sleepers, sadr, addCadre = true)
              nextRemoval(numLeft - 1)
            }
          }
          
          nextRemoval(3)
        }
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(276, "Populism/Euroscepticism", US, 3,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (role: Role, _: Boolean) => {
        val numHard     = game.getNonMuslims(Schengen) count (_.posture == Hard)
        val numSoft     = game.getNonMuslims(Schengen) count (_.posture == Soft)
        val numUntested = game.getNonMuslims(Schengen) count (_.posture == PostureUntested)
        val basicRequirement = game.usPosture == Hard && numSoft > 0
        if (role == game.humanRole)
          basicRequirement
        else {
          val prestigeAdjust = ((numHard + 1) + (if (trumpTweetsON && numUntested > 0) 1 else 0)) - (numSoft - 1)
          basicRequirement && prestigeAdjust > 0
        }
      }
      ,
      (role: Role) => {
        val softCandidates = countryNames(game.getNonMuslims(Schengen) filter (_.posture == Soft))
        val untestedCandidates = countryNames(game.getNonMuslims(Schengen) filter (_.posture == PostureUntested))
        val softTarget = if (role == game.humanRole)
          askCountry("Flip posture of which country: ", softCandidates)
        else
          USBot.posturePriority(softCandidates).get
        
        addEventTarget(softTarget)
        setCountryPosture(softTarget, Hard)
        
        if (trumpTweetsON && untestedCandidates.nonEmpty) {
          println()
          val untestedTarget = if (role == game.humanRole)
            askCountry("Set posture of which untested country: ", untestedCandidates)
          else
            USBot.posturePriority(untestedCandidates).get
        
          setCountryPosture(untestedTarget, Hard)
        }
        
        if (trumpTweetsON) {
          println()
          setTrumpTweetsOFF()
        }
        
        val numHard        = game.getNonMuslims(Schengen) count (_.posture == Hard)
        val numSoft        = game.getNonMuslims(Schengen) count (_.posture == Soft)
        val prestigeAdjust = ((numHard - numSoft) min 3) max -3
        println()
        log(s"${amountOf(numHard, "Hard Schengen")} and ${amountOf(numSoft, "Soft Schengen")}")
        if (prestigeAdjust == 0)
          log("US Prestige is not changed.")
        else if (prestigeAdjust > 0)
          increasePrestige(prestigeAdjust)
        if (prestigeAdjust < 0)
          decreasePrestige(-prestigeAdjust)
        
        println()
        addGlobalEventMarker(Euroscepticism)
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(277, "Regime Change Policy", US, 3,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (role: Role, _: Boolean) => {
        val basicRequirement = game.troopsAvailable >= 6 && game.islamistResources > game.goodResources
        if (role == game.humanRole)
          basicRequirement
        else
          basicRequirement && game.usPosture == Soft && (game hasMuslim (_.isIslamistRule))
      }
      ,
      (role: Role) => {
        // Bot will only select IR countries
        val (target, source, numTroops) = if (role == game.humanRole) {
          val t = askCountry("Regime Change in which country: ", countryNames(game.muslims))
          val s = askCountry("Deploy troops from: ", game.regimeChangeSourcesFor(t))
          val maxTroops = if (s == "track") game.troopsAvailable
                          else game.getCountry(s).maxDeployFrom
          val numTroops = askInt("Deploy how many troops: ", 6, maxTroops)
          (t, s, numTroops)
        }
        else
          (USBot.regimeChangeTarget(countryNames(game.muslims filter (_.isIslamistRule))).get, "track", 6)

        addEventTarget(target)
        testCountry(target)
        performRegimeChange(source, target, numTroops)
        decreasePrestige(2)
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(278, "Siege of Mosul", US, 3,
      NoRemove, Lapsing, NoAutoTrigger, DoesNotAlertPlot,
      (_: Role, _: Boolean) => (game hasMuslim (_.civilWar)) && (game.troopsOnMap + game.militiaOnMap) > game.cellsOnMap
      ,
      (role: Role) => {
        log("During Attrition at the end of this turn, in each Civi War")
        log("the number of cells will be halved and the number of troops")
        log("plus militia will be doubled prior to rolling attrition dice.")
        println()
        increasePrestige(1)
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(279, "SFABs", US, 3,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (_: Role, _: Boolean) => sfabCandidates.nonEmpty
      ,
      (role: Role) => {
        //  Up to two targets
        val candidates = sfabCandidates
        val targets: List[String] = if (role == game.humanRole) {
          @tailrec def nextTarget(selected: List[String]): List[String] = {
            if (selected.size == 2)
              selected.reverse
            else {
              val t = askCountry("Select country: ", candidates)
              nextTarget(t :: selected)
            }
          }
          
          nextTarget(Nil)
        }
        else {
          // Bot
          List(USBot.deployToPriority(candidates).get, USBot.deployToPriority(candidates).get)
        }
        
        for (t <- targets) {
          addEventTarget(t)
          addAdvisorsToCountry(t)
        }
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(280, "Sunni-Shia Rift", US, 3,
      Remove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (_: Role, _: Boolean) => !isIranSpecialCase && sunniShiaRiftCandidates.nonEmpty
      ,
      (role: Role) => {
        val candidates = sunniShiaRiftCandidates
        val maxCells = (candidates map game.getMuslim map (_.totalCells)).sum min 3
        if (role == game.humanRole) {
          println()
          val removed = askToRemoveCells(maxCells, candidates, sleeperFocus = true)
          for (CellsToRemove(name, (actives, sleepers, sadr)) <- removed) {
            addEventTarget(name)
            removeCellsFromCountry(name, actives, sleepers, sadr, addCadre = true)
          }
        }
        else {
          // Bot
          // We will select the cells one at a time, because
          // removal of a cell could change the Bots priorities
          @tailrec def nextRemoval(numLeft: Int): Unit = {
            val withCells = candidates filter (name => game.getMuslim(name).totalCells > 0)
            if (numLeft > 0 && withCells.nonEmpty) {
              val target = USBot.disruptPriority(withCells).get
              val (actives, sleepers, sadr) = USBot.chooseCellsToRemove(target, 1)
        
              addEventTarget(target)
              removeCellsFromCountry(target, actives, sleepers, sadr, addCadre = true)
              nextRemoval(numLeft - 1)
            }
          }
          
          nextRemoval(maxCells)
        }
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(281, "Drone Swarms", Jihadist, 1,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (_: Role, _: Boolean) => (game hasMuslim (_.civilWar)) && (game.availablePlots contains Plot1)
      ,
      (role: Role) => {
        val candidates = countryNames(game.muslims filter (_.civilWar))
        val target = if (role == game.humanRole)
          askCountry("Place plot in which country: ", candidates)
        else
          JihadistBot.plotTarget(candidates).get
        
        addEventTarget(target)
        addAvailablePlotToCountry(target, Plot1)
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(282, "Executive Order 13492", Jihadist, 1,
      Remove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (_: Role, _: Boolean) => (game.usPosture == Hard && game.prestige > 1) ||
                               (game.usPosture == Soft && game.cellsAvailable > 0)
      ,
      (role: Role) => {
        if (game.usPosture == Hard)
          decreasePrestige(1)
        else {
          val target = if (role == game.humanRole)
            askCountry("Place a cell in which country: ", countryNames(game.countries))
          else
            JihadistBot.recruitTravelToPriority(countryNames(game.countries)).get
          
          testCountry(target)
          addSleeperCellsToCountry(target, 1)
        }
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(283, "Lone Wolf", Jihadist, 1,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot, AlwaysPlayable,
      (role: Role) => {
        val testCountries = Canada::Scandinavia::India::Nil
        val untested = countryNames(game.getNonMuslims(testCountries) filter (_.isUntested))
        val tested   = countryNames(game.getNonMuslims(testCountries) filterNot (_.isUntested))
        
        val testTarget = if (role == game.humanRole)
          askCountry("Test which country: ", testCountries)
        else
          game.gwotPenalty match {
            case 0 => shuffle(if (untested.nonEmpty) untested else tested).head
            case _ => shuffle(if (tested.nonEmpty) tested else untested).head
          }
        
        addEventTarget(testTarget)
        log(s"$Jihadist selects $testTarget to be tested")
        if (game.getNonMuslim(testTarget).isUntested)
          testCountry(testTarget)
        else
          log(s"$testTarget is not untested so there is no effect")
        
        val hardCountries = countryNames(game.nonMuslims filter (_.isHard))
        if (hardCountries.nonEmpty && game.cellsAvailable > 0) {
          println()
          val cellTarget = if (role == game.humanRole)
            askCountry("Place a cell in which country: ", hardCountries)
          else
            JihadistBot.recruitTravelToPriority(hardCountries).get
          
          addEventTarget(cellTarget)
          addSleeperCellsToCountry(cellTarget, 1)
        }
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(284, "Manchester Bombing", Jihadist, 1,
      Remove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (role: Role, _: Boolean) =>
        (game.getNonMuslim(UnitedKingdom).totalCells > 0) &&
        (role == game.humanRole || (game.funding < 9 && game.getNonMuslim(UnitedKingdom).posture == game.usPosture))
      ,
      (role: Role) => {
        val newPosture = if (game.getNonMuslim(UnitedKingdom).posture == Hard) Soft else Hard
        addEventTarget(UnitedKingdom)
        setCountryPosture(UnitedKingdom, newPosture)
        increaseFunding(1)
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(285, "Mohamed Morsi Supporters", Jihadist, 1,
      Remove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (_: Role, _: Boolean) => globalEventNotInPlay(PoliticalIslamismUS) && mohamedMorsiCandidates.nonEmpty
      ,
      (role: Role) => {
        val target = if (role == game.humanRole)
          askCountry("Place reaction marker in which country: ", mohamedMorsiCandidates)
        else
          JihadistBot.markerAlignGovTarget(mohamedMorsiCandidates).get
      
        addEventTarget(target)
        testCountry(target)
        addReactionMarker(target)
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(286, "Palestinian Peace", Jihadist, 1,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (_: Role, _: Boolean) => palestinianPeaceCandidates.nonEmpty
      ,
      (role: Role) => {
        val target = if (role == game.humanRole)
          askCountry("Place reaction marker in which country: ", palestinianPeaceCandidates)
        else
          JihadistBot.markerAlignGovTarget(palestinianPeaceCandidates).get

        addEventTarget(target)
        testCountry(target)
        addReactionMarker(target)
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(287, "Sayyed Hassan Nasrallah", Jihadist, 1,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (_: Role, _: Boolean) => !game.getMuslim(Lebanon).isAlly &&
                               (isIranSpecialCase || game.getMuslim(Iran).isAdversary) &&
                               (sayyedHassanReactionCandidates.nonEmpty       ||
                                sayyedHassanBesiegedRegimeCandidates.nonEmpty ||
                                (game.cellsAvailable > 0 && sayyedHassanCellCandidates.nonEmpty))
      ,
      (role: Role) => {
        if (role == game.humanRole) {
          val reactOK   = sayyedHassanReactionCandidates.nonEmpty
          val besiegeOK = sayyedHassanBesiegedRegimeCandidates.nonEmpty
          val cellOK    = game.cellsAvailable > 0 && sayyedHassanCellCandidates.nonEmpty
          val choices = List(
            choice(reactOK,   "reaction", "Place a reaction marker"),
            choice(besiegeOK, "besieged", "Place a besieged regime marker"),
            choice(cellOK,    "cell",     "Place a cell")
          ).flatten

          askMenu("\nChoose one:", choices).head match {
            case "reaction" =>
              val target = askCountry("Place reaction marker in which country: ", sayyedHassanReactionCandidates)
              addEventTarget(target)
              testCountry(target)
              addReactionMarker(target)
              
            case "besieged" =>
              val target = askCountry("Place besieged regime marker in which country: ", sayyedHassanBesiegedRegimeCandidates)
              addEventTarget(target)
              testCountry(target)
              addBesiegedRegimeMarker(target)
            
            case _ =>
              val target = askCountry("Place cell in which country: ", sayyedHassanCellCandidates)
              addEventTarget(target)
              testCountry(target)
              addSleeperCellsToCountry(target, 1)
          }          
        }
        else {
          // Bot
          if (sayyedHassanReactionCandidates.nonEmpty) {
            val target = JihadistBot.markerAlignGovTarget(sayyedHassanReactionCandidates).get
            addEventTarget(target)
            testCountry(target)
            addReactionMarker(target)
          }
          else if (sayyedHassanBesiegedRegimeCandidates.nonEmpty) {
            val target = JihadistBot.markerAlignGovTarget(sayyedHassanBesiegedRegimeCandidates).get
            addEventTarget(target)
            testCountry(target)
            addBesiegedRegimeMarker(target)
          }
          else {
            val target = JihadistBot.recruitTravelToPriority(sayyedHassanCellCandidates).get
            addEventTarget(target)
            testCountry(target)
            addSleeperCellsToCountry(target, 1)
          }
        }
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(288, "Soldiers of the Caliphate", Jihadist, 1,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (_: Role, _: Boolean) => game.caliphateDeclared
      ,
      (role: Role) => {
        val die = dieRoll
        log(s"Die roll = $die")
        die match {
          case 1 =>
            if (game.cellsAvailable > 0) {
              addEventTarget(UnitedStates)
              addSleeperCellsToCountry(UnitedStates, 1)
            }
            else
              log("There are no cells on the funding track.")
              
          case 2 =>
            if (game.availablePlots contains Plot1) {
              val target = randomSchengenCountry.name
              testCountry(target)
              addEventTarget(target)
              addAvailablePlotToCountry(target, Plot1)
            }
            else
              log("There are no available level 1 plots.")
            
          case 3 =>
              val target = randomConvergenceTarget.name
              testCountry(target)
              addEventTarget(target)
              addReactionMarker(target)
              
          case 4 =>
            val candidates = countryNames(game.nonMuslims filter (_.isFair))
            val target = if (role == game.humanRole)
              askCountry("Set the posture of which country: ", candidates)
            else
              JihadistBot.posturePriority(candidates).get
            
            val posture = if (role == game.humanRole)
              askOneOf(s"New posture for $target (Soft or Hard): ", Seq(Soft, Hard)).get
            else
              oppositePosture(game.usPosture)
            
            testCountry(target)
            addEventTarget(target)
            setCountryPosture(target, posture)
            
          case 5 =>
            val candidates = countryNames(game.muslims filter (_.aidMarkers > 0))
            if (candidates.nonEmpty) {
              val target = if (role == game.humanRole)
                askCountry("Remove Aid marker from which country: ", candidates)
              else
                JihadistBot.markerAlignGovTarget(candidates).get
                
              addEventTarget(target)
              removeAidMarker(target)
            }
            else
              log("There are no Aid markers on the map.")
            
          case _ => // 6
            val candidates = countryNames(game.muslims filter (_.canTakeBesiegedRegimeMarker))
            if (candidates.nonEmpty) {
              val target = if (role == game.humanRole)
                askCountry("Place a Besieged Regime marker in which country: ", candidates)
              else
                JihadistBot.markerAlignGovTarget(candidates).get
              
              testCountry(target)
              addEventTarget(target)
              addBesiegedRegimeMarker(target)
            }
            else
              log("There are no countries that can take a Besieged Regime marker.") // Not likely
        }
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(289, "Strait of Hormuz", Jihadist, 1,
      NoRemove, Lapsing, NoAutoTrigger, DoesNotAlertPlot,
      (role: Role, _: Boolean) => {
         val basicReq = (isIranSpecialCase || game.getMuslim(Iran).isAdversary)
         if (role == game.humanRole)
           basicReq
         else {
           // First field of tuple is IR, second is Good
           val (irDown, goodDown) = persianGulfExporters.foldLeft((0, 0)) { case ((ir, good), m) => 
             val newIr   = ir   + (if (m.isIslamistRule) 1 else 0)
             val newGood = good + (if (m.isGood) 1 else 0)
             (newIr, newGood)
           }
           val (irUp, goodUp) = nonPersianGulfExporters.foldLeft((0, 0)) { case ((ir, good), m) => 
             val newIr   = ir +   (if (m.isIslamistRule) 1 else 0)
             val newGood = good + (if (m.isGood) 1 else 0)
             (newIr, newGood)
           }
           // Bot only plays if it would increase IR resources and not increase Good resources
           basicReq && (irUp - irDown) > 0 && (goodUp - goodDown) <= 0
         }
      }
      ,
      (role: Role) => {
        val gulf = countryNames(persianGulfExporters)
        log(s"Resource value of ${andList(gulf)}")
        log("is decreased by 1 until the end of turn.")
        log("Resource value of all other oil exporters")
        log("is increased by 1 until the end of turn.")
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(290, "Uyghur Nationalism", Jihadist, 1,
      Remove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot, AlwaysPlayable,
      (role: Role) => {
        val die = dieRoll
        log(s"Die roll = $die")
        die match {
          case 1 | 2 =>
            if (game.getMuslim(CentralAsia).canTakeAwakeningOrReactionMarker) {
              testCountry(CentralAsia)
              addEventTarget(CentralAsia)
              addReactionMarker(UnitedStates)
            }
            else
              log(s"$CentralAsia cannot take a reaction marker.")
              
          case 3 | 4 =>
            if (game.cellsAvailable > 0) {
              testCountry(China)
              addEventTarget(China)
              addSleeperCellsToCountry(China, 1)
            }
            else
              log("There are no cells on the funding track.")
                      
          case _ => // 5 | 6
            if (game.availablePlots contains Plot1) {
              testCountry(China)
              addEventTarget(China)
              addAvailablePlotToCountry(China, Plot1)
            }
            else
              log("There are no available level 1 plots.")
        }
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(291, "Vehicle-ramming Attacks", Jihadist, 1,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (_: Role, _: Boolean) => lapsingEventNotInPlay(IslamicMaghreb) &&
                               globalEventNotInPlay(TravelBan)       &&
                               (game.availablePlots contains Plot1)  &&
                               vehicleRammingCandidates.nonEmpty
      ,
      (role: Role) => {
        val target = if (role == game.humanRole)
          askCountry("Place level 1 Plot in which Hard Schengen country: ", vehicleRammingCandidates)
        else
          JihadistBot.plotPriority(vehicleRammingCandidates).get
        
        addEventTarget(target)
        addAvailablePlotToCountry(target, Plot1)
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(292, "Amaq News Agency", Jihadist, 2,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (_: Role, _: Boolean) => amaqNewsAgencyCandidates.nonEmpty
      ,
      (role: Role) => {
        @tailrec def nextCadre(num: Int, candidates: List[String]): Unit =
          if (num <= 3 && candidates.nonEmpty) {
            val target = if (role == game.humanRole)
              askCountry(s"Place ${ordinal(num)} cadre is which country: ", candidates)
            else
              JihadistBot.recruitTravelToPriority(candidates).get
            
            testCountry(target)
            addEventTarget(target)
            addCadreToCountry(target)
            nextCadre(num + 1, candidates filterNot (_ == target))
          }
        
        nextCadre(1, amaqNewsAgencyCandidates)
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(293, "Attempted Coup", Jihadist, 2,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (_: Role, _: Boolean) => attemptedCoupCandidates.nonEmpty
      ,
      (role: Role) => {
        val target = if (role == game.humanRole)
          askCountry("Place which country in Civil War: ", attemptedCoupCandidates)
        else
          JihadistBot.fewestCellsPriority(attemptedCoupCandidates).get
      
        addEventTarget(target)
        flipAllSleepersCells(target)
        startCivilWar(target)
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(294, "Barcelona Bombs", Jihadist, 2,
      Remove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (_: Role, _: Boolean) => (game.availablePlots contains Plot1)
      ,
      (role: Role) => {
        val maxPlots = (game.availablePlots count (_ == Plot1)) min 2
        val numPlots = if (maxPlots == 1 || role == game.botRole)
          maxPlots
        else
          askInt(s"Place how many Level 1 plots in $Spain", 1, maxPlots, Some(maxPlots))
        
        testCountry(Spain)
        addEventTarget(Spain)
        for (i <- 1 to numPlots)
          addAvailablePlotToCountry(Spain, Plot1)
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(295, "Black Gold", Jihadist, 2,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (_: Role, _: Boolean) => game.caliphateDeclared && game.funding < 9
      ,
      (role: Role) => {
        val die = dieRoll
        log(s"Die roll = $die")
        increaseFunding((die + 1) / 2)
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(296, "Botched Yemeni Raid", Jihadist, 2,
      Remove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (_: Role, _: Boolean) => game.prestigeLevel == High || game.prestigeLevel == VeryHigh
      ,
      (role: Role) => {
        decreasePrestige(2)
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(297, "Early Exit", Jihadist, 2,
      Remove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (_: Role, _: Boolean) => !game.caliphateDeclared && trumpTweetsON && earlyExitCandidates.nonEmpty
      ,
      (role: Role) => {
        val target = if (role == game.humanRole)
          askCountry("Which country: ", earlyExitCandidates)
        else
          JihadistBot.earlyExitPriority(earlyExitCandidates).get
          
        addEventTarget(target)
        removeAllTroopsFromCountry(target)
        removeAllAdvisorsFromCountry(target)
        decreasePrestige(1)
        addGlobalEventMarker(EarlyExit)
        setTrumpTweetsOFF()
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(298, "False Flag Attacks", Jihadist, 2,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (role: Role, _: Boolean) => (game.availablePlots exists (_ != PlotWMD)) ||
        (role == game.humanRole && game.hasNonMuslim(_.canRemovePosture))     ||
        (role == game.botRole   && game.hasNonMuslim(n => n.canRemovePosture && n.isOppositeUsPosture))
      ,
      (role: Role) => {
        val plots = (game.availablePlots filterNot (_ == PlotWMD)).sorted
        if (role == game.humanRole) {
          val postureCandidates = countryNames(game.nonMuslims filter (n => n.canRemovePosture))
          
          val choices = List(
            choice(plots.nonEmpty,             "plot",    "Place non-WMD plot in any country"),
            choice(postureCandidates.nonEmpty, "posture", "Remove posture marker from non-Muslim country")
          ).flatten

          askMenu("\nChoose one:", choices).head match {
            case "plot" =>
              val target = askCountry("Place plot in which country: ", countryNames(game.countries))
              val plot  = askPlots(plots, 1).head
              addEventTarget(target)
              addAvailablePlotToCountry(target, plot)
              
            case _ =>
              val target = askCountry("Remove posture maker from which country: ", postureCandidates)
              addEventTarget(target)
              setCountryPosture(target, PostureUntested)
          }
        }
        else {
          // Bot
          if (plots.nonEmpty) {
            addEventTarget(UnitedStates)
            addAvailablePlotToCountry(UnitedStates, plots.head)
          }
          else {
            val candidates = countryNames(game.nonMuslims filter (n => n.canRemovePosture && n.isOppositeUsPosture))
            val target = JihadistBot.posturePriority(candidates).get
            addEventTarget(target)
            setCountryPosture(target, PostureUntested)
          }
        }
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(299, "Foreign Fighters Return", Jihadist, 2,
      Remove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (_: Role, _: Boolean) => !game.caliphateDeclared &&
                               game.cellsAvailable > 0 &&
                               ((game hasNonMuslim (n => n.isGood && n.name != UnitedStates)) ||
                                (game hasNonMuslim (_.isFair)))
      ,
      (role: Role) => {
        val goodCandidates = countryNames(game.nonMuslims filter (n => n.isGood && n.name != UnitedStates))
        val fairCandidates = countryNames(game.nonMuslims filter (_.isFair))
        
          val goodTarget: Option[String] = if (goodCandidates.nonEmpty) {
            if (role == game.humanRole)
              Some(askCountry("Place a cell in which Good non-Muslim country: ", goodCandidates))
            else
              JihadistBot.recruitTravelToPriority(goodCandidates)
          }
          else
            None
          
          val fairTarget: Option[String] = if (fairCandidates.nonEmpty && (goodTarget.isEmpty || game.cellsAvailable > 1)) {
            if (role == game.humanRole)
              Some(askCountry("Place a cell in which Fair non-Muslim country: ", fairCandidates))
            else
              JihadistBot.recruitTravelToPriority(fairCandidates)
          }
          else
            None
          
          for (target <- List(goodTarget, fairTarget).flatten) {
            addEventTarget(target)
            addSleeperCellsToCountry(target, 1)
          }
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(300, "Going Underground", Jihadist, 2,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (role: Role, _: Boolean) => {
        val majorCandidates = game.majorJihadTargets(2) // 2 Ops only
        val minorCandidates = game.jihadTargets
        if (role == game.humanRole)
          majorCandidates.nonEmpty || minorCandidates.nonEmpty
        else
          goingUnderGroundBotMajorCandidates.nonEmpty || goingUnderGroundBotMinorCandidates.nonEmpty
      }
      ,
      (role: Role) => {
        val (target, major) = if (role == game.humanRole) {
          val majorCandidates = game.majorJihadTargets(2) // 2 Ops only
          val minorCandidates = game.jihadTargets
          val choices = List(
            choice(majorCandidates.nonEmpty, "major",   s"Major Jihad (${orList(majorCandidates)})"),
            choice(minorCandidates.nonEmpty, "discard", s"Minor Jihad (${orList(minorCandidates)})")
          ).flatten
          askMenu("\nExecute which type of Jihad:", choices).head match {
            case "major" => (askCountry("Major Jihad in which country: ", majorCandidates), true)
            case _       => (askCountry("Minor Jihad in which country: ", minorCandidates), false)
          }
        }
        else if (goingUnderGroundBotMajorCandidates.nonEmpty)
          (JihadistBot.majorJihadTarget(goingUnderGroundBotMajorCandidates).get, true)
        else
          (JihadistBot.minorJihadTarget(goingUnderGroundBotMinorCandidates).get, false)
        
        
        val numCells = game.getMuslim(target).totalCells min 2
        val (actives, sleepers, sadr) = if (role == game.humanRole)
          askCells(target, numCells, sleeperFocus = false)
        else
            JihadistBot.chooseCellsToRemove(target, numCells)
        val jihadTarget       = JihadTarget(target, actives, sleepers, sadr, major)
        val (_, successes, _) = performJihads(jihadTarget::Nil).head
        
        // Remaining cells that participated become sleepers
        val successCells = successes - (if (sadr) 1 else 0)  // Sadr does not flip
        
        if (successCells > 0 && !game.isCaliphateMember(target)) {
          log("\nSuccessful participating cells become sleeper cells.")
          hideActiveCells(target, successCells)
        }
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(301, "Green on Blue", Jihadist, 2,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (_: Role, _: Boolean) => false
      ,
      (role: Role) => {
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(302, "Imperial Overstretch", Jihadist, 2,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (_: Role, _: Boolean) => false
      ,
      (role: Role) => {
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(303, "Iranian Withdrawal", Jihadist, 2,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (_: Role, _: Boolean) => false
      ,
      (role: Role) => {
        // NOTE: If the die roll is successful, then the card is removed!
        if (true)
          removeCardFromGame(303)
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(304, "Loose Chemicals", Jihadist, 2,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (_: Role, _: Boolean) => false
      ,
      (role: Role) => {
        // NOTE: If the die roll is successful, then the card is removed!
        if (true)
          removeCardFromGame(304)
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(305, "Presidential Whistleblower", Jihadist, 2,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (_: Role, _: Boolean) => false
      ,
      (role: Role) => {
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(306, "Public Debate", Jihadist, 2,
      NoRemove, Lapsing, NoAutoTrigger, DoesNotAlertPlot,
      (_: Role, _: Boolean) => false
      ,
      (role: Role) => {
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(307, "Russian Subterfuge", Jihadist, 2,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (_: Role, _: Boolean) => false
      ,
      (role: Role) => {
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(308, "Battle of Marawi City", Jihadist, 3,
      Remove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (_: Role, _: Boolean) => false
      ,
      (role: Role) => {
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(309, "Easter Bombings", Jihadist, 3,
      Remove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (_: Role, _: Boolean) => false
      ,
      (role: Role) => {
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(310, "Forever War", Jihadist, 3,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (_: Role, _: Boolean) => false
      ,
      (role: Role) => {
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(311, "Gaza Border Protests", Jihadist, 3,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (_: Role, _: Boolean) => false
      ,
      (role: Role) => {
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(312, "Hama Offensive", Jihadist, 3,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (_: Role, _: Boolean) => false
      ,
      (role: Role) => {
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(313, "Hayat Tahir al-Sham", Jihadist, 3,
      Remove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (_: Role, _: Boolean) => false
      ,
      (role: Role) => {
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(314, "Jihadist African Safari", Jihadist, 3,
      Remove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (_: Role, _: Boolean) => false
      ,
      (role: Role) => {
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(315, "Khashoggi Crisis", Jihadist, 3,
      Remove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (_: Role, _: Boolean) => false
      ,
      (role: Role) => {
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(316, "Martyrdom Operation", Jihadist, 3,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (_: Role, _: Boolean) => false
      ,
      (role: Role) => {
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(317, "Qatari Crisis", Jihadist, 3,
      Remove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (_: Role, _: Boolean) => false
      ,
      (role: Role) => {
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(318, "South China Sea Crisis", Jihadist, 3,
      Remove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (_: Role, _: Boolean) => false
      ,
      (role: Role) => {
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(319, "Tehran-Beirut Land Corridor", Jihadist, 3,
      Remove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (_: Role, _: Boolean) => false
      ,
      (role: Role) => {
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(320, "Tribal Leaders Withdraw Support", Jihadist, 3,
      Remove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (_: Role, _: Boolean) => false
      ,
      (role: Role) => {
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(321, "Ungoverned Spaces", Jihadist, 3,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (_: Role, _: Boolean) => false
      ,
      (role: Role) => {
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(322, "Amnesty International", Unassociated, 1,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (_: Role, _: Boolean) => false
      ,
      (role: Role) => {
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(323, "Blasphemy", Unassociated, 1,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (_: Role, _: Boolean) => false
      ,
      (role: Role) => {
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(324, "BREXIT", Unassociated, 1,
      USRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (_: Role, _: Boolean) => false
      ,
      (role: Role) => {
        removeGlobalEventMarker(Euroscepticism)
        // Add BREXIT marker to UK
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(325, "Dissent Channel", Unassociated, 1,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (_: Role, _: Boolean) => false
      ,
      (role: Role) => {
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(326, "Filibuster/Nuclear Option", Unassociated, 1,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (_: Role, _: Boolean) => false
      ,
      (role: Role) => {
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(327, "Gaza Aid", Unassociated, 1,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (_: Role, _: Boolean) => false
      ,
      (role: Role) => {
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(328, "Hafiz Saeed Khan", Unassociated, 1,
      USRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (_: Role, _: Boolean) => false
      ,
      (role: Role) => {
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(329, "Hamza bin Laden", Unassociated, 1,
      USRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (_: Role, _: Boolean) => false
      ,
      (role: Role) => {
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(330, "IRGC", Unassociated, 1,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (_: Role, _: Boolean) => false
      ,
      (role: Role) => {
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(331, "JASTA", Unassociated, 1,
      Remove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (_: Role, _: Boolean) => false
      ,
      (role: Role) => {
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(332, "Khan Shaykhun Chemical Attack", Unassociated, 1,
      USRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (_: Role, _: Boolean) => false
      ,
      (role: Role) => {
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(333, "MbS", Unassociated, 1,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (_: Role, _: Boolean) => false
      ,
      (role: Role) => {
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(334, "Novichok Agent", Unassociated, 1,
      Remove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (_: Role, _: Boolean) => false
      ,
      (role: Role) => {
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(335, "Rohingya Genocide", Unassociated, 1,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (_: Role, _: Boolean) => false
      ,
      (role: Role) => {
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(336, "US/NK Summit", Unassociated, 1,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (_: Role, _: Boolean) => false
      ,
      (role: Role) => {
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(337, "US Border Crisis", Unassociated, 1,
      NoRemove, USLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (_: Role, _: Boolean) => false
      ,
      (role: Role) => {
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(338, "US Border Crisis", Unassociated, 2,
      USRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (_: Role, _: Boolean) => false
      ,
      (role: Role) => {
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(339, "Erdogan Dance", Unassociated, 2,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (_: Role, _: Boolean) => false
      ,
      (role: Role) => {
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(340, "EU Bolsters Iran Deal", Unassociated, 2,
      JihadistRemove, JihadistLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (_: Role, _: Boolean) => false
      ,
      (role: Role) => {
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(341, "Gulen Movement", Unassociated, 2,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (_: Role, _: Boolean) => false
      ,
      (role: Role) => {
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(342, "Gulmurod Khalimov", Unassociated, 2,
      USRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (_: Role, _: Boolean) => false
      ,
      (role: Role) => {
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(343, "JCPOA", Unassociated, 2,
      JihadistRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (_: Role, _: Boolean) => false
      ,
      (role: Role) => {
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(344, "Media Manipulation", Unassociated, 2,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (_: Role, _: Boolean) => false
      ,
      (role: Role) => {
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(345, "Operation Euphrates Shield", Unassociated, 2,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (_: Role, _: Boolean) => false
      ,
      (role: Role) => {
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(346, "Pakistani Intelligence (ISI)", Unassociated, 2,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (_: Role, _: Boolean) => false
      ,
      (role: Role) => {
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(347, "Switching Jerseys", Unassociated, 2,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (_: Role, _: Boolean) => false
      ,
      (role: Role) => {
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(348, "Travel Ban", Unassociated, 2,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (_: Role, _: Boolean) => false
      ,
      (role: Role) => {
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(349, "Turkish Coup", Unassociated, 2,
      USRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (_: Role, _: Boolean) => false
      ,
      (role: Role) => {
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(350, "UN Peace Envoy", Unassociated, 2,
      USRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (_: Role, _: Boolean) => false
      ,
      (role: Role) => {
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(351, "Advanced Persistent Threat (APT)", Unassociated, 3,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (_: Role, _: Boolean) => false
      ,
      (role: Role) => {
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(352, "al-Baghdadi", Unassociated, 3,
      USRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (role: Role, _: Boolean) => {
        (role == US       && (game hasMuslim (m => m.civilWar && m.totalTroopsAndMilitia > m.totalCells))) ||
        (role == Jihadist && (globalEventNotInPlay(AlBaghdadi) || (game.cellsAvailable > 0 && game.hasMuslim (m => m.isPoor))))
      }
      ,
      (role: Role) => if (role == US) {
        increasePrestige(2)
        decreaseFunding(1)
        removeGlobalEventMarker(AlBaghdadi)
        //  If Training Camps is is in play then the extra cell capacity
        //  is not changed.
        if (game.trainingCamp.isEmpty)
          game = game.copy(extraCellCapacity = 0)
      }
      else {  // role == Jihadist
        val candidates = countryNames(game.muslims filter (m => m.isPoor))
        if (game.cellsAvailable > 0 && candidates.nonEmpty) {
          val target = if (role == game.humanRole)
            askCountry("Place cells in which country: ", candidates)
          else
            JihadistBot.recruitTravelToPriority(candidates).get
          
          val cellsToAdd = game.cellsAvailable min 3
          addEventTarget(target)
          addSleeperCellsToCountry(target, cellsToAdd)
          
          if (cellsToAdd == 3 &&
              canDeclareCaliphate(target) &&
              ((role == game.humanRole && askDeclareCaliphate(target)) ||
               (role == game.botRole && JihadistBot.willDeclareCaliphate(target))))
            declareCaliphate(target)
        }
        addGlobalEventMarker(AlBaghdadi)
        //  If this is a campaign game and the TrainingCamps event is in play
        //  then we do not alter the trainging camp capacity (should be rare)
        if (game.trainingCamp.isEmpty) 
          placeExtraCells(if (game.caliphateDeclared) 5 else 3)
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(353, "Bowling Green Massacre", Unassociated, 3,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (_: Role, _: Boolean) => false
      ,
      (role: Role) => {
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(354, "Election Meddling", Unassociated, 3,
      NoRemove, NoLapsing, AutoTrigger, DoesNotAlertPlot,
      (role: Role, forTrigger: Boolean) => false  // Not directly playable, but will always auto trigger
      ,
      (role: Role) => {
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(355, "Fake News", Unassociated, 3,
      NoRemove, Lapsing, NoAutoTrigger, DoesNotAlertPlot,
      (_: Role, _: Boolean) => false
      ,
      (role: Role) => {
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(356, "OPEC Production Cut", Unassociated, 3,
      NoRemove, Lapsing, NoAutoTrigger, DoesNotAlertPlot,
      (_: Role, _: Boolean) => false
      ,
      (role: Role) => {
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(357, "Peace Dividend", Unassociated, 3,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (_: Role, _: Boolean) => false
      ,
      (role: Role) => {
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(358, "Political Islamism/Pan Arab Nationalism", Unassociated, 3,
      Remove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (_: Role, _: Boolean) => false
      ,
      (role: Role) => {
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(359, "Quick Win/Bad Intel", Unassociated, 3,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (_: Role, _: Boolean) => false
      ,
      (role: Role) => {
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(360, "US China Trade War", Unassociated, 3,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (_: Role, _: Boolean) => game.usPosture != game.getNonMuslim(China).posture && trumpTweetsON
      ,
      (role: Role) => {
        if (globalEventInPlay(USChinaTradeWar))
          removeGlobalEventMarker(USChinaTradeWar)
        else
          addGlobalEventMarker(USChinaTradeWar)
        setTrumpTweetsOFF()
        decreasePrestige(1)
      }
    ))
  )
}