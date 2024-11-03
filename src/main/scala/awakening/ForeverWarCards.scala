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

  val foreignInternalDefenseCandidate = (m: MuslimCountry) => m.alignment != Adversary && m.totalCells > 0 && m.canTakeMilitia
  
  val nadiaMuradCandidate = (m: MuslimCountry) => (m.name == Iraq || areAdjacent(m.name, Iraq)) && 
                                                  m.canTakeAwakeningOrReactionMarker            &&
                                                  !game.isCaliphateMember(m.name)
                                                  
  val patrioticArabDemocraciesCandidate = (m: MuslimCountry) => m.alignment != Adversary &&
                                                                m.canTakeAwakeningOrReactionMarker &&
                                                                !game.isCaliphateMember(m.name)
  def saudiAirStrikesCandidates = countryNames(
    game.muslims filter (m => m.totalCells > 0 && 
        (m.name == SaudiArabia || 
        (areAdjacent(m.name, SaudiArabia) && (m.civilWar || m.inRegimeChange || m.isIslamistRule))))
  )
    
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
  
  def trumpTripAlignmentCandidates = countryNames(game.muslims filter (m => !(m.isIslamistRule || m.isAlly || game.isCaliphateMember(m.name))))
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
  
  def governmentOfNationalAccordCandidates = countryNames(
    game.muslims filter (m => m.civilWar && !game.isCaliphateMember(m.name))
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
  
  def easterBombingsCandidates = countryNames(
    game.nonMuslims filter (n => n.isUntested && !n.isGood && (game.adjacentMuslims(n.name) exists (!_.isUntested)))
  )
  
  def martyrdomCandidates = countryNames(
    game.countries filter (c => c.totalCells > 0 && !(game.isMuslim(c.name) && game.getMuslim(c.name).isIslamistRule)) 
  ) 
  
  def tribalLeadersCandidates = countryNames(
    game.muslims filter (m => m.militia > 0 && (m.inRegimeChange || m.civilWar))
  )
  
  def ungovernedSpaceCandidates = countryNames(
    (game.countries filter (_.isPoor)) ::: 
    (game.muslims filter (_.civilWar)) ::: 
    (game.getCountries(African) filter (_.isUntested))
  )
  
  def amnestyInternationUSCandidates = countryNames(
    game.muslims filter (m => m.isAdversary && m.canTakeAwakeningOrReactionMarker && !game.isCaliphateMember(m.name))
  )
  
  def amnestyInternationJihadistCandidates = countryNames(
    game.muslims filter (m => m.isAlly && m.canTakeAwakeningOrReactionMarker && !game.isCaliphateMember(m.name))
  )
  
  def blasphemyCandidates(role: Role) = {
    def isCandidate(m: MuslimCountry): Boolean = {
      lazy val botCondition = role match {
        case US       => !m.isAdversary
        case Jihadist => !m.isAlly
      }
      
      m.totalCells > 0 && m.canTakeAwakeningOrReactionMarker && (role == game.humanRole || botCondition)
    }
      
    countryNames(game.muslims filter isCandidate)
  }
  
  def hafizSaeedKhanMuslimUSCandidates = countryNames(
    game.getMuslims(Afghanistan::Pakistan::Nil) filter { m =>
      (m.inRegimeChange && !m.isAlly) || (m.isAdversary && !m.isIslamistRule)
    }
  )
  def hafizSaeedKhanMuslimJihadistCandidates = countryNames(
    game.getMuslims(Afghanistan::Pakistan::Nil) filter (m => m.inRegimeChange || m.isAdversary) 
  )

  def hafizSaeedKhanNonMuslimUSCandidates = countryNames(
    game.getNonMuslims(India::Nil) filter (n => n.totalCells > 0 && n.posture == Soft && game.usPosture == Hard)
  )
  
  def hafizSaeedKhanNonMuslimJihadistCandidates = countryNames(
    game.getNonMuslims(India::Nil) filter (n => n.totalCells > 0)
  )
  
  def erdoganDanceCandidates = {
    val targets = (game.targetsLastPhase.ops ++ game.targetsLastPhase.event ++
                   game.targetsThisPhase.ops ++ game.targetsThisPhase.event).toList
    
    (targets filter (name => name != Iran && name != UnitedStates && distance(name, Turkey) <= 2)).sorted
  }
  
  def erdoganDanceMuslims = game.getMuslims(erdoganDanceCandidates filter game.isMuslim)
  def erdoganDanceNonMuslims = game.getNonMuslims(erdoganDanceCandidates filter game.isNonMuslim)
  
  def mediaManipulationCandidates(role: Role) = {
    val alignTest = if (role == US)
      (m: MuslimCountry) => !m.isAlly
    else
      (m: MuslimCountry) => !m.isAdversary
    
    countryNames(game.muslims filter (m => m.civilWar && m.totalCells > 0 && m.militia > 0 && alignTest(m)))    
  }
  
  def euphratesShieldCandidates(role: Role) = {
    val roleTest =  if (role == US)
      (m: MuslimCountry) => true // Can always place aid
    else
      (m: MuslimCountry) => m.militia > 0 || !m.besiegedRegime
    countryNames(game.muslims filter (m => m.civilWar && areAdjacent(m.name, Turkey) && roleTest(m)))
  }
  
  def pakistaniIntelligenceCandidates(role: Role) = {
    val pakistan = game.getMuslim(Pakistan)
    val muslims = pakistan :: game.adjacentMuslims(Pakistan)
    val test = if (role == US)
      (m: MuslimCountry) => m.totalCells > 0 || (m.canTakeMilitia && game.militiaAvailable > 0)
    else
      (m: MuslimCountry) => m.militia > 0 || game.cellsAvailable > 0
    countryNames(muslims filter test)
  }
  
  def switchingJerseysCandidates(role: Role) = {
    val test = if (role == US)
      (m: MuslimCountry) => m.totalTroopsAndMilitia > m.totalCells
    else
      (m: MuslimCountry) => m.totalCells > m.totalTroopsAndMilitia
    countryNames(game.muslims filter (m => m.civilWar && m.totalCells > 0 && m.militia > 0 && test(m)))
  }
  
  def unPeaceEnvoyCandidates(role: Role) = {
    // Can't use m.totalTroopsAndMilitia here because it is unit by unit regardless of
    // unit size (some marker troops such as NATO represent two troops)
    val usUnits = (m: MuslimCountry) => m.militia + m.troops + m.troopsMarkers.size
    val basicTest = (m: MuslimCountry) => !game.isCaliphateMember(m.name) && m.civilWar &&
                                           m.totalCells > 0 && usUnits(m) > 0
    val botTest = role match {
      case US if role == game.botRole => (m: MuslimCountry) => usUnits(m) > m.totalCells
      case Jihadist if role == game.botRole => (m: MuslimCountry) => m.totalCells > usUnits(m)
      case _ => (m: MuslimCountry) => true // Always true. Leave for human player to decide
    }
    
    countryNames(game.muslims filter (m => basicTest(m) && botTest(m)))
  }
  
  def bowlingGreenBotMarkers(role: Role): List[String] = {
    val opponent = oppositeRole(role)
    val global = game.markers filter (GlobalMarkers(_) == opponent)
    val country = game.countries flatMap (_.markers) filter (CountryMarkers(_) == opponent)
    (global ::: country).sorted.distinct
  }
  
  def bowlingGreenBotLapsing(role: Role): List[Int] = game.cardsLapsing filter { num =>
    val card = deck(num)
    (role == US && card.association == Jihadist || card.lapsing == JihadistLapsing) ||
    (role == Jihadist && card.association == US || card.lapsing == USLapsing)
  }
  
  def quickWinBadIntelCandidates(role: Role) = {
    val test = if (role == US)
      (m: MuslimCountry) => (m.inRegimeChange)
    else
      (m: MuslimCountry) => (m.inRegimeChange && (m.canTakeAwakeningOrReactionMarker || !m.besiegedRegime))
    countryNames(game.muslims filter test)
  }
  
  def rangerRegimentCandidates = countryNames(
     game.muslims filter (m => m.civilWar && m.totalCells > 0)
  )
  
  def irgcCandidates(role: Role) = {
    if (role == US)
      countryNames(game.adjacentCountries(Iran) filter (_.totalCells > 0))
    else
      countryNames(game.adjacentMuslims(Iran) filter (_.militia > 0))
  }

  // Convenience method for adding a card to the deck.
  private def entry(card: Card) = (card.number -> card)
  val deckMap: Map[Int, Card] = Map(
    // ------------------------------------------------------------------------
    entry(new Card(241, "Abdel Fattah el-Sisi", US, 1,
      Remove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot, CannotNotRemoveLastCell,
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
      () => {
        // Can we remove the last cell on the board?
        game.hasMuslim(m => m.totalCells == 2 && game.totalCellsOnMap == 2)
      }
      ,
      (role: Role, _: Boolean) => if (role == game.humanRole)
        game.hasMuslim(_.totalCells > 0)
      else {
        // If the last two cells on the map are in a Muslim country the the Bot will go for the win.
        // Otherwise, the bot will only remove cells if there is a country where cells outnumber
        // troops and miliia or if the Jihadist has not cards in hand.
        game.hasMuslim(m => m.totalCells == 2 && game.totalCellsOnMap == 2) ||
        game.hasMuslim(m => m.totalCells - m.totalTroopsAndMilitia > 4)     ||
        cacheYesOrNo(s"Does the $Jihadist player have any cards in hand? (y/n) ") ||
        game.hasMuslim(_.totalCells > 0)
      }
      ,
      (role: Role) => {
        if (role == game.humanRole) {
          val candidates = countryNames(game.muslims filter (_.totalCells > 0))
          val canDiscard = askYorN(s"Does the $Jihadist player have any cards in hand? (y/n) ")
          val choices = List(
            choice(candidates.nonEmpty, "remove", "Remove up to 2 cells in any one Muslim country"),
            choice(canDiscard,          "discard", s"Discard top card of the $Jihadist hand")
          ).flatten
          askMenu("\nChoose one:", choices).head match {
            case "discard" => 
              println()
              log(s"Discard top card of the $Jihadist hand")
              askCardsDiscarded(1)
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
          val target = game.muslims.find(m => m.totalCells == 2 && game.totalCellsOnMap == 2).map(_.name) orElse
                       USBot.disruptPriority(candidates) orElse {
                         if (cacheYesOrNo(s"Does the $Jihadist player have any cards in hand? (y/n) "))
                           None
                         else
                           USBot.disruptPriority(countryNames(game.muslims filter (_.totalCells > 0)))
                       }
          
          
          target match {
            case Some(name) => 
              val (actives, sleepers, sadr) = USBot.chooseCellsToRemove(name, 2)
              addEventTarget(name)
              removeCellsFromCountry(name, actives, sleepers, sadr, addCadre = true)
            case None =>
              println()
              log(s"You ($Jihadist) must discard one random card")
              askCardsDiscarded(1)
          }
        }
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(243, "Backlash", US, 1,
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
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot, CannotNotRemoveLastCell,
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
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot, CannotNotRemoveLastCell,
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
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot, CannotNotRemoveLastCell,
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
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot, CannotNotRemoveLastCell,
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
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot, CannotNotRemoveLastCell,
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
      () => {
        // Can we remove the last cell on the board?
        saudiAirStrikesCandidates exists { name =>
          USBot.wouldRemoveLastCell(name, if (name == Yemen) 2 else 1)
        }
      }
      ,
      (_: Role, _: Boolean) => (game.getMuslim(SaudiArabia).isNeutral || game.getMuslim(SaudiArabia).isAlly) &&
                               saudiAirStrikesCandidates.nonEmpty
      ,
      (role: Role) => {
        val target = if (role == game.humanRole)
          askCountry(s"Remove cell(s) from which country: ", saudiAirStrikesCandidates)
        else 
          USBot.disruptPriority(saudiAirStrikesCandidates).get

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
      () => {
        // Can we remove the last cell on the board?
        specialForcesCandidates exists (name => USBot.wouldRemoveLastCell(name, 1))
      }
      ,
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
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot, CannotNotRemoveLastCell,
      AlwaysPlayable,
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
        val hillaryMod  = if (game.scenarioName == awakening.scenarios.HillaryWins.name) 1 else 0
        val prestigeMod = game.prestigeModifier
        val die = getDieRoll("Enter event die roll: ", Some(role))
        val modRoll = die + hillaryMod + prestigeMod
        
        log(s"Die roll: $die")
        logNotZero(prestigeMod, "Prestige")
        logNotZero(hillaryMod,  s"${awakening.scenarios.HillaryWins.name} scenario rule")
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
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot, CannotNotRemoveLastCell,
      (role: Role, forTrigger: Boolean) => deck(251).eventConditions(role, forTrigger),
      (role: Role) => deck(251).executeEvent(role)
    )),
    // ------------------------------------------------------------------------
    entry(new Card(253, "Trump Tweets", US, 1,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot, CannotNotRemoveLastCell,
      (role: Role, forTrigger: Boolean) => deck(251).eventConditions(role, forTrigger),
      (role: Role) => deck(251).executeEvent(role)
    )),
    // ------------------------------------------------------------------------
    entry(new Card(254, "US Embassy to Jerusalem", US, 1,
      Remove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot, CannotNotRemoveLastCell,
      (role: Role, forTrigger: Boolean) => game.usPosture == Hard && trumpTweetsON &&
                                  (forTrigger || role == game.humanRole || game.prestigeLevel == Low)
      ,
      (role: Role) => {
        rollPrestige()
        increaseFunding(1)
        setTrumpTweetsOFF()
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(255, "Western Arms Sales", US, 1,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot, CannotNotRemoveLastCell,
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
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot, CannotNotRemoveLastCell,
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
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot, CannotNotRemoveLastCell,
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
      () => {
        // Can we remove the last cell on the board?
        rangerRegimentCandidates exists (name => USBot.wouldRemoveLastCell(name, 2))
      }
      ,
      (_: Role, _: Boolean) => rangerRegimentCandidates.nonEmpty
      ,
      (role: Role) => {
        val target = if (role == game.humanRole)
          askCountry("Remove cells from which Civil War country: ", rangerRegimentCandidates)
        else
          USBot.disruptPriority(rangerRegimentCandidates).get
        
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
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot, CannotNotRemoveLastCell,
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
      Remove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot, CannotNotRemoveLastCell,
      (_: Role, _: Boolean) => {
        val pakistan = game.getMuslim(Pakistan)
        !pakistan.isIslamistRule && !pakistan.isGood && pakistan.totalCells == 0 && !game.isCaliphateMember(Pakistan)
      }
      ,
      (role: Role) => {
        testCountry(Pakistan)
        improveGovernance(Pakistan, 1, true)
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(261, "Intel Community", US, 2,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot, CannotNotRemoveLastCell,
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
      () => {
        // Can we remove the last cell on the board?
        moabCandidates exists (name => USBot.wouldRemoveLastCell(name, 1))
      }
      ,
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
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot, CannotNotRemoveLastCell,
      (_: Role, _: Boolean) => game.caliphateDeclared && game.funding > 0
      ,
      (role: Role) => {
        decreaseFunding(2)
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(264, "Personal Security Contractors", US, 2,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot, CannotNotRemoveLastCell,
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
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot, CannotNotRemoveLastCell,
      (_: Role, _: Boolean) => popularMobilForcesCandidates.nonEmpty && game.militiaAvailable > 0
      ,
      (role: Role) => {
        val target = if (role == game.humanRole)
          askCountry("Place militia in which country: ", popularMobilForcesCandidates)
        else
          USBot.deployToPriority(popularMobilForcesCandidates).get
          
          val num = game.getMuslim(target).governance min game.militiaAvailable
          addEventTarget(target)
          testCountry(target)
          addMilitiaToCountry(target, num)
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(266, "Presidential Reality Show", US, 2,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot, CannotNotRemoveLastCell,
      (role: Role, _: Boolean) => {
        val prereq = !(game hasMuslim (m => m.totalTroops > 0 && (m.civilWar || m.inRegimeChange)))
        def cardExists = cacheYesOrNo("""Is there a "Trump Tweets" card or any card with prerequisite "Trump Tweets ON" in the discard pile? (y/n) """)
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
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot, CannotNotRemoveLastCell,
      (_: Role, _: Boolean) => cacheYesOrNo(s"Does the $Jihadist player have more than one card in hand? (y/n) ")
      ,
      (role: Role) => {
        if (role == game.humanRole)
          log(s"Discard a random card from the $Jihadist hand.")
        else
          log(s"You ($Jihadist) must randomly discard one card.")
        askCardsDiscarded(1)
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(268, "Trump Trip", US, 2,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot, CannotNotRemoveLastCell,
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
      () => {
        // Can we remove the last cell on the board?
        val calNum    = (airAmericaCaliphateCandidates map (name => game.getCountry(name).totalCells)).sum min 4
        val nonCalNum = (airAmericaNonCaliphateCandidates map (name => game.getCountry(name).totalCells)).sum min 3
        
        calNum == game.totalCellsOnMap || nonCalNum == game.totalCellsOnMap
      }
      ,
      (_: Role, _: Boolean) => airAmericaCaliphateCandidates.nonEmpty || airAmericaNonCaliphateCandidates.nonEmpty
      ,
      (role: Role) => {
        //  Allows US player to remove up to 4 cells from any caliphate members
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
          val calNum    = (airAmericaCaliphateCandidates map (name => game.getCountry(name).totalCells)).sum min 4
          val nonCalNum = (airAmericaNonCaliphateCandidates map (name => game.getCountry(name).totalCells)).sum min 3
        
          calNum == game.totalCellsOnMap || nonCalNum == game.totalCellsOnMap
          
          
          
          val (candidates, maxCells) = if (calNum == 4 || calNum == game.totalCellsOnMap || nonCalNum == 0)
            (airAmericaCaliphateCandidates, calNum)
          else
            (airAmericaNonCaliphateCandidates, nonCalNum)
          
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
      Remove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot, CannotNotRemoveLastCell,
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
      NoRemove, Lapsing, NoAutoTrigger, DoesNotAlertPlot, CannotNotRemoveLastCell,
      (_: Role, _: Boolean) => true
      ,
      (role: Role) => {
        log("During Attrition at the end of this turn, add +1 to the")
        log("number of cells to be removed for each hit secured by the US player.")
        decreasePrestige(1)
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(272, "Fire and Fury", US, 3,
      Remove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot, CannotNotRemoveLastCell,
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
      NoRemove, Lapsing, NoAutoTrigger, DoesNotAlertPlot, CannotNotRemoveLastCell,
      (_: Role, _: Boolean) => game.usPosture == Hard && (game hasMuslim (_.inRegimeChange))
      ,
      (role: Role) => {
        log("US player will draw 2 additional cards next turn.")
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(274, "Government of National Accord", US, 3,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot, CannotNotRemoveLastCell,
      (_: Role, _: Boolean) => governmentOfNationalAccordCandidates.nonEmpty
      ,
      (role: Role) => {
        val numMilitia = game.militiaAvailable min 2
        lazy val die = getDieRoll(s"Enter event die roll: ")
        val candidates = governmentOfNationalAccordCandidates    
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
            log(s"Die roll: $die, failure")
            log("The Civil War does not end.")
          
          case x =>
            log(s"Troops + Militia - Cells = $x")
            log(s"Die roll: $die, success!")
            log("The Civil War ends.")
            endCivilWar(target)
            increasePrestige(1)
        }
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(275, "Operation Inherent Resolve", US, 3,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      () => {
        // Can we remove the last cell on the board?
        val numCells = (game.getMuslims(Iraq::Syria::Nil) map (_.totalCells)).sum min 3
        numCells == game.totalCellsOnMap
      }
      ,
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
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot, CannotNotRemoveLastCell,
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
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot, CannotNotRemoveLastCell,
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
      NoRemove, Lapsing, NoAutoTrigger, DoesNotAlertPlot, CannotNotRemoveLastCell,
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
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot, CannotNotRemoveLastCell,
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
      () => {
        // Can we remove the last cell on the board?
        val numCells = (game.getMuslims(sunniShiaRiftCandidates) map (_.totalCells)).sum min 3
        numCells == game.totalCellsOnMap
      }
      ,
      (_: Role, forTrigger: Boolean) => !isIranSpecialCase && (sunniShiaRiftCandidates.nonEmpty || forTrigger)
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
          // removal of a cell could change the Bot's priorities
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
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot, CannotNotRemoveLastCell,
      (_: Role, _: Boolean) => (game hasMuslim (_.civilWar)) && (game.availablePlots contains Plot1)
      ,
      (role: Role) => {
        val candidates = countryNames(game.muslims filter (_.civilWar))
        val target = if (role == game.humanRole)
          askCountry("Place plot in which country: ", candidates)
        else
          JihadistBot.plotPriority(candidates).get
        
        addEventTarget(target)
        addAvailablePlotToCountry(target, Plot1, visible = true)
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(282, "Executive Order 13492", Jihadist, 1,
      Remove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot, CannotNotRemoveLastCell,
      (_: Role, forTrigger: Boolean) => 
        forTrigger ||
        (game.usPosture == Hard && game.prestige > 1) ||
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
          if (game.cellsAvailable > 0)
            addSleeperCellsToCountry(target, 1)
          else
            log(s"There are no cells available to place in $target")
        }
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(283, "Lone Wolf", Jihadist, 1,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot, CannotNotRemoveLastCell,
      AlwaysPlayable,
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
      Remove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot, CannotNotRemoveLastCell,
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
      Remove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot, CannotNotRemoveLastCell,
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
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot, CannotNotRemoveLastCell,
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
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot, CannotNotRemoveLastCell,
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
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot, CannotNotRemoveLastCell,
      (_: Role, _: Boolean) => game.caliphateDeclared
      ,
      (role: Role) => {
        val die = getDieRoll(s"Enter event die roll: ")
        log(s"Die roll: $die")
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
              addAvailablePlotToCountry(target, Plot1, visible = true)
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
      NoRemove, Lapsing, NoAutoTrigger, DoesNotAlertPlot, CannotNotRemoveLastCell,
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
      Remove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot, CannotNotRemoveLastCell,
      AlwaysPlayable,
      (role: Role) => {
        val die = getDieRoll(s"Enter event die roll: ")
        log(s"Die roll: $die")
        die match {
          case 1 | 2 =>
            if (game.getMuslim(CentralAsia).canTakeAwakeningOrReactionMarker) {
              testCountry(CentralAsia)
              addEventTarget(CentralAsia)
              addReactionMarker(CentralAsia)
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
              addAvailablePlotToCountry(China, Plot1, visible = true)
            }
            else
              log("There are no available level 1 plots.")
        }
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(291, "Vehicle-ramming Attacks", Jihadist, 1,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot, CannotNotRemoveLastCell,
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
        addAvailablePlotToCountry(target, Plot1, visible = true)
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(292, "Amaq News Agency", Jihadist, 2,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot, CannotNotRemoveLastCell,
      (_: Role, _: Boolean) => amaqNewsAgencyCandidates.nonEmpty
      ,
      (role: Role) => {
        val numCadres = if (role == game.humanRole)
          askInt("Place how many cadres", 1, 3, Some(3))
        else
          3
        
        @tailrec def nextCadre(num: Int, candidates: List[String]): Unit =
          if (num <= numCadres && candidates.nonEmpty) {
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
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot, CannotNotRemoveLastCell,
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
      Remove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot, CannotNotRemoveLastCell,
      (_: Role, forTrigger: Boolean) => 
        forTrigger ||
        game.getCountry(Spain).isUntested ||
        (game.availablePlots contains Plot1)
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
          addAvailablePlotToCountry(Spain, Plot1, visible = true)
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(295, "Black Gold", Jihadist, 2,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot, CannotNotRemoveLastCell,
      (_: Role, _: Boolean) => game.caliphateDeclared && game.funding < 9
      ,
      (role: Role) => {
        val die = getDieRoll(s"Enter event die roll: ")
        log(s"Die roll: $die")
        increaseFunding((die + 1) / 2)
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(296, "Botched Yemeni Raid", Jihadist, 2,
      Remove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot, CannotNotRemoveLastCell,
      (_: Role, _: Boolean) => game.prestigeLevel == High || game.prestigeLevel == VeryHigh
      ,
      (role: Role) => {
        decreasePrestige(2)
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(297, "Early Exit", Jihadist, 2,
      Remove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot, CannotNotRemoveLastCell,
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
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot, CannotNotRemoveLastCell,
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
            addAvailablePlotToCountry(UnitedStates, JihadistBot.preparePlots(plots).head)
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
      Remove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot, CannotNotRemoveLastCell,
      (_: Role, forTrigger: Boolean) =>
        !game.caliphateDeclared &&
        forTrigger || (
          game.cellsAvailable > 0 &&
          ((game hasNonMuslim (n => n.isGood && n.name != UnitedStates)) ||
           (game hasNonMuslim (_.isFair)))
        )                    
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
        
        val fairTarget: Option[String] = if (fairCandidates.nonEmpty) {
          if (role == game.humanRole)
            Some(askCountry("Place a cell in which Fair non-Muslim country: ", fairCandidates))
          else
            JihadistBot.recruitTravelToPriority(fairCandidates)
        }
        else
          None
        
        for (target <- List(goodTarget, fairTarget).flatten) {
          addEventTarget(target)
          if (game.cellsAvailable > 0) {
            testCountry(target)
            addSleeperCellsToCountry(target, 1)
          }
          else
            log(s"There are no cells available to place in $target")
        }
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(300, "Going Underground", Jihadist, 2,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot, CannotNotRemoveLastCell,
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
            choice(majorCandidates.nonEmpty, "major", s"Major Jihad (${orList(majorCandidates)})"),
            choice(minorCandidates.nonEmpty, "minor", s"Minor Jihad (${orList(minorCandidates)})")
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
        addEventTarget(target)
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
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot, CannotNotRemoveLastCell,
      (role: Role, _: Boolean) => game.plotData.resolvedInGreenOnBlue &&
                                  (role == game.humanRole || game.gwotPenalty == 0)
      ,
      (role: Role) => {
        val die = getDieRoll(s"Enter event die roll: ")
        increaseFunding(1)
        log(s"Die roll: $die")
        val newPosture = if (die < 4) oppositePosture(game.usPosture) else game.usPosture
        setUSPosture(newPosture)
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(302, "Imperial Overstretch", Jihadist, 2,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot, CannotNotRemoveLastCell,
      (_: Role, _: Boolean) => 
        game.troopCommitment != LowIntensity &&
        cacheYesOrNo(s"Does the $US player have least one card in hand? (y/n) ")
      ,
      (role: Role) => {
        if (role == game.humanRole)
          log(s"Discard the top card of the $US hand")
        else
          log(s"You ($US) discard a random card")
        askCardsDiscarded(1)
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(303, "Iranian Withdrawal", Jihadist, 2,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot, CannotNotRemoveLastCell,
      (_: Role, _: Boolean) => {
        val iran = game.getCountry(Iran)
        game.usPosture == Hard && iran.wmdCache > 0 && iran.totalCells > 0
      },
      (role: Role) => {
        val die = getDieRoll(s"Enter event die roll: ")
        addEventTarget(Iran)        
        log(s"Die roll: $die")
        if (die < 4) {
          moveWMDCacheToAvailable(Iran, 1)
          if (isIranSpecialCase) {
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
          }
          // Card only removed if die roll was successful
          removeCardFromGame(303)
        }
        else {
          val (actives, sleepers, sadr) = if (role == game.humanRole)
            askCells(Iran, 1, sleeperFocus = false)
          else
              JihadistBot.chooseCellsToRemove(Iran, 1)
          
          removeCellsFromCountry(Iran, actives, sleepers, sadr, addCadre = true)
        }
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(304, "Loose Chemicals", Jihadist, 2,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot, CannotNotRemoveLastCell,
      (_: Role, _: Boolean) => {
        val syria = game.getMuslim(Syria)
        syria.civilWar && syria.wmdCache > 0 && syria.totalCells > 0
      },
      (role: Role) => {
        val die = getDieRoll(s"Enter event die roll: ")
        addEventTarget(Syria)        
        log(s"Die roll: $die")
        if (die < 4) {
          moveWMDCacheToAvailable(Syria, 1)
          // Card only removed if die roll was successful
          removeCardFromGame(304)
        }
        else {
          val (actives, sleepers, sadr) = if (role == game.humanRole)
            askCells(Syria, 1, sleeperFocus = false)
          else
              JihadistBot.chooseCellsToRemove(Syria, 1)
          
          removeCellsFromCountry(Syria, actives, sleepers, sadr, addCadre = true)
        }
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(305, "Presidential Whistleblower", Jihadist, 2,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot, CannotNotRemoveLastCell,
      (_: Role, _: Boolean) => trumpTweetsON
      ,
      (role: Role) => {
        testCountry(Caucasus)
        val result = game.prestigeModifier - game.gwotPenalty
        val opPoints = -result
        log(s"GWOT penalty (-${game.gwotPenalty}) + prestige modifier (${game.prestigeModifier}) = $result")
        if (result >= 0)
          log("There is no effect")
        else {
          println()
          if (role == game.humanRole) {
            log(s"Discard from the top of the $Jihadist Bot's hand until the combined")
            log(s"Operational value is at least $opPoints")
          }
          else {
            log(s"You ($US) must discard cards until the combined Operational value")
            log(s"is at least $opPoints")
          }
        
          def nextDiscard(num: Int, pointsDiscarded: Int): List[Int] = {
            if (pointsDiscarded >= opPoints)
              Nil
            else {
              val prompt = s"Card # of the ${ordinal(num)} discard (or blank if none) "
              askCardNumber(prompt) match {
                case None         => Nil
                case Some(cardNo) =>  cardNo :: nextDiscard(num + 1, pointsDiscarded + deck(cardNo).printedOps)
              }
            }
          }
          
          for (n <- nextDiscard(1, 0); card = deck(n)) {
            if (n == AvengerCard)
                avengerCardDrawn(discarded = false)
            else if (card.autoTrigger)
              autoTriggerCardDiscarded(n)
            else if (game.botRole == Jihadist && card.eventWillTrigger(Jihadist)) {
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
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(306, "Public Debate", Jihadist, 2,
      NoRemove, Lapsing, NoAutoTrigger, DoesNotAlertPlot, CannotNotRemoveLastCell,
      NeverPlayable,  // Not playable in Solo game
      (role: Role) => {
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(307, "Russian Subterfuge", Jihadist, 2,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot, CannotNotRemoveLastCell,
      (_: Role, _: Boolean) => {
        val russia = game.getNonMuslim(Russia)
        (russia.isUntested || russia.posture == oppositePosture(game.usPosture)) &&
        cacheYesOrNo(s"Does the $US player have least one card in hand? (y/n) ")
      }
      ,
      (role: Role) => {
        if (role == game.humanRole)
          log(s"Discard the top card of the $US hand")
        else
          log(s"You ($US) must discard a random card")
        askCardsDiscarded(1)
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(308, "Battle of Marawi City", Jihadist, 3,
      Remove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot, CannotNotRemoveLastCell,
      (_: Role, forTrigger: Boolean) =>
        forTrigger ||
        game.cellsAvailable > 0 ||
        game.availablePlots.nonEmpty
      ,
      (role: Role) => {
        addEventTarget(Philippines)
        
        val plot = if (game.availablePlots.isEmpty)
           None
        else if (role == game.humanRole)
          Some(askPlots(game.availablePlots, 1).head)
        else
          Some(JihadistBot.preparePlots(game.availablePlots).head)

        println()
        testCountry(Philippines)
        if (game.cellsAvailable > 0)
          addActiveCellsToCountry(Philippines, 1)
        else
          log(s"There are no cells available to place in $Philippines")
        plot foreach (addAvailablePlotToCountry(Philippines, _))
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(309, "Easter Bombings", Jihadist, 3,
      Remove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot, CannotNotRemoveLastCell,
      (_: Role, forTrigger: Boolean) =>
        forTrigger ||
        (game.availablePlots contains Plot1) && easterBombingsCandidates.nonEmpty
      ,
      (role: Role) => {
        if ((game.availablePlots contains Plot1) && easterBombingsCandidates.nonEmpty) {
          val maxPlots = (game.availablePlots count (_ == Plot1))
          val (target, numPlots) = if (role == game.humanRole) {
              val t = askCountry("Place plot(s) in which country: ", easterBombingsCandidates)
              val n = askInt(s"Place how many Level 1 Plots in $t", 1, maxPlots, Some(maxPlots))
              (t, n)
          }
          else
            (JihadistBot.plotPriority(easterBombingsCandidates).get, maxPlots)
          
          addEventTarget(target)
          testCountry(target)
          for (i <- 1 to numPlots)
            addAvailablePlotToCountry(target, Plot1, visible = true)
        }
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(310, "Forever War", Jihadist, 3,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot, CannotNotRemoveLastCell,
      (_: Role, _: Boolean) => game.troopsOnMap + game.militiaOnMap - game.cellsOnMap > 0
      ,
      (role: Role) => {
        val difference = game.troopsOnMap + game.militiaOnMap - game.cellsOnMap
        log(f"${game.troopsOnMap}%2d troops on map")
        log(f"${game.militiaOnMap}%2d militia on map")
        log(f"${game.cellsOnMap}%2d cells on map")
        decreasePrestige(difference min 3)
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(311, "Gaza Border Protests", Jihadist, 3,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot, CannotNotRemoveLastCell,
      (_: Role, _: Boolean) => {
        val numCells = game.getCountry(Israel).totalCells
        game.availablePlots exists (_.number <= numCells)
      }
      ,
      (role: Role) => {
        val USEmbassyToJerusalem = 254
        val numCells = game.getCountry(Israel).totalCells
        val maxPlots = if (game.cardRemoved(USEmbassyToJerusalem)) 2 else 1
        
        addEventTarget(Israel)
        flipAllSleepersCells(Israel)
        val plots = if (role == game.humanRole) {
          def nextPlot(numLeft: Int, plotSum: Int, remainingPlots: List[Plot]): List[Plot] = {
            if (numLeft == 0)
              Nil
            else
              (remainingPlots dropWhile (_.number > numCells - plotSum)) match {
                case Nil => Nil
                case ps  => 
                  val p = askPlots(ps, 1).head
                  val index = ps.indexOf(p)
                  val pps = ps.take(index) ::: ps.drop(index + 1)
                  p :: nextPlot(numLeft - 1, plotSum + p.number, pps)
              }
          }
          
          println()
          nextPlot(maxPlots, 0, game.availablePlots.sorted)
        }
        else {
          // Bot
          def nextPlot(numLeft: Int, plotSum: Int, remainingPlots: List[Plot]): List[Plot] = {
            if (numLeft == 0)
              Nil
            else
              (remainingPlots dropWhile (_.number > numCells - plotSum)) match {
                case Nil => Nil
                case p::ps => p::nextPlot(numLeft - 1, plotSum + p.number, ps)
              }
          }
            
          nextPlot(maxPlots, 0, game.availablePlots.sorted)
        }
        
        println()
        for (p <- plots)
          addAvailablePlotToCountry(Israel, p)
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(312, "Hama Offensive", Jihadist, 3,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot, CannotNotRemoveLastCell,
      (role: Role, _: Boolean) => if (role == game.humanRole)
        game hasMuslim (_.civilWar)
      else {
        // Don't let Bot execute event if only civil war countries with no cells
        // unless there is a least one available cell to place there
        game hasMuslim (m => m.civilWar && (m.totalCells > 0 || game.cellsAvailable > 0))
      }
      ,
      (role: Role) => {
        val candidates = countryNames(game.muslims filter (_.civilWar))
        val target = if (role == game.humanRole)
          askCountry("Which country: ", candidates)
        else
          JihadistBot.hamaOffensiveTarget(candidates).get
        
        addEventTarget(target)
        println()
        addSleeperCellsToCountry(target, 2 min game.cellsAvailable)
        
        val caliphateCapital = game.getMuslim(target).caliphateCapital
        
        civilWarAttrition(target, hamaOffensive = true, endOfTurn = false)
      
        // Check to see if the Caliphate Capital has been displaced
        // because its country was improved to Good governance.
        if (caliphateCapital && !game.getMuslim(target).caliphateCapital)
            displaceCaliphateCapital(target)
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(313, "Hayat Tahir al-Sham", Jihadist, 3,
      Remove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot, CannotNotRemoveLastCell,
      (_: Role, forTrigger: Boolean) =>
        game.getMuslim(Syria).civilWar &&
        (forTrigger || (game.cellsAvailable > 0 || game.adjacentCountriesWithCells(Syria).nonEmpty))
      ,
      (role: Role) => {
        val trackCells   = 3 min game.cellsAvailable
        val adjWithCells = game.adjacentCountriesWithCells(Syria)
        val maxAdjCells  = (adjWithCells map (_.cells)).sum
        val mapCells     = ((3 - trackCells) min maxAdjCells) max 0
        
        addEventTarget(Syria)

        addSleeperCellsToCountry(Syria, trackCells)
        // If there were not enough cells on the track
        // then we must make up the difference from adjacent
        // countries as much as possible.
        if (mapCells > 0) {
          println()
          val cellItems = if (role == game.humanRole)
            askCellsFromAnywhere(mapCells, false, adjWithCells map (_.name), sleeperFocus = false)
          else {
            def nextAdjacent(cellsLeft: Int, candidates: List[String]): List[CellsItem] = {
              if (cellsLeft == 0 || candidates.isEmpty)
                Nil
              else {
                val target = JihadistBot.hayatTahirTarget(candidates).get
                val m = game.getMuslim(target)
                val actives  = cellsLeft min m.activeCells
                val sleepers = (cellsLeft - actives) min m.sleeperCells
                val remain   = cellsLeft - actives - sleepers
                CellsItem(target, actives, sleepers) :: nextAdjacent(remain, candidates filterNot (_ == target))
              }
            }
            
            nextAdjacent(mapCells, adjWithCells map (_.name))
          }
          
          moveCellsToTarget(Syria, cellItems)
        }
        
        // Finally see if the caliphate will be declared
        if (trackCells + mapCells == 3 &&
           ((role == game.humanRole && canDeclareCaliphate(Syria) && askDeclareCaliphate(Syria)) ||
            (role == game.botRole && JihadistBot.willDeclareCaliphate(Syria))))
          declareCaliphate(Syria)
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(314, "Jihadist African Safari", Jihadist, 3,
      Remove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot, CannotNotRemoveLastCell,
      (_: Role, forTrigger: Boolean) =>
        forTrigger ||
        game.availablePlots.nonEmpty ||
        game.cellsAvailable > 0
      ,
      (role: Role) => {
        case class Action(name: String, item: Either[Unit, Plot])
        
        
        val actions = if (role == game.humanRole) {
          def nextAction(actionNum: Int, cellsRemaining: Int, plots: List[Plot], candidates: List[String]): List[Action] = {
            if (actionNum > 3 || (cellsRemaining == 0 && plots.isEmpty))
              Nil
            else {
              val choices = List(
                choice(cellsRemaining > 0, "cell", "Place a Cell"),
                choice(plots.nonEmpty,     "plot", "Place a Plot")
              ).flatten
              println()
              askMenu(s"${ordinal(actionNum)} action:", choices).head match {
                case "cell" =>
                  val target = askCountry("Place a Cell in which country: ", candidates)
                  Action(target, Left(())) :: nextAction(actionNum+1, cellsRemaining-1, plots, candidates filterNot (_ == target))
                  
                case _ =>
                  val target = askCountry("Place a Plot in which country: ", candidates)
                  val plot   = askPlots(plots, 1).head
                  val index  = plots.indexOf(plot)
                  val others = plots.take(index) ::: plots.drop(index + 1)
                  Action(target, Right(plot)) :: nextAction(actionNum+1, cellsRemaining, others, candidates filterNot (_ == target))
              }
            }
          }
          nextAction(1, game.cellsAvailable, game.availablePlots, African)
        }
        else {
          // Bot
          val plotsFirst = game.funding < 9
          val plotsToPlace = if (plotsFirst)
            JihadistBot.preparePlots(game.availablePlots) take 3
          else
            JihadistBot.preparePlots(game.availablePlots) take ((3 - game.cellsAvailable) max 0)
          
          val numCells = (3 - plotsToPlace.size) min game.cellsAvailable

          def nextAction(cellsLeft: Int, plots: List[Plot], candidates: List[String]): List[Action] = {
            if (cellsLeft == 0 && plots.isEmpty)
              Nil
            else if (plots.nonEmpty && (plotsFirst || cellsLeft == 0)) {
              val target = JihadistBot.plotPriority(candidates).get
              Action(target, Right(plots.head)) :: nextAction(cellsLeft, plots.tail, candidates filterNot (_ == target))
            }
            else {
              val target = JihadistBot.recruitTravelToPriority(candidates).get
              Action(target, Left(())) :: nextAction(cellsLeft - 1, plots, candidates filterNot (_ == target))
            }
          }
          
          nextAction(numCells, plotsToPlace, African)
        }
        
        println()
        actions foreach {
          case Action(name, Left(_)) =>
            addEventTarget(name)
            testCountry(name)
            addSleeperCellsToCountry(name, 1)
            
          case Action(name, Right(plot)) =>
            addEventTarget(name)
            testCountry(name)
            addAvailablePlotToCountry(name, plot)
        }
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(315, "Khashoggi Crisis", Jihadist, 3,
      Remove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot, CannotNotRemoveLastCell,
      (role: Role, _: Boolean) => role == game.humanRole ||
                                  game.prestige > 3      ||
                                  !game.getMuslim(SaudiArabia).isAdversary
      ,
      (role: Role) => {
        testCountry(Turkey)
        testCountry(SaudiArabia)
        if (role == game.humanRole) {
          val choices = List(
            "shift"    -> "Shift alignment of Saudi Arabia toward Adversary",
            "prestige" -> "Decrease prestige by 2"
          )
          askMenu("\nChoose one:", choices).head match {
            case "shift" => shiftAlignmentRight(SaudiArabia)
            case _       => decreasePrestige(2)
          }
        }
        else {
          // Bot
          if (game.prestige > 3)
            decreasePrestige(2)
          else {
            addEventTarget(SaudiArabia)
            shiftAlignmentRight(SaudiArabia)
          }
        }
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(316, "Martyrdom Operation", Jihadist, 3,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot, CannotNotRemoveLastCell,
      (_: Role, _: Boolean) => game.availablePlots.nonEmpty && martyrdomCandidates.nonEmpty
      ,
      (role: Role) => {
        val (target, (active, sleeper, sadr), plots) = if (role == game.humanRole) {
          val target = askCountry("Select country: ", martyrdomCandidates)
          val cell = askCells(target, 1, sleeperFocus = false)
          (target, cell, askAvailablePlots(2, ops = 3))
        }
        else {
          // See Event Instructions table
          val target = JihadistBot.plotPriority(martyrdomCandidates).get
          val c = game getCountry target
          val cell = if (c.activeCells > 0) (1, 0, false)
                     else if (c.hasSadr)    (0, 0, true)
                     else                   (0, 1, false)
          (target, cell, JihadistBot.preparePlots(game.availablePlots) take 2)
        }
        
        addEventTarget(target)
        removeCellsFromCountry(target, active, sleeper, sadr, addCadre = true)
        for (plot <- plots)
          addAvailablePlotToCountry(target, plot)
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(317, "Qatari Crisis", Jihadist, 3,
      Remove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot, CannotNotRemoveLastCell,
      (_: Role, _: Boolean) => game.hasMuslim(m => m.isShiaMix && m.civilWar)
      ,
      (role: Role) => {
        addEventTarget(GulfStates)
        testCountry(GulfStates)
        val m = game.getMuslim(GulfStates)
        if (m.isGood || m.isFair || !m.isAdversary) {
          if (role == game.humanRole) {
            val choices = List(
              choice(m.isGood || m.isFair, "worsen", "Worsen governance of Gulf States"),
              choice(!m.isAdversary,       "shift",  "Shift alignment of Gulf States towards Adversary")
            ).flatten
            askMenu("\nChoose one:", choices).head match {
              case "worsen" => worsenGovernance(GulfStates, 1, canShiftToIR = false)
              case _        => shiftAlignmentRight(GulfStates)
            }
          }
          else if (m.isGood || m.isFair)
            worsenGovernance(GulfStates, 1, canShiftToIR = false)
          else
            shiftAlignmentRight(GulfStates)
        }
        
        log("Iran, Gulf States, Saudi Arabia and Yemen are all now adjacent.")
        addGlobalEventMarker(QatariCrisis)
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(318, "South China Sea Crisis", Jihadist, 3,
      Remove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot, CannotNotRemoveLastCell,
      AlwaysPlayable,
      (role: Role) => {
        // Take troops from available if possible, otherwise we must 
        // ask the user where to take them from.
        val numToRemove  = 2 min (game.troopsAvailable + game.troopsOnMap)
        val numFromTrack = numToRemove min game.troopsAvailable
        val numFromMap   = numToRemove - numFromTrack
        
        val countries = if (numFromMap == 0)
          Nil
        else if (role == game.humanRole) {
          val targets = game.countries filter (_.troops > 0) map (c => MapItem(c.name, c.troops))
          println(s"Select ${amountOf(numFromMap, "troop")} from the map to remove")
          askMapItems(targets.sortBy(_.country), numFromMap, "troop")
        }
        else
          JihadistBot.troopsToTakeOffMap(numFromMap, countryNames(game.countries filter (_.troops > 0)))
        
        for (MapItem(name, num) <- MapItem("track", numFromTrack) :: countries; if num > 0) {
          if (name != "track")
            addEventTarget(name)
          takeTroopsOffMap(name, num)
        }
        
        val postureCandidates = List(Thailand, Philippines)
        val postureTarget = if (role == game.humanRole)
          askCountry("Roll posture for which country: ", postureCandidates)
        else
          JihadistBot.posturePriority(postureCandidates).get
        
        val die = getDieRoll(s"Enter event die roll: ")
        val modifiedDie = (die + game.prestigeModifier) max 1 min 6
        val newPosture = if (modifiedDie < 5) Soft else Hard
        log(s"Die roll: $die")
        log(f"${game.prestigeModifier}%+d (US prestige modifier)")
        log(s"Modified roll: $modifiedDie")
        
        addEventTarget(postureTarget)
        setCountryPosture(postureTarget, newPosture)
        addGlobalEventMarker(SouthChinaSeaCrisis)
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(319, "Tehran-Beirut Land Corridor", Jihadist, 3,
      Remove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot, CannotNotRemoveLastCell,
      (_: Role, _: Boolean) => tehranBeirutLandCorridorSatisfied
      ,
      (role: Role) => {
        increaseFunding(3)
        addEventTarget(Iran)
        addEventMarkersToCountry(Iran, TehranBeirutLandCorridor)
        log()
        log("Iran is now a 3 Resource country and will remain so as long as")
        log("neither Iran, Syria nor Lebanon become Ally or Civil War")
        log("and at least one of Iraq and Turkey are not Ally or Civil War")
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(320, "Tribal Leaders Withdraw Support", Jihadist, 3,
      Remove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot, CannotNotRemoveLastCell,
      (_: Role, forTrigger: Boolean) =>
        forTrigger ||
        tribalLeadersCandidates.nonEmpty
      ,
      (role: Role) => {
        if (role == game.humanRole) {
          def nextResponse(numLeft: Int): Unit = {
            val candidates = tribalLeadersCandidates
            if (numLeft > 0 && candidates.nonEmpty) {
              val target = askCountry("Remove militia from which country: ", candidates)
              val maxNum = game.getMuslim(target).militia min numLeft
              val num    = if (candidates.size == 1) maxNum else askInt(s"Remove how many from $target", 1, maxNum)
              addEventTarget(target)
              removeMilitiaFromCountry(target, num)
              nextResponse(numLeft - num)
            }
          }
          nextResponse(3)
        }
        else {
          // Bot
          def nextMilitia(numLeft: Int): Unit = {
            val candidates = tribalLeadersCandidates
            if (numLeft > 0 && candidates.nonEmpty) {
              val target = JihadistBot.troopsMilitiaTarget(candidates).get
              val num    = game.getMuslim(target).militia min numLeft
              addEventTarget(target)
              removeMilitiaFromCountry(target, num)
              nextMilitia(numLeft - num)
            }
          }
          nextMilitia(3)
        }
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(321, "Ungoverned Spaces", Jihadist, 3,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot, CannotNotRemoveLastCell,
      (_: Role, _: Boolean) => game.cellsAvailable > 0 && ungovernedSpaceCandidates.nonEmpty
      ,
      (role: Role) => {
        val (target, num) = if (role == game.humanRole)
          (askCountry("Which country: ", ungovernedSpaceCandidates), askInt("Place how many cells", 1, game.cellsAvailable min 3))
        else
          (JihadistBot.recruitTravelToPriority(ungovernedSpaceCandidates).get, game.cellsAvailable min 3)
        
        addEventTarget(target)
        testCountry(target)
        addSleeperCellsToCountry(target, num)
        
        if (num == 3 &&
            canDeclareCaliphate(target) &&
            ((role == game.humanRole && askDeclareCaliphate(target)) ||
             (role == game.botRole && JihadistBot.willDeclareCaliphate(target))))
          declareCaliphate(target)
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(322, "Amnesty International", Unassociated, 1,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot, CannotNotRemoveLastCell,
      (role: Role, _: Boolean) => (role == US       && amnestyInternationUSCandidates.nonEmpty) ||
                                  (role == Jihadist && amnestyInternationJihadistCandidates.nonEmpty)
      ,
      (role: Role) => if (role == US) {
          val target = if (role == game.humanRole)
            askCountry("Place Awakening marker in which country: ", amnestyInternationUSCandidates)
          else
            USBot.markerAlignGovTarget(amnestyInternationUSCandidates).get
          
          addEventTarget(target)
          testCountry(target)
          addAwakeningMarker(target)
      }
      else { // Jihadist
        val target = if (role == game.humanRole)
          askCountry("Place Reaction marker in which country: ", amnestyInternationJihadistCandidates)
        else
          JihadistBot.markerAlignGovTarget(amnestyInternationJihadistCandidates).get
        
        addEventTarget(target)
        testCountry(target)
        addReactionMarker(target)
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(323, "Blasphemy", Unassociated, 1,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot, CannotNotRemoveLastCell,
      (role: Role, _: Boolean) => blasphemyCandidates(role).nonEmpty
      ,
      (role: Role) => {
        def humanMarker(target: String): Boolean =
          game.getMuslim(target).alignment match {
            case Adversary => false
            case Ally      => true
            case _         => askPlaceAwakeningOrReactionMarker
          }
        
        val candidates = blasphemyCandidates(role)
        var (target, placeAwakening) = role match {
          case _ if role == game.humanRole => 
            val target = askCountry("Place marker in which country: ", candidates)
            val marker = humanMarker(target)
            (target, marker)
          case US       => (USBot.markerAlignGovTarget(candidates).get, true)
          case Jihadist => (JihadistBot.markerAlignGovTarget(candidates).get, false)
        }
                
        addEventTarget(target)
        
        if (placeAwakening) 
          addAwakeningMarker(target)
        else
          addReactionMarker(target)
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(324, "BREXIT", Unassociated, 1,
      USRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot, CannotNotRemoveLastCell,
      (role: Role, _: Boolean) => globalEventInPlay(Euroscepticism) ||
                                  (game hasNonMuslim (n => n.isSchengen && n.totalCells > 0)) ||
                                  game.resolvedPlotTargets.exists(t => Schengen.contains(t.name))
      ,
      (role: Role) => if (role == US) {
        addEventTarget(UnitedKingdom)
        setCountryPosture(UnitedKingdom, Hard)
        addEventMarkersToCountry(UnitedKingdom, BREXIT)
      }
      else { // Jihadist
        decreasePrestige(1)
        addEventTarget(UnitedKingdom)
        testCountry(UnitedKingdom)
        val source = closestWithCells(UnitedKingdom) match {
          case Nil => None
          case candidates if role == game.humanRole =>
            Some(askCountry(s"Move cell to $UnitedKingdom from which country: ", candidates))
          case candidates =>
            JihadistBot.travelFromTarget(UnitedKingdom, candidates)
        }
        
        source foreach { name =>
          moveCellsBetweenCountries(name, UnitedKingdom, 1, game.getCountry(name).activeCells > 0, forTravel = false)
        }
      }  
    )),
    // ------------------------------------------------------------------------
    entry(new Card(325, "Dissent Channel", Unassociated, 1,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot, CannotNotRemoveLastCell,
      (role: Role, _: Boolean) => (role == US && game.funding > 1) ||
                                  (role == Jihadist && game.prestige > 1)
      ,
      (role: Role) => {
        if (role == US)
          decreaseFunding(1)
        else
          decreasePrestige(1)
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(326, "Filibuster/Nuclear Option", Unassociated, 1,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot, CannotNotRemoveLastCell,
      (role: Role, _: Boolean) => {
         val basic = globalEventInPlay(TrumpTweetsON) || globalEventInPlay(TrumpTweetsOFF)
         if (role == game.humanRole)
           basic
         else if (role == US)
           globalEventInPlay(TrumpTweetsOFF)
         else
           globalEventInPlay(TrumpTweetsON)
      }
      ,
      (role: Role) => {
        val action = if (role == game.humanRole) {
          val choices = List("on" -> "Set Trump Tweets ON", "off" -> "Set Trump Tweets OFF")
          askMenu("\nChoose one: ", choices).head
        }
        else if (role == US) "on" else "off"
        
        if (action == "on")
          setTrumpTweetsON()
        else
          setTrumpTweetsOFF()
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(327, "Gaza Aid", Unassociated, 1,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot, CannotNotRemoveLastCell,
      (role: Role, _: Boolean) => (role == US && game.prestige < 12) ||
                                  (role == Jihadist && game.funding < 9)
      ,
      (role: Role) => {
        increasePrestige(1)
        increaseFunding(1)
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(328, "Hafiz Saeed Khan", Unassociated, 1,
      USRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot, CannotNotRemoveLastCell,
      (role: Role, _: Boolean) => 
        (role == US && (hafizSaeedKhanMuslimUSCandidates.nonEmpty || hafizSaeedKhanNonMuslimUSCandidates.nonEmpty)) ||
        (role == Jihadist && (hafizSaeedKhanMuslimJihadistCandidates.nonEmpty || hafizSaeedKhanNonMuslimJihadistCandidates.nonEmpty))
      ,
      (role: Role) => if (role == US) {
        val candidates = hafizSaeedKhanMuslimUSCandidates ::: hafizSaeedKhanNonMuslimUSCandidates
        val target = if (role == game.humanRole)
          askCountry("Which country: ", candidates)
        else if (hafizSaeedKhanMuslimUSCandidates.nonEmpty)
          USBot.markerAlignGovTarget(hafizSaeedKhanMuslimUSCandidates).get
        else
          USBot.posturePriority(hafizSaeedKhanNonMuslimUSCandidates).get
        
        addEventTarget(target)
        testCountry(target)
        if (target == India)
          setCountryPosture(India, Hard)
        else
          shiftAlignmentLeft(target)
      }
      else { // Jihadist
        val candidates = hafizSaeedKhanMuslimJihadistCandidates ::: hafizSaeedKhanNonMuslimJihadistCandidates
        val (target, action) = if (role == game.humanRole) {
          val name = askCountry("Which country: ", candidates)
          val choices = List(
            choice(game.availablePlots contains Plot1, "plot", "Place a level 1 Plot"),
            choice(game.cellsAvailable > 0,            "cell", "Place a Cell")
          ).flatten
          (name, askMenu("\nChoose one:", choices).head)
        }
        else if (game.availablePlots contains Plot1)
          (JihadistBot.plotPriority(candidates).get, "plot")
        else
          (JihadistBot.recruitTravelToPriority(candidates).get, "cell")
        
        addEventTarget(target)
        testCountry(target)
        if (action == "plot")
          addAvailablePlotToCountry(target, Plot1, visible = true)
        else
          addSleeperCellsToCountry(target, 1)
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(329, "Hamza bin Laden", Unassociated, 1,
      USRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot, CannotNotRemoveLastCell,
      (role: Role, _: Boolean) => (PersonalityCards exists game.cardRemoved) &&
          (
            role == game.humanRole ||
            (role == US && game.prestige < 12) ||
            (role == Jihadist && game.funding < 9)
          )
      ,
      (role: Role) => if (role == US) {
        increasePrestige(1)
      }
      else {
        increaseFunding(2)
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(330, "IRGC", Unassociated, 1,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      () => {
        // Can we remove the last cell on the board?
        irgcCandidates(US) exists (name => USBot.wouldRemoveLastCell(name, 1))
      }
      ,
      (role: Role, _: Boolean) => (isIranSpecialCase || !game.getMuslim(Iran).isAlly) && irgcCandidates(role).nonEmpty
      ,
      (role: Role) => if (role == US) {
        val (target, (actives, sleepers, sadr)) = if (role == game.humanRole) {
          val t = askCountry("Remove a cell from which country: ", irgcCandidates(role))
          (t, askCells(t, 1, true))
        }
        else  {
          val t = USBot.disruptPriority(irgcCandidates(role)).get
          (t, USBot.chooseCellsToRemove(t, 1))
          
        }
        addEventTarget(target)
        removeCellsFromCountry(target, actives, sleepers, sadr, addCadre = true)
      }
      else { // Jihadist
        val target = if (role == game.humanRole)
          askCountry("Remove a militia from which country: ", irgcCandidates(role))
        else
          JihadistBot.troopsMilitiaTarget(irgcCandidates(role)).get
        
        addEventTarget(target)
        removeMilitiaFromCountry(target, 1)
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(331, "JASTA", Unassociated, 1,
      Remove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot, CannotNotRemoveLastCell,
      NeverPlayable,  // Not playable in the Solo game
      (role: Role) => {
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(332, "Khan Shaykhun Chemical Attack", Unassociated, 1,
      USRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot, CannotNotRemoveLastCell,
      (role: Role, _: Boolean) => game.getMuslim(Syria).civilWar &&
                                  (role == US || game.getMuslim(Syria).militia > 0) 
      ,
      (role: Role) => if (role == US) {
        addEventTarget(Syria)
        if (game.getMuslim(Syria).wmdCache >0)
          removeCachedWMD(Syria, 1)
      }
      else { // Jihadist
        addEventTarget(Syria)
        removeMilitiaFromCountry(Syria, 1)
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(333, "MbS", Unassociated, 1,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot, CannotNotRemoveLastCell,
      (role: Role, _: Boolean) => game.getMuslim(SaudiArabia).canTakeAwakeningOrReactionMarker
      ,
      (role: Role) => {
        addEventTarget(SaudiArabia)
        testCountry(SaudiArabia)
        if (role == US)
          addAwakeningMarker(SaudiArabia)
        else
          addReactionMarker(SaudiArabia)
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(334, "Novichok Agent", Unassociated, 1,
      Remove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot, CannotNotRemoveLastCell,
      (role: Role, _: Boolean) => if (role == game.humanRole)
        !game.getNonMuslim(Russia).isUntested
      else
      {
        val russiaPosture = game.getNonMuslim(Russia).posture
        (role == US       && russiaPosture == oppositePosture(game.usPosture)) ||
        (role == Jihadist && russiaPosture == game.usPosture)
      }
      ,
      (role: Role) => {
        addEventTarget(UnitedKingdom)
        setCountryPosture(UnitedKingdom, oppositePosture(game.getNonMuslim(Russia).posture))
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(335, "Rohingya Genocide", Unassociated, 1,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot, CannotNotRemoveLastCell,
      (role: Role, _: Boolean) => {
        val countries  = game.getNonMuslims(India::Thailand::Nil)
        val canPosture = if (role == US) countries exists (n => n.isUntested || n.posture != game.usPosture)
                         else            countries exists (n => n.isUntested || n.posture == game.usPosture)
        (role == game.humanRole) ||
        (role == Jihadist && game.cellsAvailable > 0) ||
        canPosture
      }
      ,
      (role: Role) => {
        val names = India::Thailand::Nil
        val countries  = game.getNonMuslims(names)
        if (role == game.humanRole) {
          val choices = List(
            choice(true,                    "posture", "Set posture of India or Thailand"),
            choice(game.cellsAvailable > 0, "cell",    "Place a Cell in India or Thailand")
          ).flatten
          val action = askMenu("\nChoose one:", choices).head
          val target = askCountry("Which country: ", names)
          
          addEventTarget(target)
          testCountry(target)
          if (action == "posture") {
            val posture = askOneOf("New posture (Soft or Hard): ", Seq(Soft, Hard)).get
            setCountryPosture(target, posture)
          }
          else
            addSleeperCellsToCountry(target, 1)
        }
        else if (role == US) {
          val candidates = countries filter (n => n.isUntested || n.posture != game.usPosture)
          val target = USBot.posturePriority(countryNames(candidates)).get
          addEventTarget(target)
          setCountryPosture(target, game.usPosture)
        }
        else if (game.cellsAvailable > 0) {
          val target = JihadistBot.recruitTravelToPriority(names).get
          addEventTarget(target)
          testCountry(target)
          addSleeperCellsToCountry(target, 1)
        }
        else {
          val candidates = countries filter (n => n.isUntested || n.posture == game.usPosture)
          val target = JihadistBot.posturePriority(countryNames(candidates)).get
          addEventTarget(target)
          setCountryPosture(target, oppositePosture(game.usPosture))
        }
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(336, "US/NK Summit", Unassociated, 1,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot, CannotNotRemoveLastCell,
      (role: Role, _: Boolean) => 
        (game.worldPosture == game.usPosture || game.worldPosture == Even) && trumpTweetsON
      ,
      (role: Role) => if (role == US) {
        setTrumpTweetsOFF()
        setCountryPosture(China, game.usPosture)
        addGlobalEventMarker(USNKSummit)
      }
      else {
        setTrumpTweetsOFF()
        setCountryPosture(China, oppositePosture(game.usPosture))
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(337, "US Border Crisis", Unassociated, 1,
      NoRemove, USLapsing, NoAutoTrigger, DoesNotAlertPlot, CannotNotRemoveLastCell,
      (role: Role, _: Boolean) =>
        trumpTweetsON &&
        (role == Jihadist || cacheYesOrNo(s"Does the $US player have least one card in hand? (y/n) "))
      ,
      (role: Role) => if (role == US) {
        setTrumpTweetsOFF()
        
        if (game.troopsAvailable > 0)
          takeTroopsOffMap("track", 1)
        else {
          val candidates = countryNames(game.countries filter (_.troops > 0))
          val target = if (role == game.humanRole)
            askCountry("Remove troop from which country: ", candidates)
          else
            USBot.deployFromTarget(candidates).get
          
          addEventTarget(target)
          takeTroopsOffMap(target, 1)
        }
          
        log()
        log(s"$US player draws 1 card")
        askCardsDrawn(1)
      }
      else { // Jihadist
        setTrumpTweetsOFF()
        
        if (role == game.humanRole)
          log(s"Discard top card of the $US hand")
        else
          log(s"You ($US) must discard one random card")
        askCardsDiscarded(1)
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(338, "Abu Muhammad al-Shimali", Unassociated, 2,
      USRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      () => {
        // Can we remove the last cell on the board?
        val candidates = countryNames(game.countries filter (_.totalCells > 0))
        candidates exists (name => USBot.wouldRemoveLastCell(name, 1))
      }
      ,
      (role: Role, _: Boolean) => (role == US && ((game.countries exists (_.totalCells > 0)) || game.prestige < 12)) ||
                                  (role == Jihadist && game.cellsAvailable > 0)
      ,
      (role: Role) => if (role == US) {
        val candidates = countryNames(game.countries filter (_.totalCells > 0))
        if (candidates.nonEmpty) {
          val (target, (actives, sleepers, sadr)) = if (role == game.humanRole) {
            val t = askCountry("Remove a Cell from which country: ", candidates)
            (t, askCells(t, 1, true))
          }
          else {
            val t = USBot.disruptPriority(candidates).get
            (t, USBot.chooseCellsToRemove(t, 1))
          }
          
          addEventTarget(target)
          removeCellsFromCountry(target, actives, sleepers, sadr, addCadre = true)
        }
        increasePrestige(1)
      }
      else { // Jihadist
        val maxCells = game.cellsAvailable min 3
        val candidates = countryNames(game.muslims)
        val (target, num) = if (role == game.humanRole)
          (askCountry("Place cells in which country: ", candidates),
           askInt("Place how many cells", 1, maxCells, Some(maxCells))
          )
        else
          (JihadistBot.recruitTravelToPriority(candidates).get, maxCells)
        
        addEventTarget(target)
        testCountry(target)
        addSleeperCellsToCountry(target, num)
        if (num == 3 &&
            canDeclareCaliphate(target) &&
            ((role == game.humanRole && askDeclareCaliphate(target)) ||
             (role == game.botRole && JihadistBot.willDeclareCaliphate(target))))
          declareCaliphate(target)
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(339, "Erdogan Dance", Unassociated, 2,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot, CannotNotRemoveLastCell,
      (role: Role, _: Boolean) => if (role == game.humanRole)
        erdoganDanceCandidates.nonEmpty
      else if (role == US)
        (erdoganDanceMuslims exists (!_.isAlly)) || (erdoganDanceNonMuslims exists (_.posture != game.usPosture))
      else
        (erdoganDanceMuslims exists (!_.isAdversary)) || (erdoganDanceNonMuslims exists (n => n.isUntested || n.posture == game.usPosture))
      ,
      (role: Role) => {
        val target = if (role == game.humanRole)
          askCountry("Which country: ", erdoganDanceCandidates)
        else if (role == US && erdoganDanceMuslims.nonEmpty)
          USBot.markerAlignGovTarget(countryNames(erdoganDanceMuslims filter (!_.isAlly))).get
        else if (role == US)
          USBot.posturePriority(countryNames(erdoganDanceNonMuslims filter (_.posture != game.usPosture))).get
        else if (role == Jihadist && erdoganDanceMuslims.nonEmpty)
          JihadistBot.markerAlignGovTarget(countryNames(erdoganDanceMuslims filter (!_.isAdversary))).get
        else
          JihadistBot.posturePriority(countryNames(erdoganDanceNonMuslims filter (n => n.isUntested || n.posture == game.usPosture))).get
        
        addEventTarget(target)
        testCountry(target)
        
        (game.isMuslim(target), role) match {
          case (true,  US)       => shiftAlignmentLeft(target)
          case (true,  Jihadist) => shiftAlignmentRight(target)
          case (false, US)       => setCountryPosture(target, game.usPosture)
          case (false, Jihadist) => setCountryPosture(target, oppositePosture(game.usPosture))
        }
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(340, "EU Bolsters Iran Deal", Unassociated, 2,
      JihadistRemove, JihadistLapsing, NoAutoTrigger, DoesNotAlertPlot, CannotNotRemoveLastCell,
      (role: Role, _: Boolean) => (game.getCountry(Iran).wmdCache > 0) &&
                                  (role == game.humanRole || role == Jihadist || game.gwotPenalty > 0)
      ,
      (role: Role) => {
        val drm = if (role == US) 2 else -1

        for (name <- List(France, Germany)) {
          addEventTarget(name)
          rollCountryPosture(name, drm)
        }
        
        if (role == Jihadist) {
          log()
          log(s"$US may not perform War of Ideas in Schengen")
          log("countries for the rest of this turn.")
        }
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(341, "Gulen Movement", Unassociated, 2,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot, CannotNotRemoveLastCell,
      (role: Role, _: Boolean) => (role == game.humanRole) ||
                                  globalEventNotInPlay(GulenMovement) ||
                                  (role == US && game.getMuslim(Turkey).canTakeAwakeningOrReactionMarker) ||
                                  (role == Jihadist && game.cellsAvailable > 0)
      ,
      (role: Role) => {
        addGlobalEventMarker(GulenMovement)
        addEventTarget(Turkey)
        testCountry(Turkey)
        
        log()
        if (role == US)
          addAwakeningMarker(Turkey)
        else if (game.cellsAvailable > 0) {
          // Jihadist
          val maxNum = 2 min game.cellsAvailable
          val num = if (role == game.humanRole)
            askInt("Place how many cells", 1, maxNum, Some(maxNum))
          else
            maxNum
          addSleeperCellsToCountry(Turkey, num)
        }
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(342, "Gulmurod Khalimov", Unassociated, 2,
      USRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot, CannotNotRemoveLastCell,
      (role: Role, _: Boolean) => game.caliphateDeclared && 
                                   ((role == US && (game.prestige < 12 || game.funding > 1)) ||
                                    role == Jihadist)
      ,
      (role: Role) => if (role == US) {
        increasePrestige(1)
        decreaseFunding(1)
      }
      else { // Jihadist
        val candidates = countryNames(game.muslims)
        val target = if (role == game.humanRole)
          askCountry("Which country: ", candidates)
        else
          JihadistBot.recruitTravelToPriority(candidates).get
        
        val withCells = countryNames(game.countries filter (c => c.name != target && c.cells > 0))
        val cells = if (role == game.humanRole)
          askCellsFromAnywhere(2, true, withCells, sleeperFocus = false)
        else {
          def nextFrom(countries: List[String], remaining: Int, cellsAvailable: Int): List[CellsItem] = {
            if (remaining == 0)
              Nil
            else if (cellsAvailable > 0) {
              val n = remaining min cellsAvailable
              CellsItem("track", 0, n)::nextFrom(countries, remaining - n, cellsAvailable - n)
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
                  CellsItem(name, a, s)::nextFrom(countries filterNot (_ == name), remaining - n, cellsAvailable)
                case None => Nil
              }
            }
          }
          
          nextFrom(withCells, 2, game.cellsAvailable)
        }
        
        addEventTarget(target)
        testCountry(target)
        moveCellsToTarget(target, cells)
        
        val m = game.getMuslim(target)
        if (m.jihadOK) {
          val actives  = 2 min m.activeCells
          val sleepers = 2 - actives

          performJihads(JihadTarget(target, actives, sleepers, false, false)::Nil, ignoreFailures = true)
        }        
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(343, "JCPOA", Unassociated, 2,
      JihadistRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      () => {
        // Can we remove the last cell on the board?
        val iranCells = game.getCountry(Iran).totalCells
        game.totalCellsOnMap == 1 && iranCells == 1
      }
      ,
      (role: Role, _: Boolean) => {
        val hasWMD = game.getCountry(Iran).wmdCache > 0
        val jihadOk = (isIranSpecialCase || game.getMuslim(Iran).isAdversary) && game.getCountry(Iran).totalCells > 0
        (role == US && game.worldPosture != oppositePosture(game.usPosture) && hasWMD) ||
        (role == Jihadist && jihadOk && (isIranSpecialCase || hasWMD))
      }
      ,
      (role: Role) => if (role == US) {
        addEventTarget(Iran)
        removeCachedWMD(Iran, 1)
      }
      else { // Jihadist
        val die = getDieRoll(s"Enter event die roll: ")
        addEventTarget(Iran)
        log(s"Die roll: $die")
        if (die < 4) {
          val (actives, sleepers, sadr) = if (role == game.humanRole)
            askCells(Iran, 1, false)
          else
            JihadistBot.chooseCellsToRemove(Iran, 1)
          
          removeCellsFromCountry(Iran, actives, sleepers, sadr, addCadre = true)
          
          if (game.getCountry(Iran).wmdCache > 0)
            moveWMDCacheToAvailable(Iran, 1)
          
          if (isIranSpecialCase)
            flipIranToShiaMixMuslim()
        }
        else
          log("No Effect")
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(344, "Media Manipulation", Unassociated, 2,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot, CannotNotRemoveLastCell,
      (role: Role, _: Boolean) => mediaManipulationCandidates(role).nonEmpty
      ,
      (role: Role) => {
        val target = if (role == game.humanRole)
          askCountry("Which country: ", mediaManipulationCandidates(role))
        else if (role == US)
          USBot.markerAlignGovTarget(mediaManipulationCandidates(role)).get
        else
          JihadistBot.markerAlignGovTarget(mediaManipulationCandidates(role)).get

        if (role == US)
          shiftAlignmentLeft(target)
        else
          shiftAlignmentRight(target)
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(345, "Operation Euphrates Shield", Unassociated, 2,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      () => {
        // Can we remove the last cell on the board?
        euphratesShieldCandidates(US) exists (name => USBot.wouldRemoveLastCell(name, 1))
      }
      ,
      (role: Role, _: Boolean) => euphratesShieldCandidates(role).nonEmpty
      ,
      (role: Role) => if (role == US) {
        val withCells = euphratesShieldCandidates(role) filter (name => game.getMuslim(name).totalCells > 0)
        val target = if (role == game.humanRole)
          askCountry("Which country: ", euphratesShieldCandidates(role))
        else if (withCells.nonEmpty)
          USBot.disruptPriority(withCells).get
        else
          USBot.markerAlignGovTarget(euphratesShieldCandidates(role)).get
        
        addEventTarget(target)
        testCountry(target)
        
        if (game.getMuslim(target).totalCells > 0) {
          val (actives, sleepers, sadr) = if (role == game.humanRole)
            askCells(target, 1, true)
          else
            USBot.chooseCellsToRemove(target, 1)
          removeCellsFromCountry(target, actives, sleepers, sadr, addCadre = true)
        }
        addAidMarker(target)
      }
      else { // Jihadist
        val withMilitia = euphratesShieldCandidates(role) filter (name => game.getMuslim(name).militia > 0)
        val notBesieged = euphratesShieldCandidates(role) filter (name => !game.getMuslim(name).besiegedRegime)
        val target = if (role == game.humanRole)
          askCountry("Which country: ", euphratesShieldCandidates(role))
        else if (withMilitia.nonEmpty)
          JihadistBot.troopsMilitiaTarget(withMilitia).get
        else
          JihadistBot.markerAlignGovTarget(notBesieged).get
        
        addEventTarget(target)
        testCountry(target)
        
        if (game.getMuslim(target).militia > 0)
          removeMilitiaFromCountry(target, 1)
        
        addBesiegedRegimeMarker(target)
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(346, "Pakistani Intelligence (ISI)", Unassociated, 2,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      () => {
        // Can we remove the last cell on the board?
        pakistaniIntelligenceCandidates(US) exists (name => USBot.wouldRemoveLastCell(name, 1))
      }
      ,
      (role: Role, _: Boolean) => pakistaniIntelligenceCandidates(role).nonEmpty
      ,
      (role: Role) => if (role == US) {
        val target = if (role == game.humanRole)
          askCountry("Which country: ", pakistaniIntelligenceCandidates(role))
        else
          (pakistaniIntelligenceCandidates(role).find(name => USBot.wouldRemoveLastCell(name, 1)) orElse
          USBot.deployToPriority(pakistaniIntelligenceCandidates(role))).get
        
        addEventTarget(target)
        testCountry(target)
        
        if (game.militiaAvailable > 0 && game.getMuslim(target).canTakeMilitia)
          addMilitiaToCountry(target, 1)
        
        if (game.getMuslim(target).totalCells > 0) {
          val (actives, sleepers, sadr) = if (role == game.humanRole)
            askCells(target, 1, true)
          else
            USBot.chooseCellsToRemove(target, 1)
          removeCellsFromCountry(target, actives, sleepers, sadr, addCadre = true)
        }
      }
      else { // Jihadist
        val target = if (role == game.humanRole)
          askCountry("Which country: ", pakistaniIntelligenceCandidates(role))
        else
          JihadistBot.recruitTravelToPriority(pakistaniIntelligenceCandidates(role)).get
        
        addEventTarget(target)
        testCountry(target)
        
        if (game.cellsAvailable > 0)
          addSleeperCellsToCountry(target, 1)
        
        if (game.getMuslim(target).militia > 0)
          removeMilitiaFromCountry(target, 1)
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(347, "Switching Jerseys", Unassociated, 2,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      () => {
        // Can we remove the last cell on the board?
        switchingJerseysCandidates(US) exists (name => USBot.wouldRemoveLastCell(name, 1))
      }
      ,      
      (role: Role, _: Boolean) => switchingJerseysCandidates(role).nonEmpty
      ,
      (role: Role) => if (role == US) {
        val target = if (role == game.humanRole)
          askCountry("Which country: ", switchingJerseysCandidates(role))
        else
          (switchingJerseysCandidates(role).find(name => USBot.wouldRemoveLastCell(name, 1)) orElse
          USBot.deployToPriority(switchingJerseysCandidates(role))).get
        
        val (actives, sleepers, sadr) = if (role == game.humanRole)
          askCells(target, 1, true)
        else
          USBot.chooseCellsToRemove(target, 1)
        
        addEventTarget(target)
        removeCellsFromCountry(target, actives, sleepers, sadr, addCadre = true)
        if (game.militiaAvailable > 0)
          addMilitiaToCountry(target, 1)
      }
      else { // Jihadist
        val target = if (role == game.humanRole)
          askCountry("Which country: ", switchingJerseysCandidates(role))
        else
          JihadistBot.recruitTravelToPriority(switchingJerseysCandidates(role)).get
        
        addEventTarget(target)
        removeMilitiaFromCountry(target, 1)
        
        if (game.cellsAvailable > 0)
          addSleeperCellsToCountry(target, 1)
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(348, "Travel Ban", Unassociated, 2,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot, CannotNotRemoveLastCell,
      (role: Role, _: Boolean) => trumpTweetsON &&
                                  (role == US && globalEventNotInPlay(TravelBan) ||
                                   role == Jihadist)
      ,
      (role: Role) => {
        setTrumpTweetsOFF()
        if (role == US)
          addGlobalEventMarker(TravelBan)
        else {
          removeGlobalEventMarker(TravelBan)
          List(PatriotAct, BREXIT) foreach removeCountryEventMarkerAnywhere
          removeLapsingCards(List(Biometrics, IslamicMaghreb))
          val candidates = countryNames(game.muslims filter (_.awakening > 0))
          if (candidates.nonEmpty) {
            val target = if (role == game.humanRole)
              askCountry("Remove awakening marker from which country: ", candidates)
            else
              JihadistBot.markerAlignGovTarget(candidates).get
            removeAwakeningMarker(target)
          }
        }
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(349, "Turkish Coup", Unassociated, 2,
      JihadistRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot, CannotNotRemoveLastCell,
      (role: Role, _: Boolean) => {
        val turkey = game.getMuslim(Turkey)
        globalEventInPlay(GulenMovement) && !turkey.isUntested &&
        (
          role == game.humanRole ||
          (role == US && !(turkey.isGood && turkey.isAlly)) ||
          (role == Jihadist && !(turkey.isAdversary && turkey.isIslamistRule))
        )
      }
      ,
      (role: Role) => {
        val turkey = game.getMuslim(Turkey)
        val choices = if (role == US)
          List(
            choice(!turkey.isGood, "improve", "Improve governance 1 level"),
            choice(!turkey.isAlly, "left",    "Shift alignment towards Ally")
          ).flatten
        else
          List(
            choice(!turkey.isIslamistRule, "worsen", "Worsen governance 1 level"),
            choice(!turkey.isAdversary,    "right",  "Shift alignment towards Adversary")
          ).flatten
        
        val action = role match {
          case _  if role == game.humanRole       => askMenu("\nChoose one:", choices).head
          case US if !turkey.isGood               => "improve"
          case US                                 => "left"
          case Jihadist if !turkey.isIslamistRule => "worsen"
          case Jihadist                           => "right"
        }
        
        addEventTarget(Turkey)
        action match {
          case "improve" => improveGovernance(Turkey, 1, canShiftToGood = true)
          case "worsen"  => worsenGovernance(Turkey, 1, canShiftToIR = true)
          case "left"    => shiftAlignmentLeft(Turkey)
          case _         => shiftAlignmentRight(Turkey)
        }
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(350, "UN Peace Envoy", Unassociated, 2,
      USRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      () => {
        // Can we remove the last cell on the board?
        unPeaceEnvoyCandidates(US) exists (name => game.getCountry(name).totalCells == game.totalCellsOnMap)
      }
      ,      
      (role: Role, _: Boolean) => unPeaceEnvoyCandidates(role).nonEmpty
      ,
      (role: Role) => {
        val target = if (role == game.humanRole)
          askCountry("Which country: ", unPeaceEnvoyCandidates(role))
        else if (role == US)
          USBot.disruptPriority(unPeaceEnvoyCandidates(role)).get
        else
          JihadistBot.troopsMilitiaTarget(unPeaceEnvoyCandidates(role)).get
        
        @tailrec def nextRemoval(): Unit = {
          val m = game.getMuslim(target)
          if (m.totalCells > 0 && m.totalTroopsAndMilitia > 0) {
            val ((actives, sleepers, sadr), usUnit) = if (role == game.humanRole)
              (askCells(target, 1, role == US), askTroopOrMilitia(target))
            else if (role == US)
              (USBot.chooseCellsToRemove(target, 1), USBot.chooseTroopOrMilitiaToRemove(target))
            else
              (JihadistBot.chooseCellsToRemove(target, 1), JihadistBot.chooseTroopOrMilitiaToRemove(target))
            
            removeCellsFromCountry(target, actives, sleepers, sadr, addCadre = true)
            usUnit match {
              case "militia-cube" => removeMilitiaFromCountry(target, 1)
              case "troop-cube"   => moveTroops(target, "track", 1)
              case marker         => removeEventMarkersFromCountry(target, marker)
            }
            nextRemoval()
          }
        }
          
        addEventTarget(target)
        nextRemoval()
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(351, "Advanced Persistent Threat (APT)", Unassociated, 3,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot, CannotNotRemoveLastCell,
      (role: Role, _: Boolean) => cacheYesOrNo(s"Do both players have at least 1 card in hand? (y/n) ")
      ,
      (role: Role) => {
        val opponent = oppositeRole(role)
        log()
        if (role == game.humanRole)
          log(s"Take the top card of the $opponent hand")
        else
          log(s"Take a random card from your ($opponent) hand")
        val cardNum = askCardNumber(s"Card # of the card taken: ", allowNone = false).get
        val card    = deck(cardNum)
        val cardDisplay = s""""${card.name}""""
        
        cachedEventPlayableAnswer = None

        // Avenger card will trigger when randomly drawn.
        if (cardNum == AvengerCard) {
          log()
          log(s"""The "Avenger" card was randomly drawn, so the event triggers""")
          log(separator())
          card.executeEvent(US)
        }
        else {
          val action = if (role == game.humanRole) {
            val choices = List(
              choice(card.eventIsPlayable(role), "event",  s"Play the $cardDisplay event"),
              choice(!card.autoTrigger,          "discard",s"Discard $cardDisplay with no effect"),
              choice(true,                       "return", s"Return $cardDisplay to the $opponent hand"),
              choice(true,                       "keep",   s"Keep $cardDisplay and give another card to the $opponent")
            ).flatten
            askMenu("\nChoose one:", choices).head
          }
          else { // Bot
            if (card.autoTrigger || card.eventIsPlayable(role)) "event" else "discard"
          }
          
          action match {
            case "discard" =>
              log(s"Place $cardDisplay in the discard pile")
              processDiscardedCard(cardNum)
            case "return"  =>
              log(s"Return $cardDisplay to the top of the $opponent hand")
            case "keep"    => 
              log(s"Keep $cardDisplay and place another card from your hand the $opponent hand")
              askCardsDrawn(1)
              
            case _ => // Play the event
              log(s"\n$role executes the $cardDisplay event")
              log(separator())
              card.executeEvent(role)
              
              if (card.markLapsingAfterExecutingEvent(role))
                markCardAsLapsing(card.number)
              else if (card.removeAfterExecutingEvent(role))
                removeCardFromGame(card.number)
          }
        }
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(352, "al-Baghdadi", Unassociated, 3,
      USRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot, CannotNotRemoveLastCell,
      (role: Role, _: Boolean) => {
        (role == US       && (game hasMuslim (m => m.civilWar && m.totalTroopsAndMilitia > m.totalCells))) ||
        (role == Jihadist && (globalEventNotInPlay(AlBaghdadi) || (game.cellsAvailable > 0 && game.hasMuslim (m => m.isPoor))))
      }
      ,
      (role: Role) => if (role == US) {
        val priorExtraCellCapacity = game.extraCellCapacity

        increasePrestige(2)
        decreaseFunding(1)
        removeGlobalEventMarker(AlBaghdadi)        
      }
      else {  // role == Jihadist
        playExtraCellsEvent(AlBaghdadi)
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
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(353, "Bowling Green Massacre", Unassociated, 3,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot, CannotNotRemoveLastCell,
      (role: Role, _: Boolean) => role == game.humanRole ||
                                  bowlingGreenBotMarkers(role).nonEmpty ||
                                  bowlingGreenBotLapsing(role).nonEmpty
      ,
      (role: Role) => if (role == game.humanRole) {
        val markers: List[String] = game.markers ::: (game.countries flatMap (_.markers)).sorted.distinct
        val lapsing: List[Int] = game.cardsLapsing
        if (markers.nonEmpty || lapsing.nonEmpty) {
          val choices = List(
            choice(markers.nonEmpty, "marker",  "Remove an event marker"),
            choice(lapsing.nonEmpty, "lapsing", "Remove a lapsing card")
          ).flatten
          
          askMenu("\nChoose one:", choices).head match {
            case "marker" =>
              val marker = if (markers.size == 1)
                markers.head
              else
                askMenu("\nRemove which event marker:", markers.map(m => m -> m)).head
              if (GlobalMarkers contains marker)
                removeGlobalEventMarker(marker)
              else {
                // The Advisors marker can exist more than once
                val target = if (marker == Advisors) {
                  val candidates = countryNames(game.countries filter (_.hasMarker(marker)))
                  if (candidates.size == 1)
                    candidates.head
                  else
                    askCountry(s"""Remove "$marker" from which country: """, candidates)
                }
                else
                  (game.countries find (_.hasMarker(marker)) map (_.name)).get 

                addEventTarget(target)
                removeEventMarkersFromCountry(target, marker)
              }
                
            case _ =>
              val cardNum = if (lapsing.size == 1)
                lapsing.head
              else
                askMenu[Int]("\nRemove which lapsing card: ", lapsing map (n => n -> deck(n).name)).head
              removeLapsingCards(cardNum::Nil)
          }
        }
        log(s"\nDraw a card and add it to your ($role) hand")
        askCardsDrawn(1)
      }
      else { // Bot
        val markers = bowlingGreenBotMarkers(role)
        val lapsing = bowlingGreenBotLapsing(role)
        if (markers.nonEmpty) {
          val marker = shuffle(markers).head
          if (GlobalMarkers contains marker)
            removeGlobalEventMarker(marker)
          else {
            // The Advisors marker can exist more than once if so the JihadistBot
            // must choose the country.
            val target = if (marker == Advisors) {
              val candidates = countryNames(game.countries filter (_.hasMarker(marker)))
              JihadistBot.troopsMilitiaTarget(candidates).get
            }
            else
              (game.countries find (_.hasMarker(marker)) map (_.name)).get 
            
            addEventTarget(target)
            removeEventMarkersFromCountry(target, marker)
          }
        }
        else {
          val cardNum = shuffle(lapsing).head
          removeLapsingCards(cardNum::Nil)
        }
        
        log(s"\nPut the top card of the draw deck on top of the $role hand of cards")
        askCardsDrawn(1)
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(354, "Election Meddling", Unassociated, 3,
      NoRemove, NoLapsing, AutoTrigger, DoesNotAlertPlot, CannotNotRemoveLastCell,
      NeverPlayable,  // Not directly playable, but will always auto trigger
      (role: Role) => {
        val drm = game.getNonMuslim(Russia).posture match {
          case PostureUntested => None
          case Hard            => Some((1, "Russia Hard"))
          case Soft            => Some((-1, "Russia Soft"))
        }
        rollUSPosture(drm.toList)
        logWorldPosture()
        if (game.gwotPenalty == 0)
          increasePrestige(1)
        else
          decreasePrestige(1)
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(355, "Fake News", Unassociated, 3,
      NoRemove, Lapsing, NoAutoTrigger, DoesNotAlertPlot, CannotNotRemoveLastCell,
      (role: Role, _: Boolean) => trumpTweetsON
      ,
      (role: Role) => {
        log()
        log("The next non-Automatic event card played by either player")
        log("will be cancelled.  These cards may be played for OPs.")
        log()
        setTrumpTweetsOFF()
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(356, "OPEC Production Cut", Unassociated, 3,
      NoRemove, Lapsing, NoAutoTrigger, DoesNotAlertPlot, CannotNotRemoveLastCell,
      (role: Role, _: Boolean) => role == game.humanRole  // Bot treats as unplayable
      ,
      (role: Role) => {
        val offLimits = Set(117, 118, 236, 357)
        val boxChoice = (num : Int, plot: Boolean) => {
          val box = if (plot) "First Plot box" else "Lapsing box"
          (num -> s"${deck(num).numAndName} from the $box") 
        }
        val lapsingChoices = game.cardsLapsing map (boxChoice(_, false))
        val plotChoices    = game.firstPlotCard.toList map (boxChoice(_, true))
        val boxChoices = lapsingChoices ::: plotChoices filterNot { case (num, _) => offLimits(num) }
      
        log("""You cannot choose "OPEC Production Cut" or "Oil Price Spike"""")
        if (boxChoices.isEmpty) {
          log("\nSelect a card from the discard pile and add it to your hand.")
          askCardsDrawn(1)
        }
        else {
          val choices = boxChoices :+ (-1, "A card from the discard pile")
          askMenu("\nAdd which card to your hand:", choices).head match {
            case -1 =>
              askCardsDrawn(1)
            case num if game.firstPlotCard == Some(num) =>
              game = game.copy(firstPlotCard = None)
              if (num == AvengerCard)
                avengerCardDrawn(discarded = false)
            case num =>
              val lapsing = game.cardsLapsing filterNot (_ == num)
              game = game.copy(cardsLapsing = lapsing)
              if (num == AvengerCard)
                avengerCardDrawn(discarded = false)
          }
        }      
        log("\nThe Resource value of each Oil Exporter is reduced by 1 for the rest of the turn")
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(357, "Peace Dividend", Unassociated, 3,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot, CannotNotRemoveLastCell,
      (role: Role, _: Boolean) => role == game.humanRole &&  // Bot treats as unplayable
                                  ((role == US &&       game.troopCommitment == LowIntensity) ||
                                   (role == Jihadist && game.totalCellsOnMap <= 10)
                                  ),
      (role: Role) => {
        val offLimits = Set(117, 118, 236, 356)
        val boxChoice = (num : Int, plot: Boolean) => {
          val box = if (plot) "First Plot box" else "Lapsing box"
          (num -> s"${deck(num).numAndName} from the $box") 
        }
        val lapsingChoices = game.cardsLapsing map (boxChoice(_, false))
        val plotChoices    = game.firstPlotCard.toList map (boxChoice(_, true))
        val boxChoices = lapsingChoices ::: plotChoices filterNot { case (num, _) => offLimits(num) }
        
        log("""You cannot choose "OPEC Production Cut" or "Oil Price Spike"""")
        if (boxChoices.isEmpty) {
          log("\nSelect a card from the discard pile and add it to your hand.")
          askCardsDrawn(1)
        }
        else {
          val choices = boxChoices :+ (-1, "A card from the discard pile")
          askMenu("\nAdd which card to your hand:", choices).head match {
            case -1 =>
              askCardsDrawn(1)
            case num if game.firstPlotCard == Some(num) =>
              game = game.copy(firstPlotCard = None)
              if (num == AvengerCard)
                avengerCardDrawn(discarded = false)
            case num =>
              val lapsing = game.cardsLapsing filterNot (_ == num)
              game = game.copy(cardsLapsing = lapsing)
              if (num == AvengerCard)
                avengerCardDrawn(discarded = false)
          }
        }
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(358, "Political Islamism/Pan Arab Nationalism", Unassociated, 3,
      Remove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      () => {
        // Can we remove the last cell on the board?
        val sunniCandidates = game.muslims filter (m => m.isSunni && m.totalCells > 0)
        val shiaCandidates  = game.muslims filter (m => m.isShiaMix && m.totalCells > 0)
        val numSunni = (sunniCandidates map (_.totalCells)).sum
        val numShia  = (shiaCandidates map (_.totalCells)).sum
        numSunni == 1 && numShia == 1 && game.totalCellsOnMap == 2
      }
      ,      
      AlwaysPlayable,
      (role: Role) => if (role == US) {
        val sunniCandidates = countryNames(game.muslims filter (m => m.isSunni && m.totalCells > 0))
        val shiaCandidates = countryNames(game.muslims filter (m => m.isShiaMix && m.totalCells > 0))
        if (sunniCandidates.nonEmpty) {
          val (target, (actives, sleepers, sadr)) = if (role == game.humanRole) {
            val t = askCountry("Remove cell from which Sunni country: ", sunniCandidates)
            (t, askCells(t, 1, true))
          }
          else {
            val t = USBot.disruptPriority(sunniCandidates).get
            (t, USBot.chooseCellsToRemove(t, 1))
            
          }
          
          addEventTarget(target)
          removeCellsFromCountry(target, actives, sleepers, sadr, addCadre = true)
        }
        
        if (shiaCandidates.nonEmpty) {
          val (target, (actives, sleepers, sadr)) = if (role == game.humanRole) {
            val t = askCountry("Remove cell from which Shia-Mix country: ", shiaCandidates)
            (t, askCells(t, 1, true))
          }
          else {
            val t = USBot.disruptPriority(shiaCandidates).get
            (t, USBot.chooseCellsToRemove(t, 1))
            
          }
          
          addEventTarget(target)
          removeCellsFromCountry(target, actives, sleepers, sadr, addCadre = true)
        }
        
        println()
        decreaseFunding(1)
        addGlobalEventMarker(PoliticalIslamismUS)
      }
      else { // Jihadist
        val sunniCandidates = countryNames(game.muslims filter (_.isSunni))
        val shiaCandidates = countryNames(game.muslims filter (_.isShiaMix))
        if (sunniCandidates.nonEmpty && game.cellsAvailable > 0) {
          val target = if (role == game.humanRole)
            askCountry("Place a cell in which Sunni country: ", sunniCandidates)
          else
            JihadistBot.recruitTravelToPriority(sunniCandidates).get
            
          addEventTarget(target)
          addSleeperCellsToCountry(target, 1)
        }
        
        if (shiaCandidates.nonEmpty && game.cellsAvailable > 0) {
          val target = if (role == game.humanRole)
            askCountry("Place a cell in which Shia-Mix country: ", shiaCandidates)
          else
            JihadistBot.recruitTravelToPriority(shiaCandidates).get
            
          addEventTarget(target)
          addSleeperCellsToCountry(target, 1)
        }
        
        println()
        increaseFunding(1)
        addGlobalEventMarker(PoliticalIslamismJihadist)
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(359, "Quick Win/Bad Intel", Unassociated, 3,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot, CannotNotRemoveLastCell,
      (role: Role, _: Boolean) => quickWinBadIntelCandidates(role).nonEmpty
      ,
      (role: Role) => {
        val target = if (role == game.humanRole)
          askCountry("Which country: ", quickWinBadIntelCandidates(role))
        else if (role == US)
          USBot.markerAlignGovTarget(quickWinBadIntelCandidates(role)).get
        else
          JihadistBot.markerAlignGovTarget(quickWinBadIntelCandidates(role)).get
        
        addEventTarget(target)
        testCountry(target)
        if (role == US) {
          addAidMarker(target)
          addAwakeningMarker(target)
        }
        else {
          addBesiegedRegimeMarker(target)
          addReactionMarker(target)
        }
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(360, "US China Trade War", Unassociated, 3,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot, CannotNotRemoveLastCell,
      (role: Role, _: Boolean) => game.usPosture != game.getNonMuslim(China).posture && trumpTweetsON
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