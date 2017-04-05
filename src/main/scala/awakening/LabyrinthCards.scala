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
    
  def specialForcesCandidates: List[String] = {
    // First find all muslim countries with troops or "Advisors"
    val withTroops = countryNames(game.countries filter (c => c.totalTroops > 0))
    // Next get all countries that contain any cells and filter out the ones are are not 
    // within two of a country with forces.
    countryNames(game.countries filter { country =>
      country.totalCells > 0 && (withTroops exists (forces => distance(forces, country.name) <= 1))
    })
  }

  // Convenience method for adding a card to the deck.
  private def entry(card: Card) = (card.number -> card)
  
  val deck: Map[Int, Card] = Map(
    // ------------------------------------------------------------------------
    entry(new Card(1, "Backlash", US, 1,
      NoRemove, NoMarker, NoLapsing,  NoAutoTrigger, DoesNotAlertPlot,
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
      NoRemove, NoMarker, Lapsing,  NoAutoTrigger, DoesNotAlertPlot, AlwaysPlayable,
      (role: Role) => {
        log("For the rest of the turn travel to adjacent Good countries must roll to succeed")
        log("and no non-adjacent travel allowed")
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(3, "CTR", US, 1,
      NoRemove, CountryMarker, NoLapsing,  NoAutoTrigger, DoesNotAlertPlot,
      (role: Role) => game.usPosture == Soft && {
        val russia = game getNonMuslim Russia
        val cAsia  = game getMuslim CentralAsia
        !russia.hasMarker("CTR") || (!cAsia.hasMarker("CTR") && !cAsia.isAdversary)
      }
      ,
      (role: Role) => {
        val cAsia  = game getMuslim CentralAsia
        addEventTarget(Russia)
        addEventMarkersToCountry(Russia, "CTR")
        if (!cAsia.isAdversary) {
          addEventTarget(CentralAsia)
          addEventMarkersToCountry(CentralAsia, "CTR")
        }
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(4, "Moro Talks", US, 1,
      Remove, CountryMarker, NoLapsing,  NoAutoTrigger, DoesNotAlertPlot, AlwaysPlayable,
      (role: Role) => {
        addEventTarget(Philippines)
        testCountry(Philippines)
        decreaseFunding(1)
        removeEventMarkersFromCountry(Philippines, "Abu Sayyaf")
        addEventMarkersToCountry(Philippines, "Moro Talks")
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(5, "NEST", US, 1,
      Remove, CountryMarker, NoLapsing,  NoAutoTrigger, DoesNotAlertPlot,
      (role: Role) => role == game.humanRole
      ,
      (role: Role) => {
        addEventMarkersToCountry(UnitedStates, "NEST")
        log("Add plots in the US are now placed face up")
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(6, "Sanctions", US, 1,
      NoRemove, NoMarker, NoLapsing,  NoAutoTrigger, DoesNotAlertPlot,
      (role: Role) => (game getCountry UnitedStates).hasMarker("Patriot Act")
      ,
      (role: Role) => {
        decreaseFunding(2)
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(7, "Sanctions", US, 1,
      NoRemove, NoMarker, NoLapsing,  NoAutoTrigger, DoesNotAlertPlot,
      (role: Role) => deck(6).eventConditions(role),
      (role: Role) => deck(6).executeEvent(role)
    )),
    // ------------------------------------------------------------------------
    entry(new Card(8, "Special Forces", US, 1,
      NoRemove, NoMarker, NoLapsing,  NoAutoTrigger, DoesNotAlertPlot,
      (role: Role) => specialForcesCandidates.nonEmpty
      ,
      (role: Role) => {
        val (target, (actives, sleepers)) = if (role == game.humanRole) {
          val target = askCountry("Remove cell in which country: ", specialForcesCandidates)
          (target, askCells(target, 1))
        }
        else {
          val target = USBot.disruptPriority(specialForcesCandidates).get
          (target, USBot.chooseCellsToRemove(target, 1))
        }
        println()
        addEventTarget(target)
        removeCellsFromCountry(target, actives, sleepers, addCadre = true)
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(9, "Special Forces", US, 1,
      NoRemove, NoMarker, NoLapsing,  NoAutoTrigger, DoesNotAlertPlot,
      (role: Role) => deck(8).eventConditions(role),
      (role: Role) => deck(8).executeEvent(role)
    )),
    // ------------------------------------------------------------------------
    entry(new Card(10, "Special Forces", US, 1,
      NoRemove, NoMarker, NoLapsing,  NoAutoTrigger, DoesNotAlertPlot,
      (role: Role) => deck(8).eventConditions(role),
      (role: Role) => deck(8).executeEvent(role)
    )),
    // ------------------------------------------------------------------------
    entry(new Card(11, "Abbas", US, 2,
      Remove, GlobalMarker, NoLapsing,  NoAutoTrigger, DoesNotAlertPlot, AlwaysPlayable,
      (role: Role) => {
        addGlobalEventMarker("Abbas")
        if (game.troopCommitment != Overstretch && !game.adjacentToIslamistRule(Israel)) {
          increasePrestige(1)
          decreaseFunding(2)
        }
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(12, "Al-Azhar", US, 2,
      NoRemove, NoMarker, NoLapsing,  NoAutoTrigger, DoesNotAlertPlot,
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
      NoRemove, GlobalMarker, NoLapsing,  NoAutoTrigger, DoesNotAlertPlot, AlwaysPlayable,
      (role: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(14, "Covert Action", US, 2,
      NoRemove, NoMarker, NoLapsing,  NoAutoTrigger, DoesNotAlertPlot, AlwaysPlayable,
      (role: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(15, "Ethiopia Strikes", US, 2,
      Remove, NoMarker, NoLapsing,  NoAutoTrigger, DoesNotAlertPlot, AlwaysPlayable,
      (role: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(16, "Euro-Islam", US, 2,
      Remove, NoMarker, NoLapsing,  NoAutoTrigger, DoesNotAlertPlot, AlwaysPlayable,
      (role: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(17, "FSB", US, 2,
      NoRemove, NoMarker, NoLapsing,  NoAutoTrigger, DoesNotAlertPlot, AlwaysPlayable,
      (role: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(18, "Intel Community", US, 2,
      NoRemove, NoMarker, NoLapsing,  NoAutoTrigger, DoesNotAlertPlot, AlwaysPlayable,
      (role: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(19, "Kemalist Republic", US, 2,
      NoRemove, NoMarker, NoLapsing,  NoAutoTrigger, DoesNotAlertPlot, AlwaysPlayable,
      (role: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(20, "King Abdullah", US, 2,
      Remove, NoMarker, NoLapsing,  NoAutoTrigger, DoesNotAlertPlot, AlwaysPlayable,
      (role: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(21, "“Let’s Roll!”", US, 2,
      NoRemove, NoMarker, NoLapsing,  NoAutoTrigger, DoesNotAlertPlot, AlwaysPlayable,
      (role: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(22, "Mossad & Shin Bet", US, 2,
      NoRemove, NoMarker, NoLapsing,  NoAutoTrigger, DoesNotAlertPlot, AlwaysPlayable,
      (role: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(23, "Predator", US, 2,
      NoRemove, NoMarker, NoLapsing,  NoAutoTrigger, DoesNotAlertPlot, AlwaysPlayable,
      (role: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(24, "Predator", US, 2,
      NoRemove, NoMarker, NoLapsing,  NoAutoTrigger, DoesNotAlertPlot, AlwaysPlayable,
      (role: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(25, "Predator", US, 2,
      NoRemove, NoMarker, NoLapsing,  NoAutoTrigger, DoesNotAlertPlot, AlwaysPlayable,
      (role: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(26, "Quartet", US, 2,
      NoRemove, NoMarker, NoLapsing,  NoAutoTrigger, DoesNotAlertPlot,
      (role: Role) => globalEventInPlay("Abbas")
      ,
      (role: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(27, "Saddam Captured", US, 2,
      Remove, GlobalMarker, NoLapsing,  NoAutoTrigger, DoesNotAlertPlot, AlwaysPlayable,
      (role: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(28, "Sharia", US, 2,
      NoRemove, NoMarker, NoLapsing,  NoAutoTrigger, DoesNotAlertPlot, AlwaysPlayable,
      (role: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(29, "Tony Blair", US, 2,
      Remove, NoMarker, NoLapsing,  NoAutoTrigger, DoesNotAlertPlot, AlwaysPlayable,
      (role: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(30, "UN Nation Building", US, 2,
      NoRemove, NoMarker, NoLapsing,  NoAutoTrigger, DoesNotAlertPlot, AlwaysPlayable,
      (role: Role) => () // Not in caliphate member, see Awakening card
    )),
    // ------------------------------------------------------------------------
    entry(new Card(31, "Wiretapping", US, 2,
      NoRemove, GlobalMarker, NoLapsing,  NoAutoTrigger, DoesNotAlertPlot, AlwaysPlayable,
      (role: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(32, "Back Channel", US, 3,
      NoRemove, NoMarker, NoLapsing,  NoAutoTrigger, DoesNotAlertPlot, AlwaysPlayable,
      (role: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(33, "Benazir Bhutto", US, 3,
      Remove, GlobalMarker, NoLapsing,  NoAutoTrigger, DoesNotAlertPlot, AlwaysPlayable,
      (role: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(34, "Enhanced Measures", US, 3,
      NoRemove, GlobalMarker, NoLapsing,  NoAutoTrigger, DoesNotAlertPlot, AlwaysPlayable,
      (role: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(35, "Hijab", US, 3,
      Remove, NoMarker, NoLapsing,  NoAutoTrigger, DoesNotAlertPlot, AlwaysPlayable,
      (role: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(36, "Indo-Pakistani Talks.", US, 3,
      Remove, GlobalMarker, NoLapsing,  NoAutoTrigger, DoesNotAlertPlot, AlwaysPlayable,
      (role: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(37, "Iraqi WMD", US, 3,
      Remove, GlobalMarker, NoLapsing,  NoAutoTrigger, DoesNotAlertPlot, AlwaysPlayable,
      (role: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(38, "Libyan Deal", US, 3,
      Remove, GlobalMarker, NoLapsing,  NoAutoTrigger, DoesNotAlertPlot, AlwaysPlayable,
      (role: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(39, "Libyan WMD", US, 3,
      Remove, GlobalMarker, NoLapsing,  NoAutoTrigger, DoesNotAlertPlot, AlwaysPlayable,
      (role: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(40, "Mass Turnout", US, 3,
      NoRemove, NoMarker, NoLapsing,  NoAutoTrigger, DoesNotAlertPlot, AlwaysPlayable,
      (role: Role) => () // Not in caliphate member see Awakening cards
    )),
    // ------------------------------------------------------------------------
    entry(new Card(41, "NATO", US, 3,
      NoRemove, CountryMarker, NoLapsing,  NoAutoTrigger, DoesNotAlertPlot, AlwaysPlayable,
      (role: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(42, "Pakistani Offensive", US, 3,
      NoRemove, NoMarker, NoLapsing,  NoAutoTrigger, DoesNotAlertPlot, AlwaysPlayable,
      (role: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(43, "Patriot Act", US, 3,
      Remove, CountryMarker, NoLapsing,  NoAutoTrigger, DoesNotAlertPlot, AlwaysPlayable,
      (role: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(44, "Renditions", US, 3,
      NoRemove, GlobalMarker, NoLapsing,  NoAutoTrigger, DoesNotAlertPlot, AlwaysPlayable,
      (role: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(45, "Safer Now", US, 3,
      NoRemove, NoMarker, NoLapsing,  NoAutoTrigger, DoesNotAlertPlot, AlwaysPlayable,
      (role: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(46, "Sistani", US, 3,
      NoRemove, NoMarker, NoLapsing,  NoAutoTrigger, DoesNotAlertPlot, AlwaysPlayable,
      (role: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(47, "“The door of Ijtihad was closed”", US, 3,
      NoRemove, NoMarker, Lapsing,  NoAutoTrigger, DoesNotAlertPlot, AlwaysPlayable,
      (role: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(48, "Adam Gadahn", Jihadist, 1,
      NoRemove, NoMarker, NoLapsing,  NoAutoTrigger, DoesNotAlertPlot, AlwaysPlayable,
      (role: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(49, "Al-Ittihad al-Islami", Jihadist, 1,
      Remove, NoMarker, NoLapsing,  NoAutoTrigger, DoesNotAlertPlot, AlwaysPlayable,
      (role: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(50, "Ansar al-Islam", Jihadist, 1,
      Remove, NoMarker, NoLapsing,  NoAutoTrigger, DoesNotAlertPlot, AlwaysPlayable,
      (role: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(51, "FREs", Jihadist, 1,
      NoRemove, NoMarker, NoLapsing,  NoAutoTrigger, DoesNotAlertPlot, AlwaysPlayable,
      // Can create Caliphate (only if Saddam not captured)
      (role: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(52, "IEDs", Jihadist, 1,
      NoRemove, NoMarker, NoLapsing,  NoAutoTrigger, DoesNotAlertPlot, AlwaysPlayable,
      (role: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(53, "Madrassas", Jihadist, 1,
      NoRemove, NoMarker, NoLapsing,  NoAutoTrigger, DoesNotAlertPlot, AlwaysPlayable,
      (role: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(54, "Moqtada al-Sadr", Jihadist, 1,
      Remove, GlobalMarker, NoLapsing,  NoAutoTrigger, DoesNotAlertPlot, AlwaysPlayable,
      (role: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(55, "Uyghur Jihad", Jihadist, 1,
      Remove, NoMarker, NoLapsing,  NoAutoTrigger, DoesNotAlertPlot, AlwaysPlayable,
      (role: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(56, "Vieira de Mello Slain", Jihadist, 1,
      Remove, GlobalMarker, NoLapsing,  NoAutoTrigger, DoesNotAlertPlot, AlwaysPlayable,
      (role: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(57, "Abu Sayyaf", Jihadist, 2,
      Remove, CountryMarker, NoLapsing,  NoAutoTrigger, DoesNotAlertPlot, AlwaysPlayable,
      (role: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(58, "Al-Anbar", Jihadist, 2,
      Remove, GlobalMarker, NoLapsing,  NoAutoTrigger, DoesNotAlertPlot, AlwaysPlayable,
      (role: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(59, "Amerithrax", Jihadist, 2,
      NoRemove, NoMarker, NoLapsing,  NoAutoTrigger, DoesNotAlertPlot, AlwaysPlayable,
      (role: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(60, "Bhutto Shot", Jihadist, 2,
      Remove, GlobalMarker, NoLapsing,  NoAutoTrigger, DoesNotAlertPlot, AlwaysPlayable,
      (role: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(61, "Detainee Release", Jihadist, 2,
      NoRemove, NoMarker, NoLapsing,  NoAutoTrigger, DoesNotAlertPlot, AlwaysPlayable,
      (role: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(62, "Ex-KGB", Jihadist, 2,
      NoRemove, NoMarker, NoLapsing,  NoAutoTrigger, DoesNotAlertPlot, AlwaysPlayable,
      (role: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(63, "Gaza War", Jihadist, 2,
      NoRemove, NoMarker, NoLapsing,  NoAutoTrigger, DoesNotAlertPlot, AlwaysPlayable,
      (role: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(64, "Hariri Killed", Jihadist, 2,
      Remove, NoMarker, NoLapsing,  NoAutoTrigger, DoesNotAlertPlot, AlwaysPlayable,
      (role: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(65, "HEU", Jihadist, 2,
      Remove, NoMarker, NoLapsing,  NoAutoTrigger, DoesNotAlertPlot, AlwaysPlayable,
      (role: Role) => {
        // Blocked if CTR marker present in country
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(66, "Homegrown", Jihadist, 2,
      NoRemove, NoMarker, NoLapsing,  NoAutoTrigger, DoesNotAlertPlot, AlwaysPlayable,
      (role: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(67, "Islamic Jihad Union", Jihadist, 2,
      Remove, NoMarker, NoLapsing,  NoAutoTrigger, DoesNotAlertPlot, AlwaysPlayable,
      (role: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(68, "Jemaah Islamiya", Jihadist, 2,
      NoRemove, NoMarker, NoLapsing,  NoAutoTrigger, DoesNotAlertPlot, AlwaysPlayable,
      (role: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(69, "Kazakh Strain", Jihadist, 2,
      Remove, NoMarker, NoLapsing,  NoAutoTrigger, DoesNotAlertPlot, AlwaysPlayable,
      (role: Role) => {
        // Blocked if CTR marker present in country
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(70, "Lashkar-e-Tayyiba", Jihadist, 2,
      NoRemove, NoMarker, NoLapsing,  NoAutoTrigger, DoesNotAlertPlot, AlwaysPlayable,
      (role: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(71, "Loose Nuke", Jihadist, 2,
      Remove, NoMarker, NoLapsing,  NoAutoTrigger, DoesNotAlertPlot, AlwaysPlayable,
      (role: Role) => {
        // Blocked if CTR marker present in country
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(72, "Opium", Jihadist, 2,
      NoRemove, NoMarker, NoLapsing,  NoAutoTrigger, DoesNotAlertPlot, AlwaysPlayable,
      // Can create Caliphate (only in Afghanistan)
      (role: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(73, "Pirates", Jihadist, 2,
      Remove, GlobalMarker, NoLapsing,  NoAutoTrigger, DoesNotAlertPlot, AlwaysPlayable,
      (role: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(74, "Schengen Visas", Jihadist, 2,
      NoRemove, NoMarker, NoLapsing,  NoAutoTrigger, DoesNotAlertPlot, AlwaysPlayable,
      (role: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(75, "Schroeder & Chirac", Jihadist, 2,
      Remove, NoMarker, NoLapsing,  NoAutoTrigger, DoesNotAlertPlot, AlwaysPlayable,
      (role: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(76, "Abu Ghurayb", Jihadist, 3,
      Remove, NoMarker, NoLapsing,  NoAutoTrigger, DoesNotAlertPlot, AlwaysPlayable,
      (role: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(77, "Al-Jazeera", Jihadist, 3,
      NoRemove, NoMarker, NoLapsing,  NoAutoTrigger, DoesNotAlertPlot, AlwaysPlayable,
      (role: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(78, "Axis of Evil", Jihadist, 3,
      NoRemove, NoMarker, NoLapsing,  NoAutoTrigger, DoesNotAlertPlot, AlwaysPlayable,
      (role: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(79, "Clean Operatives", Jihadist, 3,
      NoRemove, NoMarker, NoLapsing,  NoAutoTrigger, DoesNotAlertPlot, AlwaysPlayable,
      (role: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(80, "FATA", Jihadist, 3,
      NoRemove, GlobalMarker, NoLapsing,  NoAutoTrigger, DoesNotAlertPlot, AlwaysPlayable,
      (role: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(81, "Foreign Fighters", Jihadist, 3,
      NoRemove, NoMarker, NoLapsing,  NoAutoTrigger, DoesNotAlertPlot, AlwaysPlayable,
      // Can create Caliphate
      (role: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(82, "Jihadist Videos", Jihadist, 3,
      NoRemove, NoMarker, NoLapsing,  NoAutoTrigger, DoesNotAlertPlot, AlwaysPlayable,
      (role: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(83, "Kashmir", Jihadist, 3,
      NoRemove, NoMarker, NoLapsing,  NoAutoTrigger, DoesNotAlertPlot, AlwaysPlayable,
      (role: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(84, "Leak", Jihadist, 3,
      NoRemove, NoMarker, NoLapsing,  NoAutoTrigger, DoesNotAlertPlot, AlwaysPlayable,
      (role: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(85, "Leak", Jihadist, 3,
      NoRemove, NoMarker, NoLapsing,  NoAutoTrigger, DoesNotAlertPlot, AlwaysPlayable,
      (role: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(86, "Lebanon War", Jihadist, 3,
      NoRemove, NoMarker, NoLapsing,  NoAutoTrigger, DoesNotAlertPlot, AlwaysPlayable,
      (role: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(87, "Martyrdom Operation", Jihadist, 3,
      NoRemove, NoMarker, NoLapsing,  NoAutoTrigger, DoesNotAlertPlot, AlwaysPlayable,
      (role: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(88, "Martyrdom Operation", Jihadist, 3,
      NoRemove, NoMarker, NoLapsing,  NoAutoTrigger, DoesNotAlertPlot, AlwaysPlayable,
      (role: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(89, "Martyrdom Operation", Jihadist, 3,
      NoRemove, NoMarker, NoLapsing,  NoAutoTrigger, DoesNotAlertPlot, AlwaysPlayable,
      (role: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(90, "Quagmire", Jihadist, 3,
      NoRemove, NoMarker, NoLapsing,  NoAutoTrigger, DoesNotAlertPlot, AlwaysPlayable,
      (role: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(91, "Regional al-Qaeda", Jihadist, 3,
      NoRemove, NoMarker, NoLapsing,  NoAutoTrigger, DoesNotAlertPlot, AlwaysPlayable,
      (role: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(92, "Saddam", Jihadist, 3,
      NoRemove, NoMarker, NoLapsing,  NoAutoTrigger, DoesNotAlertPlot, AlwaysPlayable,
      (role: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(93, "Taliban", Jihadist, 3,
      NoRemove, NoMarker, NoLapsing,  NoAutoTrigger, DoesNotAlertPlot, AlwaysPlayable,
      (role: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(94, "The door of Ijtihad was closed", Jihadist, 3,
      NoRemove, NoMarker, NoLapsing,  NoAutoTrigger, DoesNotAlertPlot, AlwaysPlayable,
      (role: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(95, "Wahhabism", Jihadist, 3,
      NoRemove, NoMarker, NoLapsing,  NoAutoTrigger, DoesNotAlertPlot, AlwaysPlayable,
      (role: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(96, "Danish Cartoons", Unassociated, 1,
      Remove, NoMarker, NoLapsing,  NoAutoTrigger, DoesNotAlertPlot, AlwaysPlayable,
      (role: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(97, "Fatwa", Unassociated, 1,
      NoRemove, NoMarker, NoLapsing,  NoAutoTrigger, DoesNotAlertPlot, AlwaysPlayable,
      (role: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(98, "Gaza Withdrawal", Unassociated, 1,
      Remove, NoMarker, NoLapsing,  NoAutoTrigger, DoesNotAlertPlot, AlwaysPlayable,
      (role: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(99, "HAMAS Elected", Unassociated, 1,
      Remove, NoMarker, NoLapsing,  NoAutoTrigger, DoesNotAlertPlot, AlwaysPlayable,
      (role: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(100, "Hizb Ut-Tahrir", Unassociated, 1,
      NoRemove, NoMarker, NoLapsing,  NoAutoTrigger, DoesNotAlertPlot, AlwaysPlayable,
      (role: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(101, "Kosovo", Unassociated, 1,
      NoRemove, NoMarker, NoLapsing,  NoAutoTrigger, DoesNotAlertPlot, AlwaysPlayable,
      (role: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(102, "Former Soviet Union", Unassociated, 2,
      NoRemove, NoMarker, NoLapsing,  NoAutoTrigger, DoesNotAlertPlot, AlwaysPlayable,
      (role: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(103, "Hizballah", Unassociated, 2,
      NoRemove, NoMarker, NoLapsing,  NoAutoTrigger, DoesNotAlertPlot, AlwaysPlayable,
      (role: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(104, "Iran", Unassociated, 2,
      NoRemove, NoMarker, NoLapsing,  NoAutoTrigger, DoesNotAlertPlot, AlwaysPlayable,
      (role: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(105, "Iran", Unassociated, 2,
      NoRemove, NoMarker, NoLapsing,  NoAutoTrigger, DoesNotAlertPlot, AlwaysPlayable,
      (role: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(106, "Jaysh al-Mahdi", Unassociated, 2,
      NoRemove, NoMarker, NoLapsing,  NoAutoTrigger, DoesNotAlertPlot, AlwaysPlayable,
      (role: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(107, "Kurdistan", Unassociated, 2,
      NoRemove, NoMarker, NoLapsing,  NoAutoTrigger, DoesNotAlertPlot, AlwaysPlayable,
      (role: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(108, "Musharraf", Unassociated, 2,
      NoRemove, NoMarker, NoLapsing,  NoAutoTrigger, DoesNotAlertPlot, AlwaysPlayable,
      (role: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(109, "Tora Bora", Unassociated, 2,
      Remove, NoMarker, NoLapsing,  NoAutoTrigger, DoesNotAlertPlot, AlwaysPlayable,
      (role: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(110, "Zarqawi", Unassociated, 2,
      Remove, NoMarker, NoLapsing,  NoAutoTrigger, DoesNotAlertPlot, AlwaysPlayable,
      // Can create Caliphate (onlyinIraq,Syria,Lebanon,orJordan)
      (role: Role) => () // Remove is conditional
    )),
    // ------------------------------------------------------------------------
    entry(new Card(111, "Zawahiri", Unassociated, 2,
      Remove, NoMarker, NoLapsing,  NoAutoTrigger, DoesNotAlertPlot, AlwaysPlayable,
      (role: Role) => () // Remove is conditional
    )),
    // ------------------------------------------------------------------------
    entry(new Card(112, "Bin Ladin", Unassociated, 3,
      Remove, NoMarker, NoLapsing,  NoAutoTrigger, DoesNotAlertPlot, AlwaysPlayable,
      (role: Role) => () // Remove is conditional
    )),
    // ------------------------------------------------------------------------
    entry(new Card(113, "Darfur", Unassociated, 3,
      NoRemove, NoMarker, NoLapsing,  NoAutoTrigger, DoesNotAlertPlot, AlwaysPlayable,
      (role: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(114, "GTMO", Unassociated, 3,
      NoRemove, NoMarker, Lapsing,  NoAutoTrigger, DoesNotAlertPlot, AlwaysPlayable,
      (role: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(115, "Hambali", Unassociated, 3,
      Remove, NoMarker, NoLapsing,  NoAutoTrigger, DoesNotAlertPlot, AlwaysPlayable,
      (role: Role) => () // Remove is conditional
    )),
    // ------------------------------------------------------------------------
    entry(new Card(116, "KSM", Unassociated, 3,
      Remove, NoMarker, NoLapsing,  NoAutoTrigger, DoesNotAlertPlot, AlwaysPlayable,
      (role: Role) => () // Remove is conditional
    )),
    // ------------------------------------------------------------------------
    entry(new Card(117, "Oil Price Spike", Unassociated, 3,
      NoRemove, NoMarker, Lapsing,  NoAutoTrigger, DoesNotAlertPlot, AlwaysPlayable,
      (role: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(118, "Oil Price Spike", Unassociated, 3,
      NoRemove, NoMarker, Lapsing,  NoAutoTrigger, DoesNotAlertPlot, AlwaysPlayable,
      (role: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(119, "Saleh", Unassociated, 3,
      NoRemove, NoMarker, NoLapsing,  NoAutoTrigger, DoesNotAlertPlot, AlwaysPlayable,
      (role: Role) => ()
    )),
    // ------------------------------------------------------------------------
    entry(new Card(120, "US Election", Unassociated, 3,
      NoRemove, NoMarker, NoLapsing, AutoTrigger, DoesNotAlertPlot,
      (role: Role) => false  // No directly playable, but will always auto trigger
      ,
      (role: Role) => {
        if (lapsingEventInPlay("US Consulate Attacked")) {
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

