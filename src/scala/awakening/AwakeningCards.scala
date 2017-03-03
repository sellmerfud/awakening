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

object AwakeningCards extends CardDeck {
  
  def notBlockedBy(marker: String): Boolean = !(game markerInPlay marker)
  def eventInPlay(marker: String): Boolean = (game markerInPlay marker)
  // Various tests used by the card events
  val canTakeAdvisors = (m: MuslimCountry) => !m.isAdversary && m.civilWar && m.troops == 0 && !m.hasMarker("Advisors")
  val canTakeHumanitarianAid = (m: MuslimCountry) => !m.isGood && !m.isIslamistRule && m.totalCells > 0
  val canTakePeshmerga = (m: MuslimCountry) => (m.name == Iraq || m.name == Syria) && m.totalCells > 0
  
  val cardMap: Map[Int, Card] = Map(
    entry(new Card(121, "Advisors", US, 1,
      NoRemove, Mark, NoLapsing,
      (_: Role) => (game.muslims count (_.hasMarker("Advisors"))) < 3 && (game hasMuslim canTakeAdvisors)
      ,
      (_: Role) => {
        val candidates = game.muslims filter canTakeAdvisors
        val target = if (game.humanRole == US)
          askCountry(s"Advisors in which country: ", countryNames(candidates))
        else {
          // TODO: Bot implementation
          candidates.head.name
        }
        testCountry(target)
        game = game.updateCountry(game.getMuslim(target).addMarker("Advisors"))
        log(s"Place Advisors marker in $target")
        
      }
    )),
    entry(new Card(122, "Backlash", US, 1,
      NoRemove, NoMark, NoLapsing,
      (_: Role) => game hasCountry (_.plots exists (p => !p.backlashed))
      ,
      (_: Role) => {
        val candidates = game.countries filter (_.plots exists (p => !p.backlashed))
        val countryName = if (game.humanRole == US)
          askCountry(s"Backlash in which country: ", countryNames(candidates))
        else {
          // TODO: Bot implementation
          candidates.head.name
        }
        // Pick a random plot in the country
        val country = game.getCountry(countryName)
        val PlotOnMap(target, _) :: remaining = shuffle(country.plots)
        country match {
          case m: MuslimCountry    => game = game.updateCountry(m.copy(plots = PlotOnMap(target, true) :: remaining))
          case n: NonMuslimCountry => game = game.updateCountry(n.copy(plots = PlotOnMap(target, true) :: remaining))
        }
        log(s"Backlash applied to a plot in $countryName")
      }
    )),
    entry(new Card(123, "Humanitarian Aid", US, 1,
      NoRemove, NoMark, NoLapsing, 
      (_: Role) => game hasMuslim canTakeHumanitarianAid
      ,
      (_: Role) => {
        val candidates = game.muslims filter canTakeHumanitarianAid
        val countryName = if (game.humanRole == US)
          askCountry(s"Humanitarian Aid in which country: ", countryNames(candidates))
        else {
          // TODO: Bot implementation
          candidates.head.name
        }
        val m = game.getMuslim(countryName)
        game = game.updateCountry(m.copy(aidMarkers = m.aidMarkers + 1))
        log(s"Place an aid marker in $countryName")
      }
    )),
    entry(new Card(124, "Pearl Roundabout", US, 1,
      NoRemove, NoMark, NoLapsing,
      (_: Role) => notBlockedBy("Bloody Thursday")
      ,
      (_: Role) => {
        testCountry(GulfStates)
        val m = game.getMuslim(GulfStates)
        game = game.updateCountry(m.copy(awakening = m.awakening + 1))
        log(s"Place an awakening marker in $GulfStates")
      }
    )),
    entry(new Card(125, "Peshmerga", US, 1,
      NoRemove, NoMark, NoLapsing,
      (_: Role) => (game hasMuslim canTakePeshmerga) && game.militiaAvailable > 0
      ,
      (_: Role) => {
        val candidates = game.muslims filter canTakePeshmerga
        val target = if (candidates.size == 1)
          candidates.head.name
        else if (game.humanRole == US)
          askCountry(s"Target which which country: ", countryNames(candidates))
        else {
          // TODO: Bot implementation
          candidates.head.name
        }
        val numPlaced = 2 min game.militiaAvailable
        val m = game.getMuslim(target)
        game = game.updateCountry(m.copy(militia = m.militia + numPlaced))
        log(s"Place $numPlaced militia in $target")
      }
    )),
    entry(new Card(126, "Reaper", US, 1,
      NoRemove, NoMark, NoLapsing, NoConditions,
      (_: Role) => {
        if (game.humanRole == US) {
          val prompt = "Remove cells or make Jihidist discard? "
          val choices = Seq("remove cells", "jihadist discard")
          askOneOf(prompt, choices, allowAbort = true) match {
            case Some("jihadist discard") =>
              log("Discard the top card in the Jihadist hand")
            case _ =>
              val candidates = game.muslims filter (_.totalCells > 0)
              val target = askCountry("Remove 2 cells in which country? ", countryNames(candidates))
              val (actives, sleepers) = askCells(target, 2)
              val m = game.getMuslim(target)
              game = game.updateCountry(m.copy(activeCells  = m.activeCells - actives,
                                               sleeperCells = m.sleeperCells - sleepers))
              if (sleepers > 0) log(s"Remove ${amountOf(sleepers, "sleeper cell")} from $target")
              if (actives  > 0) log(s"Remove ${amountOf(actives, "active cell")} from $target")
          }
        }
        else {
          // TODO: Check Bot instructions…
        }
      }
    )),
    entry(new Card(127, "Reaper", US, 1,
      NoRemove, NoMark, NoLapsing, NoConditions,
      (r: Role) => cardMap(126).executeEvent(r)
    )),
    entry(new Card(128, "Reaper", US, 1,
      NoRemove, NoMark, NoLapsing, NoConditions, 
      (r: Role) => cardMap(126).executeEvent(r)
    )),
    entry(new Card(129, "Special Forces", US, 1,
      NoRemove, NoMark, NoLapsing, NoConditions,
      (role: Role) => ()
    )),
    entry(new Card(130, "Special Forces", US, 1,
      NoRemove, NoMark, NoLapsing, NoConditions,
      (role: Role) => ()
    )),
    entry(new Card(131, "Arab Spring \"Fallout\"", US, 2,
      NoRemove, NoMark, NoLapsing, NoConditions,
      (role: Role) => ()
    )),
    entry(new Card(132, "Battle of Sirte", US, 2,
      NoRemove, NoMark, NoLapsing, NoConditions,
      (role: Role) => ()
    )),
    entry(new Card(133, "Benghazi Falls", US, 2,
      Remove, NoMark, NoLapsing, NoConditions,
      (role: Role) => ()
    )),
    entry(new Card(134, "Civil Resistance", US, 2,
      NoRemove, NoMark, NoLapsing, NoConditions,
      (role: Role) => ()
    )),
    entry(new Card(135, "Delta / SEALS", US, 2,
      NoRemove, NoMark, NoLapsing, NoConditions,
      (role: Role) => ()
    )),
    entry(new Card(136, "Factional Infighting", US, 2,
      NoRemove, NoMark, NoLapsing, NoConditions,
      (role: Role) => ()
    )),
    entry(new Card(137, "FMS", US, 2,
      NoRemove, NoMark, NoLapsing, NoConditions,
      (role: Role) => ()  // Note: No militia allowed in Good countries!UNSCR 1973
    )),
    entry(new Card(138, "Intel Community", US, 2,
      NoRemove, NoMark, NoLapsing, NoConditions,
      (role: Role) => ()
    )),
    entry(new Card(139, "Int'l Banking Regime", US, 2,
      NoRemove, NoMark, NoLapsing, NoConditions,
      (role: Role) => ()
    )),
    entry(new Card(140, "Maersk Alabama", US, 2,
      Remove, Mark, NoLapsing, NoConditions,
      (role: Role) => ()
    )),
    entry(new Card(141, "Malala Yousafzai", US, 2,
      Remove, NoMark, NoLapsing, NoConditions,
      (role: Role) => ()
    )),
    entry(new Card(142, "Militia", US, 2,
      NoRemove, NoMark, NoLapsing, NoConditions,
      (role: Role) => ()
    )),
    entry(new Card(143, "Obama Doctrine", US, 2,
      NoRemove, NoMark, NoLapsing, NoConditions,
      (role: Role) => ()
    )),
    entry(new Card(144, "Operation New Dawn", US, 2,
      NoRemove, NoMark, NoLapsing, NoConditions,
      (role: Role) => ()
    )),
    entry(new Card(145, "Russian Aid", US, 2,
      NoRemove, NoMark, NoLapsing, NoConditions,
      (role: Role) => ()
    )),
    entry(new Card(146, "Sharia", US, 2,
      NoRemove, NoMark, NoLapsing, NoConditions,
      (role: Role) => ()
    )),
    entry(new Card(147, "Strike Eagle", US, 2,
      NoRemove, NoMark, NoLapsing, NoConditions,
      (role: Role) => ()
    )),
    entry(new Card(148, "Tahrir Square", US, 2,
      NoRemove, NoMark, NoLapsing, NoConditions,
      (role: Role) => ()
    )),
    entry(new Card(149, "UN Nation Building", US, 2,
      NoRemove, NoMark, NoLapsing, NoConditions,
      (role: Role) => ()
    )),
    entry(new Card(150, "UNSCR 1973", US, 2,
      NoRemove, Mark, NoLapsing, NoConditions,
      (role: Role) => ()
    )),
    entry(new Card(151, "UNSCR 2118", US, 2,
      Remove, NoMark, NoLapsing, NoConditions,
      (role: Role) => ()
    )),
    entry(new Card(152, "Congress Acts", US, 3,
      NoRemove, NoMark, NoLapsing, NoConditions,
      (role: Role) => ()
    )),
    entry(new Card(153, "Facebook", US, 3,
      NoRemove, NoMark, NoLapsing, 
      (_: Role) => eventInPlay("Smartphones")
      ,
      (role: Role) => ()
    )),
    entry(new Card(154, "Facebook", US, 3,
      NoRemove, NoMark, NoLapsing,
      (_: Role) => eventInPlay("Smartphones")
      ,
      (role: Role) => ()
    )),
    entry(new Card(155, "Fracking", US, 3,
      NoRemove, Mark, NoLapsing, NoConditions,
      (role: Role) => ()
    )),
    entry(new Card(156, "Gulf Union", US, 3,
      NoRemove, NoMark, NoLapsing, NoConditions,
      (role: Role) => ()
    )),
    entry(new Card(157, "Limited Deployment", US, 3,
      NoRemove, NoMark, NoLapsing, NoConditions,
      (role: Role) => ()
    )),
    entry(new Card(158, "Mass Turnout", US, 3,
      NoRemove, NoMark, NoLapsing, NoConditions,
      (role: Role) => ()
    )),
    entry(new Card(159, "NATO", US, 3,
      NoRemove, Mark, NoLapsing, NoConditions,
      (role: Role) => ()
    )),
    entry(new Card(160, "Operation Neptune Spear", US, 3,
      NoRemove, NoMark, NoLapsing, NoConditions,
      (role: Role) => ()
    )),
    entry(new Card(161, "PRISM", US, 3,
      NoRemove, NoMark, NoLapsing, NoConditions,
      (role: Role) => ()
    )),
    entry(new Card(162, "SCAF", US, 3,
      NoRemove, NoMark, NoLapsing, NoConditions,
      (role: Role) => ()
    )),
    entry(new Card(163, "Status Quo", US, 3,
      NoRemove, NoMark, NoLapsing, NoConditions,
      (role: Role) => ()
    )),
    entry(new Card(164, "Bloody Thursday", Jihadist, 1,
      NoRemove, Mark, NoLapsing, NoConditions,
      (role: Role) => ()
    )),
    entry(new Card(165, "Coup", Jihadist, 1,
      NoRemove, NoMark, NoLapsing, NoConditions,
      (role: Role) => ()
    )),
    entry(new Card(166, "Ferguson", Jihadist, 1,
      NoRemove, NoMark, Lapsing, NoConditions,
      (role: Role) => ()
    )),
    entry(new Card(167, "Houthi Rebels", Jihadist, 1,
      Remove, NoMark, NoLapsing, NoConditions,
      (role: Role) => ()
    )),
    entry(new Card(168, "IEDs", Jihadist, 1,
      NoRemove, NoMark, NoLapsing, NoConditions,
      (role: Role) => ()
    )),
    entry(new Card(169, "Islamic Maghreb", Jihadist, 1,
      NoRemove, NoMark, Lapsing, NoConditions,
      (role: Role) => ()
    )),
    entry(new Card(170, "Theft of State", Jihadist, 1,
      NoRemove, NoMark, NoLapsing, NoConditions,
      (role: Role) => ()
    )),
    entry(new Card(171, "Abu Ghraib Jail Break", Jihadist, 2,
      Remove, NoMark, NoLapsing, NoConditions,
      (role: Role) => ()
    )),
    entry(new Card(172, "Al-Shabaab", Jihadist, 2,
      NoRemove, NoMark, NoLapsing, NoConditions,
      (role: Role) => ()
    )),
    entry(new Card(173, "Arab Winter", Jihadist, 2,
      NoRemove, NoMark, Lapsing, NoConditions,
      (role: Role) => ()
    )),
    entry(new Card(174, "Boston Marathon", Jihadist, 2,
      Remove, NoMark, NoLapsing, NoConditions,
      (role: Role) => ()
    )),
    entry(new Card(175, "Censorship", Jihadist, 2,
      NoRemove, Mark, NoLapsing, NoConditions,
      (role: Role) => ()
    )),
    entry(new Card(176, "Change of State", Jihadist, 2,
      NoRemove, NoMark, NoLapsing, NoConditions,
      (role: Role) => ()
    )),
    entry(new Card(177, "Gaza Rockets", Jihadist, 2,
      NoRemove, NoMark, NoLapsing, NoConditions,
      (role: Role) => ()
    )),
    entry(new Card(178, "Ghost Soldiers", Jihadist, 2,
      NoRemove, NoMark, NoLapsing, NoConditions,
      (role: Role) => ()
    )),
    entry(new Card(179, "Korean Crisis", Jihadist, 2,
      NoRemove, NoMark, Lapsing, NoConditions,
      (role: Role) => ()
    )),
    entry(new Card(180, "Mosul Central Bank", Jihadist, 2,
      Remove, NoMark, NoLapsing, NoConditions,
      (role: Role) => ()
    )),
    entry(new Card(181, "NPT Safeguards Ignored", Jihadist, 2,
      Remove, NoMark, NoLapsing, NoConditions,  // Note: Only remove on die roll of 1-3
      (role: Role) => ()
    )),
    entry(new Card(182, "Paris Attacks", Jihadist, 2,
      NoRemove, NoMark, NoLapsing, NoConditions,
      (role: Role) => ()
    )),
    entry(new Card(183, "Pirates", Jihadist, 2,
      Remove, Mark, NoLapsing, NoConditions,
      (role: Role) => ()
    )),
    entry(new Card(184, "Sequestration", Jihadist, 2,
      Remove, Mark, NoLapsing, NoConditions,
      (role: Role) => ()
    )),
    entry(new Card(185, "al-Maliki", Jihadist, 3,
      Remove, NoMark, NoLapsing, NoConditions,
      (role: Role) => ()
    )),
    entry(new Card(186, "Boko Haram", Jihadist, 3,
      NoRemove, NoMark, NoLapsing, NoConditions,
      (role: Role) => ()
    )),
    entry(new Card(187, "Foreign Fighters", Jihadist, 3,
      NoRemove, NoMark, NoLapsing, NoConditions,
      (role: Role) => ()
    )),
    entry(new Card(188, "ISIL", Jihadist, 3,
      Remove, NoMark, NoLapsing, NoConditions,
      (role: Role) => ()
    )),
    entry(new Card(189, "Jihadist Videos", Jihadist, 3,
      NoRemove, NoMark, NoLapsing, NoConditions,
      (role: Role) => ()
    )),
    entry(new Card(190, "Martyrdom Operation", Jihadist, 3,
      NoRemove, NoMark, NoLapsing, NoConditions,
      (role: Role) => ()
    )),
    entry(new Card(191, "Muslim Brotherhood", Jihadist, 3,
      Remove, NoMark, NoLapsing, NoConditions,
      (role: Role) => ()
    )),
    entry(new Card(192, "Quagmire", Jihadist, 3,
      NoRemove, NoMark, NoLapsing, NoConditions,
      (role: Role) => ()
    )),
    entry(new Card(193, "Regional al-Qaeda", Jihadist, 3,
      NoRemove, NoMark, NoLapsing, NoConditions,
      (role: Role) => ()
    )),
    entry(new Card(194, "Snowden", Jihadist, 3,
      Remove, NoMark, NoLapsing, NoConditions,
      (role: Role) => ()
    )),
    entry(new Card(195, "Taliban Resurgent", Jihadist, 3,
      NoRemove, NoMark, NoLapsing, NoConditions,
      (role: Role) => ()
    )),
    entry(new Card(196, "Training Camps", Jihadist, 3,
      NoRemove, Mark, NoLapsing, NoConditions,
      (role: Role) => ()
    )),
    entry(new Card(197, "Unconfirmed", Jihadist, 3,
      Remove, NoMark, NoLapsing, NoConditions,
      (role: Role) => ()
    )),
    entry(new Card(198, "US Atrocities", Jihadist, 3,
      NoRemove, NoMark, NoLapsing, NoConditions,
      (role: Role) => ()
    )),
    entry(new Card(199, "US Consulate Attacked", Jihadist, 3,
      NoRemove, NoMark, Lapsing, NoConditions,
      (role: Role) => ()
    )),
    entry(new Card(200, "Critical Middle", Unassociated, 1,
      NoRemove, NoMark, NoLapsing, NoConditions,
      (role: Role) => ()
    )),
    entry(new Card(201, "Cross Border Support", Unassociated, 1,
      NoRemove, NoMark, NoLapsing, NoConditions,
      (role: Role) => ()
    )),
    entry(new Card(202, "Cyber Warfare", Unassociated, 1,
      NoRemove, NoMark, NoLapsing, NoConditions,
      (role: Role) => ()
    )),
    entry(new Card(203, "Day of Rage", Unassociated, 1,
      Remove, NoMark, NoLapsing, NoConditions,
      (role: Role) => ()
    )),
    entry(new Card(205, "Erdoğan Effect", Unassociated, 1,
      NoRemove, NoMark, NoLapsing, NoConditions,
      (role: Role) => ()
    )),
    entry(new Card(204, "Ebola Scare", Unassociated, 1,
      Remove, NoMark, USLapsing, NoConditions,
      (role: Role) => ()
    )),
    entry(new Card(206, "Friday of Anger", Unassociated, 1,
      NoRemove, NoMark, NoLapsing, NoConditions,
      (role: Role) => ()
    )),
    entry(new Card(207, "JV / Copycat", Unassociated, 1,
      NoRemove, NoMark, NoLapsing, NoConditions,
      (role: Role) => ()
    )),
    entry(new Card(208, "Kinder – Gentler", Unassociated, 1,
      NoRemove, NoMark, NoLapsing, NoConditions,
      (role: Role) => ()
    )),
    entry(new Card(209, "Quds Force", Unassociated, 1,
      NoRemove, NoMark, NoLapsing, NoConditions,
      (role: Role) => ()
    )),
    entry(new Card(210, "Sectarian Violence", Unassociated, 1,
      NoRemove, NoMark, NoLapsing, NoConditions,
      (role: Role) => ()
    )),
    entry(new Card(211, "Smartphones", Unassociated, 1,
      NoRemove, Mark, NoLapsing, NoConditions,
      (role: Role) => ()
    )),
    entry(new Card(212, "Smartphones", Unassociated, 1,
      NoRemove, Mark, NoLapsing, NoConditions,
      (role: Role) => ()
    )),
    entry(new Card(213, "Smartphones", Unassociated, 1,
      NoRemove, Mark, NoLapsing, NoConditions,
      (role: Role) => ()
    )),
    entry(new Card(214, "3 Cups of Tea", Unassociated, 2,
      NoRemove, Mark, NoLapsing, NoConditions,
      (role: Role) => ()
    )),
    entry(new Card(215, "Abu Bakr al-Baghdadi", Unassociated, 2,
      USRemove, NoMark, NoLapsing, NoConditions,
      (role: Role) => ()
    )),
    entry(new Card(216, "Abu Sayyaf (ISIL)", Unassociated, 2,
      USRemove, NoMark, NoLapsing, NoConditions,
      (role: Role) => ()
    )),
    entry(new Card(217, "Agitators", Unassociated, 2,
      NoRemove, NoMark, NoLapsing, NoConditions,
      (role: Role) => ()
    )),
    entry(new Card(218, "Al-Nusra Front", Unassociated, 2,
      NoRemove, NoMark, NoLapsing, NoConditions,
      (role: Role) => ()
    )),
    entry(new Card(219, "Ayman al-Zawahiri", Unassociated, 2,
      USRemove, NoMark, NoLapsing, NoConditions,
      (role: Role) => ()
    )),
    entry(new Card(220, "Daraa", Unassociated, 2,
      Remove, NoMark, NoLapsing, NoConditions,
      (role: Role) => ()
    )),
    entry(new Card(221, "FlyPaper", Unassociated, 2,
      NoRemove, NoMark, NoLapsing, NoConditions,
      (role: Role) => ()
    )),
    entry(new Card(222, "Hagel", Unassociated, 2,
      Remove, NoMark, NoLapsing, NoConditions,
      (role: Role) => ()
    )),
    entry(new Card(223, "Iranian Elections", Unassociated, 2,
      Remove, NoMark, NoLapsing, NoConditions,
      (role: Role) => ()
    )),
    entry(new Card(224, "Je Suis Charlie", Unassociated, 2,
      NoRemove, NoMark, NoLapsing, NoConditions,
      (role: Role) => ()
    )),
    entry(new Card(225, "Jihadi John", Unassociated, 2,
      USRemove, NoMark, NoLapsing, NoConditions,
      (role: Role) => ()
    )),
    entry(new Card(226, "Operation Serval", Unassociated, 2,
      NoRemove, USMark, NoLapsing, NoConditions,
      (role: Role) => ()
    )),
    entry(new Card(227, "Popular Support", Unassociated, 2,
      NoRemove, NoMark, NoLapsing, NoConditions,
      (role: Role) => ()
    )),
    entry(new Card(228, "Popular Support", Unassociated, 2,
      NoRemove, NoMark, NoLapsing, NoConditions,
      (role: Role) => ()
    )),
    entry(new Card(229, "Prisoner Exchange", Unassociated, 2,
      NoRemove, NoMark, NoLapsing, NoConditions,
      (role: Role) => ()
    )),
    entry(new Card(230, "Sellout", Unassociated, 2,
      NoRemove, NoMark, NoLapsing, NoConditions,
      (role: Role) => ()
    )),
    entry(new Card(231, "Siege of \"Kobanigrad\"", Unassociated, 2,
      NoRemove, NoMark, NoLapsing, NoConditions,
      (role: Role) => ()
    )),
    entry(new Card(232, "Trade Embargo", Unassociated, 2,
      USRemove, Mark, NoLapsing, NoConditions,
      (role: Role) => ()
    )),
    entry(new Card(233, "UN Ceasefire", Unassociated, 2,
      NoRemove, NoMark, NoLapsing, NoConditions,
      (role: Role) => ()
    )),
    entry(new Card(234, "Free Syrian Army", Unassociated, 3,
      Remove, NoMark, NoLapsing, NoConditions,
      (role: Role) => ()
    )),
    entry(new Card(235, "Qadhafi", Unassociated, 3,
      NoRemove, NoMark, NoLapsing, NoConditions,
      (role: Role) => ()
    )),
    entry(new Card(236, "Oil Price Spike", Unassociated, 3,
      NoRemove, NoMark, Lapsing, NoConditions,
      (role: Role) => ()
    )),
    entry(new Card(237, "Osama bin Ladin", Unassociated, 3,
      USRemove, NoMark, NoLapsing, NoConditions,
      (role: Role) => ()
    )),
    entry(new Card(238, "Revolution", Unassociated, 3,
      NoRemove, NoMark, NoLapsing, NoConditions,
      (role: Role) => ()
    )),
    entry(new Card(239, "Truce", Unassociated, 3,
      NoRemove, Mark, Lapsing, NoConditions,
      (role: Role) => ()
    )),
    entry(new Card(240, "US Election", Unassociated, 3,
      NoRemove, NoMark, NoLapsing, NoConditions,
      (role: Role) => ()
    ))
  )
}