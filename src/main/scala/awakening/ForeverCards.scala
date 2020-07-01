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
import LabyrinthAwakening._
import USBot.PlotInCountry

object ForeverCards {
  
  // Various tests used by the card events
  val backlashCandidate = (m: MuslimCountry) =>
    (m.plots exists (p => !p.backlashed)) && !game.isCaliphateMember(m.name)

  // Convenience method for adding a card to the deck.
  private def entry(card: Card) = (card.number -> card)
  
  val deckMap: Map[Int, Card] = Map(
    // ------------------------------------------------------------------------
    entry(new Card(241, "Abdel Fattah el-Sisi", US, 1,
      Remove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (_: Role, _: Boolean) => false
      ,
      (role: Role) => {
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(242, "Avenger", US, 1,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (_: Role, _: Boolean) => false
      ,
      (role: Role) => {
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
      (_: Role, _: Boolean) => false
      ,
      (role: Role) => {
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(245, "Green Movement 2.0", US, 1,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (_: Role, _: Boolean) => false
      ,
      (role: Role) => {
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(246, "Holiday Surprise", US, 1,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (_: Role, _: Boolean) => false
      ,
      (role: Role) => {
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(247, "Nadia Murad", US, 1,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (_: Role, _: Boolean) => false
      ,
      (role: Role) => {
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(248, "Patriotic Arab Democracies Movement", US, 1,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (_: Role, _: Boolean) => false
      ,
      (role: Role) => {
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(249, "Saudi Air Strikes", US, 1,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (_: Role, _: Boolean) => false
      ,
      (role: Role) => {
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(250, "Special Forces", US, 1,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (_: Role, _: Boolean) => false
      ,
      (role: Role) => {
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(251, "Trump Tweets", US, 1,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (_: Role, _: Boolean) => false
      ,
      (role: Role) => {
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
      (_: Role, _: Boolean) => false
      ,
      (role: Role) => {
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(255, "Western Arms Sales", US, 1,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (_: Role, _: Boolean) => false
      ,
      (role: Role) => {
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(256, "White Helmets", US, 1,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (_: Role, _: Boolean) => false
      ,
      (role: Role) => {
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(257, "Women's Rights Activism", US, 1,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (_: Role, _: Boolean) => false
      ,
      (role: Role) => {
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(258, "75th Ranger Regiment", US, 2,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (_: Role, _: Boolean) => false
      ,
      (role: Role) => {
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(259, "Arab NATO", US, 2,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (_: Role, _: Boolean) => false
      ,
      (role: Role) => {
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(260, "Imran Khan", US, 2,
      Remove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (_: Role, _: Boolean) => false
      ,
      (role: Role) => {
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(261, "Intel Community", US, 2,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (_: Role, _: Boolean) => false
      ,
      (role: Role) => {
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(262, "MOAB", US, 2,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (_: Role, _: Boolean) => false
      ,
      (role: Role) => {
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(263, "Operation Tidal Wave II", US, 2,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (_: Role, _: Boolean) => false
      ,
      (role: Role) => {
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(264, "Personal Security Contractors", US, 2,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (_: Role, _: Boolean) => false
      ,
      (role: Role) => {
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(265, "Popular Mobilization Forces", US, 2,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (_: Role, _: Boolean) => false
      ,
      (role: Role) => {
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(266, "Presidential Reality Show", US, 2,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (_: Role, _: Boolean) => false
      ,
      (role: Role) => {
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(267, "Third Offset Strategy", US, 2,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (_: Role, _: Boolean) => false
      ,
      (role: Role) => {
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(268, "Trump Trip", US, 2,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (_: Role, _: Boolean) => false
      ,
      (role: Role) => {
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(269, "Air America", US, 3,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (_: Role, _: Boolean) => false
      ,
      (role: Role) => {
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(270, "Deep State", US, 3,
      Remove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (_: Role, _: Boolean) => false
      ,
      (role: Role) => {
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(272, "Fire and Fury", US, 3,
      Remove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (_: Role, _: Boolean) => false
      ,
      (role: Role) => {
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(273, "Fully Resourced COIN", US, 3,
      NoRemove, Lapsing, NoAutoTrigger, DoesNotAlertPlot,
      (_: Role, _: Boolean) => false
      ,
      (role: Role) => {
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(274, "Government of National Accord", US, 3,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (_: Role, _: Boolean) => false
      ,
      (role: Role) => {
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(275, "Operation Inherent Resolve", US, 3,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (_: Role, _: Boolean) => false
      ,
      (role: Role) => {
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(276, "Populism/Euroscepticism", US, 3,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (_: Role, _: Boolean) => false
      ,
      (role: Role) => {
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(277, "Regime Change Policy", US, 3,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (_: Role, _: Boolean) => false
      ,
      (role: Role) => {
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(278, "Siege of Mosul", US, 3,
      NoRemove, Lapsing, NoAutoTrigger, DoesNotAlertPlot,
      (_: Role, _: Boolean) => false
      ,
      (role: Role) => {
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(279, "SFABs", US, 3,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (_: Role, _: Boolean) => false
      ,
      (role: Role) => {
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(280, "Sunni-Shia Rift", US, 3,
      Remove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (_: Role, _: Boolean) => false
      ,
      (role: Role) => {
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(281, "Drone Swarms", Jihadist, 1,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (_: Role, _: Boolean) => false
      ,
      (role: Role) => {
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(282, "Executive Order 13492", Jihadist, 1,
      Remove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (_: Role, _: Boolean) => false
      ,
      (role: Role) => {
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(283, "Lone Wolf", Jihadist, 1,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (_: Role, _: Boolean) => false
      ,
      (role: Role) => {
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(284, "Manchester Bombing", Jihadist, 1,
      Remove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (_: Role, _: Boolean) => false
      ,
      (role: Role) => {
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(285, "Mohamed Morsi Supporters", Jihadist, 1,
      Remove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (_: Role, _: Boolean) => false
      ,
      (role: Role) => {
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(286, "Palestinian Peace", Jihadist, 1,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (_: Role, _: Boolean) => false
      ,
      (role: Role) => {
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(287, "Sayyed Hassan Nasrallah", Jihadist, 1,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (_: Role, _: Boolean) => false
      ,
      (role: Role) => {
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(288, "Soldiers of the Caliphate", Jihadist, 1,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (_: Role, _: Boolean) => false
      ,
      (role: Role) => {
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(289, "Strait of Hormuz", Jihadist, 1,
      NoRemove, Lapsing, NoAutoTrigger, DoesNotAlertPlot,
      (_: Role, _: Boolean) => false
      ,
      (role: Role) => {
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(290, "Uyghur Nationalism", Jihadist, 1,
      Remove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (_: Role, _: Boolean) => false
      ,
      (role: Role) => {
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(291, "Vehicle-ramming Attacks", Jihadist, 1,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (_: Role, _: Boolean) => false
      ,
      (role: Role) => {
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(292, "Amaq News Agency", Jihadist, 2,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (_: Role, _: Boolean) => false
      ,
      (role: Role) => {
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(293, "Attempted Coup", Jihadist, 2,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (_: Role, _: Boolean) => false
      ,
      (role: Role) => {
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(294, "Barcelona Bombs", Jihadist, 2,
      Remove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (_: Role, _: Boolean) => false
      ,
      (role: Role) => {
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(295, "Black Gold", Jihadist, 2,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (_: Role, _: Boolean) => false
      ,
      (role: Role) => {
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(296, "Botched Yemeni Raid", Jihadist, 2,
      Remove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (_: Role, _: Boolean) => false
      ,
      (role: Role) => {
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(297, "Early Exit", Jihadist, 2,
      Remove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (_: Role, _: Boolean) => false
      ,
      (role: Role) => {
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(298, "False Flag Attacks", Jihadist, 2,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (_: Role, _: Boolean) => false
      ,
      (role: Role) => {
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(299, "Foreign Fighters Return", Jihadist, 2,
      Remove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (_: Role, _: Boolean) => false
      ,
      (role: Role) => {
      }
    )),
    // ------------------------------------------------------------------------
    entry(new Card(300, "Going Underground", Jihadist, 2,
      NoRemove, NoLapsing, NoAutoTrigger, DoesNotAlertPlot,
      (_: Role, _: Boolean) => false
      ,
      (role: Role) => {
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
      (_: Role, _: Boolean) => false
      ,
      (role: Role) => {
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
      (_: Role, _: Boolean) => game.usPosture != game.getNonMuslim(China).posture && trumpTweetsOn
      ,
      (role: Role) => {
        if (globalEventInPlay(USChinaTradeWar))
          removeGlobalEventMarker(USChinaTradeWar)
        else
          addGlobalEventMarker(USChinaTradeWar)
        turnTrumpTweetsOff()
        decreasePrestige(1)
      }
    ))
  )
}