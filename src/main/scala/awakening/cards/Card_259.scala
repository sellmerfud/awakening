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

package awakening.cards

import awakening.LabyrinthAwakening._
import awakening.USBot

// Card Text:
// ------------------------------------------------------------------
// Play if either Saudi Arabia or Gulf States is Good.
// Place 2 Militia in or adjacent to any one Sunni country or
// Gulf States (not Iran).
// OR Reposition Militia between Muslim countries within one space
// of Saudi Arabia, Gulf States and Turkey.
// ------------------------------------------------------------------
object Card_259 extends Card2(259, "Arab NATO", US, 2, NoRemove, NoLapsing, NoAutoTrigger) {
  // Used by the US Bot to determine if the executing the event would alert a plot
  // in the given country
  override
  def eventAlertsPlot(countryName: String, plot: Plot): Boolean = false

  // Used by the US Bot to determine if the executing the event would remove
  // the last cell on the map resulting in victory.
  override
  def eventRemovesLastCell(): Boolean = false

  def reposCountries =
    (List(SaudiArabia, GulfStates, Turkey) :::
     game.adjacentMuslims(SaudiArabia).map(_.name) :::
     game.adjacentMuslims(GulfStates).map(_.name) :::
     game.adjacentMuslims(Turkey).map(_.name)).sorted.distinct

  val ispPlacementCandidate = (m: MuslimCountry) =>
    m.canTakeMilitia &&
    (m.name == GulfStates || (m.name != Iran && (m.isSunni || game.adjacentToSunni(m.name))))

  def getPlacementCandidates() = countryNames(game.muslims.filter(ispPlacementCandidate))

  def canPlace =
    game.militiaAvailable > 0 &&
    getPlacementCandidates().nonEmpty

  def getReposCandidates() = reposCountries.filter(name => game.getMuslim(name).canTakeMilitia)

  def canReposition =
    getReposCandidates().size > 1 &&
    getReposCandidates().exists(name => game.getMuslim(name).militia > 0)

  // Returns true if the printed conditions of the event are satisfied
  override
  def eventConditionsMet(role: Role) =
    game.getMuslim(SaudiArabia).isGood || game.getMuslim(GulfStates).isGood

  // Returns true if the Bot associated with the given role will execute the event
  // on its turn.  This implements the special Bot instructions for the event.
  // When the event is triggered as part of the Human players turn, this is NOT used.
  //
  // Bot will only place milita, not reposition.
  override
  def botWillPlayEvent(role: Role): Boolean = canPlace

  // Carry out the event for the given role.
  // forTrigger will be true if the event was triggered during the human player's turn
  // and it associated with the Bot player.
  override
  def executeEvent(role: Role, forTrigger: Boolean): Unit = {
    if (!canPlace && !canReposition)
      log("\nThere are no milita to place or reposition. The event has no effect.", Color.Event)
    else if (isHuman(role)) {
      val actionChoices = List(
        choice(canPlace,      "place", "Place militia"),
        choice(canReposition, "repos", "Reposition militia")
      ).flatten

      if (askMenu("Choose one:", actionChoices).head == "place") {
        val numMilita  = game.militiaAvailable min 2
        val candidates = getPlacementCandidates()
        val sunniCandidates = countryNames(
          game.muslims.filter { m =>
            m.isSunni &&
            (m.canTakeMilitia || game.adjacentMuslims(m.name).exists(_.canTakeMilitia))
          }
        )
        val inSunni = sunniCandidates.nonEmpty
        val inGulfStates = candidates.contains(GulfStates)
        val targetChoices = List(
          choice(inGulfStates, "gulf",  "Place milita in Gulf States"),
          choice(inSunni,      "sunni", "Place milita in or adjacent to a Sunni country")
        ).flatten

        if (askMenu("Choose one", targetChoices).head == "gulf") {
          addEventTarget(GulfStates)
          addMilitiaToCountry(GulfStates, game.militiaAvailable min 2)
        }
        else {
          val sunni = askCountry("Place in or adjacent to which Sunni country: ", sunniCandidates)
          val adjacent = game.adjacentMuslims(sunni).filter(_.canTakeMilitia)
          val candidates = countryNames(
            (game.getMuslim(sunni) :: game.adjacentMuslims(sunni)).filter(_.canTakeMilitia)
          )

          if (candidates.size == 1) {
            addEventTarget(candidates.head)
            addMilitiaToCountry(candidates.head, game.militiaAvailable min 2)
          }
          else {
            def placeMilitia(remaining: Int, candidates: List[String]): Unit = {
              remaining match {
                case 0 =>
                case x =>
                  println()
                  val target = askCountry("Place militia in which country: ", candidates)
                  val num    = askInt(s"Place how many militia in $target", 1, remaining)
                  if (num > 0) {
                    addEventTarget(target)
                    addMilitiaToCountry(target, num)
                    placeMilitia(remaining - num, candidates)
                  }
              }
            }

            placeMilitia(game.militiaAvailable min 2, candidates)
          }
        }
      }
      else {
        // Reposition militia
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

        val reposCandidates = getReposCandidates()
        val totalMilitia = reposCandidates.foldLeft(0) { (sum, name) => sum + game.getMuslim(name).militia }
        val starting = reposCandidates.map { name =>
          MilitiaCountry(name, game.getMuslim(name).militia)
        }
        val width = reposCandidates.map(_.length).max
        println(s"\nThere are a total of $totalMilitia militia in the target countries.")
        println(separator())
        for (MilitiaCountry(name, numMilita) <- starting)
          println(s"${padLeft(name, width)}: $numMilita militia")
        println()
        println("Assume that we start by taking all of those militia off of the map.")
        println("You will be prompted with the name of each country one by one.")
        println("Specify the number of militia that you would like in each country:\n")
        val placements = nextCountry(reposCandidates, totalMilitia).filterNot(_.noChange)
        println()
        if (placements.isEmpty)
          log("No change to the position of the existing militia.", Color.Event)
        else {
          for (p <- placements; if p.hasLess) {
            addEventTarget(p.name)
            val m = game.getMuslim(p.name)
            game = game.updateCountry(m.copy(militia = p.newMilitia))
            log(s"Remove ${p.muslim.militia - p.newMilitia} militia from ${p.name}", Color.MapPieces)
          }
          for (p <- placements; if !p.hasLess) {
            addEventTarget(p.name)
            val m = game.getMuslim(p.name)
            game = game.updateCountry(m.copy(militia = p.newMilitia))
            log(s"Add ${p.newMilitia - p.muslim.militia} militia to ${p.name}", Color.MapPieces)
          }
        }
      }
    }
    else {
      // Bot
      val target = USBot.deployToPriority(getPlacementCandidates()).get
      addEventTarget(target)
      addMilitiaToCountry(target, game.militiaAvailable min 2)
    }
  }
}
