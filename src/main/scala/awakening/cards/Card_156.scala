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
// Place a total of up to 4 Militia in or Adjacent (not in Iran) to one
// of these countries: Gulf States, Saudi Arabia, Yemen, Jordan, or Morocco,
// OR
// reposition Militia existing within the borders of these countries.
// ------------------------------------------------------------------
object Card_156 extends Card(156, "Gulf Union", US, 3, NoRemove, NoLapsing, NoAutoTrigger) {
  // Used by the US Bot to determine if the executing the event would alert a plot
  // in the given country
  override
  def eventAlertsPlot(countryName: String, plot: Plot): Boolean = false

  // Used by the US Bot to determine if the executing the event would remove
  // the last cell on the map resulting in victory.
  override
  def eventRemovesLastCell(): Boolean = false

  val GulfUnionCountries = List(GulfStates, SaudiArabia, Yemen, Jordan, Morocco).sorted

  def getPlaceCandidates: List[String] =
    GulfUnionCountries.foldLeft(Set.empty[String]) { (candidates, name) =>
      val gulfCountry = game.getMuslim(name)
      val adjacent = getAdjacent(name)
        .filter(x => game.isMuslim(x) && x != Iran)
        .map(game.getMuslim)
      val newCandidates = (gulfCountry :: adjacent)
        .filter(m => !m.truce && m.canTakeMilitia)
        .map(_.name)
      candidates ++ newCandidates
    }.toList.sorted

  def getReposCandidates: List[String] =
    GulfUnionCountries.filter { name =>
      val gulfCountry = game.getMuslim(name)
      !gulfCountry.truce && gulfCountry.canTakeMilitia
    }

  def militiaInGulfUnion =
    GulfUnionCountries.exists(name => !game.getMuslim(name).truce && game.getMuslim(name).militia > 0)

  def getBotPlaceCandidates = getPlaceCandidates
    .filter { name =>
      val m = game.getMuslim(name)
      m.totalCells > m.totalTroopsAndMilitia
    }

  // Returns true if the printed conditions of the event are satisfied
  override
  def eventConditionsMet(role: Role) =
    (game.militiaAvailable > 0 && getPlaceCandidates.nonEmpty) ||
    (militiaInGulfUnion && getReposCandidates.size > 1)


  // Returns true if the Bot associated with the given role will execute the event
  // on its turn.  This implements the special Bot instructions for the event.
  // When the event is triggered as part of the Human players turn, this is NOT used.
  //
  // Bot does not reposition Milita and only places militia in countries with Cells > TandM
  override
  def botWillPlayEvent(role: Role): Boolean =
    game.militiaAvailable > 0 && getBotPlaceCandidates.nonEmpty

  // Carry out the event for the given role.
  // forTrigger will be true if the event was triggered during the human player's turn
  // and it associated with the Bot player.
  override
  def executeEvent(role: Role): Unit = {
    val maxMilitia = 4 min game.militiaAvailable
    if (isHuman(role)) {
      // Card allows placing militia or repositioning militia in the Gulf Union countries.
      val place = game.militiaAvailable > 0 && getPlaceCandidates.nonEmpty
      val repo = militiaInGulfUnion && getReposCandidates.size > 1
      sealed trait Choice
      case object Place extends Choice
      case object Reposition extends Choice
      val choices = List(
        choice(place, Place, "Place up to 4 militia in one Gulf Union (or adjacent) country"),
        choice(repo,  Reposition,  "Reposition militia in Gulf Union countries"),
      ).flatten

      askMenu("Choose one:", choices).head match {
        case Place =>
          val target = askCountry("Place militia in which country: ", getPlaceCandidates)
          val num    = askInt(s"Place how many militia in $target", 1, maxMilitia, Some(maxMilitia))
          addEventTarget(target)
          addMilitiaToCountry(target, num)

        case Reposition => // Reposition militia with the Gulf Union countries
          val totalMilitia = GulfUnionCountries.foldLeft(0) { (sum, name) =>
            sum + game.getMuslim(name).militia
          }

          println("\nThe Gulf Union countries that can take militia are:")
          println(separator())
          for (name <- getReposCandidates; num = game.getMuslim(name).militia)
            println(s"$name: $num militia")
          println()
          println(s"There are a total of $totalMilitia militia in these countries.")
          println("Assume that we start by taking all of those militia off of the map.")
          println("You will be prompted with the name of each country one by one.")
          println("Specify the number of militia that you would like in each country:")

          case class GulfUnion(name: String, newMilitia: Int) {
            val delta = newMilitia - game.getMuslim(name).militia
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

          val placements = nextCountry(getReposCandidates, totalMilitia)
          if (placements.forall(_.delta == 0))
            log("\nNo change to the position of the existing militia in the Gulf Union", Color.Event)
          else {
            for (p <- placements; if p.delta < 0) {
              addEventTarget(p.name)
              removeMilitiaFromCountry(p.name, -p.delta)
            }
            for (p <- placements; if p.delta > 0) {
              addEventTarget(p.name)
              addMilitiaToCountry(p.name, p.delta)
            }
          }
      }
    }
    else {  // Bot
      if ((game.militiaAvailable > 0 && getPlaceCandidates.nonEmpty)) {
        // See Event Instructions table
        val candidates = if (getBotPlaceCandidates.nonEmpty)
          getBotPlaceCandidates
        else
          getPlaceCandidates

          val target = USBot.deployToPriority(USBot.highestCellsMinusTandM(candidates)).get
          addEventTarget(target)
          addMilitiaToCountry(target, maxMilitia)
      }
      else
        log("\nThe US Bot does not reposition any militia.", Color.Event)
    }
  }
}
