
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
// Copyright (c) 2017 Curt Sellmer
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

import scala.util.Random.shuffle
import scala.annotation.tailrec
import LabyrinthAwakening._

// Common routines used by both of the Bots.

trait BotHelpers {
  def botLog(msg: => String) = if (game.botLogging) log(msg)
  
  // jihadDRM for a given Muslim country
  def jihadDRM(m: MuslimCountry, major: Boolean): Int = 
    if (game.jihadistIdeology(Virulent))
      -m.reaction // Ignore awakening if Virulent for all jihads
    else if (game.jihadistIdeology(Coherent) && !major)
      -m.reaction // Ignore awakening if Coherent and minor jihads
    else
      m.awakening - m.reaction


  def benazirBhuttoPreventsJihad(m: MuslimCountry) =
    m.name == Pakistan && m.hasMarker(BenazirBhutto)

  // Return true if a die roll of 1 will succeed in the given country
  def minorJihadSuccessPossible(m: MuslimCountry) =
    !benazirBhuttoPreventsJihad(m) &&
    (1 + jihadDRM(m, major = false)) <= m.governance

  // Return true if a die roll of 1 will succeed in the given country
  def majorJihadSuccessPossible(m: MuslimCountry) = if (game.botEnhancements) {
    !benazirBhuttoPreventsJihad(m) &&
    game.totalCellCapacity - m.totalTroopsAndMilitia >= 5 &&  // Are then enough cells in play to overcome TandM
    (1 + jihadDRM(m, major = true)) <= m.governance

  }
  else {
    !benazirBhuttoPreventsJihad(m) &&
    (1 + jihadDRM(m, major = true)) <= m.governance
  }
  
  // trait representing nodes in the 
  // - Jihadist EvO Flowchart
  // - US PAR Flowchart
  trait OpFlowchartNode
  
  trait OperationDecision extends OpFlowchartNode {
    val desc: String
    def yesPath: OpFlowchartNode
    def noPath: OpFlowchartNode
    def condition(ops: Int): Boolean
    override def toString() = desc
  }

  // CountryFilters are used both when selecting a list of candidates and
  // when picking the top priority when following a Priorities Table.
  // Each filter simply takes a list of Countries as input and produces
  // a filtered list as output.
  // The see the selectCandidates() and topPriority() functions to see how
  // filters are used (slightly differently) in each type of situation.
  trait CountryFilter {
    val desc: String
    def filter(countries: List[Country]): List[Country]
    override def toString() = desc
  }
  
  // This corresponds to following amn OpP Flowchart, but is also used for
  // finding specific event candidates.
  // Process the list of countries by each CountryFilter supplied until one of the filters returns
  // a non-empty list of candidates.
  //
  // Each filter is first used against the input and if the filter does not find any matching
  // candidates, the next filter is given a chance.
  // As soon as a filter finds at least one matching country, then the process stops and the
  // results from that filter are returned.
  // If none of the filters finds at least one matching country we return Nil, 
  // which indicates that no valid candidates were found for the OpP flowchart.
  @tailrec final def selectCandidates(countries: List[Country], filters: List[CountryFilter]): List[Country] = {
    botLog(s"OpP Flowchart: [${(countries map (_.name)) mkString ", "}]")
    (countries, filters) match {
      case (Nil, _) =>
        botLog("OpP Flowchart: no countries to consider")
        Nil    // No countries to consider
      case (_, Nil) => 
        botLog("OpP Flowchart: no candidates found")
        Nil    // No filter found any candidates
      case (cs, f::fs) =>
        (f filter cs) match {
          case Nil =>            // Filter did not match anything, try the next filter
            botLog(s"OpP Flowchart ($f): failed")
            selectCandidates(cs, fs)
          case results =>        // We got some results…
            botLog(s"OpP Flowchart ($f): [${(results map (_.name) mkString ", ")}]")
            results
        }
    }
  }
  
  
  // Process the list of countries by each CountryFilter in the priorities list.
  // The priorities list represents a single column in a Priorities Table.
  // In this function each filter is processed in order until we have used all filters
  // in the list to narrow the choices to a single country.  If we go through all of
  // the filters and we still have more than one viable country, then we pick one at
  // random.
  // Note: The only time this function will return None, is if the original list of
  //       countries is empty.
  def topPriority(countries: List[Country], priorities: List[CountryFilter]): Option[Country] = {
    botLog(s"topPriority: [${(countries map (_.name)) mkString ", "}]")
    @tailrec def nextPriority(countries: List[Country], priorities: List[CountryFilter]): Option[Country] = {
      (countries, priorities) match {
        case (Nil, _)    => None
        case (c::Nil, _) => 
          botLog(s"topPriority: Picked a winner [${c.name}]")
          Some(c)                             // We've narrowed it to one
        case (cs, Nil)   =>
          val c = shuffle(cs).head              // Take one at random
          botLog(s"topPriority: Picked random country [${c.name}]")
          Some(c)
        case (cs, f::fs) =>
          (f filter cs) match {
            case Nil =>
              botLog(s"topPriority ($f) failed")
              nextPriority(cs, fs) // Filter entire list by next priority
            case rs  =>
              botLog(s"topPriority ($f) [${(rs map (_.name) mkString ", ")}]")
              nextPriority(rs, fs) // Filter matched list by next priority
          }
      }
    }
    nextPriority(countries, priorities)
  }
  
  
  // Helper function for CountryFilter boolean tests that only apply Muslim countries
  // All non-Muslim countries will return a given value (false by default)
  def muslimTest(test: (MuslimCountry) => Boolean, nonMuslim: Boolean = false)(c: Country): Boolean = c match {
    case m: MuslimCountry    => test(m)
    case n: NonMuslimCountry => nonMuslim  
  }

  // Helper function for CountryFilter boolean tests that only apply non-Muslim countries
  // All Muslim countries will return a given value (false by default)
  def nonMuslimTest(test: (NonMuslimCountry) => Boolean, muslim: Boolean = false)(c: Country): Boolean = c match {
    case m: MuslimCountry    => muslim
    case n: NonMuslimCountry => test(n)  
  }

  // Helper function for CountryFilter integer test that only apply Muslim countries
  // All Muslim countries will return a given value (false by default)
  // All non-Muslim countries will return a given value (-100 by default)
  def muslimScore(score: (MuslimCountry) => Int, nonMuslimScore: Int = -100)(c: Country): Int = c match {
    case m: MuslimCountry    => score(m)
    case n: NonMuslimCountry => nonMuslimScore  
  }
  // Helper function for CountryFilter integer test that only apply non-Muslim countries
  // All Muslim countries will return a given value (-100 by default)
  def nonMuslimScore(score: (NonMuslimCountry) => Int, muslimScore: Int = -100)(c: Country): Int = c match {
    case m: MuslimCountry    => muslimScore
    case n: NonMuslimCountry => score(n)  
  }

  
  // A boolean criteria filter that is used in OpP flowcharts
  // Filters the given countries and returns the results.
  class CriteriaFilter(val desc: String, criteria: (Country) => Boolean) extends CountryFilter {
    def filter(countries: List[Country]) = (countries filter criteria)
  }
  
  // Highest integer score filter used with Priority Tables.
  // Applies the given score function to each country in the input list and
  // takes the highest value.
  // Then returns the list of countries whose score matches that highest value.
  class HighestScorePriority(val desc: String, score: (Country) => Int) extends CountryFilter {
    def filter(countries: List[Country]): List[Country] = {
      val high = (countries map score).max
      botLog(s"Highest ($desc): score = $high")
      countries filter (c => score(c) == high)
    }
  }

  // Lowest integer score filter used with Priority Tables.
  // Applies the given score function to each country in the input list and
  // takes the lowest value.
  // Then returns the list of countries whose score matches that lowest value.
  class LowestScorePriority(val desc: String, score: (Country) => Int) extends CountryFilter {
    def filter(countries: List[Country]): List[Country] = {
      val low = (countries map score).min
      botLog(s"Lowest ($desc): score = $low")
      countries filter (c => score(c) == low)
    }
  }
  
  // Highest integer score filter used with OpP flowcharts.
  // Contains a boolean criteria to determine which countries may be considered,
  // and an integer score function that is applied to each country that matches
  // the necessary criteria.
  // First picks only candidates that satisfy the criteria.
  // Then among those takes only the ones with the highest score.
  class HighestScoreNode(val desc: String,
                         criteria: (Country) => Boolean, 
                         score:    (Country) => Int) extends CountryFilter {
    def filter(countries: List[Country]) = (countries filter criteria) match {
      case Nil        => Nil
      case candidates =>
        val high = (candidates map score).max
        candidates filter (c => score(c) == high)
    }
  }
  
  // Lowest integer score filter used with OpP flowcharts.
  // Contains a boolean criteria to determine which countries may be considered,
  // and an integer score function that is applied to each country that matches
  // the necessary criteria.
  // First picks only candidates that satisfy the criteria.
  // Then among those takes only the ones with the lowest score.
  class LowestScoreNode(val desc: String,
                        criteria: (Country) => Boolean, 
                        score:    (Country) => Int) extends CountryFilter {
    def filter(countries: List[Country]) = (countries filter criteria) match {
      case Nil        => Nil
      case candidates =>
        val low = (candidates map score).min
        candidates filter (c => score(c) == low)
    }
  }
  
  // Adjacency filter used with OpP flowcharts.
  // Returns the list of countries that are adjacent to the given target Country.
  class AdjacentCountriesNode(target: String) extends CountryFilter {
    val desc = s"Adjacent to $target"
    def filter(countries: List[Country]) = 
      (countries filter (c => areAdjacent(c.name, target)))
  }


  // This is a convenience method used when selecting multiple targets for events.
  // It picks the best target from the given list of candidates repeatedly until
  // the required number of targets have been picked or until we run out of candidates
  // to choose from.
  // As each target is selected, it is removed from the list of candidates so that
  // a given target will not be picked more than once.
  def multipleTargets(num: Int, candidates: List[String], pickBest: (List[String]) => Option[String]): List[String] = {
    def nextTarget(n: Int, targets: List[String]): List[String] = {
      if (n <= num && targets.nonEmpty) {
        pickBest(targets) match {
          case None => Nil
          case Some(name) => name :: nextTarget(n + 1, targets filterNot (_ == name))
        }
      }
      else
        Nil
    }
    nextTarget(1, candidates)
  }
}