
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

import scala.util.Random.shuffle
import scala.annotation.tailrec
import LabyrinthAwakening._

// Common routines used by both of the Bots.

trait BotHelpers {
  def botLog(msg: => String) = if (game.params.botLogging) log(msg)
  
  // jihadDRM for a given Muslim country
  def jihadDRM(m: MuslimCountry, major: Boolean): Int = 
    if (game.jihadistIdeology(Virulent))
      -m.reaction // Ignore awakening if Virulent for all jihads
    else if (game.jihadistIdeology(Coherent) && !major)
      -m.reaction // Ignore awakening if Coherent and minor jihads
    else
      m.awakening - m.reaction


  // Return true if a die roll of 1 will succeed in the given country
  def jihadSuccessPossible(m: MuslimCountry, major: Boolean) =
    (1 + jihadDRM(m, major)) <= m.governance
  
  // trait representing items in the 
  // - Jihadist EvO Flowchart
  // - US PAR Flowchart
  trait OpFlowchartItem
  
 trait OperationDecision extends OpFlowchartItem {
    val desc: String
    def yesPath: OpFlowchartItem
    def noPath: OpFlowchartItem
    def condition(ops: Int): Boolean
    override def toString() = desc
  }
  
  // Priorities are used to narrow down a list of countries to a single best country
  // per the Priorities table.
  trait Priority {
    val desc: String
    def filter(countries: List[Country]): List[Country]
    override def toString() = desc
  }

  // If no candidates match the criteria, then return the list unchanged
  // If at least on candidate matches the criteria then return all of the
  // candidates that match the criteria.
  class CriteriaPriority(val desc: String, criteria: (Country) => Boolean) extends Priority {
    def filter(countries: List[Country]) = (countries filter criteria) match {
      case Nil      => botLog(s"Criteria ($desc): match = false"); countries
      case matching => botLog(s"Criteria ($desc): match = true"); matching
    }
  }

  // Picks ALL of the candidates with the highest score and discards the rest.
  class HighestScorePriority(val desc: String, score: (Country) => Int) extends Priority {
    def filter(countries: List[Country]): List[Country] = {
      val high = (countries map score).max
      botLog(s"Highest ($desc): score = $high")
      countries filter (c => score(c) == high)
    }
  }

  // Picks ALL of the candidates with the lowest score and discards the rest.
  class LowestScorePriority(val desc: String, score: (Country) => Int) extends Priority {
    def filter(countries: List[Country]): List[Country] = {
      val low = (countries map score).min
      botLog(s"Lowest ($desc): score = $low")
      countries filter (c => score(c) == low)
    }
  }
  
  // Flowchart filters implement the logic of the Operations Priorities (OpP) flowchart
 trait FlowchartFilter {
    val desc: String
    def filter(countries: List[Country]): List[Country]
    override def toString() = desc
  }
  
  // Filters the given countries and returns the results.
  class CriteriaFilter(val desc: String, criteria: (Country) => Boolean) extends FlowchartFilter {
    def filter(countries: List[Country]) = (countries filter criteria)
  }
  
  // First picks only candidates that satisfy the criteria.
  // Then among those takes only the ones with the highest score.
  class HighestScoreFilter(val desc: String,
                          criteria: (Country) => Boolean, 
                          score:    (Country) => Int) extends FlowchartFilter {
    def filter(countries: List[Country]) = (countries filter criteria) match {
      case Nil        => Nil
      case candidates =>
        val high = (candidates map score).max
        candidates filter (c => score(c) == high)
    }
  }
  
  // First picks only candidates that satisfy the criteria.
  // Then among those takes only the ones with the lowest score.
  class LowestScoreFilter(val desc: String,
                          criteria: (Country) => Boolean, 
                          score:    (Country) => Int) extends FlowchartFilter {
    def filter(countries: List[Country]) = (countries filter criteria) match {
      case Nil        => Nil
      case candidates =>
        val low = (candidates map score).min
        candidates filter (c => score(c) == low)
    }
  }
  
  // Filters out all countries that are not adjacent to the target country.
  class AdjacentFilter(target: String) extends FlowchartFilter {
    val desc = s"Adjacent to $target"
    def filter(countries: List[Country]) = 
      (countries filter (c => areAdjacent(c.name, target)))
  }
  
  // Narrow the given countries to the best target using the given list of priorities.
  // If we run out of priorities and we have multiple candidates, the take one at random.
  @tailrec final def topPriority(countries: List[Country], priorities: List[Priority]): Option[Country] = {
    botLog(s"topPriority: [${(countries map (_.name)) mkString ", "}]")
    (countries, priorities) match {
      case (Nil, _)    => None
      case (c::Nil, _) => Some(c)                             // We've narrowed it to one
      case (cs, Nil)   => shuffle(cs).headOption              // Take one at random
      case (cs, p::ps) => topPriority(p filter countries, ps) // Filter by next priority
    }
  }
  
  // Test the list of countries by each flowchart filter until one of the filters returns
  // a non-empty list of candidates.  If none of the filters finds a match then
  // we return Nil to indicate that the operation being tried cannot be carried out.
  @tailrec final def followFlowchart(countries: List[Country], filters: List[FlowchartFilter]): List[Country] = {
    (countries, filters) match {
      case (Nil, _)    => Nil    // No countries to consider
      case (_, Nil)    => Nil    // No filter found any candidates
      case (_, f::fs) =>
        f.filter(countries) match {
          case Nil =>            // Filter did not match anything, try the next filter
            botLog(s"OpP Flowchart ($f): failed")
            followFlowchart(countries, fs)
          case results =>        // We got some results…
            botLog(s"OpP Flowchart ($f): [${(results map (_.name) mkString ", ")}]")
            results
        }
    }
  }

  // Helper function for scores that only apply Muslim countries
  def muslimScore(score: (MuslimCountry) => Int, nonMuslimScore: Int = -100)(c: Country): Int = c match {
    case m: MuslimCountry    => score(m)
    case n: NonMuslimCountry => nonMuslimScore  
  }
  // Helper function for scores that only apply non-Muslim countries
  def nonMuslimScore(score: (NonMuslimCountry) => Int, muslimScore: Int = -100)(c: Country): Int = c match {
    case m: MuslimCountry    => muslimScore
    case n: NonMuslimCountry => score(n)  
  }

  // Helper function for criteria tests that only apply Muslim countries
  def muslimTest(test: (MuslimCountry) => Boolean, nonMuslim: Boolean = false)(c: Country): Boolean = c match {
    case m: MuslimCountry    => test(m)
    case n: NonMuslimCountry => nonMuslim  
  }

  // Helper function for criteria tests that only apply Muslim countries
  def nonMuslimTest(test: (NonMuslimCountry) => Boolean, muslim: Boolean = false)(c: Country): Boolean = c match {
    case m: MuslimCountry    => muslim
    case n: NonMuslimCountry => test(n)  
  }

  // This is a convenience method used when selecting targets for events.
  def multipleTargets(num: Int, candidates: List[String], pickBest: (List[String]) => Option[String]): List[String] = {
    def nextTarget(n: Int, targets: List[String]): List[String] = {
      if (n <= num && targets.nonEmpty) {
        pickBest(candidates) match {
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