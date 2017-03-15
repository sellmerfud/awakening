
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

import LabyrinthAwakening._

// Common routines used by both of the Bots.

trait BotHelpers {
  def botLog(msg: => String) = if (game.params.botLogging) log(msg)
  
  // Helper functions used to make it easier to construct Priorities and FlowchartFilters

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