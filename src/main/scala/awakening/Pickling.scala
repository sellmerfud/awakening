
// Labyrinth Awakening
//
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

import scala.pickling.Defaults._
import scala.pickling.binary
import scala.pickling.binary._
import scala.pickling.shareNothing._
import java.io.IOException
import FUtil.Pathname

// This object handles the pickling and saving a GameState to disk,
// as well as loading pickled file and unpickling it.
// I moved this into a separate file because the scala.pickling library
// relies on compiler macros which drastically slow the compile time.
// Since this file rarely changes, sbt will not have to recompile it often.

object Pickling {
  import LabyrinthAwakening.{ GameState, game, logStartOfTurn }
  // Save the current game state.
  def saveGameState(filepath: Pathname): Unit = {
    try {
      filepath.dirname.mkpath() // Make sure that the game directory exists
      filepath.write(game.pickle.value)
    }
    catch {
      case e: IOException =>
        val suffix = if (e.getMessage == null) "" else s": ${e.getMessage}"
        println(s"IO Error writing game file ($filepath)$suffix")
      case e: Throwable =>
        val suffix = if (e.getMessage == null) "" else s": ${e.getMessage}"
        println(s"Error writing save game ($filepath)$suffix")
    }
  }
  
  // The path should be the full path to the file to load.
  // Will set the game global variable
  def loadGameState(filepath: Pathname): Unit = {
    try {
      filepath.inputStream { istream =>
        game = BinaryPickle(istream).unpickle[GameState]
      }
      // If we load a game with no plays then it represents
      // the end of a turn so we must advance the turn number.
      if (game.plays.isEmpty) {
        game = game.copy(turn = game.turn + 1)
        logStartOfTurn()
      }
    }
    catch {
      case e: IOException =>
        val suffix = if (e.getMessage == null) "" else s": ${e.getMessage}"
        println(s"IO Error reading game file ($filepath)$suffix")
        sys.exit(1)
      case e: Throwable =>
        val suffix = if (e.getMessage == null) "" else s": ${e.getMessage}"
        println(s"Error reading save game ($filepath)$suffix")
        sys.exit(1)
    }
  }
}