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

// Card Text:
// ------------------------------------------------------------------
// Play if Gülen Movement in effect and Turkey is Tested.
// If US play: Improve Turkish Governance 1 level OR Shift Alignment
// one box towards Ally:
// If Jihadist: Worsen Turkish Governance 1 level OR Shift Alignment
// one box towards Adversary. REMOVE
// ------------------------------------------------------------------
object Card_349 extends Card(349, "Turkish Coup", Unassociated, 2, JihadistRemove, NoLapsing, NoAutoTrigger) {
  // Used by the US Bot to determine if the executing the event would alert a plot
  // in the given country
  override
  def eventAlertsPlot(countryName: String, plot: Plot): Boolean = false

  // Used by the US Bot to determine if the executing the event would remove
  // the last cell on the map resulting in victory.
  override
  def eventRemovesLastCell(): Boolean = false

  // Returns true if the printed conditions of the event are satisfied
  override
  def eventConditionsMet(role: Role) =
    globalEventInPlay(GulenMovement) && game.getMuslim(Turkey).isTested

  // Returns true if the Bot associated with the given role will execute the event
  // on its turn.  This implements the special Bot instructions for the event.
  // When the event is triggered as part of the Human players turn, this is NOT used.
  override
  def botWillPlayEvent(role: Role): Boolean = {
    val turkey = game.getMuslim(Turkey)
    role match {
      case US => !turkey.isGood || !turkey.isAlly
      case Jihadist => !turkey.isAdversary || !turkey.isIslamistRule
    }
  }

  // Carry out the event for the given role.
  // forTrigger will be true if the event was triggered during the human player's turn
  // and it associated with the Bot player.
  override
  def executeEvent(role: Role, forTrigger: Boolean): Unit = {
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
      case _  if isHuman(role) && choices.isEmpty => "none"
      case _  if isHuman(role)                    => askMenu("Choose one:", choices).head
      case US if !turkey.isGood                   => "improve"
      case US                                     => "left"
      case Jihadist if !turkey.isIslamistRule     => "worsen"
      case Jihadist                               => "right"
    }

    addEventTarget(Turkey)
    action match {
      case "improve" => improveGovernance(Turkey, 1, canShiftToGood = true)
      case "worsen"  => worsenGovernance(Turkey, 1, canShiftToIR = true)
      case "left"    => shiftAlignmentLeft(Turkey)
      case "right"   => shiftAlignmentRight(Turkey)
      case _         => log("\nThe event has no effect.", Color.Event)
    }
  }
}
