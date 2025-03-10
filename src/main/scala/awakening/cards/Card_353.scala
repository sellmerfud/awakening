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

import scala.util.Random.shuffle
import awakening.LabyrinthAwakening._
import awakening.JihadistBot

// Card Text:
// ------------------------------------------------------------------
// Play if there is a Lapsing or Marked event in play.
// Immediately end that Event and Discard or Remove it as appropriate.
// Draw a Card.
// Cannot be played in a Caliphate country.
// ------------------------------------------------------------------
object Card_353 extends Card(353, "Bowling Green Massacre", Unassociated, 3, NoRemove, NoLapsing, NoAutoTrigger) {
  // Used by the US Bot to determine if the executing the event would alert a plot
  // in the given country
  override
  def eventAlertsPlot(countryName: String, plot: Plot): Boolean = false

  // Used by the US Bot to determine if the executing the event would remove
  // the last cell on the map resulting in victory.
  override
  def eventRemovesLastCell(): Boolean = false

  def bowlingGreenTargetEventInPlay: Boolean =
    game.markers.nonEmpty ||
    game.countries.exists(c => c.markers.nonEmpty && !game.isCaliphateMember(c.name)) ||
    game.eventsLapsing.nonEmpty

  def bowlingGreenBotMarkers(role: Role): List[String] = {
    val globalMarkers = game.markers
      .filter(GlobalMarkers(_) == role.opponent)
    val countryMarkers =
      game.countries
      .filterNot(c => game.isCaliphateMember(c.name))
      .flatMap(_.markers)
      .filter(CountryMarkers(_) == role.opponent)
    (globalMarkers ::: countryMarkers).sorted.distinct
  }

  def bowlingGreenBotLapsing(role: Role): List[Int] =
    game.eventsLapsing
      .filter { event =>
        val card = deck(event.cardNumber)
        (role == US && card.association == Jihadist || card.lapsing == JihadistLapsing) ||
        (role == Jihadist && card.association == US || card.lapsing == USLapsing)
      }
      .map(_.cardNumber)

  // Returns true if the printed conditions of the event are satisfied
  override
  def eventConditionsMet(role: Role) = bowlingGreenTargetEventInPlay

  // Returns true if the Bot associated with the given role will execute the event
  // on its turn.  This implements the special Bot instructions for the event.
  // When the event is triggered as part of the Human players turn, this is NOT used.
  override
  def botWillPlayEvent(role: Role): Boolean =
    bowlingGreenBotMarkers(role).nonEmpty || bowlingGreenBotLapsing(role).nonEmpty

  // Carry out the event for the given role.
  // forTrigger will be true if the event was triggered during the human player's turn
  // and it associated with the Bot player.
  override
  def executeEvent(role: Role): Unit = {
    val countryMarkers = game.countries
      .filterNot(c => game.isCaliphateMember(c.name))
      .flatMap (_.markers)
    val markerChoices = (game.markers ::: countryMarkers)
      .sorted
      .distinct
      .map(m => m -> m)
    val lapsingChoices = game.eventsLapsing
      .map(event => event.cardNumber -> deck(event.cardNumber).numAndName)
    sealed trait Choice
    case object Marker extends Choice
    case object Lapsing extends Choice
    val eventTypechoices = List(
      choice(markerChoices.nonEmpty, Marker,  "Remove an event marker"),
      choice(lapsingChoices.nonEmpty, Lapsing, "Remove a lapsing card")
    ).flatten

    // Left: marker name, Right: Lapsing card number
    val eventSelection: Either[String, Int] = if (isHuman(role)) {
      askMenu("Choose one:", eventTypechoices).head match {
        case Marker =>
          Left(askMenu("Remove which event marker:", markerChoices).head)
        case Lapsing =>
          Right(askMenu[Int]("Remove which lapsing card: ", lapsingChoices).head)
      }
    }
    else { // Bot will choose event marker first, then lapsing event
      val markers = bowlingGreenBotMarkers(role)
      val lapsing = bowlingGreenBotLapsing(role)
      if (markers.nonEmpty)
        Left(shuffle(markers).head)
      else
        Right(shuffle(lapsing).head)
    }

    eventSelection match {
      case Left(marker) if GlobalMarkers.contains(marker) =>
        removeGlobalEventMarker(marker)

      case Left(marker) =>
        // Advisors marker can exist in multiple countries
        val target = countryNames(game.countries.filter(_.hasMarker(marker))) match {
          case single::Nil => single
          case candidates if isHuman(role) => askCountry(s"""Remove "$marker" from which country: """, candidates)
          case candidates => JihadistBot.troopsMilitiaTarget(candidates).get
        }
        addEventTarget(target)
        removeEventMarkersFromCountry(target, marker)

      case Right(lapsingCardNum) =>
        removeLapsingEvent(lapsingCardNum)
    }

    log(s"\n$role player draws a card.", Color.Event)
    askCardDrawnFromDrawPile(role)
      .map(deck(_).numAndName)
      .foreach { cardDisplay =>
        if (isHuman(role))
          log(s"\nAdd $cardDisplay to your ($role) hand.", Color.Event)
        else if (game.botEnhancements)
          log(s"\nShuffle $cardDisplay into the $role Bot's hand of cards.", Color.Event)
        else
          log(s"\nPlace $cardDisplay on top of the $role Bot's hand of cards.", Color.Event)
      }
  }
}
