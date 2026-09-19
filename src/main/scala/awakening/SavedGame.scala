
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

import java.io.IOException
import FUtil.Pathname
import LabyrinthAwakening._

object SavedGame {
  val CurrentFileVersion = 3
  val CurrentLogVersion  = 1

  def save(filepath: Pathname, gameState: GameState): Unit = {
    try {
      filepath.writeFile(toJson(gameState))
    }
    catch {
      case e: IOException =>
        val suffix = if (e.getMessage == null) "" else s": ${e.getMessage}"
        println(s"IO Error writing saved game ($filepath)$suffix")
      case e: Throwable =>
        val suffix = if (e.getMessage == null) "" else s": ${e.getMessage}"
        println(s"Error writing saved game ($filepath)$suffix")
    }
  }

  private def toJson(gameState: GameState): String = {
    val top = Json.Obj(
      "file-version"     -> CurrentFileVersion,
      "software-version" -> SOFTWARE_VERSION,
      "game-state"       -> gameStateToObj(gameState)
    )
    Json.output(top, indent = Some(2))
  }

  // The path should be the full path to the file to load.
  // Will set the game global variable
  def load(filepath: Pathname): GameState = {
    try fromJson(filepath.readFile())
    catch {
      case e: IOException =>
        val suffix = if (e.getMessage == null) "" else s": ${e.getMessage}"
        throw new IllegalStateException(s"IO Error reading saved game ($filepath)$suffix", e)
      case e: Throwable =>
        val suffix = if (e.getMessage == null) "" else s": ${e.getMessage}"
        throw new IllegalStateException(s"Error reading saved game ($filepath)$suffix", e)
    }
  }

  private def fromJson(jsonValue: String): GameState = {
    val top = Json.parse(jsonValue).obj
    if (!top.contains("file-version"))
      throw new IllegalArgumentException(s"Invalid save file - No file version number")

    if (!top.contains("game-state"))
      throw new IllegalArgumentException(s"Invalid save file - No game-state")

    top("file-version").num.toInt match {
      case CurrentFileVersion => gameFromCurrentVersion(top("game-state"))
      case v => throw new IllegalArgumentException(s"Invalid save file version: $v")
    }
  }

  private def plotDataToObj(data: PlotData) =
    Json.Obj(
      "availablePlots"        -> Json.Arr.from(data.availablePlots.map(_.name)),
      "resolvedPlots"         -> Json.Arr.from(data.resolvedPlots.map(_.name)),
      "removedPlots"          -> Json.Arr.from(data.removedPlots.map(_.name)),
      "resolvedTargets"       -> Json.Arr.from(data.resolvedTargets.toList.map(plotTargetToObj)),
      "resolvedInGreenOnBlue" -> data.resolvedInGreenOnBlue
    )

  private def plotDataFromObj(data: Json.Value): PlotData =
    PlotData(
      data.obj("availablePlots").arr.map(p => Plot.apply(p.str)).toList,
      data.obj("resolvedPlots").arr.map(p => Plot.apply(p.str)).toList,
      data.obj("removedPlots").arr.map(p => Plot.apply(p.str)).toList,
      data.obj("resolvedTargets").arr.map(plotTargetFromObj).toSet,
      data.obj("resolvedInGreenOnBlue").bool
    )

  private def plotTargetToObj(t: PlotTarget) = 
    Json.Obj("name" -> t.name, "isMuslim" -> t.isMuslim)

  private def plotTargetFromObj(data: Json.Value): PlotTarget = 
    PlotTarget(data.obj("name").str, data.obj("isMuslim").bool)

  private def plotOnMapToObj(pom: PlotOnMap) =
    Json.Obj("plot" -> pom.plot.name, "backlashed" -> pom.backlashed)

  private def plotOnMapFromObj(data: Json.Value): PlotOnMap =
    PlotOnMap(Plot(data.obj("plot").str), data.obj("backlashed").bool)

  private def reservesToObj(data: Reserves) =
    Json.Obj("us" -> data.us, "jihadist" -> data.jihadist)

  private def reservesFromObj(data: Json.Value): Reserves =
    Reserves(data.obj("us").num.toInt, data.obj("jihadist").num.toInt)

  private def lapsingEventToObj(data: LapsingEntry) =
    Json.Obj("cardNumber" -> data.cardNumber, "discarded" -> data.discarded)

  private def lapsingEventFromObj(data: Json.Value): LapsingEntry =
    LapsingEntry(data.obj("cardNumber").num.toInt, data.obj("discarded").bool)


  private def cardsInUseToObj(data: Range) =
    Json.Obj("min" -> data.head, "max" -> data.last)

  private def cardsInUseFromObj(data: Json.Value): Range =
    Range.inclusive(data.obj("min").num.toInt, data.obj("max").num.toInt)

  private def cardsInHandToObj(data: CardsInHand) =
    Json.Obj("us" -> data.us, "jihadist" -> data.jihadist)

  private def cardsInHandFromObj(data: Json.Value): CardsInHand =
    CardsInHand(data.obj("us").num.toInt, data.obj("jihadist").num.toInt)
    
  private def phaseTargetsToObj(data: PhaseTargets) =
    Json.Obj(
      "ops"                          -> Json.Arr.from(data.ops),
      "disrupted"                    -> Json.Arr.from(data.disrupted),
      "testedOrImprovedToFairOrGood" -> Json.Arr.from(data.testedOrImprovedToFairOrGood),
      "event"                        -> Json.Arr.from(data.event)
    )

  private def phaseTargetsFromObj(data: Json.Value): PhaseTargets =
    PhaseTargets(
      data.obj("ops").arr.map(_.str).toSet,
      data.obj("disrupted").arr.map(_.str).toSet,
      data.obj("testedOrImprovedToFairOrGood").arr.map(_.str).toSet,
      data.obj("event").arr.map(_.str).toSet
    )

  private def turnActionToObj(turnAction: TurnAction) = {
    val params = turnAction match {
      case PlayedCard(role, cardNum, None) =>
        Json.Obj("role" -> role.toString, "cardNum" -> cardNum)
      case PlayedCard(role, cardNum, Some(SecondCard(card2))) =>
        Json.Obj("role" -> role.toString, "cardNum" -> cardNum, "secondCardNum" -> card2)
      case PlayedCard(role, cardNum, Some(AdditionalCard(card2))) =>
        Json.Obj("role" -> role.toString, "cardNum" -> cardNum, "additionalCardNum" -> card2)
      case PlayedReassement(card1, card2) =>
        Json.Obj("card1" -> card1, "card2"   -> card2)
      case VoluntaryCadreRemoval(num) =>
        Json.Obj("num" -> Json.Num(num))
      case USDiscardedLastCard(cardNum) =>
        Json.Obj("cardNum" -> Json.Num(cardNum))
      case EndOfActionPhase(role, phaseNum, numPlots) =>
        Json.Obj("role" -> role.toString, "phaseNum" -> phaseNum, "numPlots" -> numPlots)
      case AdjustmentMade(desc) =>
        Json.Obj("desc" -> Json.Str(desc))
    }
    Json.Obj("actionType" -> turnAction.name, "params" -> params)
  }

  private def turnActionFromObj(data: Json.Value): TurnAction = {
    val params = data.obj("params").obj
    data.obj("actionType").str match {
      case "PlayedCard" if params.contains("secondCardNum") && !params("secondCardNum").isNull =>
        PlayedCard(Role(params("role").str), params("cardNum").num.toInt, Some(SecondCard(params("secondCardNum").num.toInt)))
      case "PlayedCard" if params.contains("additionalCardNum") && !params("additionalCardNum").isNull =>
        PlayedCard(Role(params("role").str), params("cardNum").num.toInt, Some(AdditionalCard(params("additionalCardNum").num.toInt)))
      case "PlayedCard" =>
        PlayedCard(Role(params("role").str), params("cardNum").num.toInt, None)
      // Account for misspelling in earlier versions
      case "PlayedReassessment" | "PlayedReassement" =>
        PlayedReassement(params("card1").num.toInt, params("card2").num.toInt)
      case "VoluntaryCadreRemoval" =>
        VoluntaryCadreRemoval(params("num").num.toInt)
      case "USDiscardedLastCard" =>
        USDiscardedLastCard(params("cardNum").num.toInt)
      case "EndOfActionPhase" =>
        EndOfActionPhase(
          Role(params("role").str), params("phaseNum").num.toInt, params("numPlots").num.toInt)
      case "AdjustmentMade" =>
        AdjustmentMade(params("desc").str)
    }
  }

  private def countryToObj(country: Country) = {
    val (countryType, params) = country match {
      case m: MuslimCountry =>
        ("MuslimCountry", Json.Obj(
          "name"             -> m.name,
          "governance"       -> m.governance,
          "pieces"           -> Json.Arr.from(m.pieces.explode().map(_.name)),
          "cadres"           -> m.cadres,
          "plots"            -> Json.Arr.from(m.plots.map(plotOnMapToObj)),
          "markers"          -> Json.Arr.from(m.markers.map(_.name)),
          "isSunni"          -> m.isSunni,
          "resources"        -> m.printedResources,
          "alignment"        -> m.alignment,
          "oilExporter"      -> m.oilExporter,
          "aidMarkers"       -> m.aidMarkers,
          "regimeChange"     -> m.regimeChange,
          "besiegedRegime"   -> m.besiegedRegime,
          "civilWar"         -> m.civilWar,
          "awakening"        -> m.awakening,
          "reaction"         -> m.reaction,
          "wmdCache"         -> m.wmdCache,
          "truce"            -> m.truce
        ))
      case n: NonMuslimCountry =>
        ("NonMuslimCountry", Json.Obj(
          "name"            -> n.name,
          "governance"      -> n.governance,
          "pieces"          -> Json.Arr.from(n.pieces.explode().map(_.name)),
          "cadres"          -> n.cadres,
          "plots"           -> Json.Arr.from(n.plots.map(plotOnMapToObj)),
          "markers"         -> Json.Arr.from(n.markers.map(_.name)),
          "postureValue"    -> n.postureValue,
          "recruitOverride" -> n.recruitOverride,
          "wmdCache"        -> n.wmdCache,
          "iranSpecialCase" -> n.iranSpecialCase
        ))
    }
    Json.Obj("countryType" -> countryType, "params" -> params)
  }


  private def countryFromObj(data: Json.Value): Country = {
    val params = data.obj("params").obj
    data.obj("countryType").str match {
      case "MuslimCountry" =>
        MuslimCountry(
          params("name").str,
          params("governance").num.toInt,
          Pieces.fromTypes(params("pieces").arr.map(name => PieceType(name.str))),
          params("cadres").num.toInt,
          params("plots").arr.map(plotOnMapFromObj).toList,
          params("markers").arr.map(name => CountryMarker(name.str)).toList,
          params("isSunni").bool,
          params("resources").num.toInt,
          params("alignment").str,
          params("oilExporter").bool,
          params("aidMarkers").num.toInt,
          params("regimeChange").str,
          params("besiegedRegime").bool,
          params("civilWar").bool,
          params("awakening").num.toInt,
          params("reaction").num.toInt,
          params("wmdCache").num.toInt,
          params("truce").bool
        )
      case "NonMuslimCountry" =>
        NonMuslimCountry(
          params("name").str,
          params("governance").num.toInt,
          Pieces.fromTypes(params("pieces").arr.map(name => PieceType(name.str))),
          params("cadres").num.toInt,
          params("plots").arr.map(plotOnMapFromObj).toList,
          params("markers").arr.map(name => CountryMarker(name.str)).toList,
          params("postureValue").str,
          params("recruitOverride").num.toInt,
          params("wmdCache").num.toInt,
          params("iranSpecialCase").bool
        )

      case x => throw new IllegalArgumentException(s"Invalid country type: $x")
    }
  }

  private def gameStateToObj(gameState: GameState) = {
    Json.Obj(
      "scenarioName"         -> gameState.scenarioName,
      "scenarioNotes"        -> Json.Arr.from(gameState.scenarioNotes),
      "startingMode"         -> gameState.startingMode.toString,
      "gameLength"           -> gameState.gameLength,
      "deckNumber"           -> gameState.deckNumber,
      "campaign"             -> gameState.campaign,
      "currentMode"          -> gameState.currentMode.toString,
      "humanRole"            -> gameState.humanRole.toString,
      "humanAutoRoll"        -> gameState.humanAutoRoll,
      "botDifficulties"      -> Json.Arr.from(gameState.botDifficulties.map(_.name)),
      "turn"                 -> gameState.turn,
      "prestige"             -> gameState.prestige,
      "usPosture"            -> gameState.usPosture,
      "funding"              -> gameState.funding,
      "countries"            -> Json.Arr.from(gameState.countries.map(countryToObj)),
      "markers"              -> Json.Arr.from(gameState.markers.map(_.name)),
      "plotData"             -> plotDataToObj(gameState.plotData),
      "offMapTroops"         -> gameState.offMapTroops,
      "reserves"             -> reservesToObj(gameState.reserves),
      "cardsInUse"           -> cardsInUseToObj(gameState.cardsInUse),
      "cardsInHand"          -> cardsInHandToObj(gameState.cardsInHand),
      "turnActions"          -> Json.Arr.from(gameState.turnActions.map(turnActionToObj)),
      "firstPlotEntry"       -> gameState.firstPlotEntry.map(lapsingEventToObj).getOrElse(Json.Null),
      "eventsLapsing"        -> Json.Arr.from(gameState.eventsLapsing.map(lapsingEventToObj)),
      "cardsDiscarded"       -> Json.Arr.from(gameState.cardsDiscarded),
      "cardsRemoved"         -> Json.Arr.from(gameState.cardsRemoved),
      "targetsThisPhase"     -> phaseTargetsToObj(gameState.targetsThisPhase),
      "targetsLastPhase"     -> phaseTargetsToObj(gameState.targetsLastPhase),
      "caliphateCapital"     -> game.caliphateCapital.map(Json.Str).getOrElse(Json.Null),
      "ignoreVictory"        -> game.ignoreVictory,
      "botLogging"           -> gameState.botLogging,
      "botEnhancements"      -> gameState.botEnhancements,
      "enhBotDifficulty"     -> gameState.enhBotDifficulty.toString,
      "manualDieRolls"       -> gameState.manualDieRolls,
      "history"              -> Json.Arr.from(gameState.history.map(gameSegmentToObj)),
      "description"          -> gameState.description,
      "showColor"            -> gameState.showColor
      )
  }
  
  private def gameSegmentToObj(seg: GameSegment) =
    Json.Obj(
      "saveNumber" -> seg.saveNumber,
      "endOfTurn"  -> seg.endOfTurn,
      "summary"    -> Json.Arr.from(seg.summary)
    )

  private def gameSegmentFromObj(data: Json.Value): GameSegment = {
    GameSegment(
      data.obj("saveNumber").num.toInt,
      data.obj("endOfTurn").bool,
      data.obj("summary").arr.map(_.str).toList
    )
  }
  // Note: We no longer support save file versions less than 3.
  private def gameFromCurrentVersion(data: Json.Value): GameState = {
    val caliphateCapital: Option[String] = if (data.obj("caliphateCapital").isNull)
      None
    else
      Some(data.obj("caliphateCapital").str)
    val firstPlotEntry: Option[LapsingEntry] = if (data.obj("firstPlotEntry").isNull)
      None
    else
      Some(lapsingEventFromObj(data.obj("firstPlotEntry")))

    GameState(
      data.obj("scenarioName").str,
      data.obj("scenarioNotes").arr.map(_.str).toSeq,
      GameMode(data.obj("startingMode").str),
      data.obj("gameLength").num.toInt,
      data.obj("deckNumber").num.toInt,
      data.obj("campaign").bool,
      GameMode(data.obj("currentMode").str),
      Role(data.obj("humanRole").str),
      data.obj("humanAutoRoll").bool,
      data.obj("botDifficulties").arr.map(x => BotDifficulty(x.str)).toList,
      data.obj("turn").num.toInt,
      data.obj("prestige").num.toInt,
      data.obj("usPosture").str,
      data.obj("funding").num.toInt,
      data.obj("countries").arr.map(countryFromObj).toList,
      data.obj("markers").arr.map(x => GlobalMarker(x.str)).toList,
      plotDataFromObj(data.obj("plotData")),
      data.obj("offMapTroops").num.toInt,
      reservesFromObj(data.obj("reserves")),
      cardsInUseFromObj(data.obj("cardsInUse")),
      cardsInHandFromObj(data.obj("cardsInHand")),
      data.obj("turnActions").arr.map(turnActionFromObj).toList,
      firstPlotEntry,
      data.obj("eventsLapsing").arr.map(lapsingEventFromObj).toList,
      data.obj("cardsDiscarded").arr.map(_.num.toInt).toList,
      data.obj("cardsRemoved").arr.map(_.num.toInt).toList,
      phaseTargetsFromObj(data.obj("targetsThisPhase")),
      phaseTargetsFromObj(data.obj("targetsLastPhase")),
      caliphateCapital,
      data.obj("ignoreVictory").bool,
      data.obj("botLogging").bool,
      data.obj("botEnhancements").bool,
      EnhBotDifficulty.fromString(data.obj("enhBotDifficulty").str),
      data.obj("manualDieRolls").bool,
      data.obj("history").arr.map(gameSegmentFromObj).toVector,
      data.obj("description").str,
      data.obj.get("showColor").map(_.bool).getOrElse(true)
    )
  }

  // Methods to save and load the log files
  private def logEntryToObj(entry: LogEntry) =
    Json.Obj(
      "text" -> entry.text,
      "color" -> entry.color.map(x => Json.Str(x.name)).getOrElse(Json.Null)
    )

  private def logEntryFromObj(data: Json.Value): LogEntry = {
    val color: Option[Color] = if (data.obj("color").isNull)
      None
    else
      Some(Color.fromName(data.obj("color").str));

    LogEntry(data.obj("text").str, color)
  }

  private def logToJson(entries: Vector[LogEntry]): String = {
    val top = Json.Obj(
      "file-version"     -> CurrentLogVersion,
      "software-version" -> SOFTWARE_VERSION,
      "log"              -> Json.Arr.from(entries.map(logEntryToObj))
    )
    Json.output(top, Some(2))
  }

  private def logFromVersion1(entries: Json.Value): Vector[LogEntry] = {
    entries.arr.map(logEntryFromObj).toVector
  }

  def saveLog(filepath: Pathname, entries: Vector[LogEntry]): Unit = {
    try {
      filepath.writeFile(logToJson(entries))
    }
    catch {
      case e: IOException =>
        val suffix = if (e.getMessage == null) "" else s": ${e.getMessage}"
        println(s"IO Error writing log file ($filepath)$suffix")
      case e: Throwable =>
        val suffix = if (e.getMessage == null) "" else s": ${e.getMessage}"
        println(s"Error writing log file ($filepath)$suffix")
    }
  }

  private def logFromJson(jsonValue: String): Vector[LogEntry] = {
      val top = Json.parse(jsonValue).obj
      if (!top.contains("file-version"))
        throw new IllegalArgumentException(s"Invalid save file - missing file version number")

      if (!top.contains("log"))
        throw new IllegalArgumentException(s"Invalid save file - missing log entries")

      top("file-version").num.toInt match {
        case 1 => logFromVersion1(top("log"))
        case v => throw new IllegalArgumentException(s"Invalid log file version: $v")
      }
  }


   // The path should be the full path to the file to load.
  // Will set the game global variable
  def loadLog(filepath: Pathname): Vector[LogEntry] = {
    try logFromJson(filepath.readFile())
    catch {
      case e: Json.JsonException =>
          // Older versions did not store the log as json
        // If we cannot parse the file then treat it as a regular
        // text file.
        filepath.readLines().toVector.map { line =>
          LogEntry(line, None)
        }
      case e: IOException =>
        val suffix = if (e.getMessage == null) "" else s": ${e.getMessage}"
        throw new IllegalStateException(s"IO Error reading log file ($filepath)$suffix", e)
      case e: Throwable =>
        val suffix = if (e.getMessage == null) "" else s": ${e.getMessage}"
        throw new IllegalStateException(s"Error reading log file ($filepath)$suffix", e)
    }
  }
}

