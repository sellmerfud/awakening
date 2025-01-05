
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
  val CurrentFileVersion = 2
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
    val top = Map(
      "file-version"     -> CurrentFileVersion,
      "software-version" -> SOFTWARE_VERSION,
      "game-state"       -> gameStateToMap(gameState)
    )
    Json.build(top)
  }

  // // The path should be the full path to the file to load.
  // // Will set the game global variable
  // def load_old(filepath: Pathname): GameState = {
  //   val gs = try fromGameJson(filepath.readFile())
  //   catch {
  //     case e: IOException =>
  //       val suffix = if (e.getMessage == null) "" else s": ${e.getMessage}"
  //       println(s"IO Error reading saved game ($filepath)$suffix")
  //       sys.exit(1)
  //     case e: Throwable =>
  //       val suffix = if (e.getMessage == null) "" else s": ${e.getMessage}"
  //       println(s"Error reading saved game ($filepath)$suffix")
  //       sys.exit(1)
  //   }
  //   // If there are no plays then this a save file for the
  //   // end of the turn.  In this case we increment the turn counter
  //   if (gs.plays.isEmpty)
  //     gs.copy(turn = gs.turn + 1)
  //   else
  //     gs
  // }

  // The path should be the full path to the file to load.
  // Will set the game global variable
  def load(filepath: Pathname): GameState = {
    try fromJson(filepath.readFile())
    catch {
      case e: IOException =>
        val suffix = if (e.getMessage == null) "" else s": ${e.getMessage}"
        println(s"IO Error reading saved game ($filepath)$suffix")
        sys.exit(1)
      case e: Throwable =>
        val suffix = if (e.getMessage == null) "" else s": ${e.getMessage}"
        println(s"Error reading saved game ($filepath)$suffix")
        sys.exit(1)
    }
  }

  private def fromJson(jsonValue: String): GameState = {
    val top = asMap(Json.parse(jsonValue))
    if (!top.contains("file-version"))
      throw new IllegalArgumentException(s"Invalid save file - No file version number")

    if (!top.contains("game-state"))
      throw new IllegalArgumentException(s"Invalid save file - No game-state")

    asInt(top("file-version")) match {
      case 3 => gameFromVersion3(asMap(top("game-state")))
      case v => throw new IllegalArgumentException(s"Invalid save file version: $v")
    }
  }

  private def asString(x: Any): String = x.toString

  private def asBoolean(x: Any): Boolean = x match {
    case b: Boolean => b
    case _          => throw new Exception(s"Not a valid Boolean value: $x")
  }

  private def asInt(x: Any): Int = x match {
    case i: Int => i
    case _      => throw new Exception(s"Not a valid Integer value: $x")
  }

  private def asMap(x: Any): Map[String, Any] = x match {
    case m: Map[_, _] => m.asInstanceOf[Map[String, Any]]
    case _      => throw new Exception(s"Not a valid Map value!")
  }
  private def asList(x: Any): List[Any] = x match {
    case l: List[_] => l.asInstanceOf[List[Any]]
    case _      => throw new Exception(s"Not a valid List value!")
  }


  private def plotDataToMap(data: PlotData): Map[String, Any] =
    Map(
      "availablePlots"        -> (data.availablePlots map (_.name)),
      "resolvedPlots"         -> (data.resolvedPlots map (_.name)),
      "removedPlots"          -> (data.removedPlots map (_.name)),
      "resolvedTargets"       -> data.resolvedTargets.toList.map(plotTargetToMap),
      "resolvedInGreenOnBlue" -> data.resolvedInGreenOnBlue
    )

  private def plotDataFromMap(data: Map[String, Any]): PlotData =
    PlotData(
      asList(data("availablePlots")) map (x => Plot.apply(asString(x))),
      asList(data("resolvedPlots")) map (x => Plot.apply(asString(x))),
      asList(data("removedPlots")) map (x => Plot.apply(asString(x))),
      (asList(data("resolvedTargets")) map (x => plotTargetFromMap(asMap(x)))).toSet,
      asBoolean(data("resolvedInGreenOnBlue"))
    )

  private def plotTargetToMap(t: PlotTarget): Map[String, Any] = 
    Map("name" -> t.name, "isMuslim" -> t.isMuslim)

  private def plotTargetFromMap(data: Map[String, Any]): PlotTarget = 
    PlotTarget(asString(data("name")), asBoolean(data("isMuslim")))

  private def plotOnMapToMap(pom: PlotOnMap): Map[String, Any] =
    Map("plot" -> pom.plot.name, "backlashed" -> pom.backlashed)

  private def plotOnMapFromMap(data: Map[String, Any]): PlotOnMap =
    PlotOnMap(Plot(asString(data("plot"))), asBoolean(data("backlashed")))

  private def reservesToMap(data: Reserves): Map[String, Any] =
    Map("us" -> data.us, "jihadist" -> data.jihadist)

  private def reservesFromMap(data: Map[String, Any]): Reserves =
    Reserves(asInt(data("us")), asInt(data("jihadist")))

  private def cardsInHandToMap(data: CardsInHand): Map[String, Any] =
    Map("us" -> data.us, "jihadist" -> data.jihadist)

  private def cardsInHandFromMap(data: Map[String, Any]): CardsInHand =
    CardsInHand(asInt(data("us")), asInt(data("jihadist")))

  private def phaseTargetsToMap(data: PhaseTargets): Map[String, Any] =
    Map(
      "ops"                          -> data.ops,
      "disrupted"                    -> data.disrupted,
      "testedOrImprovedToFairOrGood" -> data.testedOrImprovedToFairOrGood,
      "event"                        -> data.event
    )

  private def phaseTargetsFromMap(data: Map[String, Any]): PhaseTargets =
    PhaseTargets(
      (asList(data("ops")) map asString).toSet,
      (asList(data("disrupted")) map asString).toSet,
      (asList(data("testedOrImprovedToFairOrGood")) map asString).toSet,
      (asList(data("event")) map asString).toSet
    )

  private def playToMap(play: Play): Map[String, Any] = {
    val params = play match {
      case PlayedCard(role, cardNum, None) =>
        Map("role" -> role.toString, "cardNum" -> cardNum)
      case PlayedCard(role, cardNum, Some(SecondCard(card2))) =>
        Map("role" -> role.toString, "cardNum" -> cardNum, "secondCardNum" -> card2)
      case PlayedCard(role, cardNum, Some(AdditionalCard(card2))) =>
        Map("role" -> role.toString, "cardNum" -> cardNum, "additionalCardNum" -> card2)
      case PlayedReassement(card1, card2) =>
        Map("card1" -> card1, "card2"   -> card2)
      case PlotsResolved(num) =>
        Map("num" -> num)
      case AdjustmentMade(desc) =>
        Map("desc" -> desc)
    }
    Map("playType" -> play.name, "params" -> params)
  }

  private def playFromMap(data: Map[String, Any]): Play = {
    val params = asMap(data("params"))
    asString(data("playType")) match {
      case "PlayedCard" if params.contains("secondCardNum") && params("secondCardNum") != null =>
        PlayedCard(Role(asString(params("role"))), asInt(params("cardNum")), Some(SecondCard(asInt(params("secondCardNum")))))
      case "PlayedCard" if params.contains("additionalCardNum") && params("additionalCardNum") != null =>
        PlayedCard(Role(asString(params("role"))), asInt(params("cardNum")), Some(AdditionalCard(asInt(params("additionalCardNum")))))
      case "PlayedCard" =>
        PlayedCard(Role(asString(params("role"))), asInt(params("cardNum")), None)
      // Account for misspelling in earlier versions
      case "PlayedReassessment" | "PlayedReassement" =>
        PlayedReassement(asInt(params("card1")), asInt(params("card2")))
      case "PlotsResolved" =>
        PlotsResolved(asInt(params("num")))
      case "AdjustmentMade" =>
        AdjustmentMade(asString(params("desc")))
    }
  }

  private def countryToMap(country: Country): Map[String, Any] = {
    val (countryType, params) = country match {
      case m: MuslimCountry =>
        ("MuslimCountry", Map(
          "name"             -> m.name,
          "governance"       -> m.governance,
          "sleeperCells"     -> m.sleeperCells,
          "activeCells"      -> m.activeCells,
          "hasCadre"         -> m.hasCadre,
          "plots"            -> (m.plots map plotOnMapToMap),
          "markers"          -> m.markers,
          "isSunni"          -> m.isSunni,
          "resources"        -> m.printedRsources,
          "alignment"        -> m.alignment,
          "troops"           -> m.troops,
          "militia"          -> m.militia,
          "oilExporter"      -> m.oilExporter,
          "aidMarkers"       -> m.aidMarkers,
          "regimeChange"     -> m.regimeChange,
          "besiegedRegime"   -> m.besiegedRegime,
          "civilWar"         -> m.civilWar,
          "caliphateCapital" -> m.caliphateCapital,
          "awakening"        -> m.awakening,
          "reaction"         -> m.reaction,
          "wmdCache"         -> m.wmdCache
        ))
      case n: NonMuslimCountry =>
        ("NonMuslimCountry", Map(
          "name"            -> n.name,
          "governance"      -> n.governance,
          "sleeperCells"    -> n.sleeperCells,
          "activeCells"     -> n.activeCells,
          "hasCadre"        -> n.hasCadre,
          "troops"          -> n.troops,
          "plots"           -> (n.plots map plotOnMapToMap),
          "markers"         -> n.markers,
          "postureValue"    -> n.postureValue,
          "recruitOverride" -> n.recruitOverride,
          "wmdCache"        -> n.wmdCache,
          "iranSpecialCase" -> n.iranSpecialCase
        ))
    }
    Map("countryType" -> countryType, "params" -> params)
  }

  private def countryFromMap(data: Map[String, Any]): Country = {
    val params = asMap(data("params"))
    asString(data("countryType")) match {
      case "MuslimCountry" =>
        MuslimCountry(
          asString(params("name")),
          asInt(params("governance")),
          asInt(params("sleeperCells")),
          asInt(params("activeCells")),
          asBoolean(params("hasCadre")),
          asList(params("plots")) map (x => plotOnMapFromMap(asMap(x))),
          asList(params("markers")) map asString,
          asBoolean(params("isSunni")),
          asInt(params("resources")),
          asString(params("alignment")),
          asInt(params("troops")),
          asInt(params("militia")),
          asBoolean(params("oilExporter")),
          asInt(params("aidMarkers")),
          asString(params("regimeChange")),
          asBoolean(params("besiegedRegime")),
          asBoolean(params("civilWar")),
          asBoolean(params("caliphateCapital")),
          asInt(params("awakening")),
          asInt(params("reaction")),
          asInt(params("wmdCache"))
        )
      case "NonMuslimCountry" =>
        NonMuslimCountry(
          asString(params("name")),
          asInt(params("governance")),
          asInt(params("sleeperCells")),
          asInt(params("activeCells")),
          asBoolean(params("hasCadre")),
          asInt(params("troops")),
          asList(params("plots")) map (x => plotOnMapFromMap(asMap(x))),
          asList(params("markers")) map asString,
          asString(params("postureValue")),
          asInt(params("recruitOverride")),
          asInt(params("wmdCache")),
          asBoolean(params("iranSpecialCase"))
        )

      case x => throw new IllegalArgumentException(s"Invalid country type: $x")
    }
  }

  private def gameStateToMap(gameState: GameState) = {
    Map(
      "scenarioName"        -> gameState.scenarioName,
      "startingMode"        -> gameState.startingMode,
      "campaign"            -> gameState.campaign,
      "scenarioNotes"       -> gameState.scenarioNotes,
      "currentMode"         -> gameState.currentMode,
      "humanRole"           -> gameState.humanRole.toString,
      "humanAutoRoll"       -> gameState.humanAutoRoll,
      "botDifficulties"     -> (gameState.botDifficulties map (_.name)),
      "turn"                -> gameState.turn,
      "prestige"            -> gameState.prestige,
      "usPosture"           -> gameState.usPosture,
      "funding"             -> gameState.funding,
      "countries"           -> (gameState.countries map countryToMap),
      "markers"             -> gameState.markers,
      "plotData"            -> plotDataToMap(gameState.plotData),
      "sequestrationTroops" -> gameState.sequestrationTroops,
      "history"             -> gameState.history,
      "offMapTroops"        -> gameState.offMapTroops,
      "reserves"            -> reservesToMap(gameState.reserves),
      "cardsInHand"         -> cardsInHandToMap(gameState.cardsInHand),
      "plays"               -> (gameState.plays map playToMap),
      "firstPlotCard"       -> (gameState.firstPlotCard getOrElse null),
      "cardsLapsing"        -> gameState.cardsLapsing,
      "cardsRemoved"        -> gameState.cardsRemoved,
      "targetsThisPhase"    -> phaseTargetsToMap(gameState.targetsThisPhase),
      "targetsLastPhase"    -> phaseTargetsToMap(gameState.targetsLastPhase),
      "ignoreVictory"       -> game.ignoreVictory,
      "botLogging"          -> gameState.botLogging,
      "botEnhancements"     -> gameState.botEnhancements,
      "manualDieRolls"      -> gameState.manualDieRolls,
      "history"             -> (gameState.history map gameSegmentToMap),
      "description"         -> gameState.description,
      "showColor"           -> gameState.showColor
      )
  }
  
  private def gameSegmentToMap(seg: GameSegment): Map[String, Any] =
    Map(
      "save_number" -> seg.save_number,
      "summary"     -> seg.summary
    )

  private def gameSegmentFromMap(data: Map[String, Any]): GameSegment = {
    GameSegment(
      asInt(data("save_number")),
      asList(data("summary")) map (_.toString)
    )
  }
  // Note: We no longer support save file versions less than 3.
  private def gameFromVersion3(data: Map[String, Any]): GameState = {
    GameState(
      asString(data("scenarioName")),
      GameMode(asString(data("startingMode"))),
      asBoolean(data("campaign")),
      asList(data("scenarioNotes")) map asString,
      GameMode(asString(data("currentMode"))),
      Role(asString(data("humanRole"))),
      asBoolean(data("humanAutoRoll")),
      asList(data("botDifficulties")) map (x => BotDifficulty(asString(x))),
      asInt(data("turn")),
      asInt(data("prestige")),
      asString(data("usPosture")),
      asInt(data("funding")),
      asList(data("countries")) map (c => countryFromMap(asMap(c))),
      asList(data("markers")) map asString,
      plotDataFromMap(asMap(data("plotData"))),
      asBoolean(data("sequestrationTroops")),
      asInt(data("offMapTroops")),
      reservesFromMap(asMap(data("reserves"))),
      cardsInHandFromMap(asMap(data("cardsInHand"))),
      asList(data("plays")) map (p => playFromMap(asMap(p))),
      if (data("firstPlotCard") == null) None else Some(asInt(data("firstPlotCard"))),
      asList(data("cardsLapsing")) map asInt,
      asList(data("cardsRemoved")) map asInt,
      phaseTargetsFromMap(asMap(data("targetsThisPhase"))),
      phaseTargetsFromMap(asMap(data("targetsLastPhase"))),
      asBoolean(data("botLogging")),
      asBoolean(data.get("exitAfterWin").map(_ => "false").orElse(data.get("ignoreVictory")).getOrElse("false")),
      data.get("botEnhancements").map(asBoolean).getOrElse(false),
      data.get("manualDieRolls").map(asBoolean).getOrElse(false),
      (asList(data("history")) map (s => gameSegmentFromMap(asMap(s)))).toVector,
      asString(data("description")),
      asBoolean(data.get("showColor") getOrElse true)
    )
  }

  // Methods to save and load the log files
  private def logEntryToMap(entry: LogEntry): Map[String, Any] =
    Map(
      "text" -> entry.text,
      "color" -> entry.color.map(_.name).getOrElse(null)
    )

  private def logEntryFromMap(data: Map[String, Any]): LogEntry = {
    val color = if (data("color") == null)
      None
    else
      Some(Color.fromName(asString(data("color"))));

    LogEntry(asString(data("text")), color)
  }

  private def logToJson(entries: Vector[LogEntry]): String = {
    val top = Map(
      "file-version"     -> CurrentLogVersion,
      "software-version" -> SOFTWARE_VERSION,
      "log"              -> (entries map logEntryToMap)
    )
    Json.build(top)
  }

  private def logFromVersion1(entries: List[Any]): Vector[LogEntry] = {
    entries.map(e => logEntryFromMap(asMap(e))).toVector
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
      val top = asMap(Json.parse(jsonValue))
      if (!top.contains("file-version"))
        throw new IllegalArgumentException(s"Invalid save file - missing file version number")

      if (!top.contains("log"))
        throw new IllegalArgumentException(s"Invalid save file - missing log entries")

      asInt(top("file-version")) match {
        case 1 => logFromVersion1(asList(top("log")))
        case v => throw new IllegalArgumentException(s"Invalid log file version: $v")
      }
  }


   // The path should be the full path to the file to load.
  // Will set the game global variable
  def loadLog(filepath: Pathname): Vector[LogEntry] = {
    try logFromJson(filepath.readFile())
    catch {
      case e: JsonException =>
          // Older versions did not store the log as json
        // If we cannot parse the file then treat it as a regular
        // text file.
        filepath.readLines.toVector map { line =>
          LogEntry(line, None)
        }
      case e: IOException =>
        val suffix = if (e.getMessage == null) "" else s": ${e.getMessage}"
        println(s"IO Error reading log file ($filepath)$suffix")
        sys.exit(1)
      case e: Throwable =>
        val suffix = if (e.getMessage == null) "" else s": ${e.getMessage}"
        println(s"Error reading log file ($filepath)$suffix")
        sys.exit(1)
    }
  }
}

