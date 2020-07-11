
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
  def save(filepath: Pathname, gameState: GameState): Unit = {
    try {
      filepath.dirname.mkpath() // Make sure that the game directory exists
      filepath.writeFile(toGameJson(gameState))
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
  
  // The path should be the full path to the file to load.
  // Will set the game global variable
  def load(filepath: Pathname): GameState = {
    try fromGameJson(filepath.readFile())
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
    
  private def asString(x: Any): String = x.toString
  private def asBoolean(x: Any): Boolean = x match {
    case b: Boolean => b
    case _          => throw new Exception(s"Not a valid Boolean value: $x")
  }  
  private def asInt(x: Any): Int = x match {
    case i: Int => i
    case _      => throw new Exception(s"Not a valid Boolean value: $x")
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
      "resolvedTargets"       -> data.resolvedTargets,
      "resolvedInGreenOnBlue" -> data.resolvedInGreenOnBlue
    )
  
  private def plotDataFromMap(data: Map[String, Any]): PlotData =
    PlotData(
      asList(data("availablePlots")) map (x => Plot.apply(asString(x))),
      asList(data("resolvedPlots")) map (x => Plot.apply(asString(x))),
      asList(data("removedPlots")) map (x => Plot.apply(asString(x))),
      (asList(data("resolvedTargets")) map asString).toSet,
      asBoolean(data("resolvedInGreenOnBlue"))
    )
    
  private def plotOnMapToMap(pom: PlotOnMap): Map[String, Any] =
    Map("plot" -> pom.plot.name, "backlashed" -> pom.backlashed)
  
  private def plotOnMapFromMap(data: Map[String, Any]): PlotOnMap =
    PlotOnMap(Plot(asString(data("plot"))), asBoolean(data("backlashed")))
  
  private def reservesToMap(data: Reserves): Map[String, Any] =
    Map("us" -> data.us, "jihadist" -> data.jihadist)
  
  private def reservesFromMap(data: Map[String, Any]): Reserves =
    Reserves(asInt(data("us")), asInt(data("jihadist")))
  
  private def extraCellsToMap(data: ExtraCells): Map[String, Any] =
    Map("available" -> data.available, "onMap" -> data.onMap)
  
  private def extraCellsFromMap(data: Map[String, Any]): ExtraCells =
    ExtraCells(asInt(data("available")), asInt(data("onMap")))
  
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
      case p: PlayedCard       => Map("role"  -> p.role.toString, "cardNum" -> p.cardNum)
      case p: PlayedReassement => Map("card1" -> p.card1,         "card2"   -> p.card2)
      case p: AdditionalCard   => Map("role"  -> p.role.toString, "cardNum" -> p.cardNum)
      case p: PlotsResolved    => Map("num"   -> p.num)
      case p: AdjustmentMade   => Map("desc"  -> p.desc)
    }
    Map("playType" -> play.name, "params" -> params)
  }
  
  private def playFromMap(data: Map[String, Any]): Play = {
    val params = asMap(data("params"))
    asString(data("playType")) match {
      case "PlayedCard"       => PlayedCard(Role(asString(params("role"))), asInt(params("cardNum")))
      case "PlayedReassement" => PlayedReassement(asInt(params("card1")), asInt(params("card2")))
      case "AdditionalCard"   => AdditionalCard(Role(asString(params("role"))), asInt(params("cardNum")))
      case "PlotsResolved"    => PlotsResolved(asInt(params("num")))
      case "AdjustmentMade"   => AdjustmentMade(asString(params("desc")))
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
          "resources"        -> m.resources,
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
  
  private def toGameJson(gameState: GameState): String = {
    val top = Map(
      "scenarioName"        -> gameState.scenarioName,      
      "startingMode"        -> gameState.startingMode,
      "campaign"            -> gameState.campaign,
      "scenarioNotes"       -> gameState.scenarioNotes,
      "currentMode"         -> gameState.currentMode,
      "humanRole"           -> gameState.humanRole.toString,
      "humanAutoRoll"       -> gameState.humanAutoRoll,
      "botDifficulties"     -> (gameState.botDifficulties map (_.name)),
      "sequestrationTroops" -> gameState.sequestrationTroops,
      "botLogging"          -> gameState.botLogging,
      "turn"                -> gameState.turn,
      "prestige"            -> gameState.prestige,
      "usPosture"           -> gameState.usPosture,
      "funding"             -> gameState.funding,
      "countries"           -> (gameState.countries map countryToMap),
      "markers"             -> gameState.markers,
      "plotData"            -> plotDataToMap(gameState.plotData),
      "history"             -> gameState.history,
      "offMapTroops"        -> gameState.offMapTroops,
      "reserves"            -> reservesToMap(gameState.reserves),
      "extraCellCapacity"   -> gameState.extraCellCapacity,
      "extraCells"          -> extraCellsToMap(gameState.extraCells),
      "plays"               -> (gameState.plays map playToMap),
      "firstPlotCard"       -> (gameState.firstPlotCard getOrElse null),
      "cardsLapsing"        -> gameState.cardsLapsing,
      "cardsRemoved"        -> gameState.cardsRemoved,
      "targetsThisPhase"    -> phaseTargetsToMap(gameState.targetsThisPhase),
      "targetsLastPhase"    -> phaseTargetsToMap(gameState.targetsLastPhase)
    )
    Json.build(top)
  }
  
  private def fromGameJson(jsonValue: String): GameState = {
    val top = asMap(Json.parse(jsonValue))
    GameState(
      asString(top("scenarioName")),
      GameMode(asString(top("startingMode"))),
      asBoolean(top("campaign")),
      asList(top("scenarioNotes")) map asString,
      GameMode(asString(top("currentMode"))),
      Role(asString(top("humanRole"))),
      asBoolean(top("humanAutoRoll")),
      asList(top("botDifficulties")) map (x => BotDifficulty(asString(x))),
      asBoolean(top("sequestrationTroops")),
      asBoolean(top("botLogging")),
      asInt(top("turn")),
      asInt(top("prestige")),
      asString(top("usPosture")),
      asInt(top("funding")),
      asList(top("countries")) map (c => countryFromMap(asMap(c))),
      asList(top("markers")) map asString,
      plotDataFromMap(asMap(top("plotData"))),
      (asList(top("history")) map asString).toVector,
      asInt(top("offMapTroops")),
      reservesFromMap(asMap(top("reserves"))),
      asInt(top("extraCellCapacity")),
      extraCellsFromMap(asMap(top("extraCells"))),
      asList(top("plays")) map (p => playFromMap(asMap(p))),
      if (top("firstPlotCard") == null) None else Some(asInt(top("firstPlotCard"))),
      asList(top("cardsLapsing")) map asInt,
      asList(top("cardsRemoved")) map asInt,
      phaseTargetsFromMap(asMap(top("targetsThisPhase"))),
      phaseTargetsFromMap(asMap(top("targetsLastPhase")))
    )    
  }
}

