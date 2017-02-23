
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

import scala.util.Random.{shuffle, nextInt}
import scala.annotation.tailrec
import scala.util.Properties.{lineSeparator, isWin}
import scala.collection.mutable.ListBuffer

object LabyrinthAwakening {
  
  def dieRoll = nextInt(6) + 1
  
  sealed trait Role
  case object US extends Role       { override def toString() = "US" }
  case object Jihadist extends Role { override def toString() = "Jihadist" }
  object Role {
    def apply(name: String): Role = name.toLowerCase match {
      case "us"       => US
      case "jihadist" => Jihadist
      case _ => throw new IllegalArgumentException(s"Invalid role name: $name")
    }
  }
  
  case class BotDifficulty(name: String, description: String) {
    override def toString() = f"$name%-10s - $description"
  }
  
  // US
  val OffGuard  = BotDifficulty("Off Guard", "Standard rules")
  val Competent = BotDifficulty("Competent", "Alert affects two plots in the same country")
  val Adept     = BotDifficulty("Adept"    , "Disrupt at two or more troops awards +2 prestige")
  val Vigilant  = BotDifficulty("Vigilant" , "No auto recruit in Regime Change or Civil War countries")
  val Ruthless  = BotDifficulty("Ruthless" , "US associated events trigger on first plot")
  val NoMercy   = BotDifficulty("NoMercy"  , "Ignore DRM penalty on War of Ideas")

  // Jihadist
  val Muddled    = BotDifficulty("Muddled"   , "Standard rules")
  val Coherent   = BotDifficulty("Coherent"  , "Ignore DRM penalty during Minor Jihad")
  val Attractive = BotDifficulty("Attractive", "Each successful plot places two available plot markers")
  val Potent     = BotDifficulty("Potent"    , "Each successful recruit places two available cells")
  val Infectious = BotDifficulty("Infectious", "US must play all cards")
  val Virulent   = BotDifficulty("Virulent"  , "Ignore all DRM penalties")
  
  object BotDifficulty {
    def apply(name: String): BotDifficulty = name match {
      case OffGuard.name   => OffGuard
      case Competent.name  => Competent
      case Adept.name      => Adept
      case Vigilant.name   => Vigilant
      case Ruthless.name   => Ruthless
      case NoMercy.name    => NoMercy
      case Muddled.name    => Muddled
      case Coherent.name   => Coherent
      case Attractive.name => Attractive
      case Potent.name     => Potent
      case Infectious.name => Infectious
      case Virulent.name   => Virulent
      case _ => throw new IllegalArgumentException(s"Invalid BotDifficulty name: $name")
    }
  }
  
  val NoRegimeChange    = "None"
  val GreenRegimeChange = "Green"
  val TanRegimeChange   = "Tan"

  val Canada            = "Canada"
  val UnitedStates      = "United States"
  val UnitedKingdom     = "United Kingdom"
  val Serbia            = "Serbia"
  val Israel            = "Israel"
  val India             = "India"
  val Scandinavia       = "Scandinavia"
  val EasternEurope     = "Eastern Europe"
  val Benelux           = "Benelux"
  val Germany           = "Germany"
  val France            = "France"
  val Italy             = "Italy"
  val Spain             = "Spain"
  val Russia            = "Russia"
  val Caucasus          = "Caucasus"
  val China             = "China"
  val KenyaTanzania     = "Kenya/Tanzania"
  val Thailand          = "Thailand"
  val Philippines       = "Philippines"
  val Morocco           = "Morocco"
  val AlgeriaTunisia    = "Algeria/Tunisia"
  val Libya             = "Libya"
  val Egypt             = "Egypt"
  val Sudan             = "Sudan"
  val Somalia           = "Somalia"
  val Jordan            = "Jordan"
  val Syria             = "Syria"
  val CentralAsia       = "Central Asia"
  val IndonesiaMalaysia = "Indonesia/Malaysia"
  val Turkey            = "Turkey"
  val Lebanon           = "Lebanon"
  val Yemen             = "Yemen"
  val Iraq              = "Iraq"
  val SaudiArabia       = "Saudi Arabia"
  val GulfStates        = "Gulf States"
  val Pakistan          = "Pakistan"
  val Afghanistan       = "Afghanistan"
  val Iran              = "Iran"
  val Mali              = "Mali"
  val Nigeria           = "Nigeria"
  
  val CountryAbbreviations = Map("US" -> UnitedStates, "UK" -> UnitedKingdom)
  
  // Troop commitment
  val LowIntensity = "Low Intensity"
  val War          = "War"
  val Overstretch  = "Overstretch"
  val USCardDraw = Map(LowIntensity -> 9, War -> 8, Overstretch -> 7)
  
  // Funding level
  val Tight    = "Tight"
  val Moderate = "Moderate"
  val Ample    = "Ample"
  val JihadistCardDraw = Map(Tight -> 7, Moderate -> 8, Ample -> 9)
  
  // Country Types
  val NonMuslim = "Non-Muslim"
  val Suni      = "Suni"
  val ShiaMix   = "Shia-Mix"
  
  val PostureUntested = "Untested"
  val Soft            = "Soft"
  val Hard            = "Hard"
  val Even            = "Even"  // for display only
  
  val GovernanceUntested = 0
  val Good               = 1
  val Fair               = 2
  val Poor               = 3
  val IslamistRule       = 4
  
  val govToString = Map(
    GovernanceUntested -> "Untested",
    Good               -> "Good",
    Fair               -> "Fair",
    Poor               -> "Poor",
    IslamistRule       -> "Islamist Rule")
    
  val govFromString = Map(
   "Untested"      -> GovernanceUntested,
   "Good"          -> Good,
   "Fair"          -> Fair,
   "Poor"          -> Poor,
   "Islamist Rule" -> IslamistRule)
  
  val Ally      = "Ally"
  val Neutral   = "Neutral"
  val Adversary = "Adversary"
  
  val Schengen      = Scandinavia :: Benelux :: Germany :: EasternEurope :: 
                      France :: Italy :: Spain :: Nil
  val SchengenLinks = Canada :: UnitedStates :: UnitedKingdom :: Morocco ::
                      AlgeriaTunisia :: Libya :: Serbia :: Turkey :: Lebanon ::
                      Russia :: Nil
    
  // List of names to adjacent countries
  case class CountryData(adjacent: List[String])
                    
  // A country key and a list of adjacent country keys.
  val countryData: Map[String, CountryData] = Map(
   Canada            -> CountryData(UnitedStates :: UnitedKingdom :: Schengen),
   UnitedStates      -> CountryData(Canada :: UnitedKingdom :: Philippines :: Schengen),
   UnitedKingdom     -> CountryData(Canada :: UnitedStates :: Schengen),
   Serbia            -> CountryData(Russia :: Turkey :: Schengen),
   Israel            -> CountryData(Egypt :: Jordan :: Lebanon :: Nil),
   India             -> CountryData(Pakistan :: IndonesiaMalaysia :: Nil),
   Scandinavia       -> CountryData((Schengen ::: SchengenLinks) filterNot (_ == Scandinavia)),
   EasternEurope     -> CountryData((Schengen ::: SchengenLinks) filterNot (_ == EasternEurope)),
   Benelux           -> CountryData((Schengen ::: SchengenLinks) filterNot (_ == Benelux)),
   Germany           -> CountryData((Schengen ::: SchengenLinks) filterNot (_ == Germany)),
   France            -> CountryData((Schengen ::: SchengenLinks) filterNot (_ == France)),
   Italy             -> CountryData((Schengen ::: SchengenLinks) filterNot (_ == Italy)),
   Spain             -> CountryData((Schengen ::: SchengenLinks) filterNot (_ == Spain)),
   Russia            -> CountryData(Serbia :: Turkey :: Caucasus :: CentralAsia :: Schengen),
   Caucasus          -> CountryData(Turkey :: Russia :: CentralAsia :: Iran :: Nil),
   China             -> CountryData(CentralAsia:: Thailand :: Nil),
   KenyaTanzania     -> CountryData(Sudan :: Somalia :: Nigeria :: Nil),
   Thailand          -> CountryData(China :: IndonesiaMalaysia :: Philippines :: Nil),
   Philippines       -> CountryData(UnitedStates :: IndonesiaMalaysia :: Thailand :: Nil),
   Morocco           -> CountryData(AlgeriaTunisia :: Mali :: Schengen),
   AlgeriaTunisia    -> CountryData(Morocco :: Libya :: Mali :: Schengen),
   Libya             -> CountryData(AlgeriaTunisia :: Egypt :: Sudan :: Schengen),
   Egypt             -> CountryData(Israel :: Libya :: Sudan :: Nil),
   Sudan             -> CountryData(Libya :: Egypt :: Somalia :: KenyaTanzania :: Nigeria :: Nil),
   Somalia           -> CountryData(Yemen :: Sudan :: KenyaTanzania :: Nil),
   Jordan            -> CountryData(Israel :: Syria :: Iraq :: SaudiArabia :: Nil),
   Syria             -> CountryData(Iraq :: Lebanon :: Jordan :: Turkey :: Nil),
   CentralAsia       -> CountryData(Russia :: Caucasus :: Iran :: Afghanistan :: China :: Nil),
   IndonesiaMalaysia -> CountryData(Pakistan :: IndonesiaMalaysia :: Thailand :: Philippines :: Nil),
   Turkey            -> CountryData(Serbia :: Russia :: Caucasus :: Syria :: Iran :: Schengen),
   Lebanon           -> CountryData(Syria :: Israel :: Schengen),
   Yemen             -> CountryData(SaudiArabia :: Somalia :: Nil),
   Iraq              -> CountryData(GulfStates :: SaudiArabia :: Iran :: Turkey :: Syria :: Jordan :: Nil),
   SaudiArabia       -> CountryData(Jordan :: Iraq :: GulfStates :: Yemen :: Nil),
   GulfStates        -> CountryData(SaudiArabia :: Iraq :: Iran :: Pakistan :: Nil),
   Pakistan          -> CountryData(GulfStates :: Iran :: Afghanistan :: IndonesiaMalaysia :: India :: Nil),
   Afghanistan       -> CountryData(Pakistan :: Iran :: CentralAsia :: Nil),
   Iran              -> CountryData(Afghanistan :: Pakistan :: GulfStates :: Iraq :: Turkey :: Caucasus :: CentralAsia :: Nil),
   Mali              -> CountryData(Morocco :: AlgeriaTunisia :: Nigeria :: Nil),
   Nigeria           -> CountryData(Mali :: Sudan :: KenyaTanzania :: Nil)
  )
  
  def getAdjacent(name: String): List[String] = countryData(name).adjacent
  def areAdjacent(name1: String, name2: String) = getAdjacent(name1) contains name2

  // Shorteds distance between countries
  def distance(source: String, target: String): Int = {
    def measure(current: String, visited: Set[String]): Option[Int] = {
      if (current == target)
        Some(0)
      else {
        (getAdjacent(current) filterNot visited) match {
          case Nil       => None
          case adjacents => 
            val paths = adjacents.map(a => measure(a, visited ++ adjacents)).flatten.sorted
            paths match {
              case Nil => None
              case x :: _ => Some(1 + x)
            }
        }
      }
    }
    measure(source, Set.empty).get
  }
  
  // Convert plot number to a name for display purposes.
  def plotName(plot: Int): String = plot match {
    case 4 => "Plot WMD"
    case n => s"Plot $n"
  }
  
  def plotsDisplay(plots: List[Int], humanRole: Role): String = (plots.size, humanRole) match {
    case (0, _)        => "none"
    case (n, Jihadist) => (plots.sorted.reverse map plotName).mkString(", ")
    case (1, US)       => "1 plot"
    case (x, US)       => s"$x plots"
  }
  
  val GlobalMarkers = List(
    "NATO", "Training Camps", "Bin Ladin", "Civil War", "Facebook", "Swedish Cartoons",
    "Iran Oil Crisis", "Arab Spring", "Oil Price Spike"
  ).sorted
  
  val CountryMarkers = List(
    "NATO", "Training Camps"
  )
  
  // Used to describe event markers that represent troops.
  case class TroopsMarker(name: String, num: Int)
  // Order TroopsMarkers so that markers represent smaller numbers
  // of troops come first.
  implicit val TroopsMarkerOrdering = new Ordering[TroopsMarker] {
    def compare(x: TroopsMarker, y: TroopsMarker) = x.num compare y.num
  }
  
  
  case class Card(number: Int, name: String) {
    def numAndName = s"#$number $name"
  }
  
  def entry(card: Card) = (card.number -> card)
  
  val Cards = Map(
    entry(Card(1, "Facebook")),
    entry(Card(2, "Bin Ladin")),
    entry(Card(3, "Syrian Civil War"))
  )
  
  def cardNumbers = Cards.keys.toList
  def lapsingCardNumbers = List(1, 2, 3).sorted
  def cardNumAndName(number: Int): String = Cards(number).numAndName
  def cardNumsAndNames(xs: List[Int]): String = xs map cardNumAndName mkString ", "
  
  sealed trait Country {
    val name: String
    val governance: Int
    val sleeperCells: Int
    val activeCells: Int
    val hasCadre: Boolean
    val plots: List[Int]     // 1, 2, 3, 4 == WMD
    val markers: List[String]
    val wmdCache: Int        // Number of WMD plots cached
    
    def unTested: Boolean
    def isGood    = governance == Good
    def isFair    = governance == Fair
    def isPoor    = governance == Poor
    def isIslamic = governance == IslamistRule
    
    def totalCells = sleeperCells + activeCells
    def hasMarker(name: String) = markers contains name
    
    def canAutoRecruit: Boolean
    def hasPlots = plots.nonEmpty
  }
  
  case class NonMuslimCountry(
    name: String,
    governance: Int             = Good,
    sleeperCells: Int           = 0,
    activeCells: Int            = 0,
    hasCadre: Boolean           = false,
    plots: List[Int]            = Nil,
    markers: List[String]       = Nil,
    posture: String             = PostureUntested,
    recruitOverride: Int        = 0,
    wmdCache: Int               = 0,  // Number of WMD plots cached
    iranSpecialCase: Boolean    = false
  ) extends Country {
    override def unTested = posture == PostureUntested
    override def canAutoRecruit = false
    def isSchengen = Schengen contains name
    def isHard = posture == Hard
    def isSoft = posture == Soft
  }

  case class MuslimCountry(
    name: String,
    governance: Int             = GovernanceUntested,
    sleeperCells: Int           = 0,
    activeCells: Int            = 0,
    hasCadre: Boolean           = false,
    plots: List[Int]            = Nil,
    markers: List[String]       = Nil,
    isSunni: Boolean            = true,
    resources: Int              = 0,
    alignment: String           = Neutral,
    troops: Int                 = 0,
    militia: Int                = 0,
    oilProducer: Boolean        = false,
    aidMarkers: Int             = 0,
    regimeChange: String        = NoRegimeChange,
    besiegedRegime: Boolean     = false,
    civilWar: Boolean           = false,
    caliphateCapital: Boolean   = false,
    awakening: Int              = 0,  // 0, 1, 2, 3
    reaction: Int               = 0,  // 0, -1, -2, -3
    wmdCache: Int               = 0   // Number of WMD plots cached
  ) extends Country {
    override def unTested = governance == GovernanceUntested
    def isAlly      = alignment == Ally
    def isNeutral   = alignment == Neutral
    def isAdversary = alignment == Adversary
    
    def inRegimeChange = regimeChange != NoRegimeChange
    
    def untestedWithData: Boolean = unTested && (
      totalCells > 0            ||
      hasCadre                  ||
      hasPlots                  ||
      markers.nonEmpty          ||
      totalTroopsAndMilitia > 0 ||
      inRegimeChange            ||
      besiegedRegime            ||
      civilWar                  ||
      caliphateCapital          ||
      awakening > 0             ||
      reaction < 0
    )
    
    
    override def canAutoRecruit = isIslamic || civilWar || inRegimeChange
    
    // TODO: Add other markers!!
    // The list is sorted so that markers repesent in 
    def troopsMarkers: List[TroopsMarker] = markers collect {
      case "NATO" => TroopsMarker("NATO", 2)
    }
    
    def markerTroops: Int = troopsMarkers.foldLeft(0) { (total, tm) => total + tm.num }
    def totalTroops = troops + markerTroops
    def totalTroopsAndMilitia = totalTroops + militia // Used to calculate hit for attrition
    
    def canTakeAwakeningOrReactionMarker = !(isGood || isIslamic || civilWar)
    def caliphateCandidate = civilWar || isIslamic || inRegimeChange

    // Note: The caller is responsible for handling convergence and the possible
    //       displacement of the caliphate captial.
    def improveGovernance(): MuslimCountry = {
      assert(isAlly, s"improveGovernance() called on non-ally - ${name}")
      assert(!isGood, s"improveGovernance() called on Good country - ${name}")

      if (governance == Poor)
        copy(governance = Fair, awakening = (awakening - 1) max 0)
      else
        copy(governance = Good, awakening = 0, reaction = 0, aidMarkers = 0,
             militia = 0, regimeChange = NoRegimeChange, besiegedRegime = false,
             caliphateCapital = false, civilWar = false)
    }

    // Note: The caller is repsonsible for adjusting funding, prestige,
    //       and for handling convergence
    def worsenGovernance(): MuslimCountry = {
      assert(!isIslamic, s"worsenGovernance() called on Islamist Rule country - ${name}")
      if (governance == Good)
        copy(governance = Fair, aidMarkers = (aidMarkers - 1) max 0)
      else if (governance == Fair)
        copy(governance = Poor, aidMarkers = (aidMarkers - 1) max 0, reaction = (reaction + 1) min 0)
      else  // governance == Poor
        copy(governance = IslamistRule, alignment = Adversary, awakening = 0, reaction = 0, 
             aidMarkers = 0, militia = 0, regimeChange = NoRegimeChange, besiegedRegime = false, 
             civilWar = false)
    }
  }
    
  
  trait Scenario {
    val name: String
    val prestige: Int
    val usPosture: String
    val funding: Int
    val availablePlots: List[Int]     // 1, 2, 3, 4 == WMD
    val countries: List[Country]
    val markers: List[String]
  }
  
  class Awakening2010 extends Scenario {
    val name       = "Awakening (2010 Scenario)"
    val prestige   = 5
    val usPosture = Soft
    val funding    = 5
    val availablePlots = 1 :: 1 :: 1 :: 2 :: 2 :: 3 :: Nil
    val countries = List(
      NonMuslimCountry(Canada),
      NonMuslimCountry(UnitedStates, posture = Soft),
      NonMuslimCountry(UnitedKingdom, posture = Hard),
      NonMuslimCountry(Serbia),
      NonMuslimCountry(Israel, posture = Hard),
      NonMuslimCountry(India),
      NonMuslimCountry(Scandinavia),
      NonMuslimCountry(EasternEurope),
      NonMuslimCountry(Benelux, posture = Soft),
      NonMuslimCountry(Germany),
      NonMuslimCountry(Italy),
      NonMuslimCountry(France, recruitOverride = 2, posture = Hard),
      NonMuslimCountry(Spain, recruitOverride = 2),
      NonMuslimCountry(Russia, governance = Fair),
      NonMuslimCountry(Caucasus, governance = Fair),
      NonMuslimCountry(China, governance = Fair),
      NonMuslimCountry(KenyaTanzania, governance = Fair),
      NonMuslimCountry(Thailand, governance = Fair),
      NonMuslimCountry(Philippines, governance = Fair, recruitOverride = 3),
      NonMuslimCountry(Iran, governance = Fair, wmdCache = 1, iranSpecialCase = true),
      NonMuslimCountry(Nigeria, governance = Poor),
      
      MuslimCountry(Morocco, resources = 2),
      MuslimCountry(AlgeriaTunisia, resources = 2, oilProducer = true,
                    governance = Poor, alignment = Neutral, awakening = 1),
      MuslimCountry(Libya, resources = 1, oilProducer = true),
      MuslimCountry(Egypt, resources = 3),
      MuslimCountry(Sudan, resources = 1, oilProducer = true),
      MuslimCountry(Somalia, resources = 1),
      MuslimCountry(Jordan, resources = 1),
      MuslimCountry(Syria, resources = 2, wmdCache = 2),
      MuslimCountry(CentralAsia, resources = 2),
      MuslimCountry(Turkey, isSunni = false, resources = 2),
      MuslimCountry(Lebanon, isSunni = false, resources = 1),
      MuslimCountry(Yemen, isSunni = false, resources = 1),
      MuslimCountry(Iraq, isSunni = false, resources = 3, oilProducer = true,
                    governance = Poor, alignment = Ally, troops = 2, sleeperCells = 1),
      MuslimCountry(SaudiArabia, isSunni = false, resources = 3, oilProducer = true),
      MuslimCountry(GulfStates, isSunni = false, resources = 3, oilProducer = true,
                    governance = Fair, alignment = Ally, troops = 2),
      MuslimCountry(Pakistan, isSunni = false, resources = 2, wmdCache = 3,
                    governance = Fair, alignment = Neutral, sleeperCells = 2),
      MuslimCountry(Afghanistan, isSunni = false, resources = 1, 
                    governance = Poor, alignment = Ally, troops = 6, sleeperCells = 2,
                    regimeChange = TanRegimeChange),
      MuslimCountry(Mali, resources = 1)
    )
    val markers = List.empty[String]
  }
  
  case class GameState(
    scenarioName: String,
    humanRole: Role,
    botDifficulties: List[BotDifficulty],
    turn: Int,
    prestige: Int,
    usPosture: String,
    funding: Int,
    countries: List[Country],
    markers: List[String],
    availablePlots: List[Int] = 1 :: 1 :: 1 :: 2 :: 2 :: 3 :: Nil,
    history: Vector[String] = Vector.empty,
    offMapTroops: Int = 0,
    usReserves: Int = 0,
    jihadistReserves: Int = 0,
    oilPriceSpikes: Int = 0,
    resolvedPlots: List[Int] = Nil,
    lapsing: List[Int] = Nil,      // Card numbers
    firstPlot: Option[Int] = None  // Card number
  ) {
    
    def botRole = if (humanRole == US) Jihadist else US
      
    def scenarioSummary: Seq[String] = {
      val b = new ListBuffer[String]
      b += s"Scenario: $scenarioName"
      b += s"The Bot is playing the $botRole"
      b += (if (botRole == US) "US Resolve" else "Jihadist Ideology")
      for (difficulty <- botDifficulties)
        b += s"  $difficulty"
      b.toList
    }
      
    def scoringSummary: Seq[String] = {
      val b = new ListBuffer[String]
      val adjacency = humanRole match {
        case Jihadist => if (islamistAdjacency) "with adjacency" else "without adjacency"
        case US       => "adjacency not necessary"
      }
      b += "Current Score"
      b += separator()
      b += f"Good/Fair Countries   : $numGoodOrFair%2d | Good Resources   : $goodResources%2d"
      b += f"Poor/Islamic Countries: $numPoorOrIslamic%2d | Islamic Resources: $islamistResources%2d  ($adjacency)"
      b.toList
    }
    
    def worldPostureDisplay = {
      val (worldPosture, level) = gwot
      val levelDisp = if (worldPosture == Even) "" else level.toString
      s"$worldPosture $levelDisp"
    }
    
    def statusSummary: Seq[String] = {
      val activePlots = countries filter (_.hasPlots)
      val b = new ListBuffer[String]
      b += "Status"
      b += separator()
      b += f"US posture      : $usPosture | World posture    : ${worldPostureDisplay}"
      b += f"US prestige     : $prestige%2d   | Jihadist funding : $funding%2d"
      b += f"US reserves     : $usReserves%2d   | Jihadist reserves: $jihadistReserves%2d"
      b += f"Troops on track : $troopsAvailable%2d   | Troops off map   : $offMapTroops%2d"
      b += f"Cells on track  : $cellsOnTrack%2d   | Militia on track : $militiaAvailable%2d"
      b += s"Markers         : ${if (markers.isEmpty) "none" else markers mkString ", "}"
      b += s"Lapsing         : ${if (lapsing.isEmpty) "none" else cardNumsAndNames(lapsing)}"
      b += s"1st plot        : ${firstPlot map cardNumAndName getOrElse "none"}"
      b += s"Resloved plots  : ${plotsDisplay(resolvedPlots, Jihadist)}"
      b += s"Available plots : ${plotsDisplay(availablePlots, humanRole)}"
      if (activePlots.isEmpty)
        b += s"Active plots    : none"
      else {
        b += s"Active plots"
        val fmt = "  %%-%ds: %%s".format(activePlots.map(_.name.length).max)
        for (c <- activePlots)
          b += fmt.format(c.name, plotsDisplay(c.plots, humanRole))
      }
      b.toList
    }
    
    // If show all is false, then some attriubtes will not be displayed
    // if they have value of zero.
    def countrySummary(name: String, showAll: Boolean = false): Seq[String] = {
      val b = new ListBuffer[String]
      val items = new ListBuffer[String]
      def item(num: Int, label: String, pluralize: Boolean = true): Unit = {
        if (showAll || num > 0)
          items += s"$num $label${if (num == 1 || !pluralize) "" else "s"}"
      }
      def addItems(): Unit = if (items.nonEmpty) b += s"  ${items mkString ", "}"
      def markersString(markers: List[String]): String = if (markers.isEmpty)
        "none"
      else
        markers mkString ", "

      getCountry(name) match {
        case n: NonMuslimCountry =>
          val posture = if (n.iranSpecialCase) "" else s", ${n.posture}"
          b += s"$name -- ${govToString(n.governance)}$posture"
          item(n.activeCells, "Active cell")
          item(n.sleeperCells, "Sleeper cell")
          if (n.hasCadre)
            items += "Cadre marker"
          else if (showAll)
            items += "No Cadre marker"
          addItems()
          if (showAll || n.hasPlots)
            b += s"  Plots: ${plotsDisplay(n.plots, humanRole)}"
          if (showAll || n.markers.size > 0)
            b += s"  Markers: ${markersString(n.markers)}"

        case m: MuslimCountry =>
          val gov = if (m.unTested) "Untested" else s"${govToString(m.governance)} ${m.alignment}"
          val res = s"${m.resources} ${if (m.resources == 1) "resource" else "resources"}"
          val oil = if (m.oilProducer) ", Oil producer" else ""
          b += s"$name -- $gov, $res$oil"
          item(m.activeCells, "Active cell")
          item(m.sleeperCells, "Sleeper cell")
          if (m.hasCadre)
            items += "Cadre marker"
          else if (showAll)
            items += "No Cadre marker"
          item(m.troops, "Troop")
          item(m.militia, "Militia", pluralize = false)
          addItems()
            
          items.clear
          item(m.aidMarkers, "Aid marker")
          item(m.awakening, "Awakening marker")
          item(-m.reaction, "Reaction marker")
          if (m.besiegedRegime)
            items += "Besieged regime"
          else if (showAll)
            items += "No Besieged regime"
          addItems()
            
          items.clear
          if (showAll || m.inRegimeChange)
            items += s"Regime Change (${m.regimeChange})"
          if (m.civilWar)
            items += "Civil War"
          else if (showAll)
            items += "No Civil War"
          if (m.caliphateCapital)
            items += "Caliphate Capital"
          else if (isCaliphateMember(m.name))
            items += "Caliphate member"
          else if (showAll)
            items += "Not Caliphate member"
          addItems()
          if (showAll || m.hasPlots)
            b += s"  Plots: ${plotsDisplay(m.plots, humanRole)}"
          if (showAll || m.markers.size > 0)
            b += s"  Markers: ${markersString(m.markers)}"
      }
      b.toList
    }
    
    def caliphateSummary: Seq[String] = {
      val b = new ListBuffer[String]
      b += "Caliphate"
      b += separator()
      caliphateCapital match {
        case Some(capital) =>
          // Add the capital first
          b += s"${capital}  (Capital)"
          for (member <- caliphateDaisyChain(capital).sorted.filterNot(_ == capital))
            b += member
          log("Reminder: cells in Caliphate countries are always active")
        case None =>
          b += "There is no Caliphate declared"
      }
      b.toList
    }
    
    def civilWarSummary: Seq[String] = {
      val b = new ListBuffer[String]
      b += "Civil Wars"
      b += separator()
      val civilWars = (muslim filter (_.civilWar)).toList
      if (civilWars.isEmpty)
        b += "There are no counties in civil war"
      else
        b += civilWars map (_.name) mkString ", "
      b.toList
    }
    
    
    def bot: Role = humanRole match {
      case US       => Jihadist
      case Jihadist => US
    }
    def muslim: Iterator[MuslimCountry] =
      countries.iterator filter (_.isInstanceOf[MuslimCountry]) map (_.asInstanceOf[MuslimCountry])
    def nonMuslim: Iterator[NonMuslimCountry] =
      countries.iterator filter (_.isInstanceOf[NonMuslimCountry]) map (_.asInstanceOf[NonMuslimCountry])
    
    def isMuslim(name: String)    = muslim exists (_.name == name)
    def isNonMuslim(name: String) = nonMuslim exists (_.name == name)
    
    // The methods assume a valid name and will throw an exception if an invalid name is used!
    def getCountry(name: String)   = (countries find (_.name == name)).get
    def getMuslim(name: String)    = (muslim find (_.name == name)).get
    def getNonMuslim(name: String) = (nonMuslim find (_.name == name)).get
    
    def getCountries(names: List[String]):  List[Country]          = names map getCountry
    def getMuslims(names: List[String]):    List[MuslimCountry]    = names map getMuslim
    def getNonMuslims(names: List[String]): List[NonMuslimCountry] = names map getNonMuslim
    
    def adjacentCountries(name: String)   = getCountries(getAdjacent(name))
    def adjacentMuslims(name: String)     = getMuslims(getAdjacent(name) filter isMuslim)
    def adjacentNonMuslims(name: String)  = getNonMuslims(getAdjacent(name) filter isNonMuslim)
  
    def caliphateCapital: Option[String] = muslim find (_.caliphateCapital) map (_.name)
    
    def isCaliphateMember(name: String): Boolean = {
      caliphateCapital match {
        case None => false  // No Caliphate declared
        case Some(capital) => caliphateDaisyChain(capital) contains name
      }
    }
  
    // Return a list of countries comprising the daisy chain of caliphate candidates
    // that are adjacent to the given country.
    def caliphateDaisyChain(name: String): List[String] = {
      def adjacentCandidates(x: MuslimCountry) = adjacentMuslims(x.name).filter(_.caliphateCandidate)
      @tailrec def tryNext(candidates: List[MuslimCountry], chain: List[String]): List[String] = {
        candidates match {
          case Nil                                 => chain
          case x :: xs  if (chain contains x.name) => tryNext(xs, chain)
          case x :: xs                             => tryNext(xs ::: adjacentCandidates(x), x.name :: chain)
        }
      }
      
      val m = getMuslim(name)
      if (m.caliphateCandidate)
        tryNext(List(m), Nil)
      else
        Nil
    }
    
    def setCaliphateCapital(name: String): GameState = {
      assert(isMuslim(name), s"setCaliphateCapital() called on non-muslim country: $name")
      val capital = getMuslim(name);
      assert(capital.caliphateCandidate, s"setCaliphateCapital() called on invalid country: $name")
      
      // First make sure there is no country marked as the capital
      val clearedState = updateCountries(muslim.map(_.copy(caliphateCapital = false)).toList)
      val daisyChain = clearedState.caliphateDaisyChain(name).map { memberName =>
        val member = getMuslim(memberName)
        member.copy(
          caliphateCapital = member.name == name,
          activeCells      = member.totalCells,  // All cells go active in caliphate members
          sleeperCells     = 0
        )
      }
      clearedState.updateCountries(daisyChain)
    }
    
    def updateCountry(changed: Country): GameState =
      this.copy(countries = changed :: (countries filterNot (_.name == changed.name)))
    
    // Do NOT pass multiples with the same name!
    def updateCountries(changed: List[Country]): GameState = {
      val names = changed.map(_.name).toSet
      this.copy(countries = changed ::: countries filterNot (c => names contains c.name))
    }
    
    def adjustPrestige(amt: Int): GameState = this.copy(prestige = (prestige + amt) max 1 min 12)
    def adjustFunding(amt: Int): GameState  = this.copy(funding = (funding + amt) max 1 min 9)
    
    
    // List of countries that are valid targets for War of Ideas
    def woiTargets: List[Country] = countries filter {
      case n: NonMuslimCountry => !(n.name == UnitedStates || n.name == Israel)
      case m: MuslimCountry    => m.unTested || m.isNeutral || (m.isAlly && !m.isGood)
    }
    
    
    def prestigeModifier = prestige match {
      case x if x <  4 => -1
      case x if x <  7 =>  0
      case x if x < 10 =>  2
      case _           =>  2
    }
    
    // Returns the current gwot 
    // posture (Soft, Even, Hard)
    // value 0, 1, 2, 3
    def gwot: (String, Int) = {
      val value = (nonMuslim.filterNot(_.name == UnitedStates).foldLeft(0) { 
        case (v, c) if c.isHard => v + 1
        case (v, c) if c.isSoft => v - 1
        case (v, _) => v // Untested
      })
      val posture = if (value == 0) Even else if (value < 0) Soft else Hard
      (posture, value.abs min 3)
    }
    
    def gwotPenalty: Int = {
      gwot match {
        case (posture, value)  if posture != usPosture => -value
        case _ => 0
      }
    }
    
    def troopCommitment = troopsAvailable match {
      case x if x <  5 => Overstretch
      case x if x < 10 => War
      case _           => LowIntensity
    }
    
    def fundingLevel = funding match {
      case x if x < 4 => Tight
      case x if x < 7 => Moderate
      case _          => Ample
    }
    
    def troopsAvailable  = 15 - offMapTroops - muslim.foldLeft(0) { (a, c) => a + c.troops }
    def militiaAvailable = 15 - muslim.foldLeft(0) { (a, c) => a + c.militia }
    
    def trainingCampsInPlay = markers.contains("Training Camps")
    def totalCellsInPlay = if (trainingCampsInPlay) 20 else 15
    def cellsOnMap  = countries.foldLeft(0) { (a, c) => a + c.totalCells }
    def cellsInCamp = if (trainingCampsInPlay) (totalCellsInPlay - cellsOnMap) min 5 else 0
    def cellsOnTrack = totalCellsInPlay - cellsInCamp - cellsOnMap
    
    // Number of cells available for operations
    def cellsAvailable = {
      if (trainingCampsInPlay && funding == 9)
        cellsInCamp + cellsOnTrack
      else
        fundingLevel match {
          case Tight    => (cellsOnTrack - 10) max 0
          case Moderate => (cellsOnTrack -  5) max 0
          case _        => cellsOnTrack
        }
    }
    
    def numGoodOrFair    = muslim.filter(c => c.isGood || c.isFair).size
    def numPoorOrIslamic = muslim.filter(c => c.isPoor || c.isIslamic).size
    def numIslamic       = muslim.filter(c => c.isIslamic).size
    def oilBump(c: MuslimCountry) = if (c.oilProducer) oilPriceSpikes else 0
    def goodResources =
      muslim.filter(_.isGood).foldLeft(0) { (a, c) => a + c.resources + oilBump(c) }
    def islamistResources = 
      muslim.filter(_.isIslamic).foldLeft(0) { (a, c) => a + c.resources + oilBump(c)} +
      (if (muslim.exists(_.caliphateCapital)) 1 else 0)
    // Return true if any two Islamist Rule countries are adjacent.
    def islamistAdjacency: Boolean =
      muslim.filter(_.isIslamic).toList.combinations(2).exists (xs => areAdjacent(xs.head.name, xs.last.name))
    
  }
  
  def initialGameState(scenario: Scenario, humanRole: Role, botDifficulties: List[BotDifficulty]) = GameState(
      scenario.name,
      humanRole,
      botDifficulties,
      0, // Turn number, zero indicates start of game.
      scenario.prestige,
      scenario.usPosture,
      scenario.funding,
      scenario.countries,
      scenario.markers.sorted)
  
  
  // Global variables
  var game = initialGameState(new Awakening2010, US, Muddled :: Nil)
  var previousState: Option[GameState] = None // Used to undo the last command
  
  // This history of game turns, most recent first (ie in reverse order.)
  // The current game state is NOT in the list.  It is added at the
  // end of the turn.
  // Allows user to roll back to a previous turn.
  var gameTurns = List.empty[GameState]
  
  // Returns comma separated string with last choice separated by "or"
  //    List("apples")                      => "apples"
  //    List("apples", "oranges")           => "apples or oranges"
  //    List("apples", "oranges", "grapes") => "apples, oranges or grapes"
  def orList(x: Seq[Any]) = x match {
    case Seq()     => ""
    case Seq(a)    => a.toString
    case Seq(a, b) => s"${a.toString} or ${b.toString}"
    case _         => x.dropRight(1).mkString(", ") + " or " + x.last.toString
  }
  
  // Find a match for the given string in the list of options.
  // Any unique prefix of the given options will succeed.
  def matchOne(s: String, options: Seq[String], abbreviations: Map[String, String] = Map.empty): Option[String] = {
    for ((abbr, x) <- abbreviations) {
      assert(options contains x, s"abbreviation '$abbr' maps to '$x' which is not a valid option")
    }
    
    // When showing the list of options to the user, we want to group
    // all abbreviations with the word that they represent.
    val displayList = {
    // (options ++ abbreviations.keys)
      var associations = Map.empty[String, Set[String]].withDefaultValue(Set.empty)
      for ((a, o) <- abbreviations)
        associations += o -> (associations(o) + a)
      options map {
        case o if associations(o).nonEmpty => o + associations(o).toList.sorted.mkString(" (", ",", ")")
        case o => o
      }  
    }
    val normalizedAbbreviations = for ((a, v) <- abbreviations) yield (a.toLowerCase, v)  
    val normalized = (options ++ abbreviations.keys) map (_.toLowerCase)
    if (s == "?") {
      println(s"Must be one of:\n${orList(displayList)}")
      None
    }
    else
      (normalized.distinct filter (_ startsWith s.toLowerCase)) match {
        case Seq() =>
          println(s"'$s' is not recognized. Must be one of:\n${orList(displayList)}")
          None
        case Seq(v)  =>
          normalizedAbbreviations.get(v) match {
            case Some(opt) => Some(opt)
            case None      => Some(options(normalized.indexOf(v)))
          }
        
        case many if many exists (_ == s) =>
          normalizedAbbreviations.get(s) match {
            case Some(opt) => Some(opt)
            case None      => Some(options(normalized.indexOf(s)))
          }
      
        case ambiguous =>
          println(s"'$s' is ambiguous. (${orList(ambiguous)})")
          None
      }
  }
    
  def getOneOf(prompt: String, options: Seq[Any], initial: Option[String] = None, 
               allowNone: Boolean = true, abbreviations: Map[String, String] = Map.empty): Option[String] = {
    @tailrec def testResponse(response: Option[String]): Option[String] = {
      response flatMap (s => matchOne(s, options map (_.toString), abbreviations)) match {
        case None =>
          readLine(prompt) match {
            case null | "" if allowNone => None
            case null | ""              => testResponse(None)
            case input                  => testResponse(Some(input))
          }
        case s => s
      }
    }
    testResponse(initial)
  }
  
  @tailrec def askYorN(prompt: String): Boolean = {
    def testResponse(r: String): Option[Boolean] = {
      if (r == null)
        None
      else
        r.toLowerCase match {
          case "n" | "no"  => Some(false)
          case "y" | "yes" => Some(true)
          case _           => None
        }
    }
    
    testResponse(readLine(prompt)) match {
      case Some(result) => result
      case None         => askYorN(prompt)
    }
  }
  

  
  def getCardNumber(prompt: String, initial: Option[Int] = None, allowNone: Boolean = true): Option[Int] = {
    val INT = """(\d+)""".r
    def checkNumber(input: String): Boolean = input match {
      case INT(num) if cardNumbers contains num.toInt => true
      case _ => 
        println(s"'$input' is not a valid card number")
        false
    }
    @tailrec def testResponse(response: Option[String]): Option[Int] = {
      response filter checkNumber match {
        case None =>
          readLine(prompt) match {
            case null | "" if allowNone => None
            case null | ""              => testResponse(None)
            case input                  => testResponse(Some(input))
          }
        case x => x map (_.toInt)
      }
    }
    testResponse(initial map (_.toString))
  }
  
  // Use the random Muslim table.
  // Iran and Nigeria may not yet have become Muslim countries.
  // If they are selected and non Muslim, then we simply roll again.
  def randomMuslimCountry: MuslimCountry = {
    val iranIsMuslim    = game.isMuslim(Iran)
    val nigeriaIsMuslim = game.isMuslim(Nigeria)
    @tailrec def rollOnTable: MuslimCountry = {
      dieRoll match {
        case 1 | 2                    => game.getMuslim(CentralAsia)
        case 3 if iranIsMuslim        => game.getMuslim(Iran)
        case 4                        => game.getMuslim(Mali)
        case 5 | 6 if nigeriaIsMuslim => game.getMuslim(Nigeria)
        case _                        => rollOnTable
      }
    }
    rollOnTable
  }
  

  def randomShiaMixCountry: MuslimCountry = {
    val muslimKey = List(dieRoll, dieRoll, dieRoll).sum match {
      case 3 | 4 | 5 | 6                 => Syria
      case 7 if game.isMuslim(Iran) => Iran
      case 7                             => Syria
      case 8                             => SaudiArabia
      case 9                             => Turkey
      case 10                            => Iraq
      case 11                            => GulfStates
      case 12                            => Yemen
      case 13                            => Pakistan
      case 14                            => Lebanon
      case 15 | 16 | 17 | 18             => Afghanistan
    }
    game.getMuslim(muslimKey) 
  }
  
  // Test the country if it is still untested.
  def testCountry(name: String): Unit = {
    val country = game.getCountry(name)
    if (country.unTested) {
      country match {
        case m: MuslimCountry    =>
          val newGov = if (dieRoll < 5) Poor else Fair
          game = game.updateCountry(m.copy(governance = newGov))
          log(s"${m.name} tested: ${govToString(newGov)} Neutral")
          
        case n: NonMuslimCountry =>
          val newPosture = if (dieRoll < 5) Soft else Hard
          game = game.updateCountry(n.copy(posture = newPosture))
          log(s"${n.name} tested: $newPosture")
      }
    }
  }
  
  
  
  def modifyWoiRoll(die: Int, m: MuslimCountry, ignoreGwotPenalty: Boolean = false, silent: Boolean = false): Int = {
    def logNotZero(value: Int, msg: String): Unit = if (!silent && value != 0) log(f"$value%2d: $msg")
    val prestigeMod = game.prestige match {
      case x if x < 4  => -1
      case x if x < 7  =>  0
      case x if x < 10 =>  1
      case _           =>  2
    }
    val shiftToGoodMod = if (m.isAlly && m.isGood) -1 else 0
    val gwotMod        = if (ignoreGwotPenalty) 0 else game.gwotPenalty
    val aidMod         = m.aidMarkers
    val adjToGoodMod   = if (game.adjacentMuslims(m.name) exists (_.isGood)) 1 else 0
    val awakeningMod   = m.awakening
    val reactionMod    = m.reaction
    logNotZero(prestigeMod,    "Prestige")
    logNotZero(shiftToGoodMod, "Shift to Good governance")
    logNotZero(gwotMod,        "GWOT penalty")
    logNotZero(aidMod,         "Aid")
    logNotZero(adjToGoodMod,   "Adjacent to country at Good governance")
    logNotZero(awakeningMod,   "Awakening")
    logNotZero(reactionMod,    "Reaction")
    die + (prestigeMod + shiftToGoodMod + gwotMod + aidMod + adjToGoodMod + awakeningMod + reactionMod)
  }
  
  
  // Used when the Bot is selecting new caliphate capital
  case class CaliphateCapitalCandidate(m: MuslimCountry) {
    val size = game.caliphateDaisyChain(m.name).size
  }
  
  // We define this outside of the CaliphateCapitalCandidateOrdering so it can
  // easily be called by the displaceCaliphateCapital() method.
  //
  // Sort first by daisy chain size large to small
  // then by non Ally before Ally
  // then by worse governance before better governance
  // then by worst WoI drm before better WoI drm
  def compareCapitalCandidates(x: CaliphateCapitalCandidate, y: CaliphateCapitalCandidate) = {
    if (x.size != y.size)                      y.size compare x.size         // y first: to sort large to small.
    else if (x.m.isAlly != y.m.isAlly)         x.m.isAlly compare y.m.isAlly // x first: nonAlly before Ally
    else if (x.m.governance != y.m.governance) y.m.governance compare x.m.governance // y first: because Good < Islamist Rule
    else {
      val (xMod, yMod) = (modifyWoiRoll(1, x.m, false, true), modifyWoiRoll(1, y.m, false, true))
      xMod compare yMod
    }
  }
  
  implicit val CaliphateCapitalCandidateOrdering = new Ordering[CaliphateCapitalCandidate] {
    def compare(x: CaliphateCapitalCandidate, y: CaliphateCapitalCandidate) = compareCapitalCandidates(x, y)
  }
  
  
  // Important: Assumes that the game has already been updated, such that the
  // previousCapital is no longer a caliphateCandidate! Othewise the caliphate
  // size comparisons for the the new capital candidate would include the old
  // capital which is wrong.
  def displaceCaliphateCapital(previousCapital: String): Unit = {
    // Caliphate capital displaced.  Attempt to move it to adjacent caliphate country.
    val adjacents = game.adjacentMuslims(previousCapital) filter (_.caliphateCandidate)
    log()
    log("The Caliphate capital has been displaced!")
    log(separator())
    if (adjacents.size == 0) {
      game = game.adjustFunding(-2).adjustPrestige(2)
      log(s"There are no adjacent Caliphate candidates, remove Caliphate capital")
      log(s"Reduce funding by -2 to ${game.funding}")
      log(s"Increase prestige by +2 to ${game.prestige}")
    }
    else {
      // If Jihadist is human, ask them to pick the new capital.
      // If Jihadist is a bot, the country that connects to the 
      // largest number of daisy chained caliphate memeber.
      // Then pick one at random among any ties.
      val newCapitalName = if (adjacents.size == 1)
        adjacents.head.name
      else if (game.humanRole == Jihadist) {
        val choices = adjacents map (_.name)
        getOneOf(s"Choose new capital (${orList(choices)}): ", choices, None, false).get
      }
      else {
        // The Bot pick the best candidate for the new capital base on
        // the set of conditons outlined by compareCapitalCandidates().
        // We sort the best to worst.  If more than one has the best score then 
        // we choose randomly among them.
        val sorted = adjacents.map(CaliphateCapitalCandidate).sorted
        val best   = sorted.takeWhile(compareCapitalCandidates(_, sorted.head) == 0) map (_.m.name)
        shuffle(best).head
      }
      game = game.setCaliphateCapital(newCapitalName).adjustFunding(-1).adjustPrestige(1)
      log(s"Move Caliphate capital to ${newCapitalName}")
      log(s"Reduce funding by -1 to ${game.funding}")
      log(s"Increase prestige by +1 to ${game.prestige}")
      logSummary(game.caliphateSummary)
    }
  }
  
  // Convergence
  def performConvergence(forCountry: String, awakening: Boolean): Unit = {
    def randomConvergenceTarget: MuslimCountry = {
      @tailrec def getTarget: MuslimCountry = {
        val rmc = randomMuslimCountry
        if (rmc.canTakeAwakeningOrReactionMarker)
          rmc
        else 
          shuffle(game.adjacentMuslims(rmc.name) filter (_.canTakeAwakeningOrReactionMarker)) match {
            case Nil    => getTarget  // Try again
            case x :: _ => x
          }
      }
      getTarget
    }
    
    val rmc = randomConvergenceTarget
    if (awakening) {
      game = game.updateCountry(rmc.copy(awakening = rmc.awakening + 1))
      log(s"Convergence for ${forCountry}: add awakening marker to ${rmc.name}")
    }
    else {
      game = game.updateCountry(rmc.copy(reaction = rmc.reaction + 1))
      log(s"Convergence for ${forCountry}: add reaction marker to ${rmc.name}")
    }
  }
  
  
  // Perform WoI on the given country.
  // TODO:  I think the check for invalid country should be done in the command
  //        before calling this method.
  def warOfIdeas(name: String, die: Int): Unit = {
    game.getCountry(name) match {
      case m: MuslimCountry if m.isAdversary => println("Cannot do War of Ideas in Adversary country")
      case m: MuslimCountry if m.isGood => println("Cannot do War of Ideas in muslim country with Good governance")
      case m: MuslimCountry if m.inRegimeChange && (m.troops + m.militia - m.totalCells) < 5 => 
        println("Cannot do War of Ideas in regime change country, not enought troops + militia")
      case m: MuslimCountry =>
        log()
        log(s"War of Ideas in ${m.name}")
        log(separator())
        testCountry(name)
        log(s"Roll: $die")
        val modRoll = modifyWoiRoll(die, m)
        log(s"Modified Roll: $modRoll")
        if (modRoll < 4)
          log("Failed")
        else if (modRoll == 4 && m.aidMarkers > 0)
          log("Failed, aid marker already present")
        else if (modRoll == 4) {
          game = game.updateCountry(m.copy(aidMarkers = 1))
          log("Failed, place aid marker")
        }
        else if (m.isNeutral) {
          game = game.updateCountry(m.copy(alignment = Ally))
          log("Success, shift alignment to Ally")
        }
        else {
          val caliphateCapital = m.caliphateCapital
          val improved = m.improveGovernance()
          log(s"Success, improve governance of ${improved.name} to ${govToString(improved.governance)}")
          // TODO: perhaps log that civil war, awakening, reaction, aid, etc should be removed if improved to Good ?
          game = game.updateCountry(improved)
          
          if (improved.governance == Good) {
            performConvergence(forCountry = name, awakening = true)
            if (caliphateCapital)
              displaceCaliphateCapital(name)
          }
        }
        
      case n: NonMuslimCountry if n.iranSpecialCase => println("Cannot do War of Ideas in Iran")
      case n: NonMuslimCountry =>
        log()
        log(s"War of Ideas in ${n.name}")
        log(separator())
        val newPosture = if (die > 4) Hard else Soft
        game = game.updateCountry(n.copy(posture = newPosture))
        log(s"Roll: $die")
        log(s"Posture of ${n.name} is now $newPosture")
        if (newPosture == game.usPosture && game.prestige < 12) {
          game = game.adjustPrestige(1)
          log(s"Increase US prestige by one to ${game.prestige}")
        }
        log(s"World Posture is now ${game.worldPostureDisplay}")
    }
  }

  def inspect[T](name: String, value: T): T = {
    println(s"DEBUG: $name == ${value.toString}")
    value
  }
  
  def pause() {
      readLine("Continue ↩︎ ")
  }
  
  var indentation = 0
  
  // Add a two spaces to the current indent.
  def indentLog(): Unit = indentation += 2
  def outdentLog(): Unit = indentation = (indentation - 2) max 0
  def clearIndent(): Unit = indentation = 0
  
  // Print the line to the console and save it in the game's history.
  def log(line: String = ""): Unit = {
    val indentedLine = (" " * indentation) + line
    println(indentedLine)
    game = game.copy(history = game.history :+ indentedLine)
  }
  
  def logAdjustment(name: String, oldValue: Any, newValue: Any): Unit = {
    def normalize(value: Any) = value match {
      case None                       => "none"
      case true                       => "yes"
      case false                      => "no"
      case Some(x)                    => x.toString.trim
      case x if x.toString.trim == "" => "none"
      case x                          => x.toString.trim
    }
    log(s"$name adjusted from '${normalize(oldValue)}' to '${normalize(newValue)}'")
  }
    
  def logAdjustment(countryName: String, attributeName: String, oldValue: Any, newValue: Any): Unit =
    logAdjustment(s"$countryName: $attributeName", oldValue, newValue)
  
  def separator(length: Int = 52, char: Char = '-'): String = char.toString * length

  // Sorts a list column wise.  Returns a list of rows where
  // eash row is a string with the items of that row lined up
  // with a minimum of two spaces separating the columns.
  def columnFormat(list: List[String], numCols: Int): Seq[String] = {
    def padLeft(s: String, width: Int) = s + (" " * (width - s.length))
    val numRows = (list.size + numCols - 1) / numCols
    def colsInRow(row: Int) = {
      val mod = list.size % numCols
      if (mod == 0 || row < numRows - 1) numCols else mod
    }
    val rows      = Array.fill(numRows)(new ListBuffer[String])
    val colWidths = Array.fill(numCols)(0)
    var (row, col) = (0, 0)
    for (entry <- list.sorted) {
      rows(row) += entry
      colWidths(col) = colWidths(col) max entry.length
      if (row + 1 == numRows || col >= colsInRow(row + 1) ) {
        row = 0; col += 1
      }
      else
        row += 1
    }
    
    rows map { entries =>
      (entries.toList.zipWithIndex map { case (entry, col) => 
        padLeft(entry, colWidths(col))
      }).mkString("  ")
    }
  }
  
    

  def printSummary(summary: Seq[String]): Unit = {
    println()
    summary foreach println
  }
  
  def logSummary(summary: Seq[String]): Unit = {
    log()
    summary foreach log
  }
    
  // Save the current game state.
  def saveGameState(filename: String): Unit = {
    // To be done.
  }
  
  
  def polarization(): Unit = {
    val candidates = game.muslim.filter(m => (m.awakening + m.reaction).abs > 1).toList
    log()
    log("Polarization")
    log(separator())
    if (candidates.isEmpty)
      log("No countries affected")
    else {
      // Remember any Caliphate capital in case it is displaced.
      val caliphateCapital = candidates find (_.caliphateCapital) map (_.name)
      // Work with names, because the underlying state of the countries will
      // undergo multiple changes, thus we will need to get the current copy of 
      // the country from the game each time we use it.
      val names = candidates.map(_.name)
      case class Converger(name: String, awakening: Boolean)
      implicit val ConvererOrdering = new Ordering[Converger] {
        def compare(x: Converger, y: Converger) = {
          // Awakening first, then order by name
          if (x.awakening == y.awakening) x.name compare y.name else -(x.awakening compare y.awakening)
        }
      }
      
      var convergers = List.empty[Converger]
      for (name <- names; m = game.getMuslim(name)) {
        (m.awakening + m.reaction) match {
          case 2 =>
            game = game.updateCountry(m.copy(awakening = m.awakening + 1))
            log(s"${name}: add an awakening marker")

          case -2 =>
            game = game.updateCountry(m.copy(reaction = m.reaction - 1))
            log(s"${name}: add a reaction marker")
            
          case x if x > 2 =>
            if (m.isAlly) {
              val improved = m.improveGovernance()
              game = game.updateCountry(improved)
              if (improved.isGood)
                convergers = Converger(name, awakening = true) :: convergers
              log(s"${name}: improve governance to ${govToString(improved.governance)}")
            }
            else {
              val newAlign = if (m.isNeutral) Ally else Neutral
              game = game.updateCountry(m.copy(alignment = newAlign))
              log(s"${name}: shift alignment to $newAlign")
            }


          case _ => // x < -2
            if (m.isAdversary) {
              val worsened = m.worsenGovernance()
              game = game.updateCountry(worsened)
              if (worsened.isIslamic)
                convergers = Converger(name, awakening = false) :: convergers
              log(s"${name}: degrade governance to ${govToString(worsened.governance)}")
            }
            else {
              val newAlign = if (m.isNeutral) Adversary else Neutral
              game = game.updateCountry(m.copy(alignment = newAlign))
              log(s"${name}: shift alignment to $newAlign")
            }
        }
      }
      
      // Now perform convergence for each country that became Good or Islamist Rule.
      if (convergers.nonEmpty) {
        log()
        for (Converger(name, awakening) <- convergers.sorted)
          performConvergence(forCountry = name, awakening)
      }

      // Check to see if the Caliphate Capital has been displaced because its country
      // was improved to Good governance.
      caliphateCapital foreach { capitalName =>
        if (game.getMuslim(capitalName).isGood)
          displaceCaliphateCapital(capitalName)
      }
    }
  }
  
  // Process the US civil war losses.
  // If the US is human then prompt for pieces to remove.
  // Return the number of unresolved hits
  def usCivilWarLosses(m: MuslimCountry, hits: Int): Int = {
    if (hits == 0 || m.totalTroops + m.militia == 0) {
      log(s"${m.name}: The US suffer no attrition")
      hits
    }
    else {
      // Calculate the losses for the bot.
      // If this removes all troops, troopMarkers, and militia from the country
      // and the US is human, then we can skip prompting for losses.
      // [Rule 13.3.7] The bot removes troops cubes first.
      var hitsRemaining = hits
      var troopsLost = if (m.troops >= hitsRemaining) hitsRemaining else m.troops
      hitsRemaining -= troopsLost
      var troopMarkersLost = for (TroopsMarker(name, num) <- m.troopsMarkers; if hitsRemaining > 0) 
        yield {
          hitsRemaining = (hitsRemaining - num) max 0
          name
        }
      var militiaLost = if (m.militia >= hitsRemaining) hitsRemaining else m.militia
      hitsRemaining -= militiaLost
      
      var updated = m.copy(
        troops  = m.troops  - troopsLost,
        militia = m.militia - militiaLost,
        markers = m.markers filterNot troopMarkersLost.contains
      )

      // TODO: Allow human player to pick losses.
      // if (game.human == US && updated.totalTroops + updated.militia > 0) {
      //   hitsRemaining = hits
      //   // ...
      // }
      
      game = game.updateCountry(updated)
      val b = new ListBuffer[String]
      if (troopsLost > 0)            b += s"$troopsLost troops"
      if (troopMarkersLost.nonEmpty) b += troopMarkersLost mkString ", "
      if (militiaLost > 0)           b += s"$militiaLost militia"
      log(s"${m.name}: US attrition - remove ${b mkString ", "}")
      hitsRemaining    
    }
  }
  
  // Process the Jihadist civil war losses.
  // If the Jihadist is human then prompt for pieces to remove.
  // Return the number of unresolved losses
  def jihadistCivilWarLosses(m: MuslimCountry, hits: Int): Int = {
    if (hits == 0 || m.totalCells == 0) {
      log(s"${m.name}: The Jihadists suffer no attrition")
      hits
    }
    else {
      // Remove two cells per hit is any troops present or if "Advisors" marker present.
      val multiplier = if (m.totalTroops > 0 || m.hasMarker("Advisors")) 2 else 1
      val losses     = hits * multiplier
      val (activeLost, sleepersLost) = if (m.totalCells <= losses)
        (m.activeCells, m.sleeperCells)
      else {
        val active = if (m.activeCells <= losses) m.activeCells else losses
        val remaining = losses - active
        val sleeper = if (m.sleeperCells <= remaining) m.sleeperCells else remaining
        (active, sleeper)
      }
      val hitsRemaining = ((losses - activeLost - sleepersLost) max 0) / multiplier
      
      var updated = m.copy(
        activeCells  = m.activeCells  - activeLost,
        sleeperCells = m.sleeperCells - sleepersLost
      )
      game = game.updateCountry(updated)
      
      val b = new ListBuffer[String]
      if (activeLost > 0)            b += s"$activeLost active cells"
      if (sleepersLost > 0)           b += s"$sleepersLost sleeper cells"
      log(s"${m.name}: Jihadist attrition - remove ${b mkString ", "}")
      hitsRemaining    
    }
  }
  
  def civilWarAttrition(): Unit = {
    val civilWars = game.muslim.filter(m => (m.civilWar)).toList
    log()
    log("Civil War Attrition")
    log(separator())
    if (civilWars.isEmpty)
      log("No countries in civil war")
    else {
      val caliphateCapital = civilWars find (_.caliphateCapital) map (_.name)
      for (m <- civilWars) {
        val jihadHits= m.totalCells / 6 + (if (dieRoll <= m.totalCells % 6) 1 else 0)
        val usHits    = m.totalTroopsAndMilitia / 6 + (if (dieRoll <= m.totalTroopsAndMilitia % 6) 1 else 0)
        if (jihadHits + usHits == 0)
          log(s"${m.name}: no attrition suffered by either side")
        else {
          val unfulfilledJihadHits = usCivilWarLosses(m, jihadHits)
          val unfulfilledUSHits    = jihadistCivilWarLosses(m, usHits)
          // The game state is modified by usCivilWarLosses() and jihadistCivilWarLosses()
          // so we get a fresh copy of the muslim country.
          val afterLosses = game.getMuslim(m.name)
          if (unfulfilledJihadHits + unfulfilledUSHits > 0) {
            if (unfulfilledJihadHits > 0) log(s"$unfulfilledJihadHits unfulfilled Jihadist hits against the US")
            if (unfulfilledUSHits    > 0) log(s"$unfulfilledUSHits unfulfilled US hits against the Jihadist")
            val delta = unfulfilledJihadHits - unfulfilledUSHits
            if (delta == 0)
              log("The unfulfilled hits are equal so there is no further action")
            else if (delta > 0) {
              // Shift toward Adversary/Worsen governance
              if (afterLosses.isAdversary) {
                val worsened = afterLosses.worsenGovernance()
                game = game.updateCountry(worsened)
                log(s"${worsened.name}: degrade governance to ${govToString(worsened.governance)}")
                if (worsened.isIslamic)
                  performConvergence(forCountry = worsened.name, awakening = false)
              }
              else {
                val newAlign = if (afterLosses.isNeutral) Adversary else Neutral
                game = game.updateCountry(afterLosses.copy(alignment = newAlign))
                log(s"${afterLosses.name}: shift alignment to $newAlign")
              }
            }
            else {
              // Shift toward Ally/Improve governance
              if (afterLosses.isAlly) {
                val improved = afterLosses.improveGovernance()
                game = game.updateCountry(improved)
                log(s"${improved.name}: improve governance to ${govToString(improved.governance)}")
                if (improved.isGood)
                  performConvergence(forCountry = improved.name, awakening = true)
              }
              else {
                val newAlign = if (afterLosses.isNeutral) Ally else Neutral
                game = game.updateCountry(afterLosses.copy(alignment = newAlign))
                log(s"${afterLosses.name}: shift alignment to $newAlign")
              }
            }
          }
        }
      }
      
      // Check to see if the Caliphate Capital has been displaced because its country
      // was improved to Good governance.
      caliphateCapital foreach { capitalName =>
        if (game.getMuslim(capitalName).isGood)
          displaceCaliphateCapital(capitalName)
      }
    }
  }
  
  def endTurn(): Unit = {
    // TODO: resolve plots.
    
    log()
    log("End of turn")
    log(separator())
    
    // TODO - Also check for fracking whcih can affect funding
    if (false) { // Check for pirates marker and Somalia or Yemen at Islamist Rule
      log("No funding drop because Pirates is in effect")
    }
    else {
      game = game.adjustFunding(-1)
      log(s"Jihadist funding drops to ${game.funding}")
    }
    if (game.numIslamic > 0) {
      game = game.adjustPrestige(-1)
      log(s"US prestige drops to ${game.prestige}, at least 1 country is under Islamist Rule")
    }
    else
      log(s"US prestige stays at ${game.prestige}, no countries under Islamist Rule")
    
    val (worldPosture, level) = game.gwot
    if (game.usPosture == worldPosture && level == 3) {
      game = game.adjustPrestige(1)
      log(s"World posture is $worldPosture $level and US posture is $worldPosture , prestige increases ${game.prestige}")
    }
    if (game.humanRole == US) {
      game = game.copy(usReserves = 0)
      log(s"US reserves set to zero")
    }
    else {
      game = game.copy(jihadistReserves = 0)
      log(s"Jihadist reserves set to zero")
    }
    
    if (game.resolvedPlots.nonEmpty) {
      game = game.copy(resolvedPlots = Nil, availablePlots = game.availablePlots ::: game.resolvedPlots)
      log("Return resolved plots to available plots box")
    }
    
    polarization()
    civilWarAttrition()
    
    // Calculate number of cards drawn
    val usCards = USCardDraw(game.troopCommitment)
    val jihadistCards = JihadistCardDraw(game.fundingLevel)
    log()
    log("Draw Cards")
    log(separator())
    log(s"US player will draw $usCards cards")
    log(s"Jihadist player will draw $jihadistCards cards")
    
    if (game.offMapTroops > 0) {
      log(s"Return ${game.offMapTroops} troops from the off map box to the troops track")
      game = game.copy(offMapTroops = 0)
    }
    
    if (game.lapsing.nonEmpty) {
      log(s"Discard the lapsing events: ${cardNumsAndNames(game.lapsing)}")
      game = game.copy(lapsing = Nil)
    }
    game.firstPlot foreach { num => 
      log(s"Discard the firstplot card: ${cardNumAndName(num)}")
      game = game.copy(firstPlot = None)
    }
    
    for (rc <- game.muslim filter (_.regimeChange == GreenRegimeChange)) {
      log(s"  ${rc.name}: Flip regime marker to tan side")
      game = game.updateCountry(rc.copy(regimeChange = TanRegimeChange))
    }
    
    log()
    logSummary(game.scoringSummary)
    log()
    logSummary(game.statusSummary)
        
    previousState = None  // Cannot undo at end of turn.
    gameTurns = game :: gameTurns
    saveGameState("some file name")
    
  }
  
  case object UserExit extends Exception
  
  // def doWarOfIdeas(country: Country)
  def main(args: Array[String]): Unit = {
    
    // parse cmd line args -- to be done
    // prompt for scenario -- to be done
    // prompt for bot's (jihadish ideology / us resolve) difficulty level. -- to be done
    val scenario = new Awakening2010
    // ask which side the user wishes to play -- to be done
    val (humanRole, botDifficulties) = if (false)
      (US, Muddled :: Coherent :: Attractive :: Nil)
    else
      (Jihadist, OffGuard :: Competent :: Adept :: Nil)
    
    game = initialGameState(scenario, humanRole, botDifficulties)
    
    logSummary(game.scenarioSummary)
    logSummary(game.scoringSummary)
    logSummary(game.statusSummary)
    saveGameState("initial state before first turn.")
    game = game.copy(turn = game.turn + 1)
    log()
    log()
    log(s"Start of turn ${game.turn}")
    log(separator(char = '='))
    try commandLoop()
    catch {
      case UserExit => 
    }
  }

  
  // ---------------------------------------------
  // Process all top level user commands.
  @tailrec def commandLoop(): Unit = {
    readLine(s"\n[Turn ${game.turn}] ") match {
      case null => println() // User pressed Ctrl-d (end of file)
      case cmd =>
        doCommand(cmd)
        commandLoop()
    }
  }
  
  
  // Parse the top level input and execute the appropriate command.
  def doCommand(input: String): Unit = {
    class Command(val name: String, val help: String)
    val Commands = List(
      new Command("quit",     """Save the current turn and quit the game"""),
      new Command("help",     """List available commands"""),
      new Command("us",       """Enter a card number for a US card play"""),
      new Command("jihadist", """Enter a card number for a Jihadist card play"""),
      new Command("show",     """Display the current game state
                                |  show all        - entire game state
                                |  show summary    - game summary including score
                                |  show scenario   - scenario and difficulty level
                                |  show caliphate  - countries making up the Caliphate
                                |  show civil wars - countries in civil war
                                |  show <country>  - state of a single country""".stripMargin),
      new Command("adjust",   """Adjust game settings <Minimal rule checking is applied>
                                |  adjust prestige      - US prestige level
                                |  adjust funding       - Jihadist funding level
                                |  adjust difficulty    - Jihadist ideology/US resolve
                                |  adjust lapsing       - Current lapsing events
                                |  adjust first plot    - Current first plot card
                                |  adjust markers       - Current global event markers
                                |  adjust reserves      - US and/or Jihadist reserves
                                |  adjust plots         - Available/resolved plots
                                |  adjust offmap troops - Number of troops in off map box.
                                |  adjust <country>     - Country specific settings""".stripMargin),
      new Command("history",  """Display game history"""),
      new Command("undo",     """Roll back to the last card played"""),
      new Command("rollback", """Roll back to the start of any previous turn in the game""")
    )
    val CmdNames = (Commands map (_.name)).sorted
    
    def showCommandHelp(cmd: String) = Commands find (_.name == cmd) foreach (c => println(c.help))
    
    val tokens = input.split("\\s+").toList.dropWhile(_ == "")
    tokens.headOption foreach { verb =>
      val param = if (tokens.tail.nonEmpty) Some(tokens.tail.mkString(" ")) else None
      matchOne(verb, CmdNames) foreach {
        case "quit" =>
          throw UserExit
        case "help" if param.isEmpty =>
          println("Available commands: (type help <command> for more detail)")
          println(orList(CmdNames))
        case "help"     => matchOne(param.get, CmdNames) foreach showCommandHelp
        case "us"       => usCardPlay(param)
        case "jihadist" => jihadistCardPlay(param)
        case "show"     => showGameState(param)
        case "adjust"   => adjustSettings(param)
        case "history"  => game.history foreach println  // TODO: Allow > file.txt
        case "undo"     => println("Not implemented.")
        case "rollback" => println("Not implemented.")
        case cmd        => println(s"Internal error: Command '$cmd' is not valid")
      }
    }
  }
  
  
  
  def showGameState(param: Option[String]): Unit = {
    val options = "all" ::"summary" :: "scenario" :: "caliphate" ::
                  "civil wars" :: (game.countries map (_.name)).sorted
    getOneOf("Show: ", options, param, true, CountryAbbreviations) foreach {
      case "summary"    => printSummary(game.scoringSummary); printSummary(game.statusSummary)
      case "scenario"   => printSummary(game.scenarioSummary)
      case "caliphate"  => printSummary(game.caliphateSummary)
      case "civil wars" => printSummary(game.civilWarSummary)
      case "all"        => printGameState()
      case name         => printSummary(game.countrySummary(name))
    }
  }
  
  // Print the entire game state to stdout
  def printGameState(): Unit = {
    def printCountries(title: String, countries: List[String]): Unit = {
      println()
      println(title)
      println(separator())
      if (countries.isEmpty)
        println("none")
      else
        for (name <- countries; line <- game.countrySummary(name))
          println(line)
    }
    
    printSummary(game.scenarioSummary)
    printCountries("Muslim Countries with Good Governance", (game.muslim filter(_.isGood) map (_.name)).toList.sorted)
    printCountries("Muslim Countries with Fair Governance", (game.muslim filter(_.isFair) map (_.name)).toList.sorted)
    printCountries("Muslim Countries with Poor Governance", (game.muslim filter(_.isPoor) map (_.name)).toList.sorted)
    printCountries("Muslim Countries under Islamic Rule",   (game.muslim filter(_.isIslamic) map (_.name)).toList.sorted)
    printCountries("Untested Muslim Countries with Data",   (game.muslim filter(_.untestedWithData) map (_.name)).toList.sorted)
    printCountries("Non-Muslim Countries with Hard Posture",(game.nonMuslim filter (_.isHard) map (_.name)).toList.sorted)
    printCountries("Non-Muslim Countries with Soft Posture",(game.nonMuslim filter (_.isSoft) map (_.name)).toList.sorted)
    val iranSpecial = game.nonMuslim find (_.iranSpecialCase) map (_.name)
    if (iranSpecial.nonEmpty)
      printCountries("Iran Special Case", iranSpecial.toList)
    printSummary(game.scoringSummary)
    printSummary(game.statusSummary)
    printSummary(game.civilWarSummary)
    printSummary(game.caliphateSummary)
  }
  
  def usCardPlay(number: Option[String]): Unit = {
    println("Not implemented.")
  }
  
  def jihadistCardPlay(number: Option[String]): Unit = {
    println("Not implemented.")
  }
  
  
  def adjustSettings(param: Option[String]): Unit = {
    val options = "prestige" ::"funding" :: "difficulty" :: "lapsing" :: "first plot" :: "markers" ::
                  "reserves" :: "plots" :: "offmap troops" :: (game.countries map (_.name)).sorted
    getOneOf("Adjust: ", options, param, true, CountryAbbreviations) foreach {
      case "prestige"   =>
        adjustInt("Prestige", game.prestige, 1 to 12) foreach { value =>
          logAdjustment("Prestige", game.prestige, value)
          game = game.copy(prestige = value)
        }
      case "funding"    =>
        adjustInt("Funding", game.funding, 1 to 9) foreach { value =>
          logAdjustment("Prestige", game.funding, value)
          game = game.copy(funding = value)
        }
      case "offmap troops" =>
        adjustInt("Offmap troops", game.offMapTroops, 0 to (game.offMapTroops + game.troopsAvailable)) foreach { value =>
          logAdjustment("Offmap troops", game.offMapTroops, value)
          game = game.copy(offMapTroops = value)
        }
      
      case "difficulty"    => adjustDifficulty()
      case "lapsing"       => adjustLapsing()
      case "first plot"    => adjustFirstPlot()
      case "markers"       => adjustMarkers()
      case "reserves"      => adjustReserves()
      case "plots"         => adjustPlots()
      case name            => adjustCountry(name)
    }
  }
  
  def adjustInt(name: String, current: Int, range: Range): Option[Int] = {
    val INT = """(\d+)""".r
    val prompt = s"$name is $current.  Enter new value (${range.min} - ${range.max}) "
    @tailrec def getResponse(): Option[Int] =
      readLine(prompt) match {
        case null | "" => None
        case INT(x) if range contains x.toInt => Some(x.toInt)
        case input =>
          println(s"$input is not valid")
          getResponse()
      }
    getResponse()
  }
  
  def adjustReserves(): Unit = {
    val choices = List(US.toString, Jihadist.toString)
    for {
      roleName <- getOneOf(s"Which side (${orList(choices)}): ", choices)
      role     = Role(roleName)
      current  = if (role == US) game.usReserves else game.jihadistReserves
      value    <- adjustInt(s"$role reserves", current, 0 to 2)
    } {
      logAdjustment(s"$role reserves", current, value)
      role match {
        case US       => game = game.copy(usReserves = value)
        case Jihadist => game = game.copy(jihadistReserves = value)
      }
    }
  }
  
  
  def adjustDifficulty(): Unit = {
    val INT = """(\d+)""".r
    val AllLevels = if (game.botRole == US)
      OffGuard::Competent::Adept::Vigilant::Ruthless::NoMercy::Nil
    else
      Muddled::Coherent::Attractive::Potent::Infectious::Virulent::Nil
    val AllNames = AllLevels map (_.name)
    var inEffect = game.botDifficulties map (_.name)
    
    val label = if (game.botRole == US) "US resolve" else "Jihadist ideology"
    val standard = for (num <- 1 to AllNames.size; included = AllNames take num)
      yield s"$num) ${included.mkString(", ")}"
    // Don't allow the first name as a choice, as it represents the base
    // line difficulty and cannot be removed.
    val choices = List.range(1, 7) ::: (AllNames drop 1)
      
    @tailrec def getNextResponse(): Unit = {
      val current = s"Current $label: ${inEffect.mkString(", ")}"
      val help1 = "" :: current :: separator(current.length) :: 
                  s"Select a number for standard $label combinations" :: standard.toList
      
      val help2 = "" :: s"Or enter the name of a $label to toggle its inclusion" ::
                     (AllLevels map (_.toString))
      
      help1 foreach println
      help2 foreach println
      getOneOf(s"$label: ", choices) match {
        case None =>
        case Some(INT(x)) =>
          inEffect = AllNames take x.toInt
          getNextResponse()
        case Some(name) if inEffect contains name =>
          inEffect = inEffect filterNot (_ == name)
          getNextResponse()
        case Some(name) =>
          inEffect = AllNames filter (n => (inEffect contains n) || n == name) // Maintain standard order.
          getNextResponse()
      }
    }
    getNextResponse()
    val updated = inEffect map BotDifficulty.apply
    if (updated != game.botDifficulties) {
      logAdjustment(s"$label", game.botDifficulties.map(_.name).mkString(", "), updated.map(_.name).mkString(", "))
      game = game.copy(botDifficulties = updated)
    }  
  }
  
  def adjustLapsing(): Unit = {
    var inPlay = game.lapsing
    def available = lapsingCardNumbers filterNot inPlay.contains
    @tailrec def getNextResponse(): Unit = {
      println()
      println("Lapsing events that are currently in play:")
      println(if (inPlay.isEmpty) "none" else cardNumsAndNames(inPlay.sorted))
      println()
      println("Lapsing events that are out of play:")
      println(if (available.isEmpty) "none" else cardNumsAndNames(available))
      println()
      println("Enter a card number to move it between in play and out of play.")
      getOneOf("Card #: ", lapsingCardNumbers) map (_.toInt) match {
        case None =>
        case Some(num) if inPlay contains num =>
          inPlay = inPlay filterNot(_ == num)
          getNextResponse()
        case Some(num) =>
          inPlay = num :: inPlay
          getNextResponse()
      }
    }
    getNextResponse()
    inPlay = inPlay.sorted
    if (inPlay != game.lapsing) {
      logAdjustment("Lapsing Events", cardNumsAndNames(game.lapsing), cardNumsAndNames(inPlay))
      game = game.copy(lapsing = inPlay)
    }  
  }
  
  def adjustFirstPlot(): Unit = {
    var inPlay = game.firstPlot
    println()
    println(s"Current first plot card: ${inPlay map cardNumAndName getOrElse "none"}")
    println()
    println("Enter a card number to add or remove it as the first plot card.")
    getCardNumber("Card #: ") foreach {
      case num if inPlay.exists(_ == num) => inPlay = None
      case num                            => inPlay = Some(num)
    }
    
    if (inPlay != game.firstPlot) {
      logAdjustment("First plot", game.firstPlot map cardNumAndName, inPlay map cardNumAndName)
      game = game.copy(firstPlot = inPlay)
    }  
  }
  
    
  def adjustMarkers(): Unit = {
    var inPlay = game.markers
    def available = GlobalMarkers filterNot inPlay.contains
    def showColums(xs: List[String]): Unit = {
      if (xs.isEmpty) println("none")
      else columnFormat(xs, 4) foreach println
    }
    @tailrec def getNextResponse(): Unit = {
      println()
      println("Global markers that are currently in play:")
      showColums(inPlay)
      println()
      println("Global markers that are out of play:")
      showColums(available)
      println()
      println("Enter a marker name to move it between in play and out of play.")
      getOneOf("Marker: ", GlobalMarkers) match {
        case None =>
        case Some(name) if inPlay contains name =>
          inPlay = inPlay filterNot(_ == name)
          getNextResponse()
        case Some(name) =>
          inPlay = name :: inPlay
          getNextResponse()
      }
    }
    getNextResponse()
    inPlay = inPlay.sorted
    if (inPlay != game.markers) {
      logAdjustment("Global Markers", game.markers.mkString(", "), inPlay.mkString(", "))
      game = game.copy(markers = inPlay)
    }  
  }

  // If the human is the Jihadist, the all plots are visible.
  // If the human is the US, then only resolved plots are visible.
  def adjustPlots(): Unit = {
    def showPlots(plots: Vector[Int], startIndex: Int): Unit = {
      @tailrec def showNext(list: List[Int], index: Int): Unit = list match {
        case Nil =>
        case plot :: rest =>
          println(s"$index) ${plotName(plot)}")
          showNext(rest, index + 1)
      }
      
      if (plots.isEmpty)
        println("none")
      else
        showNext(plots.toList, startIndex)
    }
    
    var available = game.availablePlots.toVector
    var resolved  = game.resolvedPlots.toVector

    if (game.humanRole == US) {
      @tailrec def getNextResponse(): Unit = {
        println()
        println("Available plots:")
        println(s"${available.size} hidden plots")
        println()
        println("Resolved plots:")
        showPlots(resolved, 1)
        println()
        if (resolved.isEmpty)
          println("Plots cannot be adusted at this time.")
        else {
          println("Select a resolved plot to make available")
          getOneOf("Plot: ", 1 to (resolved.size )) map (_.toInt) match {
            case None =>
            case Some(num) =>
              val index    = num - 1
              val selected = resolved(index)
              resolved  = resolved.patch(index, Vector.empty, 1)
              available = available :+ selected
              if (resolved.nonEmpty)
                getNextResponse()
          }
        }
      }
      getNextResponse()
    }
    else { // humanRoll == Jihadist
      @tailrec def getNextResponse(): Unit = {
        println()
        println("Available plots:")
        showPlots(available, 1)
        println()
        println("Resolved plots:")
        showPlots(resolved, available.size + 1)
        println()
        if (available.isEmpty && resolved.isEmpty)
          println("Plots cannot be adusted at this time.")
        else {
          println("Select a plot to move between available and resolved")
          getOneOf("Plot: ", 1 to (available.size + resolved.size )) map (_.toInt) match {
            case None =>
            case Some(num) if num <= available.size =>
              val index    = num - 1
              val selected = available(index)
              available = available.patch(index, Vector.empty, 1)
              resolved  = resolved :+ selected
              getNextResponse()
            
            case Some(num) =>
              val index    = num - available.size - 1
              val selected = resolved(index)
              resolved  = resolved.patch(index, Vector.empty, 1)
              available = available :+ selected
              getNextResponse()
          }
        }
      }
      getNextResponse()
    }
    val alist = available.toList.sorted
    val rlist = resolved.toList.sorted
    if (alist != game.availablePlots.sorted || rlist != game.resolvedPlots.sorted) {
      logAdjustment("Available plots", 
                    s"[${plotsDisplay(game.availablePlots, game.humanRole)}]",
                    s"[${plotsDisplay(alist, game.humanRole)}]")
      logAdjustment("Resolved plots", 
                    s"[${plotsDisplay(game.resolvedPlots, Jihadist)}]",
                    s"[${plotsDisplay(rlist, Jihadist)}]")
      game = game.copy(availablePlots = alist, resolvedPlots = rlist)
    }
    
  }
  
  def adjustCountry(name: String): Unit = {
    @tailrec def getNextResponse(): Unit = {
      println()
      println(separator())
      game.countrySummary(name, showAll = true) foreach println
      println()
        
      if (game.isMuslim(name)) {
        val choices = List(
          "alignment", "governance", "active cells", "sleeper cells", "regime change",
          "cadre", "troops", "militia", "aid", "awakening", "reaction",
          "besieged regime", "civil war", "caliphate", "plots", "markers"
        ).sorted
        getOneOf("Attribute (? for list): ", choices) match {
          case None        =>
          case Some(attribute) =>
            attribute match {
              case "alignment"       => adjustAlignment(name)
              case "governance"      => adjustGovernance(name)
              case "active cells"    => adjustActiveCells(name)
              case "sleeper cells"   => adjustSleeperCells(name)
              case "cadre"           => adjustCadre(name)
              case "troops"          => adjustTroops(name)
              case "militia"         => adjustMilitia(name)
              case "aid"             => adjustAid(name)
              case "awakening"       => adjustAwakening(name)
              case "reaction"        => adjustReaction(name)
              case "besieged regime" => adjustBesiegedRegime(name)
              case "regime change"   => adjustRegimeChange(name)
              case "civil war"       => adjustCivilWar(name)
              case "caliphate"       => adjustCaliphateCapital(name)
              case "plots"           => adJustCountryPlots(name)
              case "markers"         => adjustCountryMarkers(name)
            }
            getNextResponse()
        }
      }
      else { // Nonmuslim
        val choices = List(
          "posture", "active cells", "sleeper cells", "cadre", "plots", "markers"
        ).sorted
        getOneOf("Attribute (? for list): ", choices) match {
          case None        =>
          case Some(attribute) =>
            attribute match {
              case "posture"       => adjustPosture(name)
              case "active cells"  => adjustActiveCells(name)
              case "sleeper cells" => adjustSleeperCells(name)
              case "cadre"         => adjustCadre(name)
              case "plots"         => adJustCountryPlots(name)
              case "markers"       => adjustCountryMarkers(name)
            }
            getNextResponse()
        }
      }
    }
    getNextResponse()
  }
  
  def adjustPosture(name: String): Unit = {
    game.getCountry(name) match {
      case _: MuslimCountry => throw new IllegalArgumentException(s"Cannot set posture of Muslim country: $name")
      case n: NonMuslimCountry if n.iranSpecialCase =>
        println("Iran is a special case that does not have a posture.")
        pause()
      case n: NonMuslimCountry =>
        val choices = (PostureUntested::Soft::Hard::Nil) filterNot (_ == n.posture)
        val prompt = s"New posture (${orList(choices)}): "
        getOneOf(prompt, choices) foreach { newPosture =>
          logAdjustment(name, "Prestige", n.posture, newPosture)
          game = game.updateCountry(n.copy(posture = newPosture))
        }
    }
  }
  
  def adjustAlignment(name: String): Unit = {
    game.getCountry(name) match {
      case _: NonMuslimCountry => throw new IllegalArgumentException(s"Cannot set alignment of non-Muslim country: $name")
      case m: MuslimCountry if m.unTested =>
        println(s"$name is untested. Set the governance first.")
        pause()
      case m: MuslimCountry =>
        val choices = (Ally::Neutral::Adversary::Nil) filterNot (_ == m.alignment)
        val prompt = s"New alignment (${orList(choices)}): "
        getOneOf(prompt, choices) foreach { newAlignment =>
          logAdjustment(name, "Alignment", m.alignment, newAlignment)
          game = game.updateCountry(m.copy(alignment = newAlignment))
        }
    }
  }

  def adjustGovernance(name: String): Unit = {
    game.getCountry(name) match {
      case _: NonMuslimCountry => throw new IllegalArgumentException(s"Cannot set governance of non-Muslim country: $name")
      case m: MuslimCountry =>
        val choices = ((GovernanceUntested::Good::Fair::Poor::IslamistRule::Nil)
                      filterNot (_ == m.governance)
                      map govToString)
        val prompt = s"New governance (${orList(choices)}): "
        getOneOf(prompt, choices) map govFromString foreach { newGov =>
          // When a country becomes Good or Islamist Rule, the country cannot contain:
          //  aid, besiege regime, civil war, regime change, awakening, reaction
          // Further when the country becomes Good, it cannot be the Calipate Capital.
          val goodOrIslamist = newGov == Good || newGov == IslamistRule
          val nixCapital      = newGov == IslamistRule && m.caliphateCapital
          val nixAid          = goodOrIslamist && m.aidMarkers != 0
          val nixBesieged     = goodOrIslamist && m.besiegedRegime
          val nixCivilWar     = goodOrIslamist && m.civilWar
          val nixRegimeChange = goodOrIslamist && m.inRegimeChange
          val nixAwakening    = goodOrIslamist && m.awakening != 0
          val nixReaction     = goodOrIslamist && m.reaction != 0
          val anyWarnings     = nixCapital || nixAid || nixBesieged || nixCivilWar || 
                                nixRegimeChange || nixAwakening || nixRegimeChange
          def warn(condition: Boolean, message: String): Unit = if (condition) println(message)
          warn(nixCapital, s"$name will no longer be the Caliphate Capital.\n" +
                           "The Caliphate will be removed completely.")
          warn(nixAid, "The aid markers will be removed.")
          warn(nixBesieged, "The besieged regime marker will be removed.")
          warn(nixCivilWar, "The civil war marker will be removed.")
          warn(nixRegimeChange, "The regime change marker will be removed.")
          warn(nixAwakening, "The awakening markers will be removed.")
          warn(nixReaction, "The reaction markers will be removed.")
          if (!anyWarnings || askYorN(s"Do you wish continue (y/n)? ")) {
            var updated = m
            if (m.unTested) {
              logAdjustment(name, "Alignment", updated.alignment, Neutral)
              updated = updated.copy(alignment = Neutral) 
            }
            logAdjustment(name, "Governance", govToString(updated.governance), govToString(newGov))
            updated = updated.copy(governance = newGov)
            if (nixCapital) {
              log(s"$name lost Caliphate Capital status.  Caliphate no longer delcared.")
              updated = updated.copy(caliphateCapital = false)
            }
            if (nixAid) {
              logAdjustment(name, "Aid", updated.aidMarkers, 0)
              updated = updated.copy(aidMarkers = 0)
            }
            if (nixBesieged) {
              logAdjustment(name, "Besieged regime", updated.besiegedRegime, false)
              updated = updated.copy(besiegedRegime = false)
            }
            if (nixCivilWar) {
              logAdjustment(name, "Civil War", updated.civilWar, false)
              updated = updated.copy(civilWar = false)
            }
            if (nixRegimeChange) {
              logAdjustment(name, "Regime change", updated.regimeChange, NoRegimeChange)
              updated = updated.copy(regimeChange = NoRegimeChange)
            }
            if (nixAwakening) {
              logAdjustment(name, "Awakening markers", updated.awakening, 0)
              updated = updated.copy(awakening = 0)
            }
            if (nixReaction) {
              logAdjustment(name, "Reaction markers", updated.reaction, 0)
              updated = updated.copy(reaction = 0)
            }
            game = game.updateCountry(updated)
            pause()
          }
        }
    }
  }

  def adjustActiveCells(name: String): Unit = {
    val c = game.getCountry(name)
    val maxCells = c.activeCells + game.cellsOnTrack + game.cellsInCamp
    if (maxCells == 0) {
      println("There a no cells available to add to this country.")
      pause()
    }
    else 
      adjustInt("Active cells", c.activeCells, 0 to maxCells) foreach { value =>
        logAdjustment(c.name, "Active cells", c.activeCells, value)
        c match {
          case m: MuslimCountry    => game = game.updateCountry(m.copy(activeCells = value))
          case n: NonMuslimCountry => game = game.updateCountry(n.copy(activeCells = value))
        }
      }
  }
  
  
  def adjustSleeperCells(name: String): Unit = {
    val c = game.getCountry(name)
    val maxCells = c.sleeperCells + game.cellsOnTrack + game.cellsInCamp
    if (game.isCaliphateMember(name)) {
      println(s"$name is a Caliphate member and therefore cannot have sleeper cells.")
      pause()
      
    }
    else if (maxCells == 0) {
      println("There a no cells available to add to this country.")
      pause()
    }
    else 
      adjustInt("Sleeper cells", c.sleeperCells, 0 to maxCells) foreach { value =>
        logAdjustment(c.name, "Sleeper cells", c.sleeperCells, value)
        c match {
          case m: MuslimCountry    => game = game.updateCountry(m.copy(sleeperCells = value))
          case n: NonMuslimCountry => game = game.updateCountry(n.copy(sleeperCells = value))
        }
      }
  }
  
  def adjustCadre(name: String): Unit = {
    val c = game.getCountry(name)
    val newValue = !c.hasCadre
    logAdjustment(c.name, "Cadre", c.hasCadre, newValue)
    pause()
    c match {
      case m: MuslimCountry    => game = game.updateCountry(m.copy(hasCadre = newValue))
      case n: NonMuslimCountry => game = game.updateCountry(n.copy(hasCadre = newValue))
    }
  }
    
  def adjustTroops(name: String): Unit = {
    game.getCountry(name) match {
      case _: NonMuslimCountry => throw new IllegalArgumentException(s"Cannot add troops to non-Muslim country: $name")
      case m: MuslimCountry =>
        val maxTroops = m.troops + game.troopsAvailable
        if (maxTroops == 0) {
          println("There a no troops available to add to this country.")
          pause()
        }
        else
          adjustInt("Troops", m.troops, 0 to maxTroops) foreach { value =>
            logAdjustment(name, "Troops", m.troops, value)
            game = game.updateCountry(m.copy(troops = value))
          }
    }
  }
  
  def adjustMilitia(name: String): Unit = {
    game.getCountry(name) match {
      case _: NonMuslimCountry => throw new IllegalArgumentException(s"Cannot add militia to non-Muslim country: $name")
      case m: MuslimCountry =>
        val maxMilitia = m.militia + game.militiaAvailable
        if (maxMilitia == 0) {
          println("There a no troops available to add to this country.")
          pause()
        }
        else
          adjustInt("Militia", m.militia, 0 to maxMilitia) foreach { value =>
            logAdjustment(name, "Militia", m.militia, value)
            game = game.updateCountry(m.copy(militia = value))
          }
    }
  }
  
  def adjustAid(name: String): Unit = {
    game.getCountry(name) match {
      case _: NonMuslimCountry => throw new IllegalArgumentException(s"Cannot add aid to non-Muslim country: $name")
      case m: MuslimCountry if m.isGood =>
        println("Cannot add aid to a country with Good governance")
        pause()
      case m: MuslimCountry if m.isIslamic =>
        println("Cannot add aid to a country under Islamist Rule")
        pause()
      case m: MuslimCountry =>
        adjustInt("Aid", m.aidMarkers, 0 to 10) foreach { value =>
          logAdjustment(name, "Aid", m.aidMarkers, value)
          game = game.updateCountry(m.copy(aidMarkers = value))
        }
    }
  }
  
  def adjustAwakening(name: String): Unit = {
    game.getCountry(name) match {
      case _: NonMuslimCountry => throw new IllegalArgumentException(s"Cannot add awakening markers to non-Muslim country: $name")
      case m: MuslimCountry if m.isGood =>
        println("Cannot add awakening markers to a country with Good governance")
        pause()
      case m: MuslimCountry if m.isIslamic =>
        println("Cannot add awakening markers to a country under Islamist Rule")
        pause()
      case m: MuslimCountry if m.civilWar =>
        println("Cannot add awakening markers to a country in Civil War")
        pause()
      case m: MuslimCountry =>
        adjustInt("Awakening markers", m.awakening, 0 to 10) foreach { value =>
          logAdjustment(name, "Awakening markers", m.awakening, value)
          game = game.updateCountry(m.copy(awakening = value))
        }
    }
  }
  
  def adjustReaction(name: String): Unit = {
    game.getCountry(name) match {
      case _: NonMuslimCountry => throw new IllegalArgumentException(s"Cannot add reaction markers to non-Muslim country: $name")
      case m: MuslimCountry if m.isGood =>
        println("Cannot add reaction markers to a country with Good governance")
        pause()
      case m: MuslimCountry if m.isIslamic =>
        println("Cannot add reaction markers to a country under Islamist Rule")
        pause()
      case m: MuslimCountry if m.civilWar =>
        println("Cannot add reaction markers to a country in Civil War")
        pause()
      case m: MuslimCountry =>
        adjustInt("Reaction markers", -m.reaction, 0 to 10) foreach { value =>
          logAdjustment(name, "Reaction markers", -m.reaction, value)
          game = game.updateCountry(m.copy(reaction = -value))
        }
    }
  }
  
  def adjustBesiegedRegime(name: String): Unit = {
    game.getCountry(name) match {
      case _: NonMuslimCountry => throw new IllegalArgumentException(s"Cannot add besieged regime to non-Muslim country: $name")
      case m: MuslimCountry if m.isGood =>
        println("Cannot add besieged regime to a country with Good governance")
        pause()
      case m: MuslimCountry if m.isIslamic =>
        println("Cannot add besieged regime to a country under Islamist Rule")
        pause()
      case m: MuslimCountry =>
        val newValue = !m.besiegedRegime
        logAdjustment(name, "Besieged regime", m.besiegedRegime, newValue)
        pause()
        game = game.updateCountry(m.copy(besiegedRegime = newValue))
    }
  }
  
  def adjustRegimeChange(name: String): Unit = {
    game.getCountry(name) match {
      case _: NonMuslimCountry => throw new IllegalArgumentException(s"Cannot add Regime Change to non-Muslim country: $name")
      case m: MuslimCountry if m.isGood =>
        println("Cannot add Regime Change to a country with Good governance")
        pause()
      case m: MuslimCountry if m.isIslamic =>
        println("Cannot add Regime Change to a country under Islamist Rule")
        pause()
      case m: MuslimCountry =>
        val choices = (NoRegimeChange::GreenRegimeChange::TanRegimeChange::Nil) filterNot (_ == m.regimeChange)
        val prompt = s"New regime change value (${orList(choices)}): "
        getOneOf(prompt, choices) foreach { newValue =>
          val nixCapital = newValue == NoRegimeChange && m.caliphateCapital
          if (nixCapital)
            println(s"$name will no longer be the Caliphate Capital.\n" +
                     "The Caliphate will be removed completely.")
          if (!nixCapital || askYorN(s"Do you wish continue (y/n)? ")) {
            logAdjustment(name, "Regime change", m.regimeChange, newValue)
            var updated = m.copy(regimeChange = newValue)
            if (nixCapital) {
              log(s"$name lost Caliphate Capital status.  Caliphate no longer delcared.")
              updated = updated.copy(caliphateCapital = false)
            }
            game = game.updateCountry(updated)
            pause()
          }
        }
    }
  }
  
  def adjustCivilWar(name: String): Unit = {
    game.getCountry(name) match {
      case _: NonMuslimCountry => throw new IllegalArgumentException(s"Cannot add Civil War to non-Muslim country: $name")
      case m: MuslimCountry if m.isGood =>
        println("Cannot add Civil War to a country with Good governance")
        pause()
      case m: MuslimCountry if m.isIslamic =>
        println("Cannot add Civil War to a country under Islamist Rule")
        pause()
      case m: MuslimCountry =>
        val newValue = !m.civilWar
        val nixCapital       = !newValue && m.caliphateCapital
        val nixRegimeChange  = newValue && m.inRegimeChange
        val convertAwakening = newValue && m.awakening != 0
        val convertReaction  = newValue && m.reaction != 0
        val anyWarnings      = nixCapital || nixRegimeChange || convertAwakening || convertReaction
        def warn(condition: Boolean, message: String): Unit = if (condition) println(message)
        warn(nixCapital, s"$name will no longer be the Caliphate Capital.\n" +
                         "The Caliphate will be removed completely.")
        warn(nixRegimeChange, "The regime change marker will be removed.")
        warn(convertAwakening, "The awakening markers will be replaced with militia.")
        warn(convertReaction, "The reaction markers will be replaced with sleeper cells.")
        if (!anyWarnings || askYorN(s"Do you wish continue (y/n)? ")) {
          logAdjustment(name, "Civil War", m.civilWar, newValue)
          var updated = m.copy(civilWar = newValue)
          if (nixCapital) {
            log(s"$name lost Caliphate Capital status.  Caliphate no longer delcared.")
            updated = updated.copy(caliphateCapital = false)
          }
          if (nixRegimeChange) {
            logAdjustment(name, "Regime change", updated.regimeChange, NoRegimeChange)
            updated = updated.copy(regimeChange = NoRegimeChange)
          }
          if (convertAwakening) {
            val numMilitia = updated.militia + (updated.awakening min game.militiaAvailable)
            logAdjustment(name, "Awakening markers", updated.awakening, 0)
            logAdjustment(name, "Militia", updated.militia, numMilitia)
            updated = updated.copy(awakening = 0, militia = numMilitia)
          }
          if (convertReaction) {
            val numSleepers = updated.sleeperCells + (-updated.reaction min (game.cellsOnTrack + game.cellsInCamp))
            logAdjustment(name, "Reaction markers", -updated.reaction, 0)
            logAdjustment(name, "Sleeper cells", updated.sleeperCells, numSleepers)
            updated = updated.copy(reaction = 0, sleeperCells = numSleepers)
          }
          game = game.updateCountry(updated)
          pause()
        }        
    }
  }
  
  def adjustCaliphateCapital(name: String): Unit = {
    game.getCountry(name) match {
      case _: NonMuslimCountry => throw new IllegalArgumentException(s"Non-Muslim cannot be the Caliphate capital: $name")
      case m: MuslimCountry if !m.caliphateCandidate =>
        println(s"$name is not a valid Caliphate country.  Must be one of: Islamist Rule, Regime Change, Civil War")
        pause()
      case m: MuslimCountry =>
        (m.caliphateCapital, game.caliphateCapital) match {
          case (true, _) =>
            logAdjustment(name, "Caliphate Capital", true, false)
            game = game.updateCountry(m.copy(caliphateCapital = false))
          case (false, None) => 
            logAdjustment(name, "Caliphate Capital", false, true)
            game = game.updateCountry(m.copy(caliphateCapital = true))
          case (false, Some(previousCapitalName)) =>
            val previousCapital = game.getMuslim(previousCapitalName)
            logAdjustment(previousCapitalName, "Caliphate Capital", true, false)
            logAdjustment(name, "Caliphate Capital", false, true)
            game = game.updateCountries(previousCapital.copy(caliphateCapital = false)::m.copy(caliphateCapital = true)::Nil)
        }
    }
  }
  
  // Move plots between country and available/resolved
  def adJustCountryPlots(name: String): Unit = {
  }
  
  def adjustCountryMarkers(name: String): Unit = ()
  
  
  def cmdSaveReserves(player: Role, ops: Int): Unit = {
    player match {
      case US =>
        if (game.usReserves == 2)
          println(s"$player already has the maximum of 2 OPs reserved")
        else {
          game = game.copy(usReserves = (game.usReserves + ops) min 2)
          log(s"$player reserves increased to ${game.usReserves}")
        }
      case Jihadist =>
        if (game.jihadistReserves == 2)
          println("$player already has the maximum of 2 OPs reserved")
        else {
          game = game.copy(jihadistReserves = (game.jihadistReserves + ops) min 2)
          log(s"$player reserves increased to ${game.jihadistReserves}")
        }
    }
  }
}

