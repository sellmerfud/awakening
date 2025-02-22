
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

import java.io.{ IOException, BufferedReader, InputStreamReader }
import scala.util.Random.{shuffle, nextInt}
import scala.annotation.tailrec
import scala.util.Properties.{lineSeparator, isWin}
import scala.collection.immutable.ListMap
import scala.collection.mutable.ListBuffer
import scala.io.StdIn.readLine
import scenarios._
import FUtil.Pathname

object LabyrinthAwakening {

  lazy val SOFTWARE_VERSION: String = {
    def classLoader: ClassLoader =
      Thread.currentThread.getContextClassLoader match {
        case null => this.getClass.getClassLoader match {
          case null => ClassLoader.getSystemClassLoader
          case cl => cl
        }
        case cl => cl
      }

    val version = Option(classLoader.getResourceAsStream("version")) match {
      case None           => "Missing"
      case Some(resource) =>
        try {
          val reader = new BufferedReader(new InputStreamReader(resource))
          val version = reader.readLine.trim
          reader.close
          version
        }
        catch {
          case e: IOException => "Error"
        }
    }

    // For the florian version we append the commit if the commit_xxxxxxxxxx file exists.
    Pathname.glob(Pathname.userDir / "commit_*").headOption match {
      case None => version
      case Some(commit) => s"$version - ${commit.basename}"
    }
  }


  implicit class ThrowableWrapper(exception: Throwable) {
    def stackTrace: String = {
      val w = new java.io.StringWriter
      exception.printStackTrace(new java.io.PrintWriter(w))
      w.toString
    }
  }

  def dieRoll = nextInt(6) + 1

  // Prompt for a die roll value if so configured or just generate a random roll otherwise.
  def getDieRoll(prompt: String = "Enter die roll: ", role: Option[Role] = None, allowAbort: Boolean = true): Int = {
    if (game.manualDieRolls || (game.humanAutoRoll == false && role == Some(game.humanRole)))
      askOneOf(prompt, 1 to 6, allowAbort = allowAbort).map(_.toInt).get
    else
      dieRoll
  }

  val INTEGER = """(\d+)""".r

  // Zero or Positive numbers
  def isInteger(str: String) = str match {
    case INTEGER(str) => true
    case _ => false
  }

  val MaxPrestige = 12
  val MaxFunding = 9

  sealed trait GameMode {
    val orderValue: Int
    val cardRange: Range
  }
  case object LabyrinthMode  extends GameMode {
    val orderValue = 1
    override def toString() = "Labyrinth"
    val cardRange: Range = Range.inclusive(1, 120)
  }

  case object AwakeningMode  extends GameMode {
    val orderValue = 2
    override def toString() = "Awakening"
    val cardRange: Range = Range.inclusive(121, 240)
  }

  case object ForeverWarMode extends GameMode {
    val orderValue = 3
    override def toString() = "Forever War"
    val cardRange: Range = Range.inclusive(241, 360)
  }

  object GameMode {
    val ALL = List(LabyrinthMode, AwakeningMode, ForeverWarMode)
    implicit val GameModeOrdering: Ordering[GameMode] =
      Ordering.by { m: GameMode => m.orderValue }

    def apply(name: String): GameMode = name.toLowerCase match {
      case "labyrinth"   => LabyrinthMode
      case "awakening"   => AwakeningMode
      case "forever war" => ForeverWarMode
      case _ => throw new IllegalArgumentException(s"Invalid game mode name: $name")
    }

    def next(current: GameMode) = ALL.dropWhile(_ != current).headOption
    
  }



  sealed trait CardAssociation {
    def assocString: String
  }
  case object Unassociated extends CardAssociation {
    override def toString() = "Unassociated"
    override def assocString = toString()
  }

  sealed trait Role extends CardAssociation {
    def opponent: Role = this match {
      case US => Jihadist
      case Jihadist => US
    }
  }

  case object US extends Role
  {
    override def toString() = "US"
    override def assocString = "US-Associated"
  }

  case object Jihadist extends Role {
    override def toString() = "Jihadist"
    override def assocString = "Jihadist-Associated"
  }

  object Role {
    def apply(name: String): Role = name.toLowerCase match {
      case "us"       => US
      case "jihadist" => Jihadist
      case _ => throw new IllegalArgumentException(s"Invalid role name: $name")
    }
  }

  case class BotDifficulty(val order: Int, name: String, description: String) {
    override def toString() = f"$name%-10s - $description"
    val key = name.toLowerCase
  }

  def isBot(role: Role) = (role == game.botRole)
  def isHuman(role: Role) = (role == game.humanRole)

  // US
  val OffGuard  = BotDifficulty(1, "Off Guard", "Standard rules")
  val Competent = BotDifficulty(2, "Competent", "Alert affects two plots in the same country")
  val Adept     = BotDifficulty(3, "Adept"    , "Disrupt at two or more troops awards +2 prestige")
  val Vigilant  = BotDifficulty(4, "Vigilant" , "No auto recruit in Regime Change or Civil War countries")
  val Ruthless  = BotDifficulty(5, "Ruthless" , "US associated events trigger on first plot")
  val NoMercy   = BotDifficulty(6, "NoMercy"  , "Ignore DRM penalty on War of Ideas")

  // Jihadist
  val Muddled    = BotDifficulty(1, "Muddled"   , "Standard rules")
  val Coherent   = BotDifficulty(2, "Coherent"  , "Ignore DRM penalty during Minor Jihad")
  val Attractive = BotDifficulty(3, "Attractive", "Each successful plot places two available plot markers")
  val Potent     = BotDifficulty(4, "Potent"    , "Each successful recruit places two available cells")
  val Infectious = BotDifficulty(5, "Infectious", "US must play all cards")
  val Virulent   = BotDifficulty(6, "Virulent"  , "Ignore all DRM penalties")

  object BotDifficulty {
    implicit val BotDifficultyOrdering: Ordering[BotDifficulty] = 
      Ordering.by { x: BotDifficulty => x.order }

    def apply(name: String): BotDifficulty = name.toLowerCase match {
      case OffGuard.key   => OffGuard
      case Competent.key  => Competent
      case Adept.key      => Adept
      case Vigilant.key   => Vigilant
      case Ruthless.key   => Ruthless
      case NoMercy.key    => NoMercy
      case Muddled.key    => Muddled
      case Coherent.key   => Coherent
      case Attractive.key => Attractive
      case Potent.key     => Potent
      case Infectious.key => Infectious
      case Virulent.key   => Virulent
      case _ => throw new IllegalArgumentException(s"Invalid BotDifficulty name: $name")
    }
  }

    sealed abstract class EnhBotDifficulty(val name: String) {
    override def toString() = name
  }

  case object EnhBotEasy extends EnhBotDifficulty("easy")
  case object EnhBotMedium extends EnhBotDifficulty("medium")
  case object EnhBotHard extends EnhBotDifficulty("hard")

  object EnhBotDifficulty {
    lazy val All = List(EnhBotEasy, EnhBotMedium, EnhBotHard)

    def fromStringOpt(name: String): Option[EnhBotDifficulty] = All.find(_.name == name)

    def fromString(name: String): EnhBotDifficulty =
      fromStringOpt(name).getOrElse {
        throw new IllegalArgumentException(s"Invalid Enhance Bot difficulty: $name")
      }
  }

  def enhBotEasy()   = game.botEnhancements && game.enhBotDifficulty == EnhBotEasy
  def enhBotMedium() = game.botEnhancements && game.enhBotDifficulty == EnhBotMedium
  def enhBotHard()   = game.botEnhancements && game.enhBotDifficulty == EnhBotHard

  trait Plot {
    val number: Int        // Dice rolled against governance in muslim countries
    val opsToPlace: Int    // Min number of ops needed to place this plot.
    val name: String
    override def toString() = name
  }

  // Order so that the most dangerous come first: WMD, 3, 2, 1
  implicit val PlotOrdering: Ordering[Plot] = new Ordering[Plot] {
    def compare(x: Plot, y: Plot) = (x, y) match {
      case (PlotWMD, PlotWMD) =>  0
      case (PlotWMD, _)       => -1
      case (_, PlotWMD)       =>  1
      case (x, y)             =>  y.number - x.number
    }
  }
  case object PlotWMD extends Plot {
    val number     = 3
    val opsToPlace = 1
    val name = "Plot WMD"
  }
  class NumberedPlot(val number: Int) extends Plot {
    val opsToPlace = number
    val name: String = s"Plot $number"
  }
  case object Plot1 extends NumberedPlot(1)
  case object Plot2 extends NumberedPlot(2)
  case object Plot3 extends NumberedPlot(3)

  object Plot {
    def apply(name: String): Plot = name match {
      case "Plot WMD" => PlotWMD
      case "Plot 1" => Plot1
      case "Plot 2" => Plot2
      case "Plot 3" => Plot3
      case x        => throw new IllegalArgumentException(s"Invalid plot name: $x")
    }
  }

  case class PlotOnMap(plot: Plot, backlashed: Boolean = false) {
    override def toString() = if (backlashed) s"${plot.name} (backlashed)" else plot.name
  }
  implicit val PlotOnMapOrdering: Ordering[PlotOnMap] =
    Ordering.by { x: PlotOnMap => x.plot }
  val NoRegimeChange    = "None"
  val GreenRegimeChange = "Green"
  val TanRegimeChange   = "Tan"

  // Troop commitment
  val LowIntensity = "Low Intensity"
  val War          = "War"
  val Overstretch  = "Overstretch"
  val USCardDraw = Map(LowIntensity -> 9, War -> 8, Overstretch -> 7)

  // Prestige Level
  val Low      = "Low"
  val Medium   = "Medium"
  val High     = "High"
  val VeryHigh = "Very High"

  def getPrestigeLevel(prestigeValue: Int) =
    prestigeValue match {
      case x if x <  4 => Low
      case x if x <  7 => Medium
      case x if x < 10 => High
      case _           => VeryHigh
    }



  // Funding level
  val Tight    = "Tight"
  val Moderate = "Moderate"
  val Ample    = "Ample"
  val JihadistCardDraw = Map(Tight -> 7, Moderate -> 8, Ample -> 9)

  // Country Types
  val NonMuslim = "Non-Muslim"
  val Sunni     = "Sunni"
  val ShiaMix   = "Shia-Mix"

  val PostureUntested = "Untested"
  val Soft            = "Soft"
  val Hard            = "Hard"
  val Even            = "Even"  // for display only

  def oppositePosture(p: String): String = {
    if (p != Hard && p != Soft)
      throw new IllegalArgumentException(s"oppositePosture($p)")
    if (p == Hard) Soft else Hard
  }

  // Returns the current GWOT
  // posture (Soft, Even, Hard)
  // value 0, 1, 2, 3
  def getGwot(hardSoftDelta: Int): (String, Int) = {
    val posture = hardSoftDelta match {
      case 0 => Even
      case d if d < 0 => Soft
      case _ => Hard
    }
    (posture, hardSoftDelta.abs min 3)
  }

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

  val DefaultCanada            = NonMuslimCountry(Canada)
  val DefaultUnitedStates      = NonMuslimCountry(UnitedStates)
  val DefaultUnitedKingdom     = NonMuslimCountry(UnitedKingdom, recruitOverride = 2)
  val DefaultSerbia            = NonMuslimCountry(Serbia)
  val DefaultIsrael            = NonMuslimCountry(Israel, postureValue = Hard)
  val DefaultIndia             = NonMuslimCountry(India)
  val DefaultScandinavia       = NonMuslimCountry(Scandinavia)
  val DefaultEasternEurope     = NonMuslimCountry(EasternEurope)
  val DefaultBenelux           = NonMuslimCountry(Benelux)
  val DefaultGermany           = NonMuslimCountry(Germany)
  val DefaultItaly             = NonMuslimCountry(Italy)
  val DefaultFrance            = NonMuslimCountry(France, recruitOverride = 2)
  val DefaultSpain             = NonMuslimCountry(Spain, recruitOverride = 2)
  val DefaultRussia            = NonMuslimCountry(Russia, governance = Fair)
  val DefaultCaucasus          = NonMuslimCountry(Caucasus, governance = Fair)
  val DefaultChina             = NonMuslimCountry(China, governance = Fair)
  val DefaultKenyaTanzania     = NonMuslimCountry(KenyaTanzania, governance = Fair)
  val DefaultThailand          = NonMuslimCountry(Thailand, governance = Fair)
  val DefaultPhilippines       = NonMuslimCountry(Philippines, governance = Fair, recruitOverride = 3)
  val DefaultIran              = NonMuslimCountry(Iran, governance = Fair, iranSpecialCase = true)
  val DefaultNigeria           = NonMuslimCountry(Nigeria, governance = Poor, recruitOverride = 3)

  val DefaultMuslimIran        = MuslimCountry(Iran, isSunni = false, printedResources = 2, oilExporter = true,
                                                governance = Fair, alignment = Adversary)
  val DefaultMuslimNigeria     = MuslimCountry(Nigeria, isSunni = true, printedResources = 2, oilExporter = true,
                                               governance = Poor, alignment = Neutral)

  val DefaultMorocco           = MuslimCountry(Morocco, printedResources = 2)
  val DefaultAlgeriaTunisia    = MuslimCountry(AlgeriaTunisia, printedResources = 2, oilExporter = true)
  val DefaultLibya             = MuslimCountry(Libya, printedResources = 1, oilExporter = true)
  val DefaultEgypt             = MuslimCountry(Egypt, printedResources = 3)
  val DefaultSudan             = MuslimCountry(Sudan, printedResources = 1, oilExporter = true)
  val DefaultSomalia           = MuslimCountry(Somalia, printedResources = 1)
  val DefaultJordan            = MuslimCountry(Jordan, printedResources = 1)
  val DefaultSyria             = MuslimCountry(Syria, printedResources = 2)
  val DefaultCentralAsia       = MuslimCountry(CentralAsia, printedResources = 2)
  val DefaultTurkey            = MuslimCountry(Turkey, isSunni = false, printedResources = 2)
  val DefaultLebanon           = MuslimCountry(Lebanon, isSunni = false, printedResources = 1)
  val DefaultYemen             = MuslimCountry(Yemen, isSunni = false, printedResources = 1)
  val DefaultIraq              = MuslimCountry(Iraq, isSunni = false, printedResources = 3, oilExporter = true)
  val DefaultSaudiArabia       = MuslimCountry(SaudiArabia, isSunni = false, printedResources = 3, oilExporter = true)
  val DefaultGulfStates        = MuslimCountry(GulfStates, isSunni = false, printedResources = 3, oilExporter = true)
  val DefaultPakistan          = MuslimCountry(Pakistan, isSunni = false, printedResources = 2, wmdCache = 3)
  val DefaultAfghanistan       = MuslimCountry(Afghanistan, isSunni = false, printedResources = 1)
  val DefaultIndonesiaMalaysia = MuslimCountry(IndonesiaMalaysia, printedResources = 3, oilExporter = true)
  val DefaultMali              = MuslimCountry(Mali, printedResources = 1)

  val LabyrinthDefaultCountries: List[Country] = List(
    DefaultCanada,
    DefaultUnitedStates,
    DefaultUnitedKingdom,
    DefaultSerbia,
    DefaultIsrael,
    DefaultIndia,
    DefaultScandinavia,
    DefaultEasternEurope,
    DefaultBenelux,
    DefaultGermany,
    DefaultItaly,
    DefaultFrance,
    DefaultSpain,
    DefaultRussia,
    DefaultCaucasus,
    DefaultChina,
    DefaultKenyaTanzania,
    DefaultThailand,
    DefaultPhilippines,
    DefaultIran,
    DefaultMorocco,
    DefaultAlgeriaTunisia,
    DefaultLibya,
    DefaultEgypt,
    DefaultSudan,
    DefaultSomalia,
    DefaultJordan,
    DefaultSyria,
    DefaultCentralAsia,
    DefaultTurkey,
    DefaultLebanon,
    DefaultYemen,
    DefaultIraq,
    DefaultSaudiArabia,
    DefaultGulfStates,
    DefaultPakistan,
    DefaultAfghanistan,
    DefaultIndonesiaMalaysia
  )

  val ExpansionDefaultCountries  = DefaultNigeria :: DefaultMali :: LabyrinthDefaultCountries

  val CountryAbbreviations = Map("US" -> UnitedStates, "UK" -> UnitedKingdom)

  val African       = Morocco::AlgeriaTunisia::Libya::Egypt::Sudan::Somalia::KenyaTanzania::
                      Mali::Nigeria::Nil
  val Schengen      = Scandinavia :: Benelux :: Germany :: EasternEurope ::
                      France :: Italy :: Spain :: Nil
  val SchengenLinks = Canada :: UnitedStates :: UnitedKingdom :: Morocco ::
                      AlgeriaTunisia :: Libya :: Serbia :: Turkey :: Lebanon ::
                      Russia :: Nil

  // A country key and a list of adjacent country keys.
  val adjacencyMap: Map[String, List[String]] = Map(
   Canada            -> (UnitedStates :: UnitedKingdom :: Schengen),
   UnitedStates      -> (Canada :: UnitedKingdom :: Philippines :: Schengen),
   UnitedKingdom     -> (Canada :: UnitedStates :: Schengen),
   Serbia            -> (Russia :: Turkey :: Schengen),
   Israel            -> (Egypt :: Jordan :: Lebanon :: Nil),
   India             -> (Pakistan :: IndonesiaMalaysia :: Nil),
   Scandinavia       -> (Schengen ::: SchengenLinks).filterNot(_ == Scandinavia),
   EasternEurope     -> (Schengen ::: SchengenLinks).filterNot(_ == EasternEurope),
   Benelux           -> (Schengen ::: SchengenLinks).filterNot(_ == Benelux),
   Germany           -> (Schengen ::: SchengenLinks).filterNot(_ == Germany),
   France            -> (Schengen ::: SchengenLinks).filterNot(_ == France),
   Italy             -> (Schengen ::: SchengenLinks).filterNot(_ == Italy),
   Spain             -> (Schengen ::: SchengenLinks).filterNot(_ == Spain),
   Russia            -> (Serbia :: Turkey :: Caucasus :: CentralAsia :: Schengen),
   Caucasus          -> (Turkey :: Russia :: CentralAsia :: Iran :: Nil),
   China             -> (CentralAsia:: Thailand :: Nil),
   KenyaTanzania     -> (Sudan :: Somalia :: Nigeria :: Nil),
   Thailand          -> (China :: IndonesiaMalaysia :: Philippines :: Nil),
   Philippines       -> (UnitedStates :: IndonesiaMalaysia :: Thailand :: Nil),
   Morocco           -> (AlgeriaTunisia :: Mali :: Schengen),
   AlgeriaTunisia    -> (Morocco :: Libya :: Mali :: Schengen),
   Libya             -> (AlgeriaTunisia :: Egypt :: Sudan :: Schengen),
   Egypt             -> (Israel :: Libya :: Sudan :: Nil),
   Sudan             -> (Libya :: Egypt :: Somalia :: KenyaTanzania :: Nigeria :: Nil),
   Somalia           -> (Yemen :: Sudan :: KenyaTanzania :: Nil),
   Jordan            -> (Israel :: Syria :: Iraq :: SaudiArabia :: Nil),
   Syria             -> (Iraq :: Lebanon :: Jordan :: Turkey :: Nil),
   CentralAsia       -> (Russia :: Caucasus :: Iran :: Afghanistan :: China :: Nil),
   IndonesiaMalaysia -> (Pakistan :: India :: Thailand :: Philippines :: Nil),
   Turkey            -> (Serbia :: Russia :: Caucasus :: Syria :: Iran :: Iraq :: Schengen),
   Lebanon           -> (Syria :: Israel :: Schengen),
   Yemen             -> (SaudiArabia :: Somalia :: Nil),
   Iraq              -> (GulfStates :: SaudiArabia :: Iran :: Turkey :: Syria :: Jordan :: Nil),
   SaudiArabia       -> (Jordan :: Iraq :: GulfStates :: Yemen :: Nil),
   GulfStates        -> (SaudiArabia :: Iraq :: Iran :: Pakistan :: Nil),
   Pakistan          -> (GulfStates :: Iran :: Afghanistan :: IndonesiaMalaysia :: India :: Nil),
   Afghanistan       -> (Pakistan :: Iran :: CentralAsia :: Nil),
   Iran              -> (Afghanistan :: Pakistan :: GulfStates :: Iraq :: Turkey :: Caucasus :: CentralAsia :: Nil),
   Mali              -> (Morocco :: AlgeriaTunisia :: Nigeria :: Nil),
   Nigeria           -> (Mali :: Sudan :: KenyaTanzania :: Nil)
  )

  val qatariCrisisAdjacencyMap: Map[String, List[String]] = Map(
    Iran         -> (Afghanistan :: Pakistan :: GulfStates :: SaudiArabia:: Yemen :: Iraq ::
                     Turkey :: Caucasus :: CentralAsia :: Nil),
    Yemen        -> (SaudiArabia :: Somalia :: GulfStates :: Iran :: Nil),
    SaudiArabia  -> (Jordan :: Iraq :: GulfStates :: Yemen :: Iran :: Nil),
    GulfStates   -> (SaudiArabia :: Iraq :: Iran :: Pakistan :: Yemen :: Nil)
  )

  def getAdjacent(name: String): List[String] = {
    val patriotAct = (game getCountry UnitedStates).hasMarker(PatriotAct)
    // We must filter against countries in the game, so we don't try
    // to access Mali, Nigeria during a Labyrinth scenario!
    val adjFilter = (adjName: String) => {
      game.hasCountry(_.name == adjName) &&
      (patriotAct == false ||
        ((name, adjName) match {
          case (UnitedStates, other) => other == Canada
          case (other, UnitedStates) => other == Canada
          case _                     => true
        })
      )
    }
    //  If QatariCrisis in play then SaudiArabia, GulfStates, Yemen, Iran are all
    //  considered adjacent
    val names = if (globalEventInPlay(QatariCrisis) && qatariCrisisAdjacencyMap.contains(name))
      qatariCrisisAdjacencyMap(name)
    else
      adjacencyMap(name)
    names.filter(adjFilter)
  }
  def areAdjacent(name1: String, name2: String) = getAdjacent(name1) contains name2

  // Shortest distance between countries
  def distance(source: String, target: String): Int = {
    def measure(current: String, visited: Set[String]): Option[Int] = {
      if (current == target)
        Some(0)
      else {
        getAdjacent(current).filterNot(visited) match {
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

  //  Return a list of countries with at least 1 cell that are closest to the
  //  target country
  def closestWithCells(target: String, allowSadr: Boolean = false): List[String] = {
    val isCandidate = (c: Country) =>
      c.name != target &&
      (if (allowSadr) c.totalCells > 0 else c.cells > 0)
    val candidates = countryNames(game.countries.filter(isCandidate))
    val ordered = (candidates.map(name => (name, distance(name, target))))
      .sortWith((a, b) => a._2 < b._2)
    if (ordered.isEmpty)
      Nil
    else
      ordered
        .takeWhile(_._2 == ordered.head._2)
        .map(_._1)
  }

  val PersionGulfExporterNames = SaudiArabia::GulfStates::Iraq::Iran::Nil

  //  When Travel Ban is in effect, no travel to the US from these
  //  countries.
  def isTravelBanCountry(name: String) = {
    if (globalEventInPlay(TravelBan)) {
      val alwaysBanned = Set(Iran, Iraq, Libya, Somalia, Sudan, Syria, Yemen)

      if (alwaysBanned contains name)
        true
      else if (game isNonMuslim name)
        false
      else {
        val m = game.getMuslim(name)
        m.civilWar || m.inRegimeChange || m.isIslamistRule
      }
    }
    else
      false
  }

  def persianGulfExporters = {
    val names = if (isIranSpecialCase)
      PersionGulfExporterNames.filterNot(_ == Iran)
    else
      PersionGulfExporterNames
    game.getMuslims(names)
  }

  def nonPersianGulfExporters =
    game.muslims.filter(m => m.oilExporter && !PersionGulfExporterNames.contains(m.name))

  def isPersionGulflExporter(name: String) = countryNames(persianGulfExporters) contains name
  def isNonPersionGulflExporter(name: String) = countryNames(nonPersianGulfExporters) contains name

  def plotsToStrings(plots: List[Plot], visible: Boolean = true): List[String] =
    (plots.size, visible) match {
      case (0, _)     => List("none")
      case (n, false) => List(amountOf(n, "hidden plot"))
      case (_, true)  => plots.sorted.map(_.name)
    }

  def plotsDisplay(plots: List[Plot], visible: Boolean = true): String = (plots.size, visible) match {
    case (0, _)     => "none"
    case (n, false) => amountOf(n, "hidden plot")
    case (_, true)  => plots.sorted.map(_.name).mkString(", ")
  }

  def mapPlotsDisplay(plots: List[PlotOnMap], visible: Boolean = true): String = {
    val lashed = plots count (_.backlashed)
    (plots.size, visible) match {
      case (0, _)                   => "none"
      case (n, false) if lashed > 0 => s"${amountOf(n, "hidden plot")}, $lashed backlashed"
      case (n, false)               => amountOf(n, "hidden plot")
      case (_, true)                => plots.sorted.map(_.toString).mkString(", ")
    }
  }

  // Used to describe event markers that represent troops.
  // prestigeLoss: if true, the marker's presence during a plot will cause loss of prestige
  case class TroopsMarker(name: String, num: Int, canDeploy: Boolean, prestigeLoss: Boolean)
  // When removing troop markers, the Bot will choose the first one
  // based on the following sort order.
  // - markers represent smaller numbers of troops come first
  // - markers that suffer prestige loss come first??
  implicit val TroopsMarkerOrdering: Ordering[TroopsMarker] = new Ordering[TroopsMarker] {
    def compare(x: TroopsMarker, y: TroopsMarker) =
      if (x.num == y.num)
        y.prestigeLoss compare x.prestigeLoss // reversed
      else
        x.num compare y.num
  }

  // Global Markers
  val Abbas                     = "Abbas"
  val AnbarAwakening            = "Anbar Awakening"
  val SaddamCaptured            = "Saddam Captured"
  val Wiretapping               = "Wiretapping"
  val EnhancedMeasures          = "Enhanced Measures"
  val Renditions                = "Renditions"
  val LeakWiretapping           = "Leak-Wiretapping"
  val LeakEnhancedMeasures      = "Leak-Enhanced Measures"
  val LeakRenditions            = "Leak-Renditions"
  val VieiraDeMelloSlain        = "Vieira de Mello Slain"
  val AlAnbar                   = "Al-Anbar"
  val MaerskAlabama             = "Maersk Alabama"
  val Fracking                  = "Fracking"
  val BloodyThursday            = "Bloody Thursday"
  val Censorship                = "Censorship"
  val Pirates1                  = "Pirates1"  // From the base game
  val Pirates2                  = "Pirates2"  // From the awakening expansion
  val Sequestration             = "Sequestration"
  val Smartphones               = "Smartphones"
  val ThreeCupsOfTea            = "3 Cups of Tea"
  val TradeEmbargoUS            = "Trade Embargo-US"
  val TradeEmbargoJihadist      = "Trade Embargo-Jihadist"
  val TrumpTweetsON             = "Trump Tweets ON"
  val TrumpTweetsOFF            = "Trump Tweets OFF"
  val Euroscepticism            = "Populism/Euroscepticism"
  val EarlyExit                 = "Early Exit"
  val QatariCrisis              = "Qatari Crisis"
  val SouthChinaSeaCrisis       = "South China Sea Crisis"
  val USNKSummit                = "US/NK Summit" // Blocks play of Korean Crisis from Awakening Cards
  val GulenMovement             = "Gulen Movement"
  val TravelBan                 = "Travel Ban"
  val AlBaghdadi                = "al-Baghdadi"
  val PoliticalIslamismUS       = "Political Islamism-US"
  val PoliticalIslamismJihadist = "Political Islamism-Jihadist"
  val USChinaTradeWar           = "US China Trade War"

  // Country Markers
  val Sadr                     = "Sadr"
  val CTR                      = "CTR"
  val MoroTalks                = "Moro Talks"
  val NEST                     = "NEST"
  val BenazirBhutto            = "Benazir Bhutto"
  val Indo_PakistaniTalks      = "Indo-Pakistani Talks"
  val IraqiWMD                 = "Iraqi WMD"
  val LibyanDeal               = "Libyan Deal"
  val LibyanWMD                = "Libyan WMD"
  val PatriotAct               = "Patriot Act"
  val AbuSayyaf                = "Abu Sayyaf"
  val BhuttoShot               = "Bhutto Shot"
  val FATA                     = "FATA"
  val Advisors                 = "Advisors"
  val UNSCR_1973               = "UNSCR 1973"
  val NATO                     = "NATO"
  val NATO2                    = "NATO-2" // NATO marker from Awakening.  In campaign game, both NATO markers can be in play
  val TrainingCamps            = "Training Camps"
  val OperationServal          = "Operation Serval"
  val TehranBeirutLandCorridor = "Tehran-Beirut Land Corridor"
  val BREXIT                   = "BREXIT"

  //  Map Markers to their Card Associations.
  //  This is needed by the "Bowling Green Massacre" event so
  //  we can determine which role the marker is associated with for
  //  the Bot decision

  val GlobalMarkers: Map[String, CardAssociation] = Map(
    Abbas -> US, AnbarAwakening -> US, SaddamCaptured -> US, Wiretapping -> US, EnhancedMeasures -> US,
    Renditions -> US, VieiraDeMelloSlain -> Jihadist, AlAnbar -> Jihadist, MaerskAlabama -> US,
    Fracking -> US, BloodyThursday -> Jihadist, Censorship -> Jihadist, Pirates1 -> Jihadist,
    Pirates2 -> Jihadist, Sequestration -> Jihadist,  Smartphones -> US, ThreeCupsOfTea -> Jihadist,
    LeakWiretapping -> Jihadist,
    LeakEnhancedMeasures -> Jihadist, LeakRenditions -> Jihadist, TrumpTweetsON -> US,
    TrumpTweetsOFF -> Unassociated, Euroscepticism -> US, EarlyExit -> Jihadist,
    QatariCrisis -> Jihadist, SouthChinaSeaCrisis -> Jihadist, USNKSummit -> US,
    GulenMovement -> Unassociated, TravelBan -> US, AlBaghdadi -> Jihadist, PoliticalIslamismUS -> US,
    PoliticalIslamismJihadist -> Jihadist, USChinaTradeWar -> Unassociated
  )

  val CountryMarkers: Map[String, CardAssociation] = Map(
    Sadr -> Jihadist, CTR -> US, MoroTalks -> US, NEST -> US, BenazirBhutto -> US,
    Indo_PakistaniTalks -> US, IraqiWMD -> US, LibyanDeal -> US, LibyanWMD -> US, PatriotAct -> US,
    AbuSayyaf -> Jihadist, BhuttoShot -> Jihadist, FATA -> Jihadist, Advisors -> US, UNSCR_1973 -> US,
    NATO -> US, NATO2 -> US, TrainingCamps -> Jihadist, OperationServal -> US,
    TehranBeirutLandCorridor -> Jihadist, BREXIT -> US,
    TradeEmbargoUS -> US, TradeEmbargoJihadist -> Jihadist
  )


  // Lapsing Event card numbers
  val Biometrics          = 2
  val TheDoorOfItjihad    = 47
  val GTMO                = 114
  val OilPriceSpike1      = 117
  val OilPriceSpike2      = 118
  val Ferguson            = 166
  val IslamicMaghreb      = 169
  val ArabWinter          = 173
  val KoreanCrisis        = 179
  val USConsulateAttacked = 199
  val EbolaScare          = 204
  val OilPriceSpike3      = 236
  val ExpandedROE         = 271
  val FullyResourcedCOIN  = 273
  val SiegeofMosul        = 278
  val StraitofHormuz      = 289
  val PublicDebate        = 306 // Not playable in solo game
  val USBorderCrisis      = 337
  val EUBolstersIranDeal  = 340
  val FakeNews            = 355
  val OPECProductionCut   = 356


  val LapsingCards = List(
    Biometrics, TheDoorOfItjihad, GTMO, OilPriceSpike1, OilPriceSpike2, Ferguson,
    IslamicMaghreb, ArabWinter, KoreanCrisis, USConsulateAttacked, EbolaScare, OilPriceSpike3,
    FullyResourcedCOIN, SiegeofMosul, StraitofHormuz, PublicDebate, USBorderCrisis,
    EUBolstersIranDeal, FakeNews, OPECProductionCut
  )

  val OilSpikeCards = Set(OilPriceSpike1, OilPriceSpike2, OilPriceSpike3)

  // From page 4 of Forever War manual.
  val PersonalityCards = Set(110, 111, 112, 115, 116, 215, 219, 225, 237, 328, 329, 338, 352)

  type CardEvent            = (Role, Boolean) => Unit
  type EventConditions      = (Role, Boolean) => Boolean
  type EventAlertsPlot      = (String, Plot) => Boolean   // Country Name, Plot
  type EventRemovesLastCell = () => Boolean
  val AlwaysPlayable: EventConditions =  (_, _) => true
  val NeverPlayable: EventConditions =  (_, _) => false
  val DoesNotAlertPlot: EventAlertsPlot = (_, _) => false
  val CannotNotRemoveLastCell: EventRemovesLastCell = () => false

  sealed trait CardRemoval
  case object NoRemove       extends CardRemoval
  case object Remove         extends CardRemoval
  case object USRemove       extends CardRemoval
  case object JihadistRemove extends CardRemoval

  sealed trait CardLapsing
  case object NoLapsing       extends CardLapsing
  case object Lapsing         extends CardLapsing
  case object USLapsing       extends CardLapsing
  case object JihadistLapsing extends CardLapsing

  val AutoTrigger   = true
  val NoAutoTrigger = false

  class Card(
    val number: Int,
    val cardName: String,
    val association: CardAssociation,
    val printedOps: Int,
    val remove: CardRemoval,
    val lapsing: CardLapsing,
    val autoTrigger: Boolean = false) {

    // Used by the US Bot to determine if the executing the event would alert a plot
    // in the given country
    def eventAlertsPlot(countryName: String, plot: Plot): Boolean = false

    // Used by the US Bot to determine if the executing the event would remove
    // the last cell on the map resulting in victory.
    def eventRemovesLastCell(): Boolean = false

    // When the Enhanced Jihadist Bot is in play and the
    // difficulty level is Easy or Medium US associated events
    // will trigger during the Jihadsit turn.  But we do not
    // want to trigger a US associated event if it would result
    // in an immediated US victory.
    // This function can be overridden by cards to police this.
    def eventWouldResultInVictoryFor(role: Role): Boolean = false

    // Returns true if the printed conditions of the event are satisfied
    def eventConditionsMet(role: Role): Boolean = false

    // Returns true if the Bot associated with the given role will execute the event
    // on its turn.  This implements the special Bot instructions for the event.
    // When the event is triggered as part of the Human players turn, this is NOT used.
    def botWillPlayEvent(role: Role): Boolean = true

    // Carry out the event for the given role.
    // forTrigger will be true if the event was triggered during the human player's turn
    // and it associated with the Bot player.
    def executeEvent(role: Role): Unit = ()

    def ops: Int = printedOps

    def numAndName = s"#$number $cardName"

    override def toString() = s"${numAndName} (${opsString(ops)})"

    def eventIsPlayable(role: Role): Boolean =
      (association == Unassociated || association == role) && eventConditionsMet(role)

    def eventWillTrigger(opponentRole: Role): Boolean = {
      association  == opponentRole &&
      (opponentRole == game.botRole || enhBotEasy() || enhBotMedium()) &&
      eventConditionsMet(opponentRole)
    }

    def markLapsingAfterExecutingEvent(role: Role) = (lapsing, role) match {
      case (Lapsing, _)                => true
      case (USLapsing, US)             => true
      case (JihadistLapsing, Jihadist) => true
      case _                           => false
    }

    def removeAfterExecutingEvent(role: Role) = (remove, role) match {
      case (Remove, _)                => true
      case (USRemove, US)             => true
      case (JihadistRemove, Jihadist) => true
      case _                          => false
    }
  }

    // Sort by card number
  implicit val CardOrdering: Ordering[Card] = new Ordering[Card] {
    def compare(x: Card, y: Card) = x.number compare y.number
  }

  sealed trait TurnAction {
    def name: String  // Used for quantifying type in save game files
  }
  sealed trait CardPlay extends TurnAction {
    val role: Role
    def numCards: Int
  }

  // Some events allow the play of an additional card.
  // In come cases this card counts as the second card of the
  // action phase.  In other cases this card is an "additional"
  // card played during the action phase and does not count as
  // one of the two cards that make up an action phase.
  sealed trait AddedCard
  case class SecondCard(num: Int) extends AddedCard
  case class AdditionalCard(num: Int) extends AddedCard

  // Used to keep track of cards played during the current turn
  // for display purposes only.  This is stored in the game state.
  // Some cards allow the play of a second card (#48 Adam Gadahn, #53 Madrassas)
  case class PlayedCard(role: Role, cardNum: Int, addedCard: Option[AddedCard]) extends CardPlay {
    override def name = "PlayedCard"

    override def numCards = addedCard match {
      case Some(SecondCard(_)) => 2
      case _ => 1
    }

    override def toString() = {
        addedCard match {
        case None => s"$role plays ${cardNumAndName(cardNum)}"
        case Some(SecondCard(num)) => s"$role plays ${cardNumAndName(cardNum)} and ${cardNumAndName(num)}"
        case Some(AdditionalCard(num)) => s"$role plays ${cardNumAndName(cardNum)} with addtional ${cardNumAndName(num)}"
      }
    }
  }

  case class PlayedReassement(card1: Int, card2: Int) extends CardPlay {
    val role = US
    override def name = "PlayedReassessment"
    override def numCards = 2
    override def toString() = s"$role plays ${cardNumAndName(card1)} and ${cardNumAndName(card2)} for reassessment"
  }

  case class USDiscardedLastCard(cardNumber: Int) extends TurnAction {
    val role = US
    override def name = "USDiscardedLastCard"
    override def toString() = s"$US discards last card ${cardNumAndName(cardNumber)}"
  }

  case class VoluntaryCadreRemoval(num: Int) extends TurnAction {
    override def name = "VoluntaryCadreRemoval"
    override def toString() = s"Jihadist removes ${amountOf(num, "cadre")}"
  }

  case class EndOfActionPhase(role: Role, phaseNum: Int, numPlots: Int) extends TurnAction {
    override def name = "EndOfActionPhase"
    override def toString() = {
      val suffix = role match {
        case US => s", ${amountOf(numPlots, "unblocked plot")} resolved"
        case _ => ""
      }
      s"End of ${ordinal(phaseNum)} $role action phase$suffix"
    }
  }

  case class AdjustmentMade(desc: String) extends TurnAction {
    override def name = "AdjustmentMade"
    override def toString() = s"Adjustment: $desc"
  }

  object deck {
    import awakening.cards._
    private def entry(card: Card) = (card.number -> card)

    val deckMap: Map[Int, Card] = Map(
      entry(Card_001), entry(Card_002), entry(Card_003), entry(Card_004), entry(Card_005),
      entry(Card_006), entry(Card_007), entry(Card_008), entry(Card_009), entry(Card_010),
      entry(Card_011), entry(Card_012), entry(Card_013), entry(Card_014), entry(Card_015),
      entry(Card_016), entry(Card_017), entry(Card_018), entry(Card_019), entry(Card_020),
      entry(Card_021), entry(Card_022), entry(Card_023), entry(Card_024), entry(Card_025),
      entry(Card_026), entry(Card_027), entry(Card_028), entry(Card_029), entry(Card_030),
      entry(Card_031), entry(Card_032), entry(Card_033), entry(Card_034), entry(Card_035),
      entry(Card_036), entry(Card_037), entry(Card_038), entry(Card_039), entry(Card_040),
      entry(Card_041), entry(Card_042), entry(Card_043), entry(Card_044), entry(Card_045),
      entry(Card_046), entry(Card_047), entry(Card_048), entry(Card_049), entry(Card_050),
      entry(Card_051), entry(Card_052), entry(Card_053), entry(Card_054), entry(Card_055),
      entry(Card_056), entry(Card_057), entry(Card_058), entry(Card_059), entry(Card_060),
      entry(Card_061), entry(Card_062), entry(Card_063), entry(Card_064), entry(Card_065),
      entry(Card_066), entry(Card_067), entry(Card_068), entry(Card_069), entry(Card_070),
      entry(Card_071), entry(Card_072), entry(Card_073), entry(Card_074), entry(Card_075),
      entry(Card_076), entry(Card_077), entry(Card_078), entry(Card_079), entry(Card_080),
      entry(Card_081), entry(Card_082), entry(Card_083), entry(Card_084), entry(Card_085),
      entry(Card_086), entry(Card_087), entry(Card_088), entry(Card_089), entry(Card_090),
      entry(Card_091), entry(Card_092), entry(Card_093), entry(Card_094), entry(Card_095),
      entry(Card_096), entry(Card_097), entry(Card_098), entry(Card_099), entry(Card_100),
      entry(Card_101), entry(Card_102), entry(Card_103), entry(Card_104), entry(Card_105),
      entry(Card_106), entry(Card_107), entry(Card_108), entry(Card_109), entry(Card_110),
      entry(Card_111), entry(Card_112), entry(Card_113), entry(Card_114), entry(Card_115),
      entry(Card_116), entry(Card_117), entry(Card_118), entry(Card_119), entry(Card_120),
      entry(Card_121), entry(Card_122), entry(Card_123), entry(Card_124), entry(Card_125),
      entry(Card_126), entry(Card_127), entry(Card_128), entry(Card_129), entry(Card_130),
      entry(Card_131), entry(Card_132), entry(Card_133), entry(Card_134), entry(Card_135),
      entry(Card_136), entry(Card_137), entry(Card_138), entry(Card_139), entry(Card_140),
      entry(Card_141), entry(Card_142), entry(Card_143), entry(Card_144), entry(Card_145),
      entry(Card_146), entry(Card_147), entry(Card_148), entry(Card_149), entry(Card_150),
      entry(Card_151), entry(Card_152), entry(Card_153), entry(Card_154), entry(Card_155),
      entry(Card_156), entry(Card_157), entry(Card_158), entry(Card_159), entry(Card_160),
      entry(Card_161), entry(Card_162), entry(Card_163), entry(Card_164), entry(Card_165),
      entry(Card_166), entry(Card_167), entry(Card_168), entry(Card_169), entry(Card_170),
      entry(Card_171), entry(Card_172), entry(Card_173), entry(Card_174), entry(Card_175),
      entry(Card_176), entry(Card_177), entry(Card_178), entry(Card_179), entry(Card_180),
      entry(Card_181), entry(Card_182), entry(Card_183), entry(Card_184), entry(Card_185),
      entry(Card_186), entry(Card_187), entry(Card_188), entry(Card_189), entry(Card_190),
      entry(Card_191), entry(Card_192), entry(Card_193), entry(Card_194), entry(Card_195),
      entry(Card_196), entry(Card_197), entry(Card_198), entry(Card_199), entry(Card_200),
      entry(Card_201), entry(Card_202), entry(Card_203), entry(Card_204), entry(Card_205),
      entry(Card_206), entry(Card_207), entry(Card_208), entry(Card_209), entry(Card_210),
      entry(Card_211), entry(Card_212), entry(Card_213), entry(Card_214), entry(Card_215),
      entry(Card_216), entry(Card_217), entry(Card_218), entry(Card_219), entry(Card_220),
      entry(Card_221), entry(Card_222), entry(Card_223), entry(Card_224), entry(Card_225),
      entry(Card_226), entry(Card_227), entry(Card_228), entry(Card_229), entry(Card_230),
      entry(Card_231), entry(Card_232), entry(Card_233), entry(Card_234), entry(Card_235),
      entry(Card_236), entry(Card_237), entry(Card_238), entry(Card_239), entry(Card_240),
      entry(Card_241), entry(Card_242), entry(Card_243), entry(Card_244), entry(Card_245),
      entry(Card_246), entry(Card_247), entry(Card_248), entry(Card_249), entry(Card_250),
      entry(Card_251), entry(Card_252), entry(Card_253), entry(Card_254), entry(Card_255),
      entry(Card_256), entry(Card_257), entry(Card_258), entry(Card_259), entry(Card_260),
      entry(Card_261), entry(Card_262), entry(Card_263), entry(Card_264), entry(Card_265),
      entry(Card_266), entry(Card_267), entry(Card_268), entry(Card_269), entry(Card_270),
      entry(Card_271), entry(Card_272), entry(Card_273), entry(Card_274), entry(Card_275),
      entry(Card_276), entry(Card_277), entry(Card_278), entry(Card_279), entry(Card_280),
      entry(Card_281), entry(Card_282), entry(Card_283), entry(Card_284), entry(Card_285),
      entry(Card_286), entry(Card_287), entry(Card_288), entry(Card_289), entry(Card_290),
      entry(Card_291), entry(Card_292), entry(Card_293), entry(Card_294), entry(Card_295),
      entry(Card_296), entry(Card_297), entry(Card_298), entry(Card_299), entry(Card_300),
      entry(Card_301), entry(Card_302), entry(Card_303), entry(Card_304), entry(Card_305),
      entry(Card_306), entry(Card_307), entry(Card_308), entry(Card_309), entry(Card_310),
      entry(Card_311), entry(Card_312), entry(Card_313), entry(Card_314), entry(Card_315),
      entry(Card_316), entry(Card_317), entry(Card_318), entry(Card_319), entry(Card_320),
      entry(Card_321), entry(Card_322), entry(Card_323), entry(Card_324), entry(Card_325),
      entry(Card_326), entry(Card_327), entry(Card_328), entry(Card_329), entry(Card_330),
      entry(Card_331), entry(Card_332), entry(Card_333), entry(Card_334), entry(Card_335),
      entry(Card_336), entry(Card_337), entry(Card_338), entry(Card_339), entry(Card_340),
      entry(Card_341), entry(Card_342), entry(Card_343), entry(Card_344), entry(Card_345),
      entry(Card_346), entry(Card_347), entry(Card_348), entry(Card_349), entry(Card_350),
      entry(Card_351), entry(Card_352), entry(Card_353), entry(Card_354), entry(Card_355),
      entry(Card_356), entry(Card_357), entry(Card_358), entry(Card_359), entry(Card_360),
    )
    def isValidCardNumber(num: Int): Boolean = deckMap.contains(num)
    def apply(num: Int): Card      = deckMap(num)  // Allows deck(4) to get a specific card
    def cards: List[Card]          = deckMap.valuesIterator.toList.sorted
    def lapsing: List[Card]        = cards.filter(_.lapsing != NoLapsing)
    def removable: List[Card]      = cards.filter(_.remove != NoRemove)

  }

  def cardNumAndName(number: Int): String = {
    deck(number).numAndName
  }
  def cardNumsAndNames(xs: List[Int]): String = xs.sorted.map(cardNumAndName).mkString(", ")

  // Add a Played Card to the list of plays
  def addPlayedCard(role: Role, cardNum: Int): Unit ={
    game = game.copy(turnActions = PlayedCard(role, cardNum, None) :: game.turnActions)
  }

  // Add a second card to the most recent Played Card
  // in the list of plays.  The will be done by events
  // and the card counts as the 2nd card of the action phase
  def addSecondCardToPlayedCard(cardNum: Int): Unit = {
    val newActions = game.turnActions match {
      case PlayedCard(role, firstCard, None) :: others =>
        PlayedCard(role, firstCard, Some(SecondCard(cardNum))) :: others
      case PlayedCard(role, firstCard, Some(_)) :: others =>
        throw new IllegalStateException("Cannot add second card. Current PlayedCard already has an added card.")
      case _ =>
        throw new IllegalStateException("Cannot add second card here.")
    }
    game = game.copy(turnActions = newActions)
  }

  // Add an additional card to the most recent Played Card
  // in the list of plays.  The will be done by events
  // and the card does not count one of the cards oof the
  // current action phase
  def addAdditionalCardToPlayedCard(cardNum: Int): Unit = {
    val newActions = game.turnActions match {
      case PlayedCard(role, firstCard, None) :: others =>
        PlayedCard(role, firstCard, Some(AdditionalCard(cardNum))) :: others
      case PlayedCard(role, firstCard, Some(_)) :: others =>
        throw new IllegalStateException("Cannot add additional card. Current PlayedCard already has an added card.")
      case _ =>
        throw new IllegalStateException("Cannot add additional card here.")
    }
    game = game.copy(turnActions = newActions)
  }


  sealed trait Country {
    val name: String
    val governance: Int
    val sleeperCells: Int
    val activeCells: Int
    val cadres: Int
    val troops: Int
    val plots: List[PlotOnMap]
    val markers: List[String]
    val wmdCache: Int        // Number of WMD plots cached

    def isMuslim: Boolean
    def isNonMuslim: Boolean = !isMuslim

    def isUntested: Boolean
    def isTested = !isUntested
    def isGood         = governance == Good
    def isFair         = governance == Fair
    def isPoor         = governance == Poor
    def isIslamistRule = governance == IslamistRule

    def hasMarker(name: String) = markers contains name
    def countMarker(name: String) = markers count (_ == name)

    def cells      = sleeperCells + activeCells
    def hasSadr    = hasMarker(Sadr)
    def totalCells = cells + (if (hasSadr) 1 else 0)
    def hasCadre   = cadres > 0
    def troopsMarkers: List[TroopsMarker] = markers collect {
      case NATO            => TroopsMarker(NATO,            2, canDeploy = true,  prestigeLoss = true)
      case NATO2           => TroopsMarker(NATO2,           2, canDeploy = true,  prestigeLoss = true)
      case UNSCR_1973      => TroopsMarker(UNSCR_1973,      1, canDeploy = false, prestigeLoss = false)
      case OperationServal => TroopsMarker(OperationServal, 1, canDeploy = false,  prestigeLoss = true)
    }

    def markerTroops: Int = troopsMarkers.foldLeft(0) { (total, tm) => total + tm.num }
    def deployableMarkerTroops = troopsMarkers
      .filter(_.canDeploy)
      .foldLeft(0) { (total, tm) => total + tm.num }
    def markerTroopsThatAffectPrestige: Int = troopsMarkers
      .foldLeft(0) { (total, tm) => total + (if (tm.prestigeLoss) tm.num else 0) }
    def totalTroops = troops + markerTroops
    def totalDeployableTroops = if (game.humanRole == US)
      troops + deployableMarkerTroops
    else
      troops  // Bot never deploys marker troops
    def totalTroopsThatAffectPrestige = troops + markerTroopsThatAffectPrestige
    def numAdvisors = countMarker(Advisors)

    def canDeployTo(ops: Int): Boolean
    def maxDeployFrom: Int
    def canDeployFrom(ops: Int) = maxDeployFrom > 0

    def hasPlots = plots.nonEmpty
    def warOfIdeasOK(ops: Int, ignoreRegimeChange: Boolean = false): Boolean
    def recruitOK(madrassas: Boolean): Boolean =
      hasCadre || totalCells > 0 || (madrassas && (isPoor || isIslamistRule))
    def autoRecruit: Boolean
    def recruitSucceeds(die: Int): Boolean
    def canTakeMilitia: Boolean
    def disruptAffectsPrestige: Boolean

    def hasAdjacent(test: Country => Boolean): Boolean =
      game.getCountries(getAdjacent(name)).exists(test)
  }

  case class NonMuslimCountry(
    name: String,
    governance: Int            = Good,
    sleeperCells: Int          = 0,
    activeCells: Int           = 0,
    cadres: Int                = 0,
    troops: Int                = 0,
    plots: List[PlotOnMap] = Nil,
    markers: List[String]      = Nil,
    postureValue: String       = PostureUntested,
    recruitOverride: Int       = 0,
    wmdCache: Int              = 0,  // Number of WMD plots cached
    iranSpecialCase: Boolean   = false
  ) extends Country {

    override def isMuslim: Boolean = false

    override def isUntested = posture == PostureUntested &&
                              !(Set(UnitedStates, Israel, Iran) contains name)
    def posture = if (name == UnitedStates) game.usPosture else postureValue
    def isSchengen = Schengen contains name
    def isHard = posture == Hard
    def isSoft = posture == Soft
    def isOppositeUsPosture = isTested && posture != game.usPosture
    def canRemovePosture = isTested && !(iranSpecialCase || name == UnitedStates || name == Israel)
    override def warOfIdeasOK(ops: Int, ignoreRegimeChange: Boolean = false) =
      ops >= governance &&
      !(iranSpecialCase || name == UnitedStates || name == Israel || (isSchengen && lapsingEventInPlay(EUBolstersIranDeal)))

    def recruitNumber = if (name == UnitedKingdom && hasMarker(BREXIT))
      1
    else if (recruitOverride != 0)
      recruitOverride
    else
      governance

    def autoRecruit = false
    def recruitSucceeds(die: Int) = die <= recruitNumber
    def addMarkers(names: String*): NonMuslimCountry = this.copy(markers = markers ++ names)
    def removeMarkers(names: String*): NonMuslimCountry = {
      var updatedMarkers = markers
      for (name <- names)
        updatedMarkers.indexOf(name) match {
          case -1 =>
          case x  => updatedMarkers = updatedMarkers.patch(x, Seq.empty, 1)
        }

      this.copy(markers = updatedMarkers)
    }

    // US posture is stored in the GameState
    def canChangePosture = !(iranSpecialCase || name == UnitedStates || name == Israel)
    def canTakeMilitia = false

    // Normally troops cannot deploy to a non-Muslim country.
    // The exception is the Abu Sayyaf event in the Philippines.
    def canDeployTo(ops: Int) = ops >= governance && name == Philippines && hasMarker(AbuSayyaf)
    def maxDeployFrom = totalDeployableTroops
    def disruptAffectsPrestige = totalTroopsThatAffectPrestige > 1

  }

  case class MuslimCountry(
    name: String,
    governance: Int            = GovernanceUntested,
    sleeperCells: Int          = 0,
    activeCells: Int           = 0,
    cadres: Int                = 0,
    plots: List[PlotOnMap]     = Nil,
    markers: List[String]      = Nil,
    isSunni: Boolean           = true,
    printedResources: Int       = 0,
    alignment: String          = Neutral,
    troops: Int                = 0,
    militia: Int               = 0,
    oilExporter: Boolean       = false,
    aidMarkers: Int            = 0,
    regimeChange: String       = NoRegimeChange,
    besiegedRegime: Boolean    = false,
    civilWar: Boolean          = false,
    caliphateCapital: Boolean  = false,
    awakening: Int             = 0,  // number of awakening markers
    reaction: Int              = 0,  // number of reaction markers
    wmdCache: Int              = 0   // Number of WMD plots cached
  ) extends Country {

    override def isMuslim: Boolean = true

    override def isUntested = governance == GovernanceUntested
    def isAlly      = !isUntested && alignment == Ally
    def isNeutral   = !isUntested && alignment == Neutral
    def isAdversary = !isUntested && alignment == Adversary

    def canExportOil = oilExporter && !hasMarker(TradeEmbargoJihadist)
    def resourceValue = {
      val corridorPlus = if (name == Iran && hasMarker(TehranBeirutLandCorridor)) 1 else 0
      val opecCutMinus = if (canExportOil && game.eventIsLapsing(OPECProductionCut)) 1 else 0
      val spikePlus    = if (canExportOil) (game.oilPriceSpikes) else 0
      val hormuzPlus   = if (game.eventIsLapsing(StraitofHormuz) && isNonPersionGulflExporter(name)) 1 else 0
      val hormuzMinus  = if (game.eventIsLapsing(StraitofHormuz) && isPersionGulflExporter(name)) 1 else 0
      printedResources + corridorPlus + spikePlus + hormuzPlus - hormuzMinus - opecCutMinus
    }

    def isShiaMix = !isSunni
    def inRegimeChange = regimeChange != NoRegimeChange

    def awakeningDelta = awakening - reaction
    def reactionDelta = reaction - awakening

    // If a muslim country is untest, then it is valid a WoI target.
    override def warOfIdeasOK(ops: Int, ignoreRegimeChange: Boolean = false) =
      !isIslamistRule  &&
      (isUntested      || ops >= governance)  &&
      !(isAdversary    || (isGood && isAlly)) &&
      (!inRegimeChange || ignoreRegimeChange || (totalTroopsAndMilitia - totalCells) >= 5)

    def autoRecruit = {
      val vigilant = game.usResolve(Vigilant)
      isIslamistRule || hasMarker(TrainingCamps) || (!vigilant && (civilWar || inRegimeChange))
    }
    def recruitSucceeds(die: Int) = autoRecruit || die <= governance


    def canTakeMilitia = !(isGood || isIslamistRule)
    def totalTroopsAndMilitia = totalTroops + militia // Used to calculate hit for attrition
    def disruptAffectsPrestige = totalTroopsAndMilitia > 1 && totalTroopsThatAffectPrestige > 0


    def canTakeAwakeningOrReactionMarker = !(isGood || isIslamistRule || civilWar)
    def canTakeBesiegedRegimeMarker = !besiegedRegime  // At most one besiegedRegime marker per country
    def canTakeAidMarker = true  // All Muslim countries can take an Aid marker by event
    def caliphateCandidate = civilWar || isIslamistRule || inRegimeChange

    def canDeployTo(ops: Int) = isAlly && !isIslamistRule && ops >= governance

    // To deploy troops out of a regime change country, we must leave behind
    // at least five more troops than cells.  Those troops that are left
    // behind may include marker troops (eg NATO), but marker troops are NOT included
    // in those that can leave.
    // `totalTroops` includes marker troops that cannot move
    def maxDeployFrom = if (inRegimeChange)
      totalDeployableTroops min ((totalTroops - (totalCells + 5)) max 0)
    else
      totalDeployableTroops

    def jihadDRM = -reactionDelta
    def jihadOK = !isIslamistRule && totalCells > 0 && !(name == Pakistan && hasMarker(BenazirBhutto))
    def majorJihadOK(ops: Int) =
      !(name == Pakistan && hasMarker(BenazirBhutto)) &&
      totalCells - totalTroopsAndMilitia >= 5 && (
        (isPoor && (ops  > 1 || besiegedRegime)) ||
        (isFair && (ops >= 3 || (ops == 2 && besiegedRegime)))
      )

    def addMarkers(names: String*): MuslimCountry = this.copy(markers = markers ++ names)
    def removeMarkers(names: String*): MuslimCountry = {
      var updatedMarkers = markers
      for (name <- names)
        updatedMarkers.indexOf(name) match {
          case -1 =>
          case x  => updatedMarkers = updatedMarkers.patch(x, Seq.empty, 1)
        }

      this.copy(markers = updatedMarkers)
    }
  }

  val LabyrinthScenario  = 1
  val AwakeningScenario  = 2
  val ForeverWarScenario = 3
  val CampaignScenario   = 4

  trait Scenario {
    val name: String
    val startingMode: GameMode
    val prestige: Int
    val usPosture: String
    val funding: Int
    val availablePlots: List[Plot]     // 1, 2, 3, 4 == WMD
    val removedPlots: List[Plot]
    val countries: List[Country]
    val markersInPlay: List[String]
    val cardsRemoved: List[Int]
    val offMapTroops: Int
    val notes: Seq[String] = Seq.empty
    val allowsCampaign: Boolean

    // Override this if the scenario requires any special setup such
    // as the Jihadist player choosing countries in which to place cells.
    val additionalSetup: () => Unit = () => ()
  }

  // There is a limit of 22 construction arguments for case classes
  // To work around this in the GameState, we will combine a couple of parameters
  case class ExtraCells(available: Int, onMap: Int)
  case class ExtraCellInfo(capacity:Int, available: Int, onMap: Int)
  case class Reserves(us: Int, jihadist: Int)
  case class CardsInHand(us: Int, jihadist: Int)

  // Keeps track of the which countries were the target of
  // -operations
  // -events
  // -improved or tested
  //
  // Some events depend on this.
  case class PhaseTargets(
    ops:                          Set[String] = Set.empty,
    disrupted:                    Set[String] = Set.empty,
    testedOrImprovedToFairOrGood: Set[String] = Set.empty,
    event:                        Set[String] = Set.empty
  ) {
    def wasOpsTarget(name: String)        = ops(name)
    def wasEventTarget(name: String)      = event(name)
    def wasOpsOrEventTarget(name: String) = wasOpsTarget(name) || wasEventTarget(name)
    def wasTestedOrImprovedToFairOrGood(name: String) = testedOrImprovedToFairOrGood(name)
  }

  case class PlotTarget(name: String, isMuslim: Boolean)

  // Keeps track of Plots that are available, resolved, remove from play
  // And also records which countries had plots resolved in the most recent
  // resolve plots phase.
  case class PlotData(
    availablePlots: List[Plot]       = Nil,
    resolvedPlots: List[Plot]        = Nil,
    removedPlots: List[Plot]         = Nil,
    resolvedTargets: Set[PlotTarget] = Set.empty,
    resolvedInGreenOnBlue: Boolean   = false // See Forever War card #301
  ) {
    def numAvailWMD = availablePlots.count(_ == PlotWMD)
    def numRemovedWMD = removedPlots.size
  }

  sealed trait Color {
    val name: String
    val sequence: String
  }

  object Color {
    lazy val all = List(Red, Blue, Cyan, Magenta, Yellow, Green)

    def fromName(name: String): Color = {
      all.find(c => c.name.toLowerCase == name.toLowerCase).getOrElse {
        throw new IllegalArgumentException(s"Invalid color name: $name")
      }
    }

    case object Red extends Color { val name: String = "Red"; val sequence: String = Console.RED; }
    case object Blue extends Color { val name: String = "Blue"; val sequence: String = Console.BLUE; }
    case object Cyan extends Color { val name: String = "Cyan"; val sequence: String = Console.CYAN; }
    // case object Magenta extends Color { val name: String = "Magenta"; val sequence: String = Console.MAGENTA; }
    case object Magenta extends Color { val name: String = "Magenta"; val sequence: String = "\u001B[95m"; }
    case object Yellow extends Color { val name: String = "Yellow"; val sequence: String = Console.YELLOW; }
    case object Green extends Color { val name: String = "Green"; val sequence: String = Console.GREEN; }

    val GameMarker: Option[Color] = Some(Blue)
    val MapMarker: Option[Color] = Some(Yellow)
    val MapPieces: Option[Color] = Some(Magenta)
    val FlipPieces: Option[Color] = Some(Cyan)
    val Event: Option[Color] = Some(Red)
    val Info: Option[Color] = Some(Yellow)
    val Other: Option[Color] = Some(Green)
    val Debug: Option[Color] = Some(Green)
  }

  case class LogEntry(text: String, color: Option[Color])

  // For the lapsing and first plot card, it is possible for'
  // the physical card to be removed by either an event such
  // as Oil Price Spike, or by the draw pile being empty when
  // and event has a side draw a card. (when this happens all
  // lapsing/first plot cards are added to the discard pile before
  // it is reshuffled)
  // When one of these cards is removed during a turn, the
  // Lapsing event is still in effect even though the card
  // has been discarded, and no card can be put in the
  // first plot box.
  // We use this class to keep track of this.

  case class LapsingEntry(cardNumber: Int, discarded: Boolean = false) {
    override def toString() = (cardNumber, discarded) match {
      case (0, true) => "(marker)" // Zero card number for 1st Plot box marker
      case (n, true) => s"${cardNumAndName(n)} (marker)"
      case _ => cardNumAndName(cardNumber)
    }
  }

  implicit val LapsingEventOrdering: Ordering[LapsingEntry] =
    Ordering.by { e: LapsingEntry => e.cardNumber }


  case class SummaryEntry(text: String, color: Option[Color])

  class Summary {
    private val _entries = new ListBuffer[SummaryEntry]

    def entries = _entries.toList

    def add(text: String, color: Option[Color] = None): Unit =
      _entries += SummaryEntry(text, color)

    def addSeq(list: Seq[String]): Unit =
      list.foreach(text => this.add(text, None))
  }


  // A game segment containing the save point number for the segment,
  // and a summary of the segment.
  case class GameSegment(saveNumber: Int, endOfTurn: Boolean, summary: Seq[String])

  case class GameState(
    scenarioName: String,
    scenarioNotes: Seq[String],
    startingMode: GameMode,  // Labyrinth, Awakening, Forever War
    gameLength: Int,   // Max number of "decks" in game
    deckNumber: Int,  // 1 to gameLength
    campaign: Boolean,
    currentMode: GameMode,  // May switch when playing a campaign game
    humanRole: Role,
    humanAutoRoll: Boolean,
    botDifficulties: List[BotDifficulty],
    turn: Int,
    prestige: Int,
    usPosture: String,
    funding: Int,
    countries: List[Country],
    markers: List[String],
    plotData: PlotData,
    sequestrationTroops: Boolean         = false,  // true if 3 troops off map due to Sequestration event
    offMapTroops: Int                    = 0,
    reserves: Reserves                   = Reserves(0, 0),
    cardsInUse: Range                    = Range.inclusive(0, 0),  // Range of cards in use (although some may have been removed)
    cardsInHand: CardsInHand             = CardsInHand(0, 0),
    turnActions: List[TurnAction]        = Nil,      // Cards plays/plot resolutions/adjustments etc. (most recent first).
    firstPlotEntry: Option[LapsingEntry] = None,     // Card number
    eventsLapsing: List[LapsingEntry]    = Nil,         // Card numbers currently lapsing
    cardsDiscarded: List[Int]            = Nil,
    cardsRemoved: List[Int]              = Nil,         // Card numbers removed from the game.
    targetsThisPhase: PhaseTargets       = PhaseTargets(),
    targetsLastPhase: PhaseTargets       = PhaseTargets(),
    ignoreVictory: Boolean               = false,
    botLogging: Boolean                  = false,
    botEnhancements: Boolean             = false, // Use enhancements to official Awakening bot algorithms
    enhBotDifficulty: EnhBotDifficulty   = EnhBotHard,
    manualDieRolls: Boolean              = false,  // Overrides humanAutoRoll
    history: Vector[GameSegment]         = Vector.empty,
    description: String                  = "",
    showColor: Boolean                   = !scala.util.Properties.isWin, // Default true except on Windows
    log: Vector[LogEntry]                = Vector.empty, // Log of the current game segment
    saveName: String                     = "") {         // The name is not saved to disk it is set when the game is loaded

    def useExpansionRules   = currentMode == AwakeningMode || currentMode == ForeverWarMode
    def scenarioNameDisplay = if (campaign) s"$scenarioName -- Campaign" else scenarioName

    def botRole = if (humanRole == US) Jihadist else US

    def usResolve(name: BotDifficulty) = botRole == US && (botDifficulties contains name)
    def jihadistIdeology(name: BotDifficulty) = botRole == Jihadist && (botDifficulties contains name)

    // This includes lapsing events whose card is still in the lapsing box
    def cardsLapsing() = eventsLapsing.filterNot(_.discarded).map(_.cardNumber)
    def cardDiscarded(num: Int) = cardsDiscarded.contains(num)
    def cardRemoved(num: Int) = cardsRemoved.contains(num)
    def eventIsLapsing(num: Int) = eventsLapsing.exists(_.cardNumber == num)

    def firstPlotCard(): Option[Int] = firstPlotEntry.filterNot(_.discarded).map(_.cardNumber)
    def isFirstPlot(num: Int) = firstPlotEntry.exists(_.cardNumber == num)
    def muslims: List[MuslimCountry] = countries.collect {
      case m: MuslimCountry => m
    }

    def nonMuslims: List[NonMuslimCountry] = countries.collect {
      case n: NonMuslimCountry => n
    }

    def availablePlots      = plotData.availablePlots
    def resolvedPlots       = plotData.resolvedPlots
    def removedPlots        = plotData.removedPlots
    def resolvedPlotTargets = plotData.resolvedTargets

    def totalAdvisorsOnMap = muslims.map(_.numAdvisors).sum
    def advisorsAvailable = (3 - totalAdvisorsOnMap ) max 0

    // The methods assume a valid name and will throw an exception if an invalid name is used!
    def getCountry(name: String) = {
      if (currentMode == LabyrinthMode && !game.campaign && (name == Nigeria || name == Mali))
        throw new IllegalArgumentException(s"getCountry() '$name' is not available in Labyrinth mode")
      countries.find(_.name == name).get
    }
    def getMuslim(name: String)    = muslims.find(_.name == name).get
    def getNonMuslim(name: String) = nonMuslims.find(_.name == name).get


    def getCountries(names: List[String]):  List[Country] = names.map(getCountry)
    def getMuslims(names: List[String]):    List[MuslimCountry] = names.map(getMuslim)
    def getNonMuslims(names: List[String]): List[NonMuslimCountry] = names.map(getNonMuslim)

    def hasCountry(test: (Country) => Boolean) = countries.exists(test)
    def hasMuslim(test: (MuslimCountry) => Boolean) = muslims.exists(test)
    def hasNonMuslim(test: (NonMuslimCountry) => Boolean) = nonMuslims.exists(test)

    def isMuslim(name: String) = hasMuslim(_.name == name)
    def isNonMuslim(name: String) = hasNonMuslim(_.name == name)

    def adjacentCountries(name: String) = getCountries(getAdjacent(name))
    def adjacentMuslims(name: String) = getMuslims(getAdjacent(name).filter(isMuslim))
    def adjacentNonMuslims(name: String)  = getNonMuslims(getAdjacent(name).filter(isNonMuslim))
    def adjacentCountriesWithCells(name: String) = getCountries(getAdjacent(name)).filter(_.cells > 0)

    def adjacentToGoodAlly(name: String) = game.adjacentMuslims(name).exists(m => m.isGood && m.isAlly)
    def adjacentToIslamistRule(name: String) = game.adjacentMuslims(name).exists(_.isIslamistRule)
    def adjacentToCivilWar(name: String) = game.adjacentMuslims(name).exists(_.civilWar)

    // Return true if the given country is adjacent to at least on Sunni Muslim Country
    def adjacentToSunni(name: String) = adjacentMuslims(name).exists(_.isSunni)
    // Return true if the given country is adjacent to at least on Shi Mix Muslim Country
    def adjacentToShiaMix(name: String) = adjacentMuslims(name).exists(_.isShiaMix)

    def caliphateCapital: Option[String] = muslims
      .find(_.caliphateCapital)
      .map(_.name)
    def caliphateDeclared = caliphateCapital.nonEmpty
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

    def updateCountry(changed: Country): GameState =
      this.copy(countries = changed :: countries.filterNot(_.name == changed.name))

    def updateCountries(changed: List[Country]): GameState = {
      val updates = changed.map(c => (c.name -> c)).toMap
      this.copy(countries = countries.map(c => updates.getOrElse(c.name, c)))
    }

    def addMarker(name: String): GameState = this.copy(markers = name :: markers)
    def removeMarker(name: String): GameState = this.copy(markers = markers.filterNot(_ == name))

    // List of countries that are valid targets for War of Ideas
    def woiTargets: List[Country] = countries.filter {
      case n: NonMuslimCountry => !(n.name == UnitedStates || n.name == Israel)
      case m: MuslimCountry    => m.isUntested || m.isNeutral || (m.isAlly && !m.isGood)
    }


    def prestigeModifier = prestige match {
      case x if x <  4 => -1
      case x if x <  7 =>  0
      case x if x < 10 =>  1
      case _           =>  2
    }

    // Hard Non-Muslims - Soft Non-Muslims
    def hardSoftDelta = game.nonMuslims.filterNot(_.name == UnitedStates).foldLeft(0) {
      case (v, c) if c.isHard => v + 1
      case (v, c) if c.isSoft => v - 1
      case (v, _) => v // Untested
    }

    // Returns the current GWOT
    // posture (Soft, Even, Hard)
    // value 0, 1, 2, 3
    def gwot: (String, Int) = getGwot(hardSoftDelta)

    def worldPosture = gwot._1

    // 0, 1, 2, 3
    def gwotPenalty: Int = {
      gwot match {
        case (posture, value)  if posture != usPosture => value
        case _ => 0
      }
    }

    def troopCommitment = troopsAvailable match {
      case x if x <  5 => Overstretch
      case x if x < 10 => War
      case _           => LowIntensity
    }

    def prestigeLevel = getPrestigeLevel(prestige)

    def fundingLevel = funding match {
      case x if x < 4 => Tight
      case x if x < 7 => Moderate
      case _          => Ample
    }

    def troopsCubesOnMap   = countries.foldLeft(0) { (a, c) => a + c.troops }
    def totalTroopsOnMap   = countries.foldLeft(0) { (a, c) => a + c.totalTroops }
    def militiaOnMap  = muslims.foldLeft(0) { (a, m) => a + m.militia }

    def troopsAvailable  = 15 - offMapTroops - troopsCubesOnMap
    def militiaAvailable = 15 - militiaOnMap

    // Extra cells are only usable when funding is at 9 or by event or civil war attrition.
    // If some of those cells are on the map and the training camp/al-Baghdadai is removed, those
    // extra cells remain on the map until eliminated.
    // Training camp cells are always the last to be placed on the map and the first
    // to be removed.

    def trainingCamp = muslims.find(_.hasMarker(TrainingCamps)).map(_.name)
    def isTrainingCamp(name: String) = trainingCamp == Some(name)

    def extraCellCapacity   = {
      if (markers contains AlBaghdadi) {
        if (caliphateDeclared) 5 else 3
      }
      else if (trainingCamp.nonEmpty) {
        if (isCaliphateMember(trainingCamp.get)) 5 else 3
      }
      else
        0
    }

    def totalCellCapacity   = 15 + extraCellCapacity
    def cellsOnMap          = countries.foldLeft(0) { (a, c) => a + c.cells }
    // totalCellsOnMap includes Sadr
    def totalCellsOnMap     = countries.foldLeft(0) { (a, c) => a + c.totalCells }

    // If "extra" cells were place on the map, then subsequently the
    // event granting the extra cells was cancelled, then there may be
    // excess "extra" cell still in play.  These remain until removed
    // and are alwasy the first to be removed.
    def excessExtraCellsOnMap = (cellsOnMap - totalCellCapacity) max 0

    // Cells available regardless of funding. For use by events.
    // Includes any extra cells for (Training Camps/al-Baghdadi)
    def cellsAvailable = (totalCellCapacity - cellsOnMap) max 0
    def extraCellsAvailable = extraCellCapacity min cellsAvailable
    def cellsOnTrack        = cellsAvailable - extraCellsAvailable

    // Number of cells available for recruit operations.
    def cellsToRecruit = {
      if (funding == 9)
        cellsAvailable
      else
        fundingLevel match {
          case Tight    => (cellsOnTrack - 10) max 0
          case Moderate => (cellsOnTrack -  5) max 0
          case _        => cellsOnTrack
        }
    }
    def sleeperCellsOnMap = countries.foldLeft(0) { (sum, c) => sum + c.sleeperCells }

    def numGoodOrFair    = muslims count (c => c.isGood || c.isFair)
    def numPoorOrIslamic = muslims count (c => c.isPoor || c.isIslamistRule)
    def numIslamistRule  = muslims count (c => c.isIslamistRule)
    def oilPriceSpikes   = eventsLapsing.count(e => OilSpikeCards(e.cardNumber))
    def goodResources =
      muslims.filter(_.isGood).map(_.resourceValue).sum
    def islamistResources =
      muslims.filter(_.isIslamistRule).map(_.resourceValue).sum + (if (caliphateDeclared) 1 else 0)
    // Return true if any two Islamist Rule countries are adjacent.
    def islamistAdjacency: Boolean =
      muslims.filter(_.isIslamistRule).combinations(2).exists (xs => areAdjacent(xs.head.name, xs.last.name))

    // Remember, troops can ALWAYS deploy to the track with a 1 op card.
    def deployPossible(ops: Int): Boolean = deployTargets(ops).nonEmpty

    // Returns Some(deployFrom, deployTo), or None
    // Remember, troops can ALWAYS deploy to the track with a 1 op card.
    // But if that is the only valid destination, then do not include "track" in the
    // list of sources.
    def deployTargets(ops: Int): Option[(List[String], List[String])] = {
      val fromCountries = countryNames(countries.filter(_.canDeployFrom(ops)))
      val toCountries   = countryNames(countries.filter(_.canDeployTo(ops)))
      (fromCountries, toCountries) match {
        case (Nil, Nil )                        => None
        case (Nil, to  ) if troopsAvailable > 0 => Some("track"::Nil, to)
        case (Nil, to  )                        => None
        case (from, Nil)                        => Some(from, "track"::Nil)
        case (from, to ) if troopsAvailable > 0 => Some("track"::from, "track"::to)
        case (from, to )                        => Some(from, "track"::to)
      }
    }

    def regimeChangeSourcesFor(target: String): List[String] = {
      val sources = countryNames(
        countries.filter(c => c.name != target && c.maxDeployFrom > 5)
      )
      if (troopsAvailable > 5)
        "track" :: sources
      else
        sources
    }

    def regimeChangeTargets: List[String] = {
      val haveSource     = (target: String) => regimeChangeSourcesFor(target).nonEmpty
      val iranStrict     = game.currentMode == ForeverWarMode || game.campaign
      val (world, value) = gwot

      def isAllowed(m: MuslimCountry) = if (m.name == Iran && iranStrict)
        (game.prestigeLevel == High || game.prestigeLevel == VeryHigh) && world == Hard && value == 3
      else
        true

      val targets = countryNames(
        muslims.filter { m =>
          (m.isIslamistRule && isAllowed(m))         ||
          (m.name == Iraq  && m.hasMarker(IraqiWMD)) ||
          (m.name == Libya && m.hasMarker(LibyanWMD))
        }
      )
      // Only a valid target if there is a source of troops available
      targets.filter(haveSource)
    }

    def regimeChangePossible(ops: Int) = {
      ops >= 3 && usPosture == Hard && regimeChangeTargets.nonEmpty
    }

    def withdrawFromTargets: List[String] = countryNames(
      muslims.filter(m => m.inRegimeChange && m.troops > 0)
    )

    def withdrawToTargets: List[String] =
      "track" :: countryNames(countries.filter(_.canDeployTo(3)))

    def withdrawPossible(ops: Int) =
        ops >= 3 && usPosture == Soft && withdrawFromTargets.nonEmpty

    // Returns the losses that would occur if this country is the
    // target of a disrupt operation.
    // Some(Either(cells, ())) or None
    def disruptLosses(name: String): Option[Either[Int, Unit]] = {
      val c = getCountry(name)
      // AlAnbar in Iraq/Syria affects max 1 cell and no cadre
      val alAnbar = globalEventInPlay(AlAnbar) && (c.name == Iraq || c.name == Syria)
      val numLosses = c match {
        case _ if alAnbar  => 1
        case m: MuslimCountry =>
          if ((m.totalTroopsAndMilitia) > 1 && (m.totalTroops > 0 || m.hasMarker(Advisors))) 2 else 1
        case n: NonMuslimCountry =>
          // Note only Philippines can have troops if AbuSayyaf marker is there
          if (n.isHard || n.totalTroops > 1) 2 else 1
      }
      if (c.cells > 0)
        Some(Left(numLosses min c.cells))
      else if (c.hasCadre && !alAnbar)
        Some(Right(()))
      else
        None
    }

    def disruptMuslimTargets(ops: Int): List[String] = countryNames(muslims.filter { m =>
      val hasTarget = if (globalEventInPlay(AlAnbar) && (m.name == Iraq || m.name == Syria))
        m.cells > 0
      else
        (m.hasCadre || m.cells > 0)
      val fataCheck = m.name != Pakistan || countryEventNotInPlay(Pakistan, FATA) || m.inRegimeChange
      val opsCheck  = ops >= (m.governance min Poor) // Treat IslamicRule same as Poor for ops value
      !m.isIslamistRule && hasTarget && fataCheck && opsCheck && (m.isAlly || (m.totalTroopsAndMilitia) > 1)
    })

    def disruptNonMuslimTargets(ops: Int): List[String] = countryNames(nonMuslims.filter { n =>
      !n.iranSpecialCase  &&
      ops >= n.governance &&
      (n.hasCadre || n.cells > 0)
    })

    def disruptTargets(ops: Int): List[String] =
      disruptMuslimTargets(ops) ::: disruptNonMuslimTargets(ops)

    def alertPossible(ops: Int) = ops >= 3 && alertTargets.nonEmpty

    def alertTargets: List[String] = countryNames(countries.filter(_.hasPlots))

    def warOfIdeasMuslimTargets(ops: Int): List[String] =
      countryNames(muslims.filter(_.warOfIdeasOK(ops)))

    def warOfIdeasNonMuslimTargets(ops: Int): List[String] =
      countryNames(nonMuslims.filter(_.warOfIdeasOK(ops)))

    def warOfIdeasTargets(ops: Int): List[String] =
      warOfIdeasMuslimTargets(ops) ::: warOfIdeasNonMuslimTargets(ops)


    def recruitTargets(madrassas: Boolean): List[String] =
      countryNames(countries.filter(_.recruitOK(madrassas)))

    def recruitPossible = lapsingEventNotInPlay(GTMO) &&
                          cellsToRecruit > 0 &&
                          recruitTargets(madrassas = false).nonEmpty

    def jihadTargets: List[String] = countryNames(muslims.filter(_.jihadOK))
    def jihadPossible = jihadTargets.nonEmpty

    def majorJihadTargets(ops: Int) = countryNames(muslims.filter(_.majorJihadOK(ops)))
    def majorJihadPossible(ops: Int) = majorJihadTargets(ops).nonEmpty

    def plotTargets: List[String] = {
      val muslimTargets = muslims.filter(m => !m.isIslamistRule && m.totalCells > 0)
      val nonMuslimTargets = nonMuslims.filter(_.totalCells > 0)
      countryNames(muslimTargets ::: nonMuslimTargets)
    }
    def plotsAvailableWith(ops: Int) = availablePlots.filter(_.opsToPlace <= ops)
    def plotPossible(ops: Int) = plotsAvailableWith(ops).nonEmpty && plotTargets.nonEmpty

    // --- Summaries --------------------------------------
    def actionSummary: Summary = {
      val summary = new Summary
      summary.add(s"Actions this turn", Color.Info)
      summary.add(separator(char = '='), Color.Info)
      if (turnActions.isEmpty)
        summary.add("none")
      else {
        summary.addSeq(turnActions.reverse.map(_.toString))
      }
      summary
    }

    def scenarioSummary: Summary = {
      val summary = new Summary
      summary.add(s"Scenario: ${scenarioNameDisplay}", Color.Info)
      summary.add(separator(char = '='), Color.Info)
      summary.add(s"Game name   : ${saveName}")
      summary.add(s"Game length : ${amountOf(game.gameLength, "deck")}")
      summary.add(separator())
      summary.add(s"The Bot is playing the $botRole")
      summary.add((if (botRole == US) "US Resolve" else "Jihadist Ideology"))

      for (difficulty <- botDifficulties)
        summary.add(s"  $difficulty")

      if (scenarioNotes.nonEmpty) {
        summary.add("")
        summary.add("Scenario Notes:")
        summary.add(separator())
        summary.addSeq(scenarioNotes)
      }

      summary.add("")
      summary.add("Options:")
      summary.add(separator())
      summary.add(s"Use Bot enhancements   : ${if (botEnhancements) "yes" else "no"}")
      summary.add(s"Enhanced Bot difficulty: $enhBotDifficulty")
      if (manualDieRolls)
        summary.add(s"Manual die rolls       : ${if (manualDieRolls) "yes" else "no"}")
      else
        summary.add(s"Human auto roll        : ${if (humanAutoRoll) "yes" else "no"}")
      summary.add(s"Bot logging            : ${if (botLogging) "yes" else "no"}")
      summary.add(s"Ignore instant vicory  : ${if (ignoreVictory) "yes" else "no"}")
      summary.add(s"Software version       : ${versionString}")
      summary
    }

    def scoringSummary: Summary = {
      val summary = new Summary
      val adjacency = humanRole match {
        case Jihadist => if (islamistAdjacency) "with adjacency" else "without adjacency"
        case US       => "adjacency not necessary"
      }
      summary.add("Current Score", Color.Info)
      summary.add(separator(char = '='), Color.Info)
      summary.add(f"Good/Fair Countries   : $numGoodOrFair%2d | Good Resources   : $goodResources%2d")
      summary.add(f"Poor/Islamic Countries: $numPoorOrIslamic%2d | Islamic Resources: $islamistResources%2d  ($adjacency)")
      summary
    }

    def worldPostureDisplay = {
      val (worldPosture, level) = gwot
      val levelDisp = if (worldPosture == Even) "" else level.toString
      s"$worldPosture $levelDisp"
    }

    def statusSummary: Summary = {
      val activePlotCountries = countries.filter(_.hasPlots)
      val summary = new Summary
      summary.add(s"Status", Color.Info)
      summary.add(separator(char = '='), Color.Info)
      summary.add(s"Game mode       : $currentMode")
      summary.add(s"Deck            : ${ordinal(deckNumber)} of $gameLength  (${amountOf(numCardsInDrawPile(), "card")} remaining)")
      summary.add(separator())
      summary.add(f"US posture      : $usPosture | World posture     : ${worldPostureDisplay}  (GWOT penalty $gwotPenalty)")
      summary.add(f"US prestige     : $prestige%2d   | Jihadist funding  : $funding%2d")
      summary.add(f"US reserves     : ${reserves.us}%2d   | Jihadist reserves : ${reserves.jihadist}%2d")
      summary.add(f"US cards        : ${cardsInHand.us}%2d   | Jihadist cards    : ${cardsInHand.jihadist}%2d")
      summary.add(separator())
      if (useExpansionRules) {
        summary.add(f"Troops on track : $troopsAvailable%2d   | Troop commitment: $troopCommitment")
        summary.add(f"Militia on track: $militiaAvailable%2d   | Troops off map    : $offMapTroops%2d")
      }
      else
        summary.add(f"Troops on track : $troopsAvailable%2d   | Troop commitment  : $troopCommitment")
      summary.add(separator())
      summary.add(f"Cells on track  : $cellsOnTrack%2d   |")
      summary.add(f"Cells to recruit: ${cellsToRecruit}%2d   | Funding level     : ${fundingLevel}")
      if (useExpansionRules) {
        val albaghdadi = (globalEventInPlay(AlBaghdadi), caliphateDeclared) match {
          case (true, true) => Some(s"$AlBaghdadi with Caliphate")
          case (true, false) => Some(s"$AlBaghdadi without Caliphate")
          case (false, _) => None
        }
        val camp = trainingCamp.map(name => s"$TrainingCamps in $name")
        val events = albaghdadi.toList ::: camp.toList
        val eventDisplay = events match {
          case Nil => " (none)     |"
          case xs  => xs.mkString(" (", ", ", ")")
        }
        summary.add(separator())
        summary.add(s"Extra cells$eventDisplay")
        val extraOnMap = (cellsOnMap - 15) max 0
        summary.add(f"Available       : ${extraCellsAvailable}%2d   | Capacity          : ${extraCellCapacity}%2d")
        summary.add(f"On map          : ${extraOnMap}%2d   |")
      }
      summary.add(separator())
      summary.addSeq(wrap("Available plots : ", plotsToStrings(availablePlots, humanRole == Jihadist)))
      if (useExpansionRules)
        summary.addSeq(wrap( "Resolved plots  : ", plotsToStrings(resolvedPlots)))
      summary.addSeq(wrap( "Removed plots   : ", plotsToStrings(removedPlots)))
      if (activePlotCountries.isEmpty)
        summary.add(s"Active plots    : none")
      else {
        summary.add(s"Active plots")
        val fmt = "  %%-%ds : %%s".format(longestString(activePlotCountries.map(_.name)))
        for (c <- activePlotCountries) {
          val visible = humanRole == Jihadist || (c.name == UnitedStates && c.hasMarker(NEST))
          summary.add(fmt.format(c.name, mapPlotsDisplay(c.plots, visible)))
        }
      }
      val countryMarkers = for (c <- countries; m <- c.markers)
        yield (s"$m (${c.name})")
      summary.addSeq(wrap("Event markers   : ", markers ::: countryMarkers))
      summary.addSeq(wrap("Lapsing         : ", eventsLapsing.sorted))
      summary.add(s"1st plot        : ${firstPlotEntry.map(_.toString).getOrElse("none")}")
      summary
    }

    // If show all is false, then some attributes will not be displayed
    // if they have value of zero.
    def countrySummary(name: String): Summary = {
      val summary = new Summary
      val items = new ListBuffer[String]
      def item(str: String): Unit =
        items += str
      def numItem(num: Int, label: String, pluralLabel: Option[String] = None): Unit = {
        if (num > 0)
          items += amountOf(num, label, pluralLabel)
      }
      def addItems(): Unit = if (items.nonEmpty)
        summary.add(s"${items.mkString(", ")}")

      getCountry(name) match {
        case n: NonMuslimCountry =>
          val specialCase = if (n.iranSpecialCase) ", Special Case" else ""
          summary.add("")
          summary.add(s"$name  (Non-Muslim$specialCase)")
          summary.add(separator(length = 54))
          summary.add(s"${govToString(n.governance)}, ${n.posture}, Recruit ${n.recruitNumber}")
          numItem(n.activeCells, "Active cell")
          numItem(n.sleeperCells, "Sleeper cell")
          numItem(n.cadres, "Cadre")
          numItem(n.troops, "Troop")
          addItems()
          if (n.hasPlots) {
            val visible = humanRole == Jihadist || (name == UnitedStates && n.hasMarker(NEST))
            summary.add(s"Plots: ${mapPlotsDisplay(n.plots, visible)}")
          }
          if (n.markers.size > 0)
            summary.add(s"Markers: ${markersString(n.markers)}")
          if (n.wmdCache > 0)
            summary.add(s"WMD cache: ${amountOf(n.wmdCache, "WMD plot")}")


        case m: MuslimCountry =>
          val gov = if (m.isUntested) "Untested" else s"${govToString(m.governance)} ${m.alignment}"
          val res = amountOf(m.resourceValue, "resource")
          val resDisp = if (m.resourceValue == m.printedResources)
            res
          else
            s"$res (${m.printedResources})"
          val oil = if (m.oilExporter && !m.hasMarker(TradeEmbargoJihadist)) List("Oil exporter") else Nil
          val autoRecruit = if (m.autoRecruit) List("Auto-Recruit") else Nil
          val desc = (gov :: resDisp :: oil ::: autoRecruit).mkString(", ")
          val muslimType = if (m.isShiaMix) "Shia-Mix" else "Sunni"
          summary.add("")
          summary.add(s"$name  (Muslim, $muslimType)")
          summary.add(separator(length = 54))
          summary.add(desc)
          numItem(m.activeCells, "Active cell")
          numItem(m.sleeperCells, "Sleeper cell")
          numItem(m.cadres, "Cadre")
          numItem(m.troops, "Troop")
          numItem(m.militia, "Militia", Some("Militia"))
          addItems()

          items.clear()
          numItem(m.aidMarkers, "Aid marker")
          numItem(m.awakening, "Awakening marker")
          numItem(m.reaction, "Reaction marker")
          if (m.besiegedRegime)
            item("Besieged regime")
          addItems()

          items.clear()
          if (m.inRegimeChange)
            item(s"Regime Change (${m.regimeChange})")
          if (m.civilWar)
            item("Civil War")
          if (m.caliphateCapital)
            item("Caliphate Capital")
          else if (isCaliphateMember(m.name))
            item("Caliphate member")
          addItems()
          if (m.hasPlots)
            summary.add(s"Plots: ${mapPlotsDisplay(m.plots, humanRole == Jihadist)}")
          if (m.markers.size > 0)
            summary.add(s"Markers: ${markersString(m.markers)}")
          if (m.wmdCache > 0)
            summary.add(s"WMD cache: ${amountOf(m.wmdCache, "WMD plot")}")
      }
      summary
    }

    def targetSummary: Summary = {
      val summary = new Summary
      val thisPhase = List(
        ("Ops", targetsThisPhase.ops),
        ("Event", targetsThisPhase.event),
        ("Disrupted", targetsThisPhase.disrupted),
        ("To fair/good", targetsThisPhase.testedOrImprovedToFairOrGood),
      )
      val lastPhase = List(
        ("Ops", targetsLastPhase.ops),
        ("Event", targetsLastPhase.event),
        ("Disrupted", targetsLastPhase.disrupted),
        ("To fair/good", targetsLastPhase.testedOrImprovedToFairOrGood),
      )
      def addEntries(entries: List[(String, Set[String])]): Unit = {
        val width = longestString(entries.map(_._1))
        for ((name, targets) <- entries)
          summary.addSeq(wrap(s"${padLeft(name, width)}: ", targets.toSeq.sorted))
      }

      summary.add(s"\nTargets this action phase", Color.Info)
      summary.add(separator(char = '='), Color.Info)
      addEntries(thisPhase)
      summary.add(s"\nTargets last action phase", Color.Info)
      summary.add(separator(char = '='), Color.Info)
      addEntries(lastPhase)

      summary
    }


    def deckSummary: Summary = {
      val summary = new Summary
      summary.add("Cards in the deck (or in player/bot hand)", Color.Info)
      summary.add(separator(char = '='), Color.Info)
      summary.addSeq(wrapInColumns("", cardsInDrawPileOrHands().map(deck(_).numAndName), maxWidth = 120))
      summary
    }

    def discardedCardsSummary: Summary = {
      val summary = new Summary
      summary.add("Cards in the discard pile", Color.Info)
      summary.add(separator(char = '='), Color.Info)
      summary.addSeq(wrapInColumns("", cardsDiscarded.map(deck(_).numAndName), maxWidth = 120))
      summary
    }

    def removedCardsSummary: Summary = {
      val summary = new Summary
      summary.add("Cards removed from the game", Color.Info)
      summary.add(separator(char = '='), Color.Info)
      summary.addSeq(wrapInColumns("", cardsRemoved.map(deck(_).numAndName), maxWidth = 120))
      summary
    }

    def caliphateSummary: Summary = {
      val summary = new Summary
      summary.add("Caliphate", Color.Info)
      summary.add(separator(char = '='), Color.Info)
      caliphateCapital match {
        case Some(capital) =>
          // Add the capital first
          summary.add(s"${capital}  (Capital)")
          for (member <- caliphateDaisyChain(capital).sorted.filterNot(_ == capital))
            summary.add(member)
        case None =>
          summary.add("There is no Caliphate declared")
      }
      summary
    }

    def civilWarSummary: Summary = {
      val summary = new Summary
      summary.add("Civil Wars", Color.Info)
      summary.add(separator(char = '='), Color.Info)
      val civilWars = muslims.filter(_.civilWar)
      if (civilWars.isEmpty)
        summary.add("There are no countries in civil war")
      else
        summary.add(civilWars.map(_.name).mkString(", "))
      summary
    }
  }

  def initialGameState(
    saveName: String,
    scenario: Scenario,
    gameLength: Int,
    campaign: Boolean,
    humanRole: Role,
    humanAutoRoll: Boolean,
    botDifficulties: List[BotDifficulty],
    showColor: Boolean,
    enhancedBot: Boolean,
    enhBotDifficulty: EnhBotDifficulty) =
  {

    var countries = if (scenario.startingMode == LabyrinthMode && !campaign)
      LabyrinthDefaultCountries
    else
      ExpansionDefaultCountries

    // Apply scenario overrides to countries.
    for (c <- scenario.countries)
      countries = c :: countries.filterNot(_.name == c.name)

    GameState(
      scenario.name,
      scenario.notes,
      scenario.startingMode,
      gameLength,
      1,      // deck number alwasy starts at 1
      campaign,
      scenario.startingMode,  // current game mode
      humanRole,
      humanAutoRoll,
      botDifficulties,
      0,      // Turn number, zero indicates start of game.
      scenario.prestige,
      scenario.usPosture,
      scenario.funding,
      countries,
      scenario.markersInPlay.sorted,
      PlotData(availablePlots = scenario.availablePlots.sorted, removedPlots = scenario.removedPlots),
      false,  // sequestrationTroops: true if 3 troops off map due to Sequestration event
      cardsInUse = scenario.startingMode.cardRange,
      cardsRemoved = scenario.cardsRemoved,
      showColor = showColor,
      botEnhancements = enhancedBot,
      enhBotDifficulty = enhBotDifficulty,
      offMapTroops = scenario.offMapTroops,
      saveName = saveName)
  }


  // Global variables
  var game = initialGameState(
    "no-name",
    Awakening,
    1,
    false,
    US,
    true,
    Muddled :: Nil,
    !scala.util.Properties.isWin,
    false,
    EnhBotHard
  )

  // Some events ask the user a question to determine if the event is
  // playable.  Sometimes we must test the event multiple times, such
  // as if the event is triggered after the user has taken an operation.
  // To avoid bothering the user twice with the same question, we simply
  // cache the answer the first time it is asked.
  // The cached answer is cleared each time a new card is played.
  var cachedEventPlayableAnswer: Option[Boolean] = None

  // Note that the question parameter is call by name so that
  // the user will not be prompted unless the cache is empty.
  def cacheQuestion(question: => Boolean): Boolean = {
    if (cachedEventPlayableAnswer.isEmpty)
      cachedEventPlayableAnswer = Some(question)
    cachedEventPlayableAnswer.get
  }

  def cacheYesOrNo(prompt: String) = cacheQuestion(askYorN(prompt))

  def pluralize(num: Int, name: String, plural: Option[String] = None) =
    (num.abs, plural) match {
      case (1, _)            => name
      case (_, Some(plural)) => plural
      case _                 => s"${name}s"
  }
  // If num is 1 use the name as is
  // otherwise either use the plural if given or add an 's' to the name.
  def amountOf(num: Int, name: String, plural: Option[String] = None) =
    s"$num ${pluralize(num, name, plural)}"

  def opsString(num: Int) = amountOf(num, "Op")

  def diceString(num: Int) = if (num == 1) "1 die" else s"$num dice"

  // Returns comma separated string with last choice separated by "and"
  //    List("apples")                      => "apples"
  //    List("apples", "oranges")           => "apples and oranges"
  //    List("apples", "oranges", "grapes") => "apples, oranges and grapes"
  def andList(x: Seq[Any]) = x match {
    case Seq()     => ""
    case Seq(a)    => a.toString
    case Seq(a, b) => s"${a.toString} and ${b.toString}"
    case _         => x.dropRight(1).mkString(", ") + ", and " + x.last.toString
  }

  // Returns comma separated string with last choice separated by "or"
  //    List("apples")                      => "apples"
  //    List("apples", "oranges")           => "apples or oranges"
  //    List("apples", "oranges", "grapes") => "apples, oranges or grapes"
  def orList(x: Seq[Any]) = x match {
    case Seq()     => ""
    case Seq(a)    => a.toString
    case Seq(a, b) => s"${a.toString} or ${b.toString}"
    case _         => x.dropRight(1).mkString(", ") + ", or " + x.last.toString
  }

  case class MatchOneError(msg: String, ambiguous: Seq[String])
  // Find a match for the given string in the list of options.
  // Any unique prefix of the given options will succeed.
  def matchOneCL(input: String, options: Seq[String], abbreviations: Map[String, String] = Map.empty): Either[MatchOneError, String] = {
    val trimmed = input.trim
    // Filter out any abbreviations that do not have a match with one of the options.
    val abbr = abbreviations.filter { case (_, name) => options.contains(name) }
    if (trimmed.isEmpty)
      Left(MatchOneError(s"Input is empty.", Seq.empty))
    else {
      val lowerInput = trimmed.toLowerCase
      val allOptions = (options ++ abbr.keys).distinct
      val paired = allOptions.map(name => name -> name.toLowerCase)
      val normalized = allOptions.map(_.toLowerCase)
      val normalizedAbbreviations = for ((a, v) <- abbr) yield (a.toLowerCase, v)
      val matches = paired.filter{ case (_, lower) => lower.startsWith(lowerInput) }
      // val matches = normalized.distinct.filter(_ startsWith trimmed.toLowerCase)
      matches match {
        case Seq() =>
          Left(MatchOneError(s"\"$trimmed\" is not valid.", Seq.empty))

        case Seq((value, _))  =>
          abbr.get(value) match {
            case Some(abbrValue) => Right(abbrValue)
            case None            => Right(value)
          }

        case many if many.exists(v => v._2 == lowerInput) =>
          // We got more than one match, but one of the matches is
          // an exact match (which happens to be a prefix of another entry)
          normalizedAbbreviations.get(lowerInput) match {
            case Some(abbrValue) => Right(abbrValue)
            case None            => Right(options(normalized.indexOf(lowerInput)))
          }

        case ambiguous =>
          Left(MatchOneError(s"\"$trimmed\" is ambiguous.", ambiguous.map(_._1)))
      }
    }
  }

  // Find a match for the given string in the list of options.
  // Any unique prefix of the given options will succeed.
  def matchOne(
    input: String,
    options: Seq[String],
    abbreviations: Map[String, String] = Map.empty,
    allowAbort: Boolean = false): Option[String] = {
    val trimmed = input.trim
    // Filter out any abbreviations that do not have a match with one of the options.
    val abbr = abbreviations.filter { case (_, name) => options contains name }
    // When showing the list of options to the user, we want to group
    // all abbreviations with the word that they represent.
    val displayList = {
      var associations = Map.empty[String, Set[String]].withDefaultValue(Set.empty)
      for ((a, o) <- abbr)
        associations += o -> (associations(o) + a)
      options.map {
        case o if associations(o).nonEmpty => o + associations(o).toList.sorted.mkString(" (", ",", ")")
        case o => o
      }
    }
    if (trimmed.isEmpty)
      None
    else if (trimmed == "?") {
      println(s"Enter one of:\n${orList(displayList)}")
      None
    }
    else {
      val lowerInput = trimmed.toLowerCase
      val allOptions = (options ++ abbr.keys).distinct
      val paired = allOptions.map(name => name -> name.toLowerCase)
      val normalized = allOptions.map(_.toLowerCase)
      val normalizedAbbreviations = for ((a, v) <- abbr) yield (a.toLowerCase, v)
      val matches = paired.filter{ case (_, lower) => lower.startsWith(lowerInput) }
      // val matches = normalized.distinct.filter(_ startsWith trimmed.toLowerCase)
      matches match {
        case Seq() =>
          println(s"'$trimmed' is not valid. Must be one of:\n${orList(displayList)}")
          None
        case Seq((value, _))  =>
          abbr.get(value) match {
            case Some(abbrValue) => Some(abbrValue)
            case None            => Some(value)
          }

        case many if many.exists(v => v._2 == lowerInput) =>
          // We got more than one match, but one of the matches is
          // an exact match (which happens to be a prefix of another entry)
          normalizedAbbreviations.get(lowerInput) match {
            case Some(abbrValue) => Some(abbrValue)
            case None            => Some(options(normalized.indexOf(lowerInput)))
          }

        case ambiguous =>
          val choices = ambiguous.toList
            .map { case (value, _) =>
              abbr.get(value) match {
                case Some(v) => v
                case None    => value
              }
            }
            .sorted
            .distinct
            .map(x => Some(x) -> x)
          val prompt = s"'$trimmed' is ambiguous.  Choose one:"
          askMenu(prompt, choices :+ (None -> "None of the above"), allowAbort = allowAbort).head

          // println(s"'$input' is ambiguous. (${orList(ambiguous)})")
          // None
      }
    }
  }

  def askOneOf(prompt: String,
               options: Seq[Any],
               initial: Option[String] = None,
               allowNone: Boolean = false,
               allowAbort: Boolean = true,
               abbr: Map[String, String] = Map.empty): Option[String] = {
    val choices = if (allowAbort) options ++ List(AbortCard) else options
    def testResponse(response: Option[String]): Option[String] = {
      response flatMap (s => matchOne(s.trim, choices.map(_.toString), abbr)) match {
        case None =>
          readLine(prompt) match {
            case null | "" if allowNone => None
            case null | ""              => testResponse(None)
            case input                  => testResponse(Some(input))
          }
        case Some(AbortCard) if allowAbort =>
          if (askYorN("Really abort (y/n)? ")) throw AbortAction else testResponse(None)
        case s => s
      }
    }
    testResponse(initial)
  }

  def askYorN(prompt: String): Boolean = {
    def testResponse(r: String): Option[Boolean] = {
      if (r == null)
        None
      else
        r.trim.toLowerCase match {
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

  def askExitAfterWin(): Boolean = {
    val choices = List(
      false -> "Continue with the game ignoring instant victory conditions.",
      true  -> "Exit the game."
    )
    askMenu("Choose one:", choices).head
  }

  def askInt(prompt: String, low: Int, high: Int, default: Option[Int] = None, allowAbort: Boolean = true): Int = {
    assert(low <= high, "askInt() low cannot be greater than high")
    if (low == high)
      low
    else {
      val choices = (low to high).toList
      default match {
        case Some(d) =>
          val p = if (choices.size > 6)
            "%s (%d - %d) Default = %d: ".format(prompt, choices.head, choices.last, d)
          else
            "%s (%s) Default = %d: ".format(prompt, orList(choices), d)
          askOneOf(p, choices, allowNone = true, allowAbort = allowAbort).map(_.toInt) match {
            case None    => d
            case Some(x) => x
          }
        case None =>
          val p = if (choices.size > 6)
            "%s (%d - %d): ".format(prompt, choices.head, choices.last)
          else
            "%s (%s): ".format(prompt, orList(choices))
          (askOneOf(p, choices, None, allowAbort = allowAbort).map(_.toInt)).get
      }
    }
  }

  def askCountry(prompt: String, candidates: List[String], allowAbort: Boolean = true): String = {
    assert(candidates.nonEmpty, s"askCountry(): list of candidates cannot be empty")
    // If only one candidate then don't bother to ask
    if (candidates.size == 1) {
      println(s"$prompt ${candidates.head}")
      candidates.head
    }
    else
      askOneOf(prompt, candidates, allowAbort = allowAbort, abbr = CountryAbbreviations).get
  }

  // Ask the user to select multiple countries from the given candidates.
  def askCountries(num: Int, candidates: List[String], allowDuplicates: Boolean = false): List[String] = {
    def nextCountry(n: Int, targets: List[String]): List[String] = {
      if (n <= num && targets.nonEmpty) {
        val name = askCountry(s"Select ${ordinal(n)} country: ", targets)
        val newTargets = if (allowDuplicates) targets else (targets.filterNot(_ == name))
        name :: nextCountry(n + 1, newTargets)
      }
      else
        Nil
    }
    nextCountry(1, candidates)
  }

  // Used when the user must select 1 or more plots in a country
  def askMapPlots(plots: List[PlotOnMap], num: Int, allowAbort: Boolean = true): List[PlotOnMap] = {
    val maxPlots  = num min plots.size
    val entries   = plots.sorted.zipWithIndex.map { case (p, i) => i.toString -> p }
    val choices   = plots.sorted.zipWithIndex.map { case (p, i) => i.toString -> p.toString }
    val plotMap   = Map(entries:_*)
    askMenu(s"Choose ${amountOf(maxPlots, "plot")}:", choices, maxPlots, allowAbort = allowAbort).map(plotMap)
  }

  // Used when the user must select 1 or more available plots to place in a country.
  def askPlots(plots: List[Plot], num: Int, allowAbort: Boolean = true): List[Plot] =
    if (plots.isEmpty || num == 0)
      Nil
    else if (plots.size == num)
      plots
    else {
      val maxPlots  = num min plots.size
      val entries   = plots.sorted.zipWithIndex.map { case (p, i) => i.toString -> p }
      val choices   = plots.sorted.zipWithIndex.map { case (p, i) => i.toString -> p.toString }
      val plotMap   = Map(entries:_*)
      askMenu(s"Choose ${amountOf(maxPlots, "plot")}:", choices, maxPlots, allowAbort = allowAbort).map(plotMap)
    }

  // Ask the user to select a number of available plots
  def askAvailablePlots(num: Int, ops: Int): List[Plot] = {
    askPlots(game.availablePlots.filter(p => ops >= p.opsToPlace), num)
  }

  // Return true for Awakening Marker, false for Reaction Marker
  def askPlaceAwakeningOrReactionMarker: Boolean = {
    val choices = List(true -> "Place awakening marker", false -> "Place reaction marker")
    askMenu("Choose one:", choices).head
  }

  // Returns (actives, sleepers)
  def askCellsNotSadr(countryName: String, numCells: Int, sleeperFocus: Boolean): (Int, Int) = {
    val c = game.getCountry(countryName)
    val (activeCells, sleeperCells) = (c.activeCells, c.sleeperCells)
    val totalCells = activeCells + sleeperCells
    val maxCells = numCells min totalCells

    if (maxCells == totalCells) (activeCells, sleeperCells)
    else if (activeCells  == 0) (0, maxCells)
    else if (sleeperCells == 0) (maxCells, 0)
    else {
      val (a, s) = (amountOf(activeCells, "active cell"), amountOf(sleeperCells, "sleeper cell"))
      println(s"\n$countryName has $a and $s")

      if (maxCells == 1)
        askMenu("Which cell:", List((1, 0) -> "Active", (0, 1) -> "Sleeper")).head
      else {
        if (sleeperFocus) {
          val smax     = maxCells min sleeperCells
          val prompt   = "\nHow many sleeper cells? "
          val sleepers = askInt(prompt, 1, smax, Some(smax))
          (maxCells - sleepers , sleepers)
        }
        else {
          val amax    = maxCells min activeCells
          val prompt  = "\nHow many active cells? "
          val actives = askInt(prompt, 1, amax, Some(amax))
          (actives, maxCells - actives)
        }
      }
    }
  }

  // Returns (actives, sleepers, sadr)
  def askCells(countryName: String, numCells: Int, sleeperFocus: Boolean): (Int, Int, Boolean) = {
    val c = game.getCountry(countryName)
    if (!c.hasSadr) {
      val (a, s) = askCellsNotSadr(countryName, numCells, sleeperFocus)
      (a, s, false)
    }
    else {
      val (activeCells, sleeperCells) = (c.activeCells, c.sleeperCells)
      val totalCells = activeCells + sleeperCells + 1 // +1 for Sadr
      val maxCells   = numCells min totalCells

      if (maxCells == totalCells)
        (activeCells, sleeperCells, true)  // Remove the lot
      else {
        val (a, s) = (amountOf(activeCells, "active cell"), amountOf(sleeperCells, "sleeper cell"))
        println(s"\n$countryName has $a and $s and Sadr is present")
        val sadr = askYorN("\nDo you want to select Sadr? (y/n) ")
        if (maxCells == 1 && sadr)
          (0, 0, true)
        else if (maxCells == 1)
          askMenu("Which cell:", List((1, 0, false) -> "Active", (0, 1, false) -> "Sleeper")).head
        else {
          if (sleeperFocus) {
            val smax     = maxCells min sleeperCells
            val prompt   = "\nHow many sleeper cells? "
            val sleepers = askInt(prompt, 1, smax, Some(smax))
            (maxCells - sleepers , sleepers, sadr)
          }
          else {
            val amax    = maxCells min activeCells
            val prompt  = "\nHow many active cells? "
            val actives = askInt(prompt, 1, amax, Some(amax))
            (actives, maxCells - actives, sadr)
          }
        }
      }
    }
  }

  sealed trait TroopOrMilitia
  case object TroopCube extends TroopOrMilitia
  case object MilitiaCube extends TroopOrMilitia
  case class TroopMarker(name: String) extends TroopOrMilitia

  // Returns "troop-cube", "militia-cube", or the name of a troop marker
  def askTroopOrMilitia(prompt: String, target: String): TroopOrMilitia = {
    val c = game.getCountry(target)
    val militia = if (game.isMuslim(target))
      game.getMuslim(target).militia
    else
      0

    assert(c.totalTroops + militia > 0, s"askTroopOrMilitia($target) called but no units present")

    if (c.troops == 0 && c.troopsMarkers.isEmpty)
      MilitiaCube
    else if (militia == 0 && c.troopsMarkers.isEmpty)
      TroopCube
    else {
      val choices = List(
        choice(c.troops > 0, TroopCube,   s"Troop cube (${c.troops} present)"),
        choice(militia > 0,  MilitiaCube, s"Militia cube (${militia} present)")
      ).flatten ++ (c.troopsMarkers.sorted.map(x => TroopMarker(x.name) -> x.name))
      askMenu("Choose one:", choices).head
    }
  }

  def askPosture(country: String): String =
    askSimpleMenu(s"Select the posture of $country: ", List(Soft, Hard))


  sealed trait CardDrawSource {
    def name: String
    def contains(cardNum: Int): Boolean
    override def toString() = name
  }

  case object FromDiscard extends CardDrawSource {
    override def name = "Discard pile"
    override def contains(cardNum: Int) = game.cardsDiscarded.contains(cardNum)
  }

  case object FromLapsing extends CardDrawSource {
    override def name = "Lapsing box"
    override def contains(cardNum: Int) = game.cardsLapsing().contains(cardNum)
  }

  case object From1stPlot extends CardDrawSource {
    override def name = "First Plot box"
    override def contains(cardNum: Int) = game.isFirstPlot(cardNum)
  }

  // Used for adjustments
  case object FromRemoved extends CardDrawSource {
    override def name = "Removed pile"
    override def contains(cardNum: Int) = game.cardsRemoved.contains(cardNum)
  }

// We do keep track of the specific cards in the deck
  // or in players hands.  To draw from either of these
  // the card cannot be anywhere else.
  case object FromDrawPile extends CardDrawSource {
    override
    def name = "Draw pile"

    override
    def contains(cardNum: Int): Boolean = {
      val accountedFor = List(FromDiscard, FromLapsing, From1stPlot, FromRemoved)

      game.cardsInUse.contains(cardNum) && !accountedFor.exists(_.contains(cardNum))
    }
  }

  case class FromRole(role: Role) extends CardDrawSource {
    override def name = if (isHuman(role))
      s"$role Player hand"
      else
      s"$role Bot hand"

    // Currently we do not track specific cards in the player/bot
    // hand.  We treat them the same is any card that is not currently
    // discarded/removed/lapsing/first-plot
    override def contains(cardNum: Int) = FromDrawPile.contains(cardNum)
  }

  def cardFoundIn(sources: List[CardDrawSource], cardNum: Int) =
    sources.exists(_.contains(cardNum))

  def cardLocation(cardNum: Int): Option[CardDrawSource] = {
    val sources = List(
      FromDiscard, FromLapsing, From1stPlot, FromRemoved, FromDrawPile
    )
    sources.find(_.contains(cardNum))
  }

  def cardsInSource(source: CardDrawSource): List[Int] = {
    source match {
      case FromDrawPile|FromRole(_) => cardsInDrawPileOrHands()
      case FromDiscard => game.cardsDiscarded
      case FromLapsing => game.cardsLapsing()
      case FromRemoved => game.cardsRemoved
      case From1stPlot => game.firstPlotCard().toList
    }
  }

  // The cards that are being used in the current action phase
  def cardsInPlay(ignoreAdditional: Boolean): List[Int] = {
    game.turnActions match {
      case PlayedCard(role, firstCard, None) :: _ =>
        List(firstCard)
      case PlayedCard(role, firstCard, Some(SecondCard(secondCard))) :: _ =>
        List(firstCard, secondCard)
      case PlayedReassement(firstCard, secondCard) :: _ =>
        List(firstCard, secondCard)
      case PlayedCard(role, firstCard, Some(AdditionalCard(_))) :: _  if ignoreAdditional =>
        List(firstCard)
      case PlayedCard(role, firstCard, Some(AdditionalCard(additionalCard))) :: _ =>
        List(firstCard, additionalCard)
      case _ =>
        Nil  // For other actions there are no cards in play
    }
  }

  // Cards not in Discard, Lapsing, 1stPlot, Removed
  // This includes cards potentially in player hands
  def cardsInDrawPileOrHands(): List[Int] = {
    val notInDrawPile =
      game.cardsDiscarded.toSet ++
      game.cardsLapsing().toSet ++
      game.firstPlotCard().toSet ++
      game.cardsRemoved.toSet ++
      cardsInPlay(ignoreAdditional = false).toSet

    game.cardsInUse
      .toSet
      .diff(notInDrawPile)
      .toList
      .sorted
  }

  // The number of cards currently in the deck.
  def numCardsInDrawPile(): Int =
    cardsInDrawPileOrHands().size - game.cardsInHand.us - game.cardsInHand.jihadist

  // Ask the use for a card number
  def askCardNumber(
    sources: List[CardDrawSource],
    prompt: String,
    initial: Option[String] = None,
    allowNone: Boolean = true,
    only: Set[Int] = Set.empty,
    except: Set[Int] = Set.empty,
    opsRequired: Set[Int] = Set.empty,
    assocRequired: Set[CardAssociation] = Set.empty,
    allowAbort: Boolean = true,
    ): Option[Int] = {

    def opsOk(cardNum: Int) = opsRequired.isEmpty || opsRequired(deck(cardNum).printedOps)

    def opsList = orList(opsRequired.toList.sorted)

    def assocOk(cardNum: Int) = assocRequired.isEmpty || assocRequired(deck(cardNum).association)

    def assocList = orList(assocRequired.toList.map(_.assocString))

    def onlyOk(cardNum: Int) = only.isEmpty || only(cardNum)

    def exceptOk(cardNum: Int) = except.isEmpty || !except(cardNum)

    def cardList(cardNums: Set[Int]) = orList(cardNums
      .toList
      .sorted
      .map(cardNumAndName))

    def validCardNumber(num: Int) = {
      var isValid = false
      def display = s"[${cardNumAndName(num)}]"

      if (!deck.isValidCardNumber(num))
        println(s"$num is not a valid card number for any of the scenario decks.")
      else if (!game.cardsInUse.contains(num) && game.campaign)
        println(s"$display is not a valid card for the current campaign scenario.")
      else if (!game.cardsInUse.contains(num))
        println(s"$display is not a valid card for the current scenario.")
      else if (!cardFoundIn(sources, num)) {
        println(s"$display is not in the ${orList(sources.map(_.name))}")
        cardLocation(num).foreach { location =>
          println(s"It is in the ${location.name}")
        }
      }
      else if (!onlyOk(num)) {
        println(s"${cardNumAndName(num)} is not allowed.")
        println(s"Select one of ${cardList(only)}")
      }
      else if (!exceptOk(num)) {
        println(s"${cardNumAndName(num)} cannot be drawn.")
        println(s"Select a card other than ${cardList(except)}")
      }
      else if (!opsOk(num))
        println(s"${cardNumAndName(num)} is not a $opsList Ops card.")
      else if (!assocOk(num))
        println(s"${cardNumAndName(num)} is not $assocList.")
      else
        isValid = true

      isValid
    }

    val ABORT = raw"(?i)(?:a|ab|abo|abor|abort)".r
    // Test the reponse and prompt again if necessary
    def testResponse(response: Option[String]): Option[Int] = {
      response match {
        case None =>
          readLine(prompt) match {
            case null | "" if allowNone => None
            case null | "" => testResponse(None)
            case ABORT() if allowAbort =>
              if (askYorN("Really abort (y/n)? "))
                throw AbortAction
              else
                testResponse(None)
            case input => testResponse(Some(input))
          }

        case Some(INTEGER(input)) =>
          val num = input.toInt
          if (validCardNumber(num))
            Some(num)
          else
            testResponse(None)

          case Some(input) =>
            println(s"$input is not a valid card number.")
            testResponse(None)
      }
    }

    // If we have any only set, then show a
    // menu with only those cards.
    val onlyList = if (only.nonEmpty)
      sources
        .flatMap(cardsInSource)
        .filter(n => onlyOk(n) && exceptOk(n) && opsOk(n) && assocOk(n))
    else
      Nil

    if (initial.isEmpty && onlyList.nonEmpty) {
      val cardChoices = onlyList.map(n => Some(n) -> cardNumAndName(n))
      val menuChoices = if (allowNone)
        cardChoices :+ (None -> "None")
      else
        cardChoices
      askMenu(prompt, menuChoices).head
    }
    else
      testResponse(initial)
  }

  // At the end of a game the US wins if it has more than twice as many
  // Resources at Good governance than the Jihadist has Resources at
  // Islamist Rule.  Otherwise, the Jihadist wins.
  // Any country with a Green regime change marker is counted as
  // Islamist Rule.
  //
  def logEndGameScoring(): Unit = {
    case class ScoreEntry(name: String, resources: Int, empty: Boolean = false) {
      override def toString() = if (empty)
        " " * 26
      else
        f"$name%-18s      $resources%2d"
    }
    val emptyEntry = ScoreEntry("", -1, true)

    val jihadistBonus = new ListBuffer[ScoreEntry]
    val goodMuslims = game.muslims.filter(m => m.isGood && m.regimeChange != GreenRegimeChange)
    val irMuslims = game.muslims.filter(m => m.isIslamistRule || m.regimeChange == GreenRegimeChange)
    val anyGreenRC = game.hasMuslim(_.regimeChange == GreenRegimeChange)
    if (game.caliphateDeclared)
      jihadistBonus += ScoreEntry("Caliphate Capital", 1)
    // The  Trump Takes command scenario starts with one WMD plot removed.
    // If any other WMD was used/alerted then there will be at least 2 removed
    if (game.scenarioName == TrumpTakesCommand.name && game.plotData.numRemovedWMD == 1)
      jihadistBonus += ScoreEntry("No WMD used", 1)

    val noEntries = List(ScoreEntry("None", 0))
    val goodResources = goodMuslims.map(c => ScoreEntry(c.name, c.resourceValue))
    val goodEntries = if (goodResources.nonEmpty) goodResources else noEntries
    val irResources = irMuslims.map(c => ScoreEntry(c.name, c.resourceValue)):::jihadistBonus.toList
    val irEntries = if (irResources.nonEmpty) irResources else noEntries
    val usScore = goodResources.map(_.resources).sum
    val jihadistScore = irResources.map(_.resources).sum
    // The US must have more than twice the points of the Jihadist
    // Also in solo games the HUMAN US must have a minimum of good resources
    // base on the length of the game:
    //   1 deck : at least  6 Good resources
    //   2 decks: at least  9 Good resources
    //   3 decks: at least 12 Good resources
    val usMinimum = 3 + (game.gameLength * 3)
    val usHasMinimum = isBot(US) || usScore >= usMinimum
    val usHasMoreThanTwice = usScore > jihadistScore * 2
    val winner = if (usHasMinimum && usHasMoreThanTwice) US else Jihadist
    val scoreEntries = goodEntries.zipAll(irEntries, emptyEntry, emptyEntry)
    val header  = "Good Resources              Islamist Rule Resources"
    val header2 = "                            (Plus green regime change)"
    val divider = "--------------------------  --------------------------"
    log()
    log("Game Over after exhausting the final deck", Color.Info)
    log(separator(char = '='), Color.Info)
    log(s"$winner victory!")
    log()
    log(header)
    if (anyGreenRC)
      log(header2)
    log(divider)
    for ((us, jihadist) <- scoreEntries)
      log(s"$us  $jihadist")
    log(divider)
    log(s"${ScoreEntry("", usScore)}  ${ScoreEntry("", jihadistScore)}")
    log()
    if (winner == US) {
      if (isHuman(US))
        log(s"Good resources are at least $usMinimum.")
      log(s"Good resources are more than twice Islamist Rule resources.")
    }
    else {
      if (!usHasMinimum)
        log(s"Good resources less than $usMinimum.")
      if (!usHasMoreThanTwice)
        log(s"Good resources are not more than twice Islamist Rule resources.")
    }
    pause()
  }

  // This is used when playing a campaign scenario.
  // When the Awakening cards are added we must:
  // Change Syria to Shia-Mix
  // Add WMD cache to Syria(2) and Iran (1)
  // update the currentMode
  // Test Algeria and add an awakening marker
  // (if possible, otherwise a random muslim country - From Forever War rules)
  def addAwakeningCards(): Unit = {
    val syria         = game.getMuslim(Syria)
    val iran          = game.getNonMuslim(Iran)
    val algeria       = game.getMuslim(AlgeriaTunisia)
    @tailrec def randomAwakeTarget(): String = {
        randomMuslimCountry match {
          case m if m.canTakeAwakeningOrReactionMarker => m.name
          case _ => randomAwakeTarget()
        }
    }

    game = game
      .updateCountry(syria.copy(isSunni = false))
      .copy(
        currentMode = AwakeningMode,
        deckNumber = game.deckNumber + 1,
        cardsInUse = Range.inclusive(game.cardsInUse.head, AwakeningMode.cardRange.last)
      )

    log()
    if (numCardsInDrawPile() == 0)
      log("Shuffle the Awakening cards to form a new draw pile.", Color.Info)
    else
      log("Shuffle the Awakening cards and place them beneath the existing draw pile.", Color.Info)
    log("\nThe Awakening expansion rules are now in effect.", Color.Info)
    log("The Bot will now use the Awakening priorities.", Color.Info)
    // If Syria is under Islamist rule then the WMD cache should be added to the available plots.
    log()
    if (syria.isIslamistRule) {
      log("Syria is now Shia-Mix country, place the Syria country mat on the board.", Color.Info)
      log("Because Syria is under Islamist Rule, add the two WMD plots", Color.Info)
      log("from the Syria cache to the available plots box.", Color.Info)
      addRemovedWMDToAvailable(2)
    }
    else {
      log("Syria is now Shia-Mix country, place the Syria country mat on the board.", Color.Info)
      addRemovedWMDToCache(Syria, 2)
    }
    log()
    // Add WMD to Iran cache
    addRemovedWMDToCache(Iran, 1)


    //  Place an awakening marker in Algeria/Tunisia if possible
    //  otherwise in a random muslim country
    val awakeningTarget = if (algeria.canTakeAwakeningOrReactionMarker)
      AlgeriaTunisia
    else {
      log(s"$AlgeriaTunisia cannot take an awakening marker.", Color.Info)
      log("An awakening marker will be added to a random Muslim country.", Color.Info)
      randomAwakeTarget()
    }

    testCountry(awakeningTarget)
    addAwakeningMarker(awakeningTarget)
  }

  // This is used when playing a campaign scenario.
  // When the Forever War cards are added we must:
  // update the currentMode
  // This mainly affects restrictions on Regime Change in Iran.
  // See rule 15.2.2.1 in the Forever War Rulebook
  def addForeverWarCards(): Unit = {
    game = game
      .copy(
        currentMode = ForeverWarMode,
        deckNumber = game.deckNumber + 1,
        cardsInUse = Range.inclusive(game.cardsInUse.head, ForeverWarMode.cardRange.last)
      )
    log()
    if (numCardsInDrawPile() == 0)
      log("Shuffle the Forever War cards to form a new draw pile.", Color.Info)
    else
      log("Shuffle the Forever War cards and place them beneath the existing draw pile.", Color.Info)

    log("\nThe Bot will now use the Forever War priorities.", Color.Info)
  }

  // This function is called when a card must be drawn from the
  // draw pile and the draw pile is empty.
  // There are several outcomes given this situation depending
  // on the type of game that is being played.
  //
  // Campaign game (2, 3 decks)
  // ---------------------------------------------------------------------
  // If we just exhausted the final deck, the the game ends immediately
  // otherwise, any lapsing cards and any first plot card are discared
  // and replaced with markers (the lapsing event are still in effect),
  // then the new deck associated with the next expansion is added to
  // the draw pile and the game continues.  All rules associated with
  // the next expansion are now in effect.
  //
  // Single scenario game (1, 2, or 3 decks)
  // ---------------------------------------------------------------------
  // If we just exhausted the final deck, then the game ends immediately
  // otherwise, any lapsing cards and any first plot card are discared
  // and replaced with markers (the lapsing event are still in effect),
  // then the discards are suffled and added to the draw pile and the game
  // continues.

  def handleEmptyDrawPile(atEndOfTurn: Boolean): Unit = {

    if (game.deckNumber == game.gameLength) {
      // The game ends immediately.
      // Lapsing cards are not discarded.
      logEndGameScoring()

      // If this was not a 3 deck game, then give the user a
      // chance to extend the game.
      val choices = Range.inclusive(game.gameLength + 1, 3).toList
        .map(len => Some(len) -> s"Extend game to $len decks") :+ (None, "Quit game")
      askMenu("\nChoose one:", choices).head match {
        case Some(len) =>
          log(s"\nGame length extended from ${game.gameLength} to $len decks.", Color.Info)
          game = game.copy(gameLength = len)
        case None =>
          saveGameState(Some("Game Over"), true)
          throw QuitGame
      }
    }

    // Lapsing events, and any 1st Plot card are discarded
    // and replace with markers to so show that the events
    // are still in effect until the end of turn, and
    // no card may be placed in the 1st plot box.
    removeLapsingAnd1stPLot()

    if (game.campaign) {
      GameMode.next(game.currentMode) match {
        case Some(AwakeningMode) => addAwakeningCards()
        case Some(ForeverWarMode) => addForeverWarCards()
        case _ =>
      }
    }
    else {
      // Single scenario game
      if (numCardsInDrawPile() == 0)
        log("\nShuffle the discard pile to form a new draw pile.", Color.Info)
      else
        log("\nShuffle the discard pile and place it beneath the existing draw pile.", Color.Info)
      log(s"This begins the ${ordinal(game.deckNumber)} deck of ${game.gameLength}.", Color.Info)
      log("Move the deck marker one space to the right.", Color.Info)
      game = game.copy(
        deckNumber = game.deckNumber + 1,
        cardsDiscarded = Nil
      )
    }
  }

  // These cards require special handling in some of the
  // card draw/discard function.
  val CriticalMiddle   = 200
  val AvengerCard      = 242


  // Process a card drawn by the given role from the given source.
  // The card is removed from the source and the hand size of the role
  // is increased (unless the card is `Avenger` in which case the event is
  // triggered and the card is placed in the discard  pile.)
  // Returns true if the card was taken into hand.
  def processCardDrawn(role: Role, cardNum: Int, source: CardDrawSource, triggerAvenger: Boolean = true): Boolean = {
    log(s"\n$role draws ${cardNumAndName(cardNum)} from the $source", Color.Info)
    source match {
      case FromDrawPile =>
        // We don't keep track of cards in the deck

      case FromDiscard =>
        game = game.copy(cardsDiscarded = game.cardsDiscarded.filterNot(_ == cardNum))

      case FromLapsing =>
        log("Mark the event as still lapsing.", Color.Info)
        val newLapsing = game.eventsLapsing.map {
          case LapsingEntry(`cardNum`, _) => LapsingEntry(cardNum, discarded = true)
          case other => other
        }
        game = game.copy(eventsLapsing = newLapsing)

      case From1stPlot =>
        log("Place an inuse marker in the first plot box.", Color.Info)
        // Card number of zero used for 1st Plot marker
        game = game.copy(firstPlotEntry = Some(LapsingEntry(0, discarded = true)))

      case FromRemoved =>
        game = game.copy(cardsRemoved = game.cardsRemoved.filterNot(_ == cardNum))

      case FromRole(from) =>
        decreaseCardsInHand(from, 1)

      case other =>
        throw new IllegalStateException(s"processCardDrawn() cannot use this source: ${other.name}")
    }

    // Avenger event triggers and is placed in the
    // discard pile.  Otherwise the player that drew the
    // card adds it to their hand
    if (cardNum == AvengerCard && triggerAvenger) {
      avengerCardDrawn()
      false
    }
    else {
      increaseCardsInHand(role, 1)
      true
    }
  }

  // This function is used to draw 1 or more cards from one or
  // more sources: draw pile, discard pile, removed pile, lapsing box(es),
  // 1st plot box, US hand, Jihadist hand
  //
  // This function is a bit of a Swiss Army knife and takes a  lot of
  // arguments.  Most calles will probably want to call one of the simpler
  // functions that make use of this one.
  //
  // This function validates that the card number entered is valid for the
  // current scenario and that it exists on of the given sources.
  // There are parameters to limit it to card with a given number of Ops,
  // and/or a given association.
  //
  // For each card draw, the card is removed from the source, and the
  // number of cards in hand for the given role is increased.
  //
  // -- This function will handle triggering the Avenger event if it is drawn.
  // -- If the only source is the Draw Pile and there are not enough cards in
  //    the draw pile to satisfy the request then this function will
  ///   call handleEmptyDrawPile()
  // Cards returned as Left(Int) have been discarded  (Avenger)
  def askCardsDrawnFull(
    role: Role,                      // Role that is drawing cards
    totalNum: Int,                   // The number of cards to draw
    sources: List[CardDrawSource],   // One or more sources from which to draw
    lessOk: Boolean = false,         // true if < that numCards is allowed, including zero
    optPrompt: Option[String] = None, // Override the default prompt
    triggerAvenger: Boolean = true,
    only: Set[Int] = Set.empty,
    except: Set[Int] = Set.empty,
    opsRequired: Set[Int] = Set.empty,
    assocRequired: Set[CardAssociation] = Set.empty,
  ): List[Either[Int, Int]] = {
    val blankIfNone = if (lessOk) " (blank if none)" else ""

    def prompt(num: Int) = optPrompt
      .getOrElse {
        if (totalNum > 1)
          s"\nWhat is the # the ${ordinal(num)} card that was drawn$blankIfNone: "
        else
          s"\nWhat is the # of the card that was drawn$blankIfNone: "
      }


    def nextDraw(num: Int): List[Either[Int, Int]] =
      if (num <= totalNum) {
        // If only source is the draw pile and the draw pile is empty...
        if (sources == List(FromDrawPile) && numCardsInDrawPile() == 0) {
          log("\nThe draw pile is empty and a card must be drawn.", Color.Info)
          handleEmptyDrawPile(atEndOfTurn = false)
        }

        val optCardNum = askCardNumber(
          sources,
          prompt(num),
          allowNone = lessOk,
          only = only,
          except = except,
          opsRequired = opsRequired,
          assocRequired = assocRequired)

        optCardNum match {
          case None =>
            return Nil

          case Some(cardNum) =>
            val source = sources.find(_.contains(cardNum)).get
            // If card was taken into hand (not Avenger) then add it as Right(num)
            // Otherwise Left(num)
            if (processCardDrawn(role, cardNum, source, triggerAvenger))
              Right(cardNum)::nextDraw(num + 1)
            else
              Left(cardNum)::nextDraw(num + 1)
        }
      }
      else
        Nil

    nextDraw(1)
  }

  // Ask for 1 required card drawn from the Draw pile
  // Will return None if Avenger was drawn triggered and discarded
  def askCardDrawnFromDrawPile(role: Role, optPrompt: Option[String] = None): Option[Int] =
    askCardsDrawnFull(role, 1, List(FromDrawPile), optPrompt = optPrompt).head.toOption

  // Ask for `n` required cards drawb from the Draw pile
  def askMultipleCardsDrawnFromDrawPile(role: Role, num: Int, optPrompt: Option[String] = None): List[Int] =
    askCardsDrawnFull(role, num, List(FromDrawPile), optPrompt = optPrompt)
      .flatMap(_.toSeq)

  // Ask for 1 required card drawn from the Opponents hand
  // Will return None if Avenger was drawn triggered and discarded
  def askCardDrawnFromOpponent(role: Role, optPrompt: Option[String] = None, except: Set[Int] = Set.empty): Option[Int] = {
    askCardsDrawnFull( role, 1, List(FromRole(role.opponent)), optPrompt = optPrompt, except = except).head.toOption
  }


  // Ask for 1 required card drawn from the Discard pile
  // Will return None if Avenger was drawn triggered and discarded
  def askCardDrawnFromRemovedPile(role: Role, optPrompt: Option[String] = None, only: Set[Int] = Set.empty): Option[Int] =
    askCardsDrawnFull(role, 1, List(FromRemoved), optPrompt = optPrompt, only = only).head.toOption

  // Ask for 1 required card drawn from the Discard pile
  // Will return None if Avenger was drawn triggered and discarded
  def askCardDrawnFromDiscardPile(role: Role, optPrompt: Option[String] = None, only: Set[Int] = Set.empty): Option[Int] =
    askCardsDrawnFull(role, 1, List(FromDiscard), optPrompt = optPrompt, only = only).head.toOption

  // Events such as Oil Price Spike, OPEC Production Cut, and Peace Dividend
  // allow the player to draw a card from either the discard pile or from a
  // "box" (Lapsing or 1st Plot)
  // This method manages that process.
  // The caller should display a message about which cards should be prohibited.
  def askCardDrawnFromDiscardOrBox(role: Role, prohibited: Set[Int]): Unit = {
    val sources = new ListBuffer[CardDrawSource]()
    if (game.cardsDiscarded.nonEmpty)
      sources += FromDiscard
    if (game.cardsLapsing().nonEmpty)
      sources += FromLapsing
    if (game.firstPlotCard().nonEmpty)
      sources += From1stPlot

    if (sources.isEmpty)
      log("\nThere are no cards in the Discard pile, Lapsing box, or First Plot box.", Color.Event)
    else {
      displayLine(s"\nSelect a card from the ${orList(sources.map(_.name).toList)} and add it to your hand.")
      askCardsDrawnFull(role, 1, sources.toList, except = prohibited)
    }
  }

  def processDiscardedCard(role: Role, cardNum: Int, triggerRole: Option[Role] = None): Unit = {
    val card = deck(cardNum)
    val eventName = card.cardName

    decreaseCardsInHand(role, 1)
    log(s"\n$role discards ${cardNumAndName(cardNum)}", Color.Event)
    if (cardNum == AvengerCard)
      avengerCardDrawn(discarded = true)
    else if (card.autoTrigger) {
      log(s"\"$eventName\" is being discarded, so the event triggers", Color.Event)
      log(separator())
      card.executeEvent(US)  // Role does not matter
    }
    else if (triggerRole.exists(role => card.eventWillTrigger(role))) {
      log(s"\nThe \"$eventName\" event is triggered.", Color.Event)
      performCardEvent(card, Jihadist, triggered = true)
    }
    else if (triggerRole.nonEmpty)
      log(s"\nThe \"$eventName\" event does not trigger.", Color.Event)
    // Finally, place the card in the discard pile
    addCardToDiscardPile(cardNum)
  }

  // Return card number that was discarded, or None.
  def askCardBeingDiscarded(
    role: Role,
    optPrompt: Option[String] = None,
    triggerRole: Option[Role] = None,
    opsRequired: Set[Int] = Set.empty,
    assocRequired: Set[CardAssociation] = Set.empty,
    only: Set[Int] = Set.empty,
    allowNone: Boolean = false): Option[Int] = {
    val blankIfNone = if (allowNone) " (blank if none)" else ""
    val prompt = optPrompt.getOrElse(s"What # of the card being discarded$blankIfNone: ")

    val optCardNum = askCardNumber(
      FromRole(role)::Nil,
      prompt,
      allowNone = allowNone,
      opsRequired = opsRequired,
      assocRequired = assocRequired,
      only = only)

    // If we got a card, then remove it from hand and add
    // it to the discard pile processing events as necessary
    optCardNum
      .foreach { cardNum =>
        processDiscardedCard(role, cardNum, triggerRole)
      }
    optCardNum
  }

  // Convience method for discarding by number of cards
  def askCardsDiscarded(
    role: Role,
    numCards: Int,
     lessOk: Boolean = false,
     triggerRole: Option[Role] = None,
     only: Set[Int] = Set.empty,
  ): List[Int] = {
    val blankIfNone = if (lessOk)
       " (blank if none)"
    else
      ""
    def nextCard(currentNum: Int): List[Int] = {
      if (currentNum <= numCards) {
        val prompt = (numCards, only.nonEmpty) match {
          case (1, true) =>
            s"\nWhat is the $role card being discarded: "
          case (_, true) =>
            s"\nWhat is the ${ordinal(currentNum)} $role card being discarded: "
          case (1, false) =>
            s"\nWhat is the # of the $role card being discarded$blankIfNone: "
          case (_, false) =>
            s"\nWhat is the # of the ${ordinal(currentNum)} $role card being discarded$blankIfNone: "
        }
        askCardBeingDiscarded(
          role,
          Some(prompt),
          only = only,
          triggerRole = triggerRole,
          allowNone = lessOk
        ) match {
          case None =>
            Nil
          case Some(cardNum) =>
            cardNum :: nextCard(currentNum + 1)
        }
      }
      Nil
    }

    nextCard(1)
  }

  // Convience method for discarding by total number of Ops
  def askCardsDiscardedByOps(role: Role, totalOps: Int, triggerRole: Option[Role] = None) = {

    def nextCard(currentNum: Int, accumulatedOps: Int): Unit = {
      if (accumulatedOps < totalOps && hasCardInHand(role)) {
        val prompt =
          s"\nWhat is the # of the ${ordinal(currentNum)} $role card being discarded: (blank if none) "

        displayLine(s"\n${amountOf(accumulatedOps, "Op")} of $totalOps have been accounted for.", Color.Info)
        askCardBeingDiscarded(role, Some(prompt), triggerRole) match {
          case None =>
          case Some(cardNum) =>
            nextCard(currentNum + 1, accumulatedOps + deck(cardNum).printedOps)
        }
      }
    }

    nextCard(1, 0)
  }

  // The Avenger card has a Special clause that its event
  // triggers whenever it is randomly drawn or discared.
  def avengerCardDrawn(discarded: Boolean = false): Unit = {
    val card = deck(AvengerCard)
    val name = card.cardName
    val action = if (discarded)
      "is being discarded"
    else
      "was drawn"

    log()
    log(s"""${card.numAndName} $action, so the event triggers""", Color.Event)
    log(separator())
    card.executeEvent(US)
  }

  // Convenience method for creating choices for the askMenu() function.
  def choice[T](condition: Boolean, value: T, desc: String): Option[(T, String)] =
    if (condition) Some(value -> desc) else None

  def choice[T](condition: Boolean, value: T): Option[T] = if (condition) Some(value) else None

  def choice[T](condition: Boolean, value: T, desc: String, detail: Seq[String]): Option[(T, (String, Seq[String]))] =
    if (condition) Some(value -> (desc, detail)) else None

  def choice[T](condition: Boolean, value: T, char: Char, desc: String): Option[(T, Char, String)] =
    if (condition) Some((value, char, desc)) else None

  // Present a numbered menu of choices
  // Allow the user to choose 1 or more choices and return
  // a list of keys to the chosen items.
  // Caller should println() a brief description of what is being chosen.
  // items is a list of items.
  def askSimpleMenu[T](prompt: String, items: List[T], allowAbort: Boolean = true): T =
    askMenu(prompt, items.map(x => x -> x.toString), allowAbort = allowAbort).head

  // Present a numbered menu of choices
  // Allow the user to choose 1 or more choices and return
  // a list of keys to the chosen items.
  // Caller should println() a brief description of what is being chosen.
  // items is a list of (key -> display) for each item in the menu.
  def askMenu[T](
    prompt: String,
    items: List[(T, String)],
    numChoices: Int = 1,
    repeatsOK: Boolean = false,
    allowAbort: Boolean = true): List[T] = {
    def nextChoice(num: Int, itemsRemaining: ListMap[T, String]): List[T] = {
      if (itemsRemaining.isEmpty || num > numChoices)
        Nil
      else if (itemsRemaining.size == 1)
        itemsRemaining.keys.head :: Nil
      else {
        val indexMap = (itemsRemaining.keys.zipWithIndex.map(_.swap)).toMap
        val choicePrompt = if (numChoices > 1)
          s"${ordinal(num)} Selection: "
        else
          "Selection: "
        if (items.size > 1 &&  prompt != "") {
          println()
          println(prompt)
          println(separator())
        }
        for ((key, i) <- itemsRemaining.keysIterator.zipWithIndex)
          println(s"${i+1}: ${itemsRemaining(key)}")
        println(separator())
        val choice = askOneOf(choicePrompt, 1 to itemsRemaining.size, allowAbort = allowAbort).get.toInt
        val index  = choice - 1
        val key    = indexMap(index)
        val remain = if (repeatsOK) itemsRemaining else itemsRemaining - key
        indexMap(index) :: nextChoice(num + 1, remain)
      }
    }
    nextChoice(1, ListMap(items:_*))
  }

    // Present a numbered menu of choices
  // Allow the user to choose 1 or more choices and return
  // a list of keys to the chosen items.
  // Caller should println() a brief description of what is being chosen.
  // items is a list of (key -> display) for each item in the menu.
  def askMenuWithWrap[T](
    menuPrompt: String = "",
    items: List[(T, (String, Seq[String]))],
    delim: String = ", ",
    sameLine: Boolean = true,
    allowAbort: Boolean = true): T = {

    assert(items.nonEmpty, "askMenuWithWrap() called with empty items list")
    if (items.size == 1)
      items.head._1
    else {
      val itemMap = ListMap(items:_*)

      val numWidth = itemMap.size.toString.size
      println()
      println(menuPrompt)
      println(separator(char = '='))

      val indexMap = itemMap.keys.zipWithIndex.map(_.swap).toMap

      for ((key, i) <- itemMap.keysIterator.zipWithIndex) {
        val number = String.format(s"%${numWidth}d: ", i+1)
        val (name, detail) = itemMap(key)
        if (sameLine) {
          val prefix = s"${number}${name} "
          wrap(prefix, detail, delim = delim, showNone = false).foreach(println)
        }
        else {
          val offset = " " * (numWidth + 2)
          println(s"${number}${name}")
          if (detail.nonEmpty)
            wrap(offset, detail, delim = delim, showNone = false).foreach(println)
        }
      }
      println(separator())
      val choice = askOneOf("Selection: ", 1 to itemMap.size, allowAbort = allowAbort).get.toInt
      indexMap(choice - 1) // convert back to zero based
    }
  }

  // Present a list of choices where each choice is associated with
  // a singgle characters.  When the user enters the characters
  // the corresponding item is returned.  If there are other
  // characters on the line entered they are returned in as and optional
  // argument.
  //
  // If the `numericItem` parameter is not None, then entering
  // a number instead of one of the letters will return the
  // associated item with the numeric value as he argument.
  //
  // Each item tupel contains:
  // T      - the item value returned
  // Char   - the characters used to select the item
  // String - the text displayed for the item
  //
  def argMenu[T](prompt: String, items: List[(T, Char, String)], numericItem: Option[T] = None, allowNone: Boolean = true): Option[(T, Option[String])] = {
    if (items.isEmpty)
      throw new IllegalArgumentException("argMenu() passed an empty item list")
    val itemMap = items.map(i => i._2 -> i._1).toMap
    val validOptions = orList(items.map(_._2))

    val NUMBER = raw"(\d+)".r
    val VALID = raw"([^\s]+)(?:\s(.*))?".r
    def getInput(): Option[(T, Option[String])] = {
      val input = readLine(prompt) match {
        case null => ""
        case i    => i.trim
      }
      input match {
        case "" if allowNone =>
          None
        case "" =>
          getInput()

        case NUMBER(num) if numericItem.nonEmpty =>
          Some((numericItem.get, Some(num)))

        case VALID(action, param) if itemMap.contains(action(0)) =>
          Some((itemMap(action(0)), Option(param).map(_.trim)))

        case _ =>
          println(s"Not valid. Enter one of: $validOptions")
          getInput()
      }
    }

    for ((_, char, display) <- items)
        println(s"$char: $display")
      println(separator(54))
    getInput()
  }


  // Name of country and number of items in that country.
  case class MapItem(country: String, num: Int)
  // This method is used when a number of items such as troops, sleeper cells, etc.
  // must be selected from the map.
  // targets     - a list MapItem instances where items can be selected.
  // numItems    - the number that must be selected
  // name        - should be the singular name of the item being selected (troop, sleeper cell)
  // Returns the list if MapItems with the results.
  // Before calling this, the caller should print a line to the terminal describing
  // what is being selected.
  def askMapItems(targets: List[MapItem], numItems: Int, name: String): List[MapItem] = {
    targets match {
      case _  if numItems == 0 => Nil  // The required number have been selected
      case target :: Nil       => MapItem(target.country, numItems) :: Nil // Last country remaining
      case _ =>
        val maxLen = longestString(targets.map(_.country))
        val fmt = "%%-%ds  (%%s)".format(maxLen)
        def disp(t: MapItem) = {fmt.format(t.country, amountOf(t.num, name))}
        val choices = targets.map(t => t.country -> disp(t))
        println()
        val country = askMenu(s"${amountOf(numItems, name)} remaining, choose country", choices).head
        val limit   = targets.find(_.country == country).get.num min numItems
        val num     = askInt("How many", 1, limit)
        val newTargets = targets.map { t =>
          if (t.country == country)
            t.copy(num = t.num - num)
          else
            t
        }.filterNot (_.num == 0)
        MapItem(country, num) :: askMapItems(newTargets, numItems - num, name)
    }
  }

  // This is used then the user is asked to select a number of cells from multiple
  // locations: country names or "track".
  case class CellsItem(name: String, actives: Int, sleepers: Int) {
    val total = actives + sleepers
  }

  def askCellsFromAnywhere(num: Int, trackOK: Boolean, names: List[String], sleeperFocus: Boolean): List[CellsItem] = {
    if (num == 0)
      Nil
    else {
      def nextChoice(numRemaining: Int, sources: List[CellsItem]): List[CellsItem] = {
        if (numRemaining == 0 || sources.isEmpty)
          Nil
        else {
          val len = longestString(sources.map(_.name))
          val fmttrk = "%%-%ds  (contains %%d cells)".format(len)
          val fmt    = "%%-%ds  (contains %%d active and %%d sleeper)".format(len)
          def disp(i: CellsItem) = if (i.name == "track")
            fmttrk.format(i.name, i.sleepers)
          else
            fmt.format(i.name, i.actives, i.sleepers)
          if (numRemaining != num)
            println()
          val (src, (a, s)) = sources match {
            case (src @ CellsItem(name, a, 0))::Nil => (src, (a min numRemaining, 0))
            case (src @ CellsItem(name, 0, s))::Nil => (src, (0, s min numRemaining))
            case _ =>
              val src = if (sources.size == 1)
                sources.head
              else {
                val choices = sources.map(i => i.name -> disp(i))
                val name = askMenu(s"${amountOf(numRemaining, "cell")} remaining, choose from:", choices).head
                sources.find (_.name == name).get
              }
              val name = src.name
              val mx = numRemaining min src.total
              val n = askInt(s"Choose how many cells from $name", 1, mx, Some(mx))
              val (a, s) = if (name == "track") (0, n) else askCellsNotSadr(name, n, sleeperFocus)
              (src, (a, s))
          }
          val newSources = if (src.total == (a + s))
            sources.filterNot(_.name == src.name)
          else
            sources.map { x =>
              if (x.name == src.name)
                x.copy(actives = x.actives - a, sleepers = x.sleepers - s)
              else
                x
            }
          CellsItem(src.name, a, s) :: nextChoice(numRemaining - a - s, newSources)
        }
      }

      // First create a CellsItem for all potential sources that have cells.
      val trackSrc = if (trackOK && game.cellsAvailable > 0)
        List(CellsItem("track", 0, game.cellsAvailable))
      else
        Nil
      val countrySrcs = game.getCountries(names)
        .filter(_.totalCells > 0)
        .map(c => CellsItem(c.name, c.activeCells, c.sleeperCells))
      nextChoice(num, trackSrc ::: countrySrcs)
    }
  }

  def moveCellsToTarget(target: String, items: List[CellsItem]): Unit = {
    items foreach {
      case CellsItem("track", _ , n) =>
        addSleeperCellsToCountry(target, n)

      case CellsItem(name, a, s)     =>
        moveCellsBetweenCountries(name, target, a, true, forTravel = false)
        moveCellsBetweenCountries(name, target, s, false, forTravel = false)
    }
  }

  //  cells: (activeCells, sleeperCells, Sadr)
  case class CellsToRemove(countryName: String, cells: (Int, Int, Boolean))

  def askToRemoveCells(maxCells: Int, upto: Boolean, countryNames: List[String], sleeperFocus: Boolean): Vector[CellsToRemove] = {

    @tailrec def askNext(numToRemove: Int, choices: List[(String, String)], removed: Vector[CellsToRemove]): Vector[CellsToRemove] = {
      val totalRemaining = choices.foldLeft(0) {
        case (total, (name, _)) => total + game.getCountry(name).totalCells
      }

      if (numToRemove == 0 || choices.isEmpty)
        removed
      else if (!upto && totalRemaining <= numToRemove) {
        val remainder = for {
          (name, _) <- choices.toVector
          c = game.getCountry(name)
        } yield
          CellsToRemove(name, (c.activeCells, c.sleeperCells, c.hasSadr))

        removed :++ remainder
      }
      else {

        val name = askMenu("Remove cells from which country:", choices).head
        val c    = game.getCountry(name)
        val others = totalRemaining - c.totalCells
        val minNum = if (upto) 0 else (numToRemove - others) max 0 min c.totalCells
        val maxNum = c.totalCells min numToRemove
        val num = askInt(s"\nRemove how many cells from $name", minNum, maxNum)
        val (actives, sleepers, sadr) = askCells(name, num, sleeperFocus)
        askNext(
          numToRemove - num,
          choices.filterNot(_._1 == name),
          removed :+ CellsToRemove(name, (actives, sleepers, sadr))
        )
      }
    }

    val countries = game.getCountries(countryNames)
      .filter(_.totalCells > 0)
      .sortBy(_.name)

    val totalCells = countries
      .foldLeft(0) { (total, country) => total + country.totalCells }

    val choices = for {
      name <- countryNames
      c = game.getCountry(name)
      if c.totalCells > 0
    } yield {
      val b = new ListBuffer[String]
      if (c.sleeperCells > 0)
        b += amountOf(c.sleeperCells, "sleeper")
      if (c.activeCells > 0)
        b += amountOf(c.activeCells, "active")
      if (c.hasSadr)
        b += "Sadr"
      name -> s"$name ${b.toList.mkString("(", ", ", ")")}"
    }
    askNext(maxCells min totalCells, choices, Vector.empty)
  }

  // Use the random Muslim table.
  val randomMuslimTable = Vector(
    Vector(Morocco,        Syria,       ""),
    Vector(AlgeriaTunisia, Jordan,      IndonesiaMalaysia),
    Vector(Libya,          Turkey,      Lebanon),
    Vector(Egypt,          SaudiArabia, Iraq),
    Vector(Sudan,          GulfStates,  Afghanistan),
    Vector(Somalia,        Yemen,       Pakistan))

  def randomMuslimCountry: MuslimCountry = {
    // Secondary, Muslim table
    // Iran and Nigeria may not yet have become Muslim countries.
    // If they are selected and non Muslim, then we simply roll again.
    def secondaryRandomMuslimCountry: MuslimCountry = {
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

    if (game.manualDieRolls) {
      val candidates = countryNames(game.muslims)
      game.getMuslim(askCountry("Select \"Random\" country: ", candidates, allowAbort = false))
    }
    else {
      val row = dieRoll - 1        // tan die
      val col = (dieRoll - 1) / 2  // black die
      if (row == 0 && col == 2)
        secondaryRandomMuslimCountry
      else
        game.getMuslim(randomMuslimTable(row)(col))
    }
  }


  def randomShiaMixList: List[MuslimCountry] = {
    val candidates = List(Syria, SaudiArabia, Turkey, Iraq, GulfStates, Yemen, Pakistan, Lebanon, Afghanistan)
    val targets = if (game.isMuslim(Iran))
      Iran :: candidates
    else
      candidates
    game.getMuslims(candidates)
  }

  def randomShiaMixCountry: MuslimCountry = if (game.manualDieRolls) {
    val candidates = countryNames(randomShiaMixList)
    game.getMuslim(askCountry("Select \"Random Shia Mix\" country: ", candidates, allowAbort = false))
  }
  else {
    val muslimKey = List(dieRoll, dieRoll, dieRoll).sum match {
      case 3 | 4 | 5 | 6                 => Syria
      case 7 if game.isMuslim(Iran)      => Iran
      case 7                             => Syria
      case 8                             => SaudiArabia
      case 9                             => Turkey
      case 10                            => Iraq
      case 11                            => GulfStates
      case 12                            => Yemen
      case 13                            => Pakistan
      case 14                            => Lebanon
      case _  /* 15 | 16 | 17 | 18 */    => Afghanistan
    }
    game.getMuslim(muslimKey)
  }

  def randomSchengenCountry: NonMuslimCountry = {
    dieRoll match {
      case 1 => game.getNonMuslim(Scandinavia)
      case 2 => game.getNonMuslim(Benelux)
      case 3 => game.getNonMuslim(Germany)
      case 4 => game.getNonMuslim(France)
      case 5 => game.getNonMuslim(Spain)
      case _ => game.getNonMuslim(Italy)
    }
  }

  def addOpsTarget(name: String): Unit = {
    if (name != "track") {
      val targets = game.targetsThisPhase
      game = game.copy(targetsThisPhase = targets.copy(ops = targets.ops + name))
    }
  }

  def addDisruptedTarget(name: String): Unit = {
    val targets = game.targetsThisPhase
    game = game.copy(targetsThisPhase = targets.copy(disrupted = targets.disrupted + name))
  }

  def addTestedOrImprovedToFairOrGood(name: String): Unit = {
    val targets = game.targetsThisPhase
    game = game.copy(targetsThisPhase = targets.copy(
      testedOrImprovedToFairOrGood = targets.testedOrImprovedToFairOrGood + name))
  }

  def addEventTarget(names: String*): Unit = {
    val targets = game.targetsThisPhase
    game = game.copy(targetsThisPhase = targets.copy(event = targets.event ++ names))
  }

  // If the country is untested, test it and return true
  // otherwise return false.
  def testCountry(name: String): Boolean = {
    val country = game.getCountry(name)
    if (country.isUntested) {
      country match {
        case m: MuslimCountry    =>
          val die = getDieRoll(s"Enter test die roll for $name: ")
          val newGov = if (die < 5)
            Poor
          else
            Fair
          if (newGov == Fair)
            addTestedOrImprovedToFairOrGood(name)
          game = game.updateCountry(m.copy(governance = newGov, alignment = Neutral))
          log(s"${m.name} tested: Set to ${govToString(newGov)} Neutral", Color.MapPieces)

        case n: NonMuslimCountry =>
          val die = getDieRoll(s"Enter test die roll for $name: ")
          val newPosture = if (die < 5)
            Soft
          else
            Hard
          game = game.updateCountry(n.copy(postureValue = newPosture))
          log(s"${n.name} tested: Set posture to $newPosture", Color.MapPieces)
          logWorldPosture()
      }
      true
    }
    else
      false
  }

  // This is used by some events.
  def rollGovernance(name: String): Unit = {
    val m = game getMuslim name
    val die = getDieRoll(s"Enter governance die roll for $name: ")
    log(s"Governance die roll: $die")
    val newGov = if (die < 5) Poor else Fair
    game = game.updateCountry(m.copy(governance = newGov))
    log(s"Set the governance of $name to ${govToString(newGov)}", Color.MapPieces)
  }

  // The default drm when rolling US posture is +1
  def rollUSPosture(drms: List[(Int, String)] = Nil): Unit = {
    val die = getDieRoll("Enter US posture die roll: ")
    val allDrms = (1, "Rolling US Posture") :: drms
    val modifiedDie = die + allDrms.map(_._1).sum
    val newPosture = if (modifiedDie < 5)
      Soft
    else
      Hard
    log(s"Roll United States posture")
    log(s"Die roll: $die")
    for ((drm, desc) <- allDrms)
      log(f"$drm%+d  $desc")
    if (allDrms.nonEmpty)
      log(s"Modified roll: $modifiedDie")

    setUSPosture(newPosture)
  }

  def setUSPosture(newPosture: String): Unit = {
    if (game.usPosture == newPosture)
      log(s"US posture remains $newPosture", Color.MapMarker)
    else {
      game = game.copy(usPosture = newPosture)
      log(s"Set US posture to $newPosture", Color.MapMarker)
    }
  }

  def rollCountryPosture(name: String, drm: Int = 0, logWorld: Boolean = true): Unit = {
    assert(name != UnitedStates, "rollCountryPosture() called for United States.  Use RollUSPosture()")
    val n = game.getNonMuslim(name)
    val die = getDieRoll(s"Enter posture die roll for $name: ")
    val modifiedDie = die + drm
    log(s"\nRoll posture of $name")
    log(s"Die roll: $die")
    if (drm != 0) {
      log(f"$drm%+d drm")
      log(s"Modified roll: $modifiedDie")
    }
    val newPosture = if (modifiedDie < 5) Soft else Hard
    if (n.posture == newPosture)
      log(s"The posture of $name remains $newPosture", Color.MapPieces)
    else {
      game = game.updateCountry(n.copy(postureValue = newPosture))
      log(s"Set the posture of $name to $newPosture", Color.MapPieces)
      if (logWorld)
        logWorldPosture()
    }
  }


  def modifyWoiRoll(die: Int, m: MuslimCountry, ignoreGwotPenalty: Boolean = false, silent: Boolean = false): Int = {
    def logNotZero(value: Int, msg: String): Unit =
      if (!silent && value != 0) log(f"$value%+2d $msg")
    val prestigeMod      = game.prestigeModifier
    val shiftToGoodMod   = if (m.isAlly && m.isFair) -1 else 0
    val gwotMod          = if (ignoreGwotPenalty) 0 else -game.gwotPenalty
    val aidMod           = m.aidMarkers
    val adjToGoodAllyMod = if (game.adjacentToGoodAlly(m.name)) 1 else 0
    val awakeningMod     = m.awakening
    val reactionMod      = -m.reaction
    logNotZero(prestigeMod,      "Prestige")
    logNotZero(shiftToGoodMod,   "Shift to Good governance")
    logNotZero(gwotMod,          "GWOT penalty")
    logNotZero(aidMod,           "Aid")
    logNotZero(adjToGoodAllyMod, "Adjacent to Good Ally")
    logNotZero(awakeningMod,     "Awakening")
    logNotZero(reactionMod,      "Reaction")

    val bonus   = aidMod + adjToGoodAllyMod + awakeningMod + (if (prestigeMod > 0) prestigeMod else 0)
    val penalty = shiftToGoodMod + gwotMod + reactionMod + (if (prestigeMod < 0) prestigeMod else 0)

    val drm = if (game.usResolve(NoMercy)) {
      if (!silent && penalty < 0)
        log(s"$US Bot with NoMercy resolve ignores DRM penalty")
      bonus
    }
    else
      bonus + penalty

    val modRoll = die + drm
    if (!silent && (bonus > 0 || penalty < 0))
      log(s"Modified roll: $modRoll")
    modRoll
  }

  def modifyJihadRoll(die: Int, m: MuslimCountry, major: Boolean, silent: Boolean = false): Int = {
    def logNotZero(value: Int, msg: String): Unit =
      if (!silent && value != 0) log(f"$value%+2d $msg")

    val awakeningMod = m.awakening
    val reactionMod  = -m.reaction
    val drm          =  awakeningMod + reactionMod
    logNotZero(awakeningMod,   "Awakening")
    logNotZero(reactionMod,    "Reaction")

    val finalDrm = if (game.jihadistIdeology(Virulent)) {
      if (!silent && drm > 0)
        log(s"$Jihadist Bot with Virulent Ideology ignores DRM penalty for Jihad")
      0
    }
    else if (game.jihadistIdeology(Coherent) && !major) {
      if (!silent && drm > 0)
        log(s"$Jihadist Bot with Coherent Ideology ignores DRM penalty for Minor Jihad")
      0
    }
    else
      drm
    val modRoll = die + finalDrm
    if (!silent && (awakeningMod != 0 || reactionMod != 0))
      log(s"Modified roll: $modRoll")
    modRoll
  }

  // Return true if no caliphate exists and the given country is a caliphate candidate.
  def canDeclareCaliphate(capital: String): Boolean  =
    game.useExpansionRules  &&
    !game.caliphateDeclared &&
    game.isMuslim(capital)  &&
    game.getMuslim(capital).caliphateCandidate

  def askDeclareCaliphate(capital: String): Boolean =
    askYorN(s"Do you wish to declare a Caliphate with $capital as the Capital (y/n)? ")


  // Throws an exception if a Caliphate already exists
  def declareCaliphate(capital: String): Unit = {
    val side = if (isHuman(Jihadist)) "Player" else "Bot"
    val prevGameState = game
    assert(!game.caliphateDeclared, "declareCaliphate() called and a Caliphate Capital already on the map")
    log(s"\nJihadist $side declares $capital as the Caliphate Capital.", Color.Event)
    setCaliphateCapital(capital)
    logExtraCellCapacityChange(prevGameState)
    increaseFunding(2)
    logSummary(game.caliphateSummary)
  }

  def setCaliphateCapital(name: String): Unit = {
    assert(game.isMuslim(name), s"setCaliphateCapital() called on non-muslim country: $name")
    val capital = game.getMuslim(name);
    assert(capital.caliphateCandidate, s"setCaliphateCapital() called on invalid country: $name")
    // First make sure there is no country marked as the capital
    game = game.updateCountries(game.muslims.map(_.copy(caliphateCapital = false)))
    val daisyChain = game.caliphateDaisyChain(name).map { memberName =>
      val member = game.getMuslim(memberName)
      if (member.sleeperCells > 0)
        log(s"Activate all sleeper cells in $memberName", Color.MapPieces)
      member.copy(
        caliphateCapital = member.name == name,
        activeCells      = member.cells,  // All cells go active in caliphate members
        sleeperCells     = 0
      )
    }
    log(s"Place Caliphate capital marker in ${capital.name}", Color.MapPieces)
    game = game.updateCountries(daisyChain)
  }


  // Important: Assumes that the game has already been updated, such that the
  // previousCapital is no longer a caliphateCandidate! Otherwise the caliphate
  // size comparisons for the new capital candidate would include the old
  // capital, which is wrong.
  def displaceCaliphateCapital(previousCapital: String): Unit = {
    // Used when the Bot is selecting new caliphate capital
    case class CaliphateCapitalCandidate(m: MuslimCountry) {
      val size = game.caliphateDaisyChain(m.name).size
    }
    // Sort first by daisy chain size large to small
    // then by non Ally before Ally
    // then by worse governance before better governance
    // then by worst WoI drm before better WoI drm
    def compareCapitalCandidates(x: CaliphateCapitalCandidate, y: CaliphateCapitalCandidate) = {
      if (x.size != y.size)
        y.size compare x.size // y first: to sort large to small.
      else if (x.m.isAlly != y.m.isAlly)
        x.m.isAlly compare y.m.isAlly // x first: nonAlly before Ally
      else if (x.m.governance != y.m.governance)
        y.m.governance compare x.m.governance // y first: because Good < Islamist Rule
      else {
        val (xMod, yMod) = (modifyWoiRoll(1, x.m, false, true), modifyWoiRoll(1, y.m, false, true))
        xMod compare yMod
      }
    }
    implicit val CaliphateCapitalCandidateOrdering = new Ordering[CaliphateCapitalCandidate] {
      def compare(x: CaliphateCapitalCandidate, y: CaliphateCapitalCandidate) = compareCapitalCandidates(x, y)
    }
    // Caliphate capital displaced.  Attempt to move it to adjacent caliphate country.
    val adjacents = game.adjacentMuslims(previousCapital).filter(_.caliphateCandidate)
    log()
    log("The Caliphate capital has been displaced!")
    log(separator())
    if (adjacents.size == 0) {
      log(s"There are no adjacent Caliphate candidates, remove Caliphate capital")
      decreaseFunding(2)
      increasePrestige(2)
    }
    else {
      // If Jihadist is human, ask them to pick the new capital.
      // If Jihadist is a bot, the country that connects to the
      // largest number of daisy chained caliphate members.
      // Then pick one at random among any ties.
      val newCapitalName = if (adjacents.size == 1)
        adjacents.head.name
      else if (game.humanRole == Jihadist) {
        val choices = adjacents.map(_.name)
        askCountry(s"Choose new Caliphate capital (${orList(choices)}): ", choices, allowAbort = false)
      }
      else {
        // The Bot pick the best candidate for the new capital based on
        // the set of conditions outlined by compareCapitalCandidates().
        // We sort the best to worst.  If more than one has the best score, then
        // we choose randomly among them.
        val sorted = adjacents.map(CaliphateCapitalCandidate).sorted
        val best   = sorted
          .takeWhile(compareCapitalCandidates(_, sorted.head) == 0)
          .map(_.m.name)
        shuffle(best).head
      }
      setCaliphateCapital(newCapitalName)
      decreaseFunding(1)
      increasePrestige(1)
      logSummary(game.caliphateSummary)
    }
  }

  // Check to see if there are any sleeper cells in any caliphate members
  // and flip them to active
  def flipCaliphateSleepers(): Unit = {
    for {
      capital <- game.caliphateCapital
      member  <- game.caliphateDaisyChain(capital)
      m       =  game.getMuslim(member)
      if m.sleeperCells > 0
    } {
      game = game.updateCountry(m.copy(sleeperCells = 0, activeCells = m.cells))
      log(s"$member is now a caliphate member, flip all sleeper cells to active", Color.FlipPieces)
    }
  }

  // Select a random county to take an awakening or reaction marker
  def randomConvergenceTarget: MuslimCountry = {
    @tailrec def getTarget(): MuslimCountry = {
      val rmc = randomMuslimCountry
      if (rmc.canTakeAwakeningOrReactionMarker)
        rmc
      else {
        val candidates = game.adjacentMuslims(rmc.name)
          .filter(_.canTakeAwakeningOrReactionMarker)
        candidates match {
          case Nil   => getTarget()  // Try again
          case valid => shuffle(valid).head
        }
      }
    }
    getTarget()
  }

  // Convergence
  def performConvergence(forCountry: String, awakening: Boolean): Unit = {

    if (game.useExpansionRules) {
      if (lapsingEventInPlay(ArabWinter))
        log("No convergence performed because \"Arab Winter\" is in effect", Color.Info)
      else {
        val target = randomConvergenceTarget.name
        log(s"\nConvergence for ${forCountry} takes place in ${target}")
        log(separator())
        testCountry(target)
        if (awakening)
          addAwakeningMarker(target)
        else
          addReactionMarker(target)
      }
    }
  }

  // Format the given sequence of strings in a comma separated list
  // such that we do not exceed the given maxWith.
  def wrap[T](prefix: String, values: Seq[T], maxWidth: Int = 100, showNone: Boolean = true, delim: String = ", "): Seq[String] = {
    val stringValues = values.map(_.toString)
    val b = new ListBuffer[String]
    val s = new StringBuilder(prefix)
    var first = true
    if (stringValues.isEmpty) {
      if (showNone)
        s.append("none")
    }
    else {
      val margin = " " * prefix.length
      s.append(stringValues.head)
      for (v <- stringValues.tail) {
        s.append(delim)
        if (s.length + v.length < maxWidth)
          s.append(v)
        else {
          b += s.toString
          s.clear()
          s.append(margin).append(v)
        }
      }
    }
    b += s.toString
    b.toList
  }

  def wrapInColumns[T](
    prefix: String,
    values: Seq[T],
    maxWidth: Int = 100,
    separator: String = " | ",
    showNone: Boolean = true
  ): Seq[String] = {
    def getColWidths(numCols: Int, strings: Seq[String]): List[Int] = {
      val extra = strings.size % numCols
      val padded = if (extra == 0)
        strings
      else
        strings :++ Seq.fill(numCols - extra)("")
      padded
        .grouped(numCols)
        .toList
        .transpose
        .foldLeft(List.empty[Int]) { (widths, entries) => longestString(entries) :: widths}
        .reverse
    }

    def findNumCols(numCols: Int, strings: Seq[String]): (Int, List[Int]) = {
      val colWidths = getColWidths(numCols, strings)
      if (numCols == 1 || colWidths.sum + ((numCols - 1) * separator.length) + prefix.length <= maxWidth)
        (numCols, colWidths)
      else
        findNumCols(numCols - 1, strings)
    }

    if (values.isEmpty) {
      if (showNone)
        Seq("none")
      else
        Seq.empty
    }
    else {
      val b = new ListBuffer[String]
      val strings = values.map(_.toString)
      val (numCols, colWidths) = findNumCols(8 min strings.length, strings)
      val rows = strings.grouped(numCols)
      val margin = " " * prefix.length
      var first = true

      for (row <- rows) {
        val cols = for ((colText, width) <- row.zip(colWidths))
          yield padLeft(colText, width)
        if (first)
          b += prefix + cols.mkString(separator)
        else
          b += margin + cols.mkString(separator)
        first = false
      }
      b.toList
    }
  }

  //  Return true  if the user enters skip.
  //  This allows the caller to discontinue calling pause.
  def pause(optPrompt: Option[String] = None): Boolean = {
    val prompt = optPrompt.getOrElse("Press Enter to continue")
    val (color, reset) = if (game.showColor)
      (Console.GREEN, Console.RESET)
    else
      ("", "")

    readLine(s"\n${color}>>>>> [$prompt] ${reset}") == "skip"
  }

  def displayLine(text: String = "", color: Option[Color] = None): Unit = {
    color match {
      case Some(color) if game == null || game.showColor =>
        println(s"${color.sequence}${text}${Console.RESET}")
      case _ =>
        println(text)
    }
  }

  // Print the line to the console and save it in the game's history.
  def log(line: String = "", color: Option[Color] = None, echo: Boolean = true): Unit = {
      if (echo)
        displayLine(line, color)
      game = game.copy(log = game.log :+ LogEntry(line, color))
  }

  def debug(line: String, color: Option[Color] = None): Unit =
    log(s"DEBUG: $line", color.orElse(Color.Debug))

  def logAdjustment(name: String): Unit = {
    log(s"\n$name adjusted.", Color.Info)
  }

  def inspect[T](name: String, value: T): T = {
    debug(s"$name == ${value.toString}")
    value
  }

  def logAdjustment(name: String, oldValue: Any, newValue: Any): Unit = {
    def normalize(value: Any) = value match {
      case None                       => "none"
      case Some(x)                    => x.toString.trim
      case true                       => "yes"
      case false                      => "no"
      case s: Seq[_] if s.isEmpty     => "none"
      case s: Seq[_]                  => s.map(_.toString).mkString(", ")
      case x if x.toString.trim == "" => "none"
      case x                          => x.toString.trim
    }
    log(s"\n$name adjusted from [${normalize(oldValue)}] to [${normalize(newValue)}]", Color.Info)
  }

  def logAdjustment(countryName: String, attributeName: String, oldValue: Any, newValue: Any): Unit =
    logAdjustment(s"$countryName: $attributeName", oldValue, newValue)

  def logCardPlay(player: Role, card: Card, opsOnly: Boolean = false, allowOpponentTrigger: Boolean = true): Unit = {
    val opponent = player.opponent
    val fakeNews = if (lapsingEventInPlay(FakeNews))
     """, but will be cancelled by "Fake News""""
    else
      ""

    log()
    log(separator(char = '='), Color.Other)
    log(s"$player plays $card", Color.Other)

    if (card.autoTrigger)
      log(s"The ${card.association} event will automatically trigger", Color.Info)
    else if ((card.association == player || card.association == Unassociated) && !opsOnly) {
      if (card.eventIsPlayable(player))
        log(s"The ${card.association} event is playable$fakeNews", Color.Info)
      else
        log(s"The ${card.association} event is NOT playable", Color.Info)
    }
    else if (card.association == opponent && allowOpponentTrigger) {
      if (card.eventWillTrigger(opponent))
        log(s"The $opponent event conditions are currently satisfied$fakeNews.", Color.Info)
      else
        log(s"The $opponent event conditions are NOT currently satisfied.", Color.Info)
    }
  }

  def separator(length: Int = 72, char: Char = '-'): String = char.toString * length

  def markersString(markers: List[String]): String = if (markers.isEmpty)
    "none"
  else
    markers mkString ", "

  // Return a sorted list of the names of the given countries
  def countryNames(candidates: List[Country]) = candidates
    .map(_.name)
    .sorted
    .distinct

  // Get ordinal number.
  def ordinal(i: Int): String = i match {
    case x if x != 11 && x % 10 == 1 => s"${x}st"
    case x if x != 12 && x % 10 == 2 => s"${x}nd"
    case x if x != 13 && x % 10 == 3 => s"${x}rd"
    case x => s"${x}th"
  }

  def longestString(strings: Seq[String]): Int =
    if (strings.isEmpty)
      0
    else
      strings.map(_.length).max

  def padLeft(s: String, width: Int) = s + (" " * (width - s.length))

  // Sorts a list column wise.  Returns a list of rows where
  // each row is a string with the items of that row lined up
  // with a minimum of two spaces separating the columns.
  def columnFormat(list: List[String], numCols: Int): Seq[String] = {
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

    rows.toIndexedSeq.map { entries =>
      val paddedCols = entries.toList.zipWithIndex.map {
        case (entry, col) => padLeft(entry, colWidths(col))
      }
      paddedCols.mkString("  ")
    }
  }


  def logWorldPosture(): Unit = {
    log(s"World Posture is ${game.worldPostureDisplay}  (GWOT penalty ${game.gwotPenalty})", Color.Info)
  }


  def printSummary(summary: Summary): Unit = {
    println()
    for (SummaryEntry(text, color) <- summary.entries)
      displayLine(text, color)
  }

  def logSummary(summary: Summary): Unit = {
    log()
    for (SummaryEntry(text, color) <- summary.entries)
      log(text, color)
  }

  def printHistorySegment(saveNumber: Int, lastSaveNumber: Int, pauseAfter: Boolean, optWriter: Option[java.io.Writer] = None): Unit = {
    var pauseNext = pauseAfter
    if (saveNumber <= lastSaveNumber) {
      val prompt   = if (saveNumber == lastSaveNumber)
        "Press Enter to continue"
      else
        "Press Enter to continue (or 'skip' to skip these prompts)"
      val header   = s">>> History of save point $saveNumber <<<\n"
      val entries = loadSavedLog(game.saveName, saveNumber)

      optWriter match {
        case None =>
          displayLine(separator(char = '='), Some(Color.Green))
          displayLine(header, Some(Color.Green))
          for (LogEntry(line, color) <- entries)
            displayLine(line, color)
            if (pauseAfter)
              pauseNext = !pause(Some(prompt)) // Keep pausing unless user enters skip

        case Some(stream) =>
          stream.write(separator() + lineSeparator)
          stream.write(header + lineSeparator)
          for (LogEntry(line, _) <- entries)
            stream.write(line + lineSeparator)
      }
      printHistorySegment(saveNumber + 1, lastSaveNumber, pauseNext, optWriter)
    }
  }

  // Display some or all of the game log.
  // usage:
  // history            - Shows the log starting from the most recent save point (Same as history -1)
  // history -n         - Shows the log starting from the nth most recent save point
  // history n          - Shows the log starting from the nth save point
  // history n num      - Shows num log entries starting from the nth save point
  // history -n num     - Shows num log entries starting from the nth most recent save point
  // history all        - Shows the entire log (Same as history 0)
  // You may add >file to the end of any history command to write the history to disk.""".stripMargin
  def historyCommand(param: Option[String]): Unit = {

    param.map(_.trim) match {
      case Some(param) if param != "" =>
        historyCommandLine(param)
      case _ =>
        historyMenu()
    }
  }

  // cmdline:  one or more arguments separated by spaces
  def historyCommandLine(cmdline: String): Unit = {
    case class HistoryError(msg: String) extends Exception
    try {
      def redirect(tokens: List[String]): Option[Pathname] = {
        tokens match {
          case Nil => None
          case x::_  if !(x startsWith ">") => None
          case ">":: Nil => throw HistoryError("No filename specified after '>'")
          case ">"::file::_ => Some(Pathname(file))
          case file::_ => Some(Pathname(file.drop(1)))
        }
      }

      val lastAction = mostRecentSaveNumber(game.saveName).get
      val maxIndex = game.history.size - 1
      val NUM      = """(-?\d+)""".r
      val HELP     = """(?:\?|(?:--)?(?i:help)|-h)""".r
      val WORD     = """([a-z]+)""".r
      val REDIR    = """>.*""".r
      val tokens   = cmdline
        .split("\\s+")
        .toList
        .map(_.toLowerCase)
        .dropWhile(_ == "")

      def indexVal(str: String): Int = str.toInt match {
        case x if x < 0 => (game.history.size + x) max 0
        case x          => x min maxIndex
      }

      val (startIndex, count, redirect_path) = tokens match {
        case HELP()::_ =>
          (-1, -1, None)
        case NUM(x)::NUM(y)::xs =>
          (indexVal(x), y.toInt, redirect(xs))
        case NUM(x)::xs =>
          (indexVal(x), 0, redirect(xs))
        case WORD(w)::xs if "last".startsWith(w) =>
          (lastAction, lastAction, redirect(xs))
        case WORD(w)::xs if "turn".startsWith(w) =>
          (findFirstSavePointOfTurn(game.turn).get, lastAction, redirect(xs))
        case WORD(w)::xs if "prev".startsWith(w) && game.turn > 1 =>
          (findFirstSavePointOfTurn(game.turn - 1).get, findFirstSavePointOfTurn(game.turn).get, redirect(xs))
        case WORD(w)::xs if "all".startsWith(w) =>
          (0, 0, redirect(xs))
        case Nil =>
          // Should not get here, but need to avoid compiler match not exhaustive warning
          (maxIndex, 0, None)
        case p::_ =>
          throw HistoryError(s"Invalid input: $cmdline\nType h ? for help.")
      }
      val endIndex = count match {
        case -1         => -1
        case c if c < 1 => maxIndex
        case c          => (startIndex + (c - 1)) min maxIndex
      }

      if (count == -1) {
        val help = """|
          |History command help
          |-----------------------------------------------------------------------------
          |If you enter the 'history' command with no arguments you will see a menu.
          |
          |You can bypass the menu by following the command with arguments.
          |h last   -- Show the history of the last action
          |h turn   -- Show the history of the current turn
          |h prev   -- Show the history of the previous turn
          |h all    -- Show the entire history (same as h 0)
          |h help   -- Show this help message
          |
          |To show the history of a specific save point or range of save points you can
          |follow the h command with a starting point and an optional number of entries.
          |Save point numbers start a zero.
          |h -1     -- Show history of the most recent save point
          |h -n     -- Show all history starting with the nth most recent save point
          |h -n num -- Show history of num save points starting with the nth most recent
          |h n      -- Show all history starting with the nth save point
          |h n num  -- Show history of num save points starting with the nth save point
          |
          |Finally, any of the above commands can be followed by a redirect argument
          |to save the requested history in a file instead of printing it to the screen.
          |h all >/tmp/game_history
          |-----------------------------------------------------------------------------
          """.stripMargin
        displayLine(help)
      }
      else
        redirect_path match {
          case None =>
            printHistorySegment(startIndex, endIndex, true, None)

          case Some(path) =>
            if (path.isDirectory)
              throw new HistoryError(s"Cannot redirect to a directory ($path)!")
            path.writer { stream =>
              printHistorySegment(startIndex, endIndex, false, Some(stream))
            }
            println(s"\nHistory was written to file: $path")
        }
    }
    catch {
      case e: IOException => println(s"IOException: ${e.getMessage}")
      case HistoryError(msg) => println(msg)
    }
  }

    // Turn 1 starts a save point 1
  // For the later turns we must find the save point containing the
  // log entry for the end of the previous turn.
  def findFirstSavePointOfTurn(turnNumber: Int): Option[Int] = {
    import scala.util.matching.Regex
    val lastAction = mostRecentSaveNumber(game.saveName).get
    val endOfTurnRE = new Regex(s"^End of turn ${turnNumber - 1}$$")

    def findGameSegment(saveNumber: Int): Option[Int] = {
      if (saveNumber > lastAction)
        None
      else {
        val found = loadSavedLog(game.saveName, saveNumber)
          .exists(entry => endOfTurnRE.matches(entry.text))

        if (found)
          Some(saveNumber + 1)
        else
          findGameSegment(saveNumber + 1)
      }
    }

    if (turnNumber == 1)
      return Some(1)
    else
      findGameSegment(1)
  }


  def historyMenu(): Unit = {
    sealed trait MenuChoice
    case object Last extends MenuChoice
    case object Current extends MenuChoice
    case object Prev extends MenuChoice
    case object Turn extends MenuChoice
    case object All extends MenuChoice
    case object Done extends MenuChoice

    val lastAction = mostRecentSaveNumber(game.saveName).get
    val choices = List(
      choice(true, Last, "Last action"),
      choice(true, Current, "Current turn actions"),
      choice(game.turn > 1, Prev, "Previous turn actions"),
      choice(game.turn > 1, Turn, "Choosen turn actions"),
      choice(true, All, "Entire history"),
      choice(true, Done, "Finished"),
    ).flatten

    askMenu("Show history of:", choices).head match {
      case Last =>
        printHistorySegment(lastAction, lastAction, false)

      case Current =>
        findFirstSavePointOfTurn(game.turn) match {
          case Some(start) if start == game.history.size =>
            displayLine("\nNo actions yet in the current turn.", Color.Event)
          case Some(start) =>
            printHistorySegment(start, lastAction, true)
          case None =>
            displayLine("\nCould not determine save point.", Color.Event)
        }

      case Prev =>
        (findFirstSavePointOfTurn(game.turn - 1), findFirstSavePointOfTurn(game.turn)) match {
          case (Some(start), Some(next)) =>
            printHistorySegment(start, next - 1, true)
          case _ =>
            displayLine("\nCould not determine save point.", Color.Event)
        }

      case Turn =>
        val turn = askInt("Show history of which turn", 1, game.turn)
        findFirstSavePointOfTurn(turn) match {
          case Some(start) if start == game.history.size =>
            displayLine("\nNo actions yet in the current turn.", Color.Event)
          case Some(start) if turn == game.turn =>
            printHistorySegment(start, lastAction, true)
          case Some(start) =>
            findFirstSavePointOfTurn(turn + 1) match {
              case Some(next) =>
                printHistorySegment(start, next - 1, true)
              case None =>
                displayLine("\nCould not determine save point.", Color.Event)
            }
          case None =>
            displayLine("\nCould not determine save point.", Color.Event)
        }

      case All =>
        printHistorySegment(0, lastAction, true)
      case Done =>
    }
  }


  // We assume that the current working directory
  // set as the installed directory and thus the game directory
  // is in ./games.  The script used to invoke the program will
  // ensure that is the case.
  val gamesDir = Pathname("./games")


  // Save a brief description of the game.
  // The descriptions are used when showing current games
  def saveGameDescription(name: String, desc: String): Unit = {
    val path = gamesDir/name/"description"
    path.writeFile(desc)
  }

  def loadGameDescription(name: String): String = {
    val path = gamesDir/name/"description"
    if (path.exists)
      path.readFile().trim
    else
      ""
  }

  def getSaveName(saveNumber: Int) = f"save-$saveNumber%03d"

  def getLogName(saveNumber: Int)  = f"log-$saveNumber%03d"

  def getSaveFileNumber(filename: Pathname): Option[Int] = {
    val SAVE_FILE = """(?:save|log)-(\d+)""".r
    filename.basename.toString match {
      case SAVE_FILE(n) => Some(n.toInt)
      case _            => None
    }
  }

  def saveGameState(desc: Option[String] = None, endOfTurn: Boolean = false): Unit = {
    val saveNumber  = game.history.size
    val save_path   = gamesDir/game.saveName/getSaveName(saveNumber)
    val log_path    = gamesDir/game.saveName/getLogName(saveNumber)
    val segmentDesc = desc.orElse(game.turnActions.headOption.map(_.toString))getOrElse("")
    val cardsThisPhase = numCardsPlayedInCurrentPhase()

    val scName = if (game.campaign)
      s"${game.scenarioName} campaign"
    else
      game.scenarioName
    val gameDesc    = Seq(
      s"scenario: $scName",
      s"playing ${game.humanRole}",
      if (endOfTurn) "" else s"turn ${game.turn}",
      segmentDesc,
    ).filterNot(_.isEmpty).mkString(", ")
    val segment = GameSegment(saveNumber, endOfTurn, Seq(segmentDesc).filterNot(_.isEmpty))

    // Make sure that the game directory exists
    save_path.dirname.mkpath()
    SavedGame.saveLog(log_path, game.log)
    game = game.copy(
      description = gameDesc,
      history = game.history :+ segment,
      log = Vector.empty,
    )
    SavedGame.save(save_path, game)
    saveGameDescription(game.saveName, gameDesc)
  }

  def loadGameState(name: String, saveNumber: Int): GameState = {
    val save_path = gamesDir/name/getSaveName(saveNumber)
    SavedGame.load(save_path).copy(
      saveName = name,         // Set the game name, it could change if the directory was renamed
      log = Vector.empty)  // Start with an empty log for the current save point
  }

  // Given a directory for a saved game finds the most recent save file.
  def mostRecentSaveNumber(name: String): Option[Int] = {
    import Pathname.glob
    val dir = gamesDir/name
    if (dir.isDirectory) {
      val entries = glob(dir/"save-*") flatMap { child =>
        getSaveFileNumber(child)
      }
      entries.sortBy(num => -num).headOption
    }
    else
      None
  }

  def loadMostRecent(name: String): GameState = {
    val saveNumber = mostRecentSaveNumber(name) getOrElse {
      throw new IllegalStateException(s"No saved file found for game '$name'")
    }
    loadGameState(name, saveNumber)
  }
    // Return the list of saved games
  def savedGames(): List[String] =
    gamesDir
      .children(withDirectory = false)
      .toList
      .map(_.toString)
      .filter(name => mostRecentSaveNumber(name).nonEmpty)

  def loadSavedLog(name: String, saveNumber: Int): Vector[LogEntry] = {

    val log_path = gamesDir/name/getLogName(saveNumber)
    if (log_path.exists) {
      SavedGame.loadLog(log_path)
    }
    else
      Vector.empty
  }

  sealed trait GameFile
  case class TurnFile(num: Int) extends GameFile
  case class PlayFile(num: Int) extends GameFile



  val VALID_NAME = """([-A-Za-z0-9_ ]+)""".r

  // Ask the user for a name for their new game.
  def askGameName(prompt: String): Option[String] = {
    def getName(): Option[String] = {
      readLine(prompt) match {
        case null => None
        case x if x.trim.length == 0 => None
        case VALID_NAME(name) if name.length > 55 =>
          println("Game names must 55 characters or less")
          getName()

        case VALID_NAME(name) =>
          if ((gamesDir/name).exists) {
            displayLine(s"\nA game called '$name' already exists.", Color.Info)
            if (askYorN(s"Do you want to overwrite the existing game (y/n)? ")) {
              (gamesDir/name).rmtree()
              Some(name)
            }
            else
              getName()
          }
          else
            Some(name)
        case name =>
          println("Game names must consist of one or more letters, numbers, spaces, dashes or underscores")
          getName()
      }
    }
    getName()
  }

  // Returns (savePoint, pageNum)
  // `savePoint` the saveNumber of turn to roll back to.
  // `pageNum` is the current page that was show when the user made a
  // selection.  It is used so that this funciton call be called again
  // if necessary showing the same page for consistency.
  def askSavePoint(prompt: String, startPage: Int): Option[(Int, Int)] = {
    try {
      val pages = game.history.drop(1).reverse.sliding(25, 25).toList
      val firstPage = 0
      val lastPage  = pages.size -1
      val PAGE_UP   = -1
      val PAGE_DOWN = -2
      val CANCEL    = -3

      def showPage(pageNum: Int): Option[(Int, Int)] = {
        val width = longestString(pages(pageNum).map(_.saveNumber.toString))
        val fmt = s"%${width}d"
        val saveChoices: List[(Int, (String, Seq[String]))] = pages(pageNum)
          .toList
          .filterNot(_.endOfTurn)   // Don't allow rollback to end of turn
          .map {
            case GameSegment(saveNumber, endOfTurn, summary) =>
              saveNumber -> (s"[Save point $fmt]".format(saveNumber), summary)
          }
        val otherChoices: List[(Int, (String, Seq[String]))] = List(
          choice(pageNum > firstPage, PAGE_UP,   "Page up, show newer save points ", Seq.empty),
          choice(pageNum < lastPage,  PAGE_DOWN, "Page down, show older save points ", Seq.empty),
          choice(true,                CANCEL,    "Cancel", Seq.empty)
        ).flatten

        val current = game.history.last
        // displayLine("\nRollback to the beginning of a previous save point.", Color.Info)
        displayLine("The save points are displayed with the most recent first.", Color.Info)

        askMenuWithWrap(prompt, saveChoices:::otherChoices) match {
          case CANCEL      => None
          case PAGE_UP     => showPage(pageNum - 1)
          case PAGE_DOWN   => showPage(pageNum + 1)
          case saveNumber => Some(saveNumber, pageNum)
        }
      }

      if (game.history.size > 1)
        showPage(startPage)
      else {
        println("\nThere are no previous save points")
        None
      }
    }
    catch {
      case AbortAction => None
    }
  }

  // Allows the user to roll back to the beginning of any turn.
  def rollback(): Unit = {
    val prompt = "Roll back to replay starting at which save point:"

    def promptUser(startPage: Int): Option[Int] = {
      displayLine("\nRollback to the beginning of a selected save point.", Color.Info)
      askSavePoint(prompt, 0) match {
        case Some((saveNumber, pageNum)) =>
          if (askYorN(s"Are you sure you want to rollback to this save point? (y/n) "))
            Some(saveNumber)
          else
            promptUser(pageNum)
        case None =>
          None
      }
    }

    promptUser(0)
      .foreach { saveNumber =>
        val target = saveNumber - 1  // We actually load the previous savePoint
        val oldGameState = game
        val segment = game.history.find(_.saveNumber == saveNumber).get
        displayLine(s"\nRolling back to start of [Save point ${segment.saveNumber}] ${segment.summary.head}", Color.Info)
        game = loadGameState(game.saveName, target)
        saveGameDescription(game.saveName, game.description)  // Update the description file
        // Remove all safe files that succeed this one.
        // We are exploring anew
        removeSaveFiles(game.saveName, target + 1)
        removeLogFiles(game.saveName, target + 1)
        displayGameStateDifferences(oldGameState, game)
      }
  }

  // Allow the user to select a save point.
  // Then they can use the show command to explore the
  // game state at that save point.
  def inspect(): Unit = {
    // Pronpt for what to show while inspecting a seleted save point
    def inspectPrompt(segment: GameSegment): Unit = {
      displayLine(s"\nInspecting prior to [Save point ${segment.saveNumber}] ${segment.summary.head}", Color.Info)
      displayLine(separator(), Color.Info)
      readLine("Inspect (blank to cancel): ") match {
        case null | "" =>
        case input =>
          showCommand(Some(input))
          inspectPrompt(segment)
      }
    }

    def nextInspect(startPage: Int): Unit = {
      val prompt = "Inspect which save point:"
      displayLine("\nInspect the game at the start of a selected save point.", Color.Info)
      askSavePoint(prompt, startPage) match {
        case Some((saveNumber, pageNum)) =>
          val segment = game.history.find(_.saveNumber == saveNumber).get
          val target = saveNumber  - 1
          val oldGameState = game
          game = loadGameState(game.saveName, target)
          inspectPrompt(segment)
          game = oldGameState
          nextInspect(pageNum)
        case None =>
      }
    }

    nextInspect(0)
  }

  // Remove turn files starting with the given save file number and all
  // those that follow that number.
  def removeSaveFiles(name: String, num: Int): Unit = {
    import Pathname.glob
    for {
      path    <- glob(gamesDir/name/"save-*")
      saveNum <- getSaveFileNumber(path)
              if saveNum >= num
    } path.delete()
  }

  def removeLogFiles(name: String, num: Int): Unit = {
    import Pathname.glob
    for {
      path    <- glob(gamesDir/name/"log-*")
      saveNum <- getSaveFileNumber(path)
              if saveNum >= num
    } path.delete()
  }



  // Display a list of what needs to be done to get the game board into
  // the proper state when going from one state to another.
  def displayGameStateDifferences(from: GameState, to: GameState): Unit = {
    var headerShown = false
    val b = new ListBuffer[String]
    def showHeader(): Unit = if (!headerShown) {
      headerShown = true
      println()
      println("The following changes should be made to the game board")
      println(separator())
    }

    def show(oldValue: Any, newValue: Any, msg: String) =
      if (oldValue != newValue) b += msg

    show(from.botDifficulties, to.botDifficulties,
        "Set the bot difficulties to %s".format(to.botDifficulties.map(_.name).mkString(", ")))
    show(from.goodResources, to.goodResources, s"Set Good Resources to ${to.goodResources}")
    show(from.islamistResources, to.islamistResources, s"Set Islamic Resources to ${to.islamistResources}")
    show(from.numGoodOrFair, to.numGoodOrFair, s"Set Good/Fair Countries to ${to.numGoodOrFair}")
    show(from.numPoorOrIslamic, to.numPoorOrIslamic, s"Set Poor/Islamic Countries to ${to.numPoorOrIslamic}")
    show(from.prestige, to.prestige, s"Set prestige to ${to.prestige}")
    show(from.usPosture, to.usPosture, s"Set US posture to ${to.usPosture}")
    show(from.gwot, to.gwot, s"Set World posture to ${to.worldPostureDisplay}")
    show(from.reserves.us, to.reserves.us, s"Set US reserves to ${to.reserves.us}")
    show(from.reserves.jihadist, to.reserves.jihadist, s"Set Jihadist reserves to ${to.reserves.jihadist}")
    show(from.troopsAvailable, to.troopsAvailable,
          s"Set troops on the track to ${to.troopsAvailable} (${to.troopCommitment})")
    show(from.offMapTroops, to.offMapTroops, s"Set troops in off-map box to ${to.offMapTroops}")
    show(from.militiaAvailable, to.militiaAvailable, s"Set available militia to ${to.militiaAvailable}")
    show(from.funding, to.funding, s"Set jihadist funding to ${to.funding} (${to.fundingLevel})")
    show(from.cellsOnTrack, to.cellsOnTrack, s"Set cells on the funding track to ${to.cellsOnTrack}")
    show(from.extraCellsAvailable, to.extraCellsAvailable, s"Set cells in \"extra cells\" area to ${to.extraCellsAvailable}")
    show(from.resolvedPlots.sorted, to.resolvedPlots.sorted,
          s"Set resolvedPlots plots to ${plotsDisplay(to.resolvedPlots)}")
    show(from.availablePlots.sorted, to.availablePlots.sorted,
          s"Set available plots to ${plotsDisplay(to.availablePlots, to.humanRole == Jihadist)}")
    show(from.markers.sorted,  to.markers.sorted,
            s"Set global event markers to: ${markersString(to.markers)}" )
    (from.firstPlotEntry, to.firstPlotEntry) match {
      case (x, y) if x == y =>  // No change
      case (_, Some(event)) =>
        b += s"Place $event int the first plot box"
      case (_, None) =>
        b += "There should be no first plot card or marker"
    }
    if (from.eventsLapsing.sorted != to.eventsLapsing.sorted) {
      b += "The following events are lapsing:"
      if (to.eventsLapsing.isEmpty)
        b += "  None"
      to.eventsLapsing
        .sorted
        .foreach(event => b += s"  $event")
    }
    if (from.cardsRemoved.sorted != to.cardsRemoved.sorted) {
      b += "The following cards have been removed from play:"
      if (to.cardsRemoved.isEmpty)
        b += "  None"
      to.cardsRemoved
        .sorted
        .foreach(c => b += s"  ${cardNumAndName(c)}")
    }

    if (b.nonEmpty) {
      showHeader()
      b.foreach(println)
    }

    def showChange(value: Any, desc: String) = value match {
      case true =>  b += s"Add $desc"
      case false => b += s"Remove $desc"
      case x     => b += s"Set $desc to $x"
    }
    def showC[T](f: Country, optTo: Option[Country], value: (Country) => T, desc: String) = {
      optTo foreach { t =>
        val newValue = value(t)
        if (value(f) != newValue) showChange(newValue, desc)
      }
    }
    def showM[T](f: MuslimCountry, optTo: Option[MuslimCountry], value: (MuslimCountry) => T, desc: String) = {
      optTo foreach { t =>
        val newValue = value(t)
        if (value(f) != newValue) showChange(newValue, desc)
      }
    }
    def showN[T](f: NonMuslimCountry, optTo: Option[NonMuslimCountry], value: (NonMuslimCountry) => T, desc: String) = {
      optTo foreach { t =>
        val newValue = value(t)
        if (value(f) != newValue) showChange(newValue, desc)
      }
    }

    for (fromM <- from.muslims) {
      b.clear()
      // Iran and Nigeria can be flipped to non-Muslim side.
      val toM = if (to isMuslim fromM.name) Some(to getMuslim fromM.name) else None
      val toN = if (to isNonMuslim fromM.name) Some(to getNonMuslim fromM.name) else None

      if (toN.nonEmpty)
        b += s"Flip the country mat to the non-Muslim side"
      showC(fromM, toM orElse toN, m => govToString(m.governance), "governance")
      showM(fromM, toM, _.alignment, "alignment")
      showC(fromM, toM orElse toN, _.sleeperCells, "sleeper cells")
      showC(fromM, toM orElse toN, _.activeCells, "active cells")
      showC(fromM, toM orElse toN, _.cadres, "cadre markers")
      showC(fromM, toM orElse toN, _.troops, "troops")
      showM(fromM, toM, _.militia, "militia")

      showM(fromM, toM, _.aidMarkers, "aid markers")
      showM(fromM, toM, _.awakening, "awakening markers")
      showM(fromM, toM, _.reaction, "reaction markers")
      showM(fromM, toM, _.besiegedRegime, "besieged regime marker")
      showM(fromM, toM, _.regimeChange, "Regime Change")
      showM(fromM, toM, _.caliphateCapital, "Caliphate capital marker")
      toM foreach { t =>
        val fromVal = from.isCaliphateMember(fromM.name)
        val toVal   = to.isCaliphateMember(t.name)
        if (fromVal != toVal)
          b += s"${if (toVal) "Add" else "Remove"} Caliphate member marker"
      }
      showC(fromM, toM orElse toN, _.wmdCache, "WMD cache")
      (toM orElse toN) foreach { t =>
        val newVal = t.plots.sorted
        if (fromM.plots.sorted != newVal)
          b += s"Plots: ${mapPlotsDisplay(newVal, to.humanRole == Jihadist)}"
      }
      (toM orElse toN) foreach { t =>
        val newVal = t.markers.sorted
        if (fromM.markers.sorted != newVal)
          b += s"Event markers: ${markersString(newVal)}"
      }

      if (b.nonEmpty) {
        showHeader()
        b.prepend(s"\n${fromM.name} changes:\n${separator()}")
        b foreach println
      }
    }

    for (fromN <- from.nonMuslims) {
      b.clear()
      // Iran and Nigeria can be flipped to Muslim side.
      val toM = if (to isMuslim fromN.name) Some(to getMuslim fromN.name) else None
      val toN = if (to isNonMuslim fromN.name) Some(to getNonMuslim fromN.name) else None

      showN(fromN, toN, _.posture, "posture")
      showC(fromN, toN orElse toM, _.sleeperCells, "sleeper cells")
      showC(fromN, toN orElse toM, _.activeCells, "active cells")
      showC(fromN, toN orElse toM, _.cadres, "cadre marker")
      showC(fromN, toN orElse toM, _.troops, "troops")
      showC(fromN, toN orElse toM, _.wmdCache, "WMD cache")
      (toN orElse toM) foreach { t =>
        val newVal = t.plots.sorted
        if (fromN.plots.sorted != newVal) {
          val visible = to.humanRole == Jihadist ||
                      (t.name == UnitedStates && t.hasMarker(NEST))
          b += s"Set plots to ${mapPlotsDisplay(newVal, visible)}"
        }
      }
      (toN orElse toM) foreach { t =>
        val newVal = t.markers.sorted
        if (fromN.markers.sorted != newVal)
          b += s"Set markers to ${markersString(newVal)}"
      }
      if (b.nonEmpty) {
        showHeader()
        b.prepend(s"\n${fromN.name} changes:\n${separator()}")
        b foreach println
      }
    }
  }

  def addToReserves(role: Role, ops: Int): Unit = {
    role match {
      case US =>
        val opsAdded = ops min (2 - game.reserves.us)
        if (opsAdded > 0) {
          game = game.copy(reserves = game.reserves.copy(us = game.reserves.us + opsAdded))
          log(
            s"$US adds ${opsString(opsAdded)} to reserves.  Reserves now ${opsString(game.reserves.us)}",
            Color.MapMarker)
        }
      case Jihadist =>
        val opsAdded = ops min (2 - game.reserves.jihadist)
        if (opsAdded > 0) {
          game = game.copy(reserves = game.reserves.copy(jihadist = game.reserves.jihadist + opsAdded))
          log(
            s"$Jihadist adds ${opsString(opsAdded)} to reserves.  Reserves now ${opsString(game.reserves.jihadist)}",
            Color.MapMarker)
        }
    }
  }

  def subtractFromReserves(role: Role, ops: Int): Unit = {
    role match {
      case US if game.reserves.us > 0  && ops > 0 =>
        val newReserves = (game.reserves.us - ops) max 0
        val opsRemoved = game.reserves.us - newReserves
        game = game.copy(reserves = game.reserves.copy(us = newReserves))
        log(
          s"$US subtracts ${opsString(opsRemoved)} from reserves.  Reserves now ${opsString(newReserves)}",
          Color.MapMarker)
      case Jihadist if game.reserves.jihadist > 0 && ops > 0 =>
        val newReserves = (game.reserves.jihadist - ops) max 0
        val opsRemoved = game.reserves.jihadist - newReserves
        game = game.copy(reserves = game.reserves.copy(jihadist = newReserves))
        log(
          s"$Jihadist subtracts ${opsString(opsRemoved)} from reserves.  Reserves now ${opsString(newReserves)}",
          Color.MapMarker)
      case _ =>
    }
  }

  def clearReserves(role: Role): Unit = {
    if (role == US && game.reserves.us > 0) {
      game = game.copy(reserves = game.reserves.copy(us = 0))
      log(s"$role reserves set to zero", Color.MapMarker)
    }
    else if (role == Jihadist && game.reserves.jihadist > 0) {
      game = game.copy(reserves = game.reserves.copy(jihadist = 0))
      log(s"$role reserves set to zero", Color.MapMarker)
    }
  }

  def increaseCardsInHand(role: Role, num: Int): Unit = {
    val newCardsInHand = role match {
      case US => game.cardsInHand.copy(us = game.cardsInHand.us + num)
      case Jihadist => game.cardsInHand.copy(jihadist = game.cardsInHand.jihadist + num)
    }
    game = game.copy(cardsInHand = newCardsInHand)
  }

  def decreaseCardsInHand(role: Role, num: Int): Unit = {
    val newCardsInHand = role match {
      case US => game.cardsInHand.copy(us = game.cardsInHand.us - (num min game.cardsInHand.us))
      case Jihadist => game.cardsInHand.copy(jihadist = game.cardsInHand.jihadist - (num min game.cardsInHand.jihadist))
    }
    game = game.copy(cardsInHand = newCardsInHand)
  }

  def numCardsInHand(role: Role) = role match {
    case US => game.cardsInHand.us
    case Jihadist => game.cardsInHand.jihadist
  }

  def hasCardInHand(role: Role) = numCardsInHand(role) > 0

  def polarization(): Unit = {
    val candidates = game.muslims.filter(m => m.awakeningDelta.abs > 1)
    log()
    log("Polarization")
    log(separator())
    if (candidates.isEmpty)
      log("No countries affected", Color.Info)
    else {
      // Remember any Caliphate capital in case it is displaced.
      val caliphateCapital = candidates.find(_.caliphateCapital).map(_.name)
      val priorGameState = game
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

      // Keep track of countries that cause convergence and do the convergence at the
      // end so it does not affect the polarization in progress.
      var convergers = List.empty[Converger]
      for (name <- names; m = game.getMuslim(name)) {
        m.awakeningDelta match {
          case 0  =>  // Nothing happens
          case 2  => addAwakeningMarker(name)
          case -2 => addReactionMarker(name)
          case x if x > 2 =>
            log()
            if (m.isAlly) {
              improveGovernance(name, 1, canShiftToGood = true, endOfTurn = true, convergenceOK = false)
              if (game.getMuslim(name).isGood)
                convergers = Converger(name, awakening = true) :: convergers
            }
            else
              shiftAlignmentLeft(name)


          case _ => // x < -2
            log()
            if (m.isAdversary) {
              worsenGovernance(name, levels = 1, canShiftToIR = true, endOfTurn = true, convergenceOK = false)
              if (game.getMuslim(name).isIslamistRule)
                convergers = Converger(name, awakening = false) :: convergers
            }
            else
              shiftAlignmentRight(name)
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
        if (game.getMuslim(capitalName).caliphateCapital == false) {
          displaceCaliphateCapital(capitalName)
          logExtraCellCapacityChange(priorGameState)
        }
      }
    }
  }


  // Process the US civil war losses.
  // If the US is human then prompt for pieces to remove.
  // Return the number of unresolved hits
  def usCivilWarLosses(m: MuslimCountry, hits: Int, hamaOffensive: Boolean = false): Int = {
    if (hits == 0 || m.totalTroopsAndMilitia == 0)
      hits
    else {
      val maxAbsorb     = m.troops + m.militia + m.troopsMarkers.map(_.num).sum
      val multiplier    = if (hamaOffensive) 2 else 1
      val losses        = hits * multiplier
      val unfulfilled   = (losses - maxAbsorb) max 0
      val hitsRemaining =  (unfulfilled + multiplier - 1) / multiplier

      log()
      log(s"$US must absorb ${amountOf(losses, "loss", Some("losses"))}")
      if (hamaOffensive)
        log(s"$multiplier losses per hit [Hama Offensive]", Color.Info)

      val (markersLost, troopsLost, militiaLost) = if (game.humanRole == US) {
        // If there are any markers representing troops or
        // if there is a mix of multiple type of cubes that can take losses (troops and militia),
        // and the hits are not sufficient to eliminate all forces present,
        // then allow the user to choose which units absorb the losses.
        val mixedCubes = m.troops > 0 && m.militia > 0
        if (losses >= m.totalTroopsAndMilitia)
           (m.troopsMarkers.map(_.name), m.troops, m.militia)
        else if (!mixedCubes && m.troopsMarkers.isEmpty) {
          if (m.troops > 0)
            (Nil, m.troops min losses, 0)
          else
            (Nil, 0, m.militia min losses)
        }
        else {
          var (troopsLost, militiaLost, markersLost) = (0, 0, List.empty[String])

          def nextHit(lossesRemaining: Int, markers: List[TroopsMarker], troops: Int, militia: Int): Unit = {
            if (lossesRemaining > 0 && (markers.nonEmpty || troops > 0 || militia > 0)) {
              val choices = List(
                choice(troops > 0,  TroopCube,  "Troop cube"),
                choice(militia > 0, MilitiaCube,"Militia cube")
              ).flatten ++
                (markers.sorted.map(m => TroopMarker(m.name) -> s"${m.name}  (absorbs ${amountOf(m.num, "loss", Some("losses"))})"))
              println(s"$US must take ${amountOf(lossesRemaining, "more loss", Some("more losses"))}")
              askMenu("Which unit will take the next loss:", choices).head match {
                case TroopCube   =>
                  troopsLost  += 1; nextHit(lossesRemaining - 1, markers, troops - 1, militia)
                case MilitiaCube =>
                  militiaLost += 1; nextHit(lossesRemaining - 1, markers, troops, militia - 1)
                case TroopMarker(name) =>
                  val marker = markers.find(_.name == name).get
                  val lossesAbsorbed = marker.num
                  markersLost = name :: markersLost
                  nextHit(lossesRemaining - lossesAbsorbed, markers.filterNot(_.name == name), troops, militia)
              }
            }
          }

          nextHit(losses, m.troopsMarkers, m.troops, m.militia)
          (markersLost, troopsLost, militiaLost)
        }
      }
      else {
        // Calculate the losses for the bot.
        // [Rule 13.3.7] The bot removes troops cubes first.
        // Then militia, then troops markers
        var lossesRemaining = losses
        val troopsLost = lossesRemaining min m.troops
        lossesRemaining -= troopsLost
        val militiaLost = lossesRemaining min m.militia
        lossesRemaining -= militiaLost

        val markersLost = for (TroopsMarker(name, num, _, _) <- m.troopsMarkers.sorted; if lossesRemaining > 0)
          yield {
            lossesRemaining = lossesRemaining - num
            name
          }

        (markersLost, troopsLost, militiaLost)
      }

      removeEventMarkersFromCountry(m.name, markersLost:_*)
      moveTroops(m.name, "track", troopsLost)
      removeMilitiaFromCountry(m.name, militiaLost)
      hitsRemaining
    }
  }

  // Process the Jihadist civil war losses.
  // If the Jihadist is human then prompt for pieces to remove.
  // Return the number of unresolved hits
  def jihadistCivilWarLosses(m: MuslimCountry, hits: Int): Int = {
    if (hits == 0 || m.totalCells == 0)
      hits
    else {
      // Remove two cells per hit if any troops present or if "Advisors" marker present.
      val troopsOrAdvisors = m.totalTroops > 0 || m.hasMarker(Advisors)
      val expandedROE = lapsingEventInPlay(ExpandedROE)
      val multiplier    = (if (troopsOrAdvisors) 2 else 1) +
                          (if (expandedROE) 1 else 0)
      val losses        = hits * multiplier
      val unfulfilled   = (losses - m.totalCells) max 0
      val hitsRemaining =  (unfulfilled + multiplier - 1) / multiplier
      val desc = List(
        if (troopsOrAdvisors) Some("Troops/Advisors") else None,
        if (expandedROE) Some("Expanded ROE") else None,
      ).flatten.mkString(", ")
      log()
      log(s"$Jihadist must absorb ${amountOf(losses, "loss", Some("losses"))}")
      if (multiplier > 1)
        log(s"$multiplier losses per hit [$desc]", Color.Info)

      val (activesLost, sleepersLost, sadr) = if (m.totalCells <= losses)
        (m.activeCells, m.sleeperCells, m.hasSadr)
      else if (game.humanRole == Jihadist)
        askCells(m.name, losses, sleeperFocus = false)
      else
        JihadistBot.chooseCellsToRemove(m.name, losses)

      removeCellsFromCountry(m.name, activesLost, sleepersLost, sadr, addCadre = true)
      hitsRemaining
    }
  }

  //  Perform Civil War attrition in the named country
  //
  //  Note:
  //  There is a special case here for Nigeria.
  //  Whenever Nigeria is an Ally and contains no cells, it converts
  //  to a Non-Muslim country.  If this happens, it will end the civil war.
  //  We must check for this after removing cell AND after any shift to Ally
  //  due to unfulfilled hits on the Jihadist.

  def civilWarAttrition(name: String, hamaOffensive: Boolean, endOfTurn: Boolean): Unit = {
    assert(game.getMuslim(name).civilWar, s"civilWarAttrition() called on non-Civil War country: $name")

    val siegeOfMosul = lapsingEventInPlay(SiegeofMosul)
    val hamaOffensiveDisplay = if (hamaOffensive)
      Some(" (Hama Offensive)")
    else
      ""
    // Get the fresh instance of the Muslim country
    val m = game.getMuslim(name)
    // If Siege of Mosul in play then cells are halved and (troops + militia)
    // are doubled
    val usPresence = List(
      if (m.militia > 0) Some(s"${m.militia} militia") else None,
      if (m.numAdvisors > 0) Some(amountOf(m.numAdvisors, "Advisors marker")) else None,
      if (m.troops > 0) Some(amountOf(m.troops, "troop")) else None,
    ).flatten ::: m.troopsMarkers
    val jihadistPresence = List(
        if (m.cells > 0) Some(amountOf(m.cells, "cell")) else None,
        if (m.hasSadr) Some("Sadr marker") else None,
    ).flatten
    val totalCells = if (siegeOfMosul) m.totalCells / 2 else m.totalCells
    val totalTroopsAndMilitia = if (siegeOfMosul) m.totalTroopsAndMilitia * 2 else m.totalTroopsAndMilitia
    val usPresenceString = usPresence match {
      case Nil => "none"
      case _ => usPresence.mkString(", ") + (
                  if (siegeOfMosul)
                    s" [doubled] = $totalTroopsAndMilitia"
                  else
                    s" = $totalTroopsAndMilitia"
                )
    }
    val jihadistPresenceString = jihadistPresence match {
      case Nil => "none"
      case _ => jihadistPresence.mkString(", ") + (
                  if (siegeOfMosul)
                    s" [halved] = $totalCells"
                  else
                    s" = $totalCells"
                )
    }

    log()
    log(s"Attrition: $name$hamaOffensiveDisplay")
    log(separator())
    if (siegeOfMosul) {
      log(s"Siege of Mosul is in effect: cells are halved, troops/militia are doubled", Color.Info)
    }
    log(s"US presence: $usPresenceString")
    log(s"Jihadist presence: $jihadistPresenceString")


    val jihadDie = getDieRoll(s"Enter Jihadist attrition die roll for $name: ")
    val usDie = getDieRoll(s"Enter US attrition die roll for $name: ")
    val jihadHits = totalCells / 6 + (if (jihadDie <= totalCells % 6) 1 else 0)
    val usHits    = totalTroopsAndMilitia / 6 + (if (usDie <= totalTroopsAndMilitia % 6) 1 else 0)

    if (totalCells > 0 || totalTroopsAndMilitia > 0) {
      if (totalTroopsAndMilitia > 0 && totalTroopsAndMilitia % 6 != 0)
        log(s"\nUS die roll : $usDie")
      else if (totalTroopsAndMilitia > 0)
        log(s"\nUS die roll : not necessary")

      if (totalCells > 0 && totalCells % 6 != 0)
        log(s"Jihadist die roll: $jihadDie")
      else if (totalCells > 0)
        log(s"Jihadist die roll: not necessary")

      log(s"The US inflicts ${amountOf(usHits, "hit")} on the Jihadist")
      log(s"\nThe Jihadist inflicts ${amountOf(jihadHits, "hit")} on the US")

      if (jihadHits + usHits > 0) {
        val unfulfilledJihadHits = usCivilWarLosses(m, jihadHits, hamaOffensive)
        val unfulfilledUSHits    = jihadistCivilWarLosses(m, usHits)

        //  Make sure the country (ie. Nigeria) is still a Non Muslim country
        if (game.isMuslim(name)) {
          val delta = unfulfilledJihadHits - unfulfilledUSHits
          if (delta == 0) {
            if (unfulfilledJihadHits != 0)
              log(s"\nBoth sides have ${amountOf(unfulfilledJihadHits, "unfulfilled hit")}.  No further effects.")
          }
          else {
            log()
            if (unfulfilledJihadHits > 0)
              log(s"${amountOf(unfulfilledJihadHits, "unfulfilled Jihadist hit")} against the US")
            if (unfulfilledUSHits > 0)
              log(s"${amountOf(unfulfilledUSHits, "unfulfilled US hit")} against the Jihadist")
            if (delta > 0) {
              val (shifts, newAlign) = (delta, m.alignment) match {
                case (_, Adversary) => (0, Adversary)
                case (1, Ally)      => (1, Neutral)
                case (_, Neutral)   => (1, Adversary)
                case _              => (2, Adversary)
              }


              if (shifts > 0)
                setAlignment(m.name, newAlign)

              val steps = delta - shifts
              if (steps > 0)
                worsenGovernance(m.name, levels = steps, canShiftToIR = true, endOfTurn = endOfTurn)
            }
            else {
              // Shift toward Ally/Improve governance
              val (shifts, newAlign) = (-delta, m.alignment) match {
                case (_, Ally)      => (0, Ally)
                case (1, Adversary) => (1, Neutral)
                case (_, Neutral)   => (1, Ally)
                case _              => (2, Ally)
              }

              if (shifts > 0)
                setAlignment(m.name, newAlign)

              //  Make sure the country (ie. Nigeria) is still a Non Muslim country
              //  It may have just shifted to Ally with no cells present!
              if (game.isMuslim(name)) {
                val steps = -delta - shifts
                if (steps > 0)
                  improveGovernance(m.name, steps, canShiftToGood = true, endOfTurn = endOfTurn)
              }
            }
          }
        }
      }
    }
  }

  def endTurnCivilWarAttrition(): Unit = {
    def civilWars = game.muslims.filter(_.civilWar)
    log()
    log("Civil War Attrition")
    log(separator())
    if (civilWars.isEmpty)
      log("No countries in civil war")
    else {
      val caliphateCapital = civilWars.find(_.caliphateCapital).map(_.name)
      val priorGameState = game
      val totalAdvisors = civilWars.map(_.numAdvisors).sum
      // Add militia for any Advisors present
      if (civilWars.exists(_.numAdvisors > 0)) {
        log("\nAdvisors")
        log(separator())
        if (game.militiaAvailable == 0)
          log("There are no available militia")
        else if (game.militiaAvailable >= totalAdvisors) {
          for (m <- civilWars; num = m.numAdvisors; if num > 0)
            addMilitiaToCountry(m.name, num)
        }
        else {
          //  There are not enough available militia for all Advisors so
          //  ask where to place them.
          if (game.humanRole == US) {
            var advisorMap: Map[String, Int] = civilWars
              .filter(_.numAdvisors > 0)
              .map(m => (m.name, m.numAdvisors)).toMap
            def nextMilita(numLeft: Int): Unit = if (numLeft > 0) {
              val unfulfilled = advisorMap.values.sum
              val target      = askCountry("Place militia in which country: ", advisorMap.keys.toList.sorted)
              val numAdvisors = advisorMap(target)
              val maxNum      = numAdvisors min numLeft
              val num         = askInt(s"Place how many militia in $target", 1, maxNum)
              addMilitiaToCountry(target, num)
              if (num == numAdvisors)
                advisorMap -= target
              else
                advisorMap += (target -> (numAdvisors - num))
              nextMilita(numLeft - num)

            }
            println("\nThere are more Advisors markers than available militia")
            nextMilita(game.militiaAvailable)

          }
          else {
            def nextMilita(numLeft: Int, candidates: List[String]): Unit = if (numLeft > 0) {
              val target = USBot.deployToPriority(candidates).get
              val m = game.getMuslim(target)
              val num = m.numAdvisors min game.militiaAvailable
              addMilitiaToCountry(target, num)
              nextMilita(numLeft - num, candidates.filterNot(_ == target))
            }
            nextMilita(game.militiaAvailable, countryNames(civilWars.filter(_.numAdvisors > 0)))
          }

        }

      }

      for (name <- civilWars.map(_.name))
        civilWarAttrition(name, hamaOffensive = false, endOfTurn = true)

      // Check to see if the Caliphate Capital has been displaced because its country
      // was improved to Good governance.
      caliphateCapital foreach { capitalName =>
        if (game.getMuslim(capitalName).caliphateCapital == false) {
          displaceCaliphateCapital(capitalName)
          logExtraCellCapacityChange(priorGameState)
        }
      }
    }
  }

  def performWarOfIdeas(name: String, ops: Int, ignoreGwotPenalty: Boolean = false): Unit = {
    game.getCountry(name) match {
      case m: MuslimCountry =>
        assert(!m.isAdversary, "Cannot do War of Ideas in Adversary country")
        assert(!(m.isGood && m.isAlly), "Cannot do War of Ideas in muslim Good Ally")
        addOpsTarget(name)
        log()
        testCountry(name)
        val tested = game.getMuslim(name)
        if (ops < tested.governance)
          log(s"Not enough Ops to complete War of Ideas in $name")
        else {
          log(s"$US performs War of Ideas in $name")
          log(separator())
          val die = getDieRoll(s"Enter WoI die roll for $name: ", Some(US))
          log(s"Die roll: $die")
          val modRoll = modifyWoiRoll(die, tested, ignoreGwotPenalty)
          if (modRoll <= 4) {
            log("Failure")
            log(s"$name remains ${govToString(tested.governance)} ${tested.alignment}", Color.MapPieces)
            if (modRoll == 4 && tested.aidMarkers == 0)
              addAidMarker(name)
          }
          else if (tested.isNeutral) {
            log("Success")
            setAlignment(name, Ally)
          }
          else {
            log("Success")
            improveGovernance(name, 1, canShiftToGood = true)
          }
        }

      case n: NonMuslimCountry if n.iranSpecialCase => println("Cannot do War of Ideas in Iran")
      case n: NonMuslimCountry =>
        log()
        log(s"$US performs War of Ideas in $name")
        log(separator())
        val die = getDieRoll(s"Enter WoI die roll for $name: ", Some(US))
        val newPosture = if (die > 4) Hard else Soft
        game = game.updateCountry(n.copy(postureValue = newPosture))
        log(s"Die roll: $die")
        if (newPosture == n.posture)
          log(s"Posture of $name remains $newPosture", Color.MapPieces)
        else
          log(s"Change posture of $name from ${n.posture} to $newPosture", Color.MapPieces)
        if (newPosture == game.usPosture && game.prestige < 12) {
          log(s"Increase US prestige by 1 (New posture matches US posture)")
          increasePrestige(1)
        }
        logWorldPosture()
    }
  }

  // • Deploy at least six troops into the country.
  // • Place a green Regime Change marker on them (4.8.2).
  // • Roll its Governance on the Country Tests table.
  // • Shift its Alignment to Ally.
  // • Shift any Sleeper cells there to Active (4.7.4.1).
  // • Roll Prestige
  def performRegimeChange(source: String, dest: String, numTroops: Int): Unit = {
    log()
    moveTroops(source, dest, numTroops)
    val m = game.getMuslim(dest)
    val die = getDieRoll(s"Enter governance die roll for $dest: ", Some(US))
    log(s"Governance die roll: $die")
    val newGov = if (die < 5) Poor else Fair
    addOpsTarget(dest)
    game = game.updateCountry(m.copy(
      governance   = newGov,
      alignment    = Ally,
      regimeChange = GreenRegimeChange,
      activeCells  = m.activeCells + m.sleeperCells,
      sleeperCells = 0
    ))
    log(s"Place a green regime change marker in $dest", Color.MapPieces)
    log(s"Set governance of ${m.name} ${govToString(newGov)}", Color.MapPieces)
    log(s"Set alignment of ${m.name} Ally", Color.MapPieces)
    if (m.sleeperCells > 0)
      log(s"Flip the ${amountOf(m.sleeperCells, "sleeper cell")} in ${m.name} to active", Color.FlipPieces)
    rollPrestige()
    endCivilWar(dest)  // Can't have civil war in regime change performed due to event, etc.
    flipCaliphateSleepers()
    if (dest == Iraq)
      removeEventMarkersFromCountry(Iraq, "Iraqi WMD")
    if (dest == Libya)
      removeEventMarkersFromCountry(Libya, "Libyan WMD")

    logSummary(game.scoringSummary)
    log()
  }

  // • Deploy any number troops out of the Regime Change country (regardless of cells present).
  // • Remove any Aid markers there.
  // • Place a Besieged Regime marker there (if there is not one already).
  // • Roll Prestige.
  def performWithdraw(source: String, dest: String, numTroops: Int): Unit = {
    log()
    addOpsTarget(dest)
    moveTroops(source, dest, numTroops)
    val m = game.getMuslim(source)
    if (m.aidMarkers > 0)
      log(s"Remove aid marker${if (m.aidMarkers > 1) "s" else ""} from ${m.name}", Color.MapPieces)
    if (!m.besiegedRegime)
      log(s"Add besieged regime marker to ${m.name}", Color.MapPieces)
    game = game.updateCountry(m.copy(aidMarkers = 0,besiegedRegime = true))
    rollPrestige()
  }

  def performDisrupt(target: String): Unit = {
    val AlAnbarTargets = Set(Iraq, Syria)
    val bumpPrestige = game.getCountry(target).disruptAffectsPrestige
    testCountry(target)  // It is possible that the country was reverted to untested by an event.
    addOpsTarget(target)
    addDisruptedTarget(target)
    if (globalEventInPlay(AlAnbar) && AlAnbarTargets(target))
      log(s"Disrupt removes only 1 cell [$AlAnbar]", Color.Event)

    game.disruptLosses(target) match {
      case Some(Left(numCells)) =>
        val (actives, sleepers) = if (game.humanRole == US)
          askCellsNotSadr(target, numCells, sleeperFocus = false)
        else
          USBot.chooseCellsToDisrupt(target, numCells)
        flipSleeperCells(target, sleepers)
        removeCellsFromCountry(target, actives, 0, false, addCadre = true)
      case Some(Right(_)) =>
        removeCadresFromCountry(target, 1)
      case None =>
        throw new IllegalStateException(s"performDisrupt(): $target has no cells or cadre")
    }
    if (bumpPrestige && game.usResolve(Adept)) {
      log(s"$US Bot with Adept resolve disrupt increases prestige by 2")
      increasePrestige(2)
    }
    else if (bumpPrestige)
      increasePrestige(1)
  }

  def performAlert(countryName: String, plotOnMap: PlotOnMap): Unit = {
    addOpsTarget(countryName)
    testCountry(countryName)  // It is possible that the country was reverted to untested by an event.
    val c = game.getCountry(countryName)
    assert(c.plots contains plotOnMap, s"performAlert(): $countryName does not contain $plotOnMap")

    val plot = plotOnMap.plot
    val prestigeDelta = if (plot == PlotWMD) 1 else 0
    val i = c.plots.indexOf(plotOnMap)
    val remaining = c.plots.take(i) ::: c.plots.drop(i + 1)
    val updated = c match {
      case m: MuslimCountry    => game = game.updateCountry(m.copy(plots = remaining))
      case n: NonMuslimCountry => game = game.updateCountry(n.copy(plots = remaining))
    }
    if (plot == PlotWMD) {
      val updatedPlots = game.plotData.copy(removedPlots = plot :: game.removedPlots)
      game = game.copy(plotData = updatedPlots)
      log(s"$plot alerted in $countryName, remove it from the game.", Color.MapPieces)
      if (game.useExpansionRules) {
        log(s"Increase prestige 1 (alerting WMD plot)")
        increasePrestige(1)
      }
    }
    else if (game.useExpansionRules) {
      val updatedPlots = game.plotData.copy(resolvedPlots = plot :: game.resolvedPlots)
      game = game.copy(plotData = updatedPlots)
      log(s"$plot alerted in $countryName, move it to the resolved plots box.", Color.MapPieces)
    }
    else {
      val updatedPlots = game.plotData.copy(availablePlots = plot :: game.availablePlots)
      game = game.copy(plotData = updatedPlots)
      log(s"$plot alerted in $countryName, move it to the available plots box.", Color.MapPieces)
    }
  }


  case class TravelAttempt(from: String, to: String, active: Boolean)

  // Return a list of (Destination, success)
  def performTravels(attempts: List[TravelAttempt]): List[(String, Boolean)] = {
    def handleResult(success: Boolean, from: String, to: String, active: Boolean): Boolean = {
      if (success) {
        // We may have had to roll for adjacent travel or travel within a Schengen country
        // if Islamic Maghreb event is in play
        if (from == to) {
          if (active)
            hideActiveCells(to, 1)
          else
            log("Travelling a sleeper cell within the same country has no further effect", Color.MapPieces)
        }
        else
          moveCellsBetweenCountries(from, to, 1, active, forTravel = true)
      }
      else if (active)
        removeCellsFromCountry(from, 1, 0, false, addCadre = false)
      else
        removeCellsFromCountry(from, 0, 1, false, addCadre = false)
      success
    }

    attempts match {
      case Nil => Nil
      case TravelAttempt(from, to, active) :: remaining =>
        log()
        log(s"Attempt travel from $from to $to")
        log(separator())
        testCountry(to)
        addOpsTarget(to)
        val success = if (travelIsAutomatic(from, to)) {
          val qatari = if (globalEventInPlay(QatariCrisis) &&
                           !game.getCountry(to).isIslamistRule && // Would be automatic anyway
                           qatariCrisisAdjacencyMap.contains(from) &&
                           qatariCrisisAdjacencyMap.contains(to))

             s" [$QatariCrisis]"
          else
            ""

          log(s"Travel from $from to $to succeeds automatically$qatari")
          handleResult(true, from, to, active)
        }
        else {
          val die = getDieRoll("Enter travel die roll: ", Some(Jihadist))
          log(s"Die roll: $die")
          val modRoll = if (game.isTrainingCamp(from)) {
            log(s"-1 Travelling from Training Camps")
            log(s"Modified roll: ${die - 1}")
            die - 1
          }
          else
            die
          val success = modRoll <= game.getCountry(to).governance
          log(if (success) "Success" else "Failure")
          handleResult(success, from, to, active)
        }
        (to, success) :: performTravels(remaining)
    }
  }


  case class PlotAttempt(name: String, active: Boolean)

  // NOTE: The ops parameter is used to determine which type of Plots
  //       are available for selection.
  // BUT : The Bot ignores this and simply selects at random
  //       among all available plots.
  // Returns the number of attempts actually made as it is possible
  // that we run out of available plots.
  def performPlots(ops: Int, attempts: List[PlotAttempt]): Int = {
    attempts match {
      case Nil => 0
      case _ if game.availablePlots.isEmpty =>
        log(s"There are no more available plots")
        0
      case PlotAttempt(name, active) :: remaining =>
        log()
        log(s"Plot attempt in $name")
        log(separator())
        testCountry(name)  // It is possible that the country was reverted to untested by an event.
        if (active)
          log("Using an already active cell")
        else
          flipSleeperCells(name, 1)
        val die = getDieRoll("Enter plot die roll: ", Some(Jihadist))
        log(s"Die roll: $die")
        val success = die <= game.getCountry(name).governance
        log(if (success) "Success" else "Failure")

        if (success) {
          // Ask the user which plot to place.  The Bot takes a random plot regardless of ops spent.
          val plots = if (game.humanRole == Jihadist)
            askAvailablePlots(1, ops)
          else if (game.jihadistIdeology(Attractive)) {
            log(s"$Jihadist Bot with Attractive Ideology places two plots")
            JihadistBot.preparePlots(game.availablePlots).take(2)
          }
          else
            JihadistBot.preparePlots(game.availablePlots).take(1)

          for (plot <- plots)
            addAvailablePlotToCountry(name, plot)
        }

        1 + performPlots(ops, remaining)
    }
  }

  def showPlots(plots: Vector[Plot]): Unit = {
    @tailrec def showNext(list: List[Plot], index: Int): Unit = list match {
      case Nil =>
      case plot :: rest =>
        println(s"$index) ${plot.name}")
        showNext(rest, index + 1)
    }
    showNext(plots.toList, 1)
  }

  case class JihadTarget(name: String, actives: Int, sleepers: Int, sadr: Boolean, major: Boolean)

  // Perform Jihads on the given targets.
  // Return a List of (name, successes, success with Sadr) to indicate the number of success achieved in
  // each target.
  def performJihads(targets: List[JihadTarget], ignoreFailures: Boolean = false): List[(String, Int, Boolean)] = {
    targets match {
      case Nil => Nil
      case JihadTarget(name, actives, sleepers, sadr, major)::remaining =>
        val m = game.getMuslim(name)
        val jihad = if (major) "Major Jihad" else "Jihad"
        assert(!m.isIslamistRule, s"Cannot perform $jihad in Islamist Rule country")
        assert(!(major && m.isGood), s"Cannot perform $jihad in country with Good governance")
        val numAttempts = actives + sleepers + (if (sadr) 1 else 0)
        log()
        log(s"Conduct $jihad in $name")
        log(separator())
        testCountry(name)  // It is possible that the country was reverted to untested by an event.
        if (major) {
          log(s"Rolling ${diceString(numAttempts)}")
          if (m.sleeperCells > 0)
            flipAllSleepersCells(name)
        }
        else {
          val disp = new ListBuffer[String]
          if (actives > 0)  disp += amountOf(actives, "active cell")
          if (sleepers > 0) disp += amountOf(sleepers, "sleeper cell")
          if (sadr)         disp += "Sadr"
          log(s"Rolling ${diceString(numAttempts)}, using ${disp.mkString(", ")}")
          if (sleepers > 0)
            flipSleeperCells(name, sleepers)
        }
        def nextAttempt(num: Int): Int = num match {
          case n if n <= numAttempts =>
            val ord = if (numAttempts == 1) "" else s"${ordinal(num)} "
            val die = getDieRoll(s"Enter ${ord}Jihad die roll: ", Some(Jihadist))
            log(s"\n${ord}Die roll: $die")
            val modRoll = modifyJihadRoll(die, m, major)
            val result  = modRoll <= m.governance
            log(if (result) "Success" else "Failure")
            (if (result) 1 else 0) + nextAttempt(num + 1)
          case _ => 0  // No more attempts
        }
        val successes = nextAttempt(1)
        val failures  = numAttempts - successes
        val sadrSuccess = sadr && successes > 0  // Sadr is last to fail!
        val sadrFailure = sadr && failures == numAttempts
        val majorSuccess = major && (
          (m.isPoor && (successes  > 1 || (successes == 1 && m.besiegedRegime))) ||
          (m.isFair && (successes == 3 || (successes == 2 && m.besiegedRegime)))
        )
        log()
        if (major)
          log(s"Major Jihad ${if (majorSuccess) "succeeds" else "fails"}")
        // Remove one active cell for each failure
        if (!ignoreFailures)
          if (sadrFailure)
            removeCellsFromCountry(name, failures - 1, 0, true, addCadre = false)
          else
            removeCellsFromCountry(name, failures, 0, false, addCadre = false)

        // Remove 1 aid marker for each successful die roll
        removeAidMarker(name, successes min m.aidMarkers)
        worsenGovernance(name, levels = successes, canShiftToIR = majorSuccess)
        // A major jihad failure rolling 3 dice in a country that was
        // already at Poor governance before the operation began will
        // add a besieged regime marker and shift alignment toward ally
        if (major && !majorSuccess && m.governance == Poor && numAttempts == 3) {
          addBesiegedRegimeMarker(name)
          shiftAlignmentLeft(name)
        }
        (name, successes, sadrSuccess) :: performJihads(remaining)
    }
  }


  def performCardEvent(card: Card, role: Role, triggered: Boolean = false): Boolean = {
    if (!card.autoTrigger && lapsingEventInPlay(FakeNews)) {
      log("\n%s event \"%s\" is cancelled by \"Fake News\"".format(card.association, card.cardName), Color.Event)
      removeLapsingEvent(FakeNews)
      false  // Event was cancelled
    }
    else if (!card.autoTrigger && triggered && !card.eventConditionsMet(role)) {
      log("\n%s event \"%s\" does not trigger. The event conditions are not satisfied. ".format(card.association, card.cardName))
      false
    }
    else {
      if (card.autoTrigger)
        log("\n%s automatic event \"%s\" triggers".format(card.association, card.cardName))
      else if (triggered)
        log("\n%s event \"%s\" triggers".format(card.association, card.cardName))
      else
        log("\n%s executes the \"%s\" event".format(role, card.cardName))
      log(separator())
      card.executeEvent(role)

      if (card.markLapsingAfterExecutingEvent(role))
        putCardInLapsingBox(card.number)
      else if (card.removeAfterExecutingEvent(role))
        removeCardFromGame(card.number)
      true
    }
  }

  var _ignoreDiscardAtEndOfTurn = false
  // Used by Boko Haram event
  def setIgnoreDiscardAtEndOfTurn(value: Boolean): Unit = {
    _ignoreDiscardAtEndOfTurn = value
  }

  def ignoreDiscardAtEndOfTurn() = _ignoreDiscardAtEndOfTurn

  def addCardToDiscardPile(cardNumber: Int): Unit = {
    if (cardNumber == CriticalMiddle) {
      log("\nIMPORTANT!", Color.Event)
      log("Place the \"Critical Middle\" card in the approximate middle of the draw pile.", Color.Event)
    }
    else {
      log("\nPut %s in the discard pile".format(deck(cardNumber).numAndName), Color.Event)
      game = game.copy(cardsDiscarded = cardNumber :: game.cardsDiscarded)
    }
  }

  def removeCardFromGame(cardNumber: Int): Unit = {
    log("\nRemove %s from the game".format(deck(cardNumber).numAndName), Color.Event)
    game = game.copy(cardsRemoved = cardNumber :: game.cardsRemoved)
  }

  def putCardInLapsingBox(cardNumber: Int): Unit = {
    log("\nPut %s in the lapsing box".format(deck(cardNumber).numAndName), Color.Event)
    game = game.copy(eventsLapsing = LapsingEntry(cardNumber) :: game.eventsLapsing)
  }

  def cardDrawnFromFirstPlotBox(cardNumber: Int): Unit = {
    log("\n%s drawn from the 1st Plot box".format(deck(cardNumber).numAndName), Color.Event)
    log("Place an inuse marker in the first plot box.", Color.Event)
    // Card number of zero used for 1st Plot marker
    game = game.copy(firstPlotEntry = Some(LapsingEntry(0, discarded = true)))
  }

  // Prestige roll used
  //   After regime change, withdraw, unblocked plot in the US, or by event.
  def rollPrestige(): Unit = {
    log("\nRoll Prestige...", Color.Info)
    log(separator(), Color.Info)
    val dirDie = getDieRoll(s"Enter die roll for prestige change direction: ")
    val shiftDice   = List(
      getDieRoll(s"Enter 1st die roll for prestige change value: "),
      getDieRoll(s"Enter 2nd die roll for prestige change value: ")
    )
    val shiftAmount = shiftDice.min
    val drm = if (game.gwotPenalty > 0) -1 else 0
    val increase = (dirDie + drm >= 5)

    log(s"Direction roll: $dirDie")
    if (game.gwotPenalty > 0) {
      log(s"-1: GWOT penalty is not zero")
      log(s"Modified roll: ${dirDie - 1}")
    }
    log(s"Rolls for shift amount: ${shiftDice.head} and ${shiftDice.last} (lowest value is used)")
    if (increase)
      increasePrestige(shiftAmount)
    else
      decreasePrestige(shiftAmount)
  }

  // Note: The caller is responsible for handling convergence and the possible
  //       displacement of the caliphate capital.
  def improveGovernance(name: String, levels: Int, canShiftToGood: Boolean, endOfTurn: Boolean = false,
                        convergenceOK: Boolean = true): Unit = {
    if (levels > 0) {
      val m = game.getMuslim(name)
      assert(!m.isGood, s"improveGovernance() called on Good country - $name")
      val minGov = if (canShiftToGood) Good else Fair
      val newGov = (m.governance - levels) max minGov
      val delta  = m.governance - newGov
      if (delta == 0)
        log(s"The governance of $name remains ${govToString(m.governance)}", Color.MapPieces)
      else {
        if (newGov == Good) {
          // Note: "Training Camps" marker is handle specially.
          log(s"Improve governance of $name to ${govToString(newGov)}", Color.MapPieces)
          if (m.besiegedRegime) log(s"Remove besieged regime marker from $name", Color.MapPieces)
          if (m.aidMarkers > 0) log(s"Remove ${amountOf(m.aidMarkers, "aid marker")} from $name", Color.MapPieces)
          if (m.awakening > 0 ) log(s"Remove ${amountOf(m.awakening, "awakening marker")} from $name", Color.MapPieces)
          if (m.reaction > 0  ) log(s"Remove ${amountOf(m.reaction, "reaction marker")} from $name", Color.MapPieces)
          if (m.militia > 0   ) log(s"Remove ${m.militia} militia from $name", Color.MapPieces)

          val improved = m.copy(governance = Good, awakening = 0, reaction = 0, aidMarkers = 0,
                 militia = 0, besiegedRegime = false)
          game = game.updateCountry(improved)
          removeTrainingCamp_?(name)
          endRegimeChange(name, noDisplacement = endOfTurn)
          endCivilWar(name, noDisplacement = endOfTurn)
          if (convergenceOK)
            performConvergence(forCountry = name, awakening = true)
        }
        else {
          log(s"Improve the governance of $name to ${govToString(newGov)}", Color.MapPieces)
          if (m.awakening > 0)
            log(s"Remove ${amountOf(delta min m.awakening, "awakening marker")} from $name", Color.MapPieces)
          val improved = m.copy(governance = newGov,
                 awakening  = (m.awakening - delta) max 0) // Rempove one awakening for each level actually improved
          game = game.updateCountry(improved)
        }
        if (newGov == Good || newGov == Fair)
          addTestedOrImprovedToFairOrGood(name)

        if (newGov == Good) {
          logSummary(game.scoringSummary)
          log()
          checkAutomaticVictory() // Will Exit game if auto victory has been achieved
        }
      }
    }
  }

  // Degrade the governance of the given country and log the results.
  // Note: The caller is responsible for handling convergence!
  def worsenGovernance(name: String, levels: Int, canShiftToIR: Boolean, endOfTurn: Boolean = false,
                       convergenceOK: Boolean = true): Unit = {
    if (levels > 0) {
      val wasCaliphateMember = game.isCaliphateMember(name)
      val m = game.getMuslim(name)
      assert(!m.isIslamistRule, s"worsenGovernance() called on Islamist Rule country - $name")
      val maxGov = if (canShiftToIR) IslamistRule else Poor
      val newGov = (m.governance + levels) min maxGov
      val delta  = newGov - m.governance
      if (delta == 0)
        log(s"The governance of $name remains ${govToString(m.governance)}", Color.MapPieces)
      else {
        if (newGov == IslamistRule) {
          log(s"Set governance of $name to ${govToString(newGov)}", Color.MapPieces)
          increaseFunding(m.resourceValue)
          if (m.totalTroopsThatAffectPrestige > 0) {
            log(s"Set US prestige to 1 (troops present)", Color.MapMarker)
            game = game.copy(prestige = 1)
          }
          // Always remove aid when degraded to IR
          if (m.besiegedRegime) log(s"Remove besieged regime marker from $name", Color.MapPieces)
          if (m.aidMarkers > 0) log(s"Remove ${amountOf(m.aidMarkers, "aid marker")} from $name", Color.MapPieces)
          if (m.awakening > 0 ) log(s"Remove ${amountOf(m.awakening, "awakening marker")} from $name", Color.MapPieces)
          if (m.reaction > 0  ) log(s"Remove ${amountOf(m.reaction, "reaction marker")} from $name", Color.MapPieces)
          if (m.militia > 0   ) log(s"Remove ${m.militia} militia from $name", Color.MapPieces)
          // When a country becomes IR, any plots in the country remain
          val degraded = m.copy(
            governance = IslamistRule, alignment = Adversary, awakening = 0, reaction = 0,
            aidMarkers = 0, militia = 0, besiegedRegime = false)
          game = game.updateCountry(degraded)
          moveWMDCacheToAvailable(name, m.wmdCache)
          removeAllAdvisorsFromCountry(name) // Country becomes Adversary
          endRegimeChange(name, noDisplacement = endOfTurn)
          endCivilWar(name, noDisplacement = endOfTurn)
          if (!wasCaliphateMember && game.isCaliphateMember(name))
            log(s"Place a Caliphate Country marker in $name", Color.MapPieces)
          flipCaliphateSleepers()
          if (convergenceOK)
            performConvergence(forCountry = name, awakening = false)
          checkAutomaticVictory() // Will Exit game if auto victory has been achieved
          logSummary(game.scoringSummary)
          log()
        }
        else {
          log(s"Degrade the governance of $name to ${govToString(newGov)}", Color.MapPieces)
          if (m.reaction > 0)
            log(s"Remove ${amountOf(delta min m.reaction, "reaction marker")} from $name", Color.MapPieces)
          // Remove One reaction for each level actually degraded
          val degraded = m.copy(governance = newGov, reaction = (m.reaction - delta)  max 0)
          game = game.updateCountry(degraded)
        }
      }
    }
  }

  def setGovernance(name: String, gov: Int, alignment: Option[String] = None): Unit = {
    val m = game getMuslim name
    if (m.isUntested) {
      val align = alignment getOrElse Neutral
      game = game.updateCountry(m.copy(governance = gov, alignment = align))
      log(s"Set $name to ${govToString(gov)} $align", Color.MapPieces)
    }
    else {
      // Use the improveGovernance/degradeGovernance functions to make sure
      // all the details are covered such as ending civil war, etc.
      val govDelta = m.governance - gov
      if (govDelta > 0)
        improveGovernance(name, govDelta, canShiftToGood = true)
      else if (govDelta < 0)
        worsenGovernance(name, -govDelta, canShiftToIR = true)

      alignment foreach { align => setAlignment(name, align) }
    }
  }

  def setAlignment(name: String, newAlign: String): Unit = {
    var m = game.getMuslim(name)
    if (m.isUntested || m.alignment != newAlign) {
      log(s"Set the alignment of $name to $newAlign", Color.MapPieces)
      game = game.updateCountry(m.copy(alignment = newAlign))
      if (name == Iran && newAlign != Adversary && (game.getCountry(Iran).hasMarker(TradeEmbargoJihadist))) {
        removeEventMarkersFromCountry(Iran, TradeEmbargoJihadist)
        log("Iran may resume oil exports", Color.Event)
      }
      else if (name == Iran && newAlign != Adversary && (game.getCountry(Iran).hasMarker(TradeEmbargoUS))) {
        removeEventMarkersFromCountry(Iran, TradeEmbargoUS)
      }

      //  Setting a country to Ally could disrupt the Tehran Beirut Land Corridor
      checkTehranBeirutLandCorridor()

      if (newAlign == Adversary)
        removeAllAdvisorsFromCountry(name)

      if (name == Nigeria)
        makeNigeriaNonMuslim_?() // rule 11.3.3.3
    }
  }

  def shiftAlignmentLeft(name: String): Unit = {
    var m = game.getMuslim(name)
    m.alignment match {
      case Adversary => setAlignment(name, Neutral)
      case Neutral   => setAlignment(name, Ally)
      case _         => log(s"The alignment of $name remains ${m.alignment}", Color.MapPieces)
    }
  }

  def shiftAlignmentRight(name: String): Unit = {
    var m = game.getMuslim(name)
    m.alignment match {
      case Ally    => setAlignment(name, Neutral)
      case Neutral => setAlignment(name, Adversary)
      case _       => log(s"The alignment of $name remains ${m.alignment}", Color.MapPieces)
    }
  }

  // Remove all markers from the country (except any wmd cache)
  // and make it untested.
  def setCountryToUntested(name: String): Unit = {
    log(s"Revert $name to an untested country", Color.Event)
    log(separator())
    if (game isMuslim name) {
      val m = game.getMuslim(name)
      removeCellsFromCountry(name, m.activeCells, m.sleeperCells, true, addCadre = false)
      removeCadresFromCountry(name, 1)
      moveTroops(name, "track", m.troops)
      removeMilitiaFromCountry(name, m.militia)
      for (p <- m.plots)
        removePlotFromCountry(name, p, toAvailable = true)
      removeEventMarkersFromCountry(name, m.markers:_*)
      removeBesiegedRegimeMarker(name)
      removeAidMarker(name, m.aidMarkers)
      removeAwakeningMarker(name, m.awakening)
      removeReactionMarker(name, m.reaction)
      // Ending the regime change or civil war will also remove the caliphateCapital
      // status if it is in effect
      endRegimeChange(name)
      endCivilWar(name)
      game = game.updateCountry(game.getMuslim(name).copy(governance = GovernanceUntested))
      log(s"Remove the ${govToString(m.governance)} governance marker from $name", Color.MapPieces)
    }
    else {
      val n = game.getNonMuslim(name)
      removeCellsFromCountry(name, n.activeCells, n.sleeperCells, true, addCadre = false)
      removeCadresFromCountry(name, 1)
      moveTroops(name, "track", n.troops)
      for (p <- n.plots)
        removePlotFromCountry(name, p, toAvailable = true)
      removeEventMarkersFromCountry(name, n.markers:_*)
      game = game.updateCountry(game.getNonMuslim(name).copy(postureValue = PostureUntested))
      log(s"Remove the ${n.posture} posture marker from $name", Color.MapPieces)
      logWorldPosture()
    }
  }

  def setCountryPosture(name: String, newPosture: String): Unit = {
    val n = game.getNonMuslim(name)
    assert(n.canChangePosture, s"Cannot set posture in $name")
    if (n.posture == newPosture)
      log(s"The posture of $name remains $newPosture", Color.MapPieces)
    else {
      game = game.updateCountry(n.copy(postureValue = newPosture))
      log(s"Set posture of $name to $newPosture", Color.MapPieces)
      logWorldPosture()
    }
  }

  def isIranSpecialCase = game.getCountry(Iran) match {
    case n: NonMuslimCountry if n.iranSpecialCase => true
    case _ => false
  }

  def flipIranToShiaMixMuslim(): Unit = {
    // See Event Instructions table
    log("\nFlip Iran country mat to its Shia-Mix Muslim side", Color.Event)
    log("Set Iran to Fair Adversary", Color.MapPieces)
    val iran = game.getNonMuslim(Iran)
    game = game.updateCountry(DefaultMuslimIran.copy(
      sleeperCells = iran.sleeperCells,
      activeCells  = iran.activeCells,
      cadres       = iran.cadres,
      plots        = iran.plots,
      markers      = iran.markers,
      wmdCache     = iran.wmdCache
    ))
  }

  //  Used to enforce the Tehran Beirut Corridor
  //  event. Card #319
  def tehranBeirutLandCorridorSatisfied = {
    val tehranBeirutCandidate = (name: String) => name match {
      case Iran if isIranSpecialCase => true
      case _ =>
        val m = game.getMuslim(name)
        !(m.isUntested || m.isAlly || m.civilWar)
    }

    List(Iran, Syria, Lebanon).forall(tehranBeirutCandidate) &&
    List(Iraq, Turkey).exists(tehranBeirutCandidate)
  }

  //  Check to see if the Tehran Beirut Land Corridor has been disrupted.
  def checkTehranBeirutLandCorridor(): Unit = {
    if (countryEventInPlay(Iran, TehranBeirutLandCorridor) && !tehranBeirutLandCorridorSatisfied) {
      log(s"The $TehranBeirutLandCorridor has been disrupted.", Color.Event)
      log("Iran is now a 2 Resource country again.", Color.Event)
      removeEventMarkersFromCountry(Iran, TehranBeirutLandCorridor)
    }
  }

  def countryEventInPlay(countryName: String, markerName: String) =
    (game getCountry countryName).hasMarker(markerName)
  def countryEventNotInPlay(countryName: String, markerName: String) =
    !(countryEventInPlay(countryName, markerName))
  def countryEventInPlayAnywhere(markerName: String) =
    game.countries.exists(c => countryEventInPlay(c.name, markerName))

  def globalEventInPlay(name: String)     = game.markers contains name
  def globalEventNotInPlay(name: String)  = !globalEventInPlay(name)
  def lapsingEventInPlay(cardNum: Int)    = game.eventIsLapsing(cardNum)
  def lapsingEventNotInPlay(cardNum: Int) = !lapsingEventInPlay(cardNum)

  def addGlobalEventMarker(marker: String): Unit = {
    if (globalEventNotInPlay(marker)) {
      game = game.addMarker(marker)
      log(s"""Place "$marker" marker in the Events In Play box""", Color.MapMarker)
    }
  }

  def removeGlobalEventMarker(marker: String): Unit = {
    if (globalEventInPlay(marker)) {
      val priorGameState = game
      game = game.removeMarker(marker)

      // Handle any board state changes caused by the
      // marker removal
      marker match {
        // Move troops from out of play to track when
        // South China Seas Crisis marker is removed.
        case SouthChinaSeaCrisis =>
          log(s"\nSouth China Seas Crisis ends", Color.Event)
          moveOfMapTroopsToTrack(2)

        case Sequestration =>
          log("Sequestration ends", Color.Event)
          moveOfMapTroopsToTrack(3)
          game = game.copy(sequestrationTroops = false)

        case _ =>
      }
      log(s"""Remove "$marker" marker from the Events In Play box""", Color.MapMarker)
      logExtraCellCapacityChange(priorGameState)
    }
  }

  def trumpTweetsON: Boolean = globalEventInPlay(TrumpTweetsON)

  def setTrumpTweetsON(): Unit = if (!trumpTweetsON) {
    if (globalEventInPlay(TrumpTweetsOFF)) {
      game = game.removeMarker(TrumpTweetsOFF).addMarker(TrumpTweetsON)
      log(s"""Flip the "Trump Tweets" marker to ON""", Color.FlipPieces)
    }
    else
      addGlobalEventMarker(TrumpTweetsON)
  }

  def setTrumpTweetsOFF(): Unit = if (trumpTweetsON) {
    game = game.removeMarker(TrumpTweetsON).addMarker(TrumpTweetsOFF)
    log(s"""Flip the "Trump Tweets" marker to OFF""", Color.FlipPieces)
  }

  def addEventMarkersToCountry(countryName: String, markers: String*): Unit = {
    for (marker <- markers) {
      val c = game.getCountry(countryName)
      if (!(c hasMarker marker)) {
        log(s"""Place "$marker" marker in $countryName""", Color.MapMarker)
        c match {
          case m: MuslimCountry    => game = game.updateCountry(m.addMarkers(marker))
          case n: NonMuslimCountry => game = game.updateCountry(n.addMarkers(marker))
        }
      }
    }
  }


  def removeEventMarkersFromCountry(countryName: String, markers: String*): Unit = {
    for (marker <- markers) {
      val c = game.getCountry(countryName)
      if (c hasMarker marker) {
        log(s"""Remove "$marker" marker from $countryName""", Color.MapMarker)
        c match {
          case m: MuslimCountry    => game = game.updateCountry(m.removeMarkers(marker))
          case n: NonMuslimCountry => game = game.updateCountry(n.removeMarkers(marker))
        }
      }
    }
  }

  def removeCountryEventMarkerAnywhere(marker: String): Unit = {
     game.countries
      .filter(c => countryEventInPlay(c.name, marker))
      .foreach(c => removeEventMarkersFromCountry(c.name, marker))
  }

  // A country can have more than one Advisors counter
  // so we cannot call addEventMarkersToCountry() as it
  // only adds a marker if it is not already present.
  def addAdvisorsToCountry(countryName: String): Unit = if (game isMuslim countryName) {
    val m = game.getMuslim(countryName)
    log(s"""Place "$Advisors" marker in $countryName""", Color.MapPieces)
    game = game.updateCountry(m.addMarkers(Advisors))
  }

  def removeAllAdvisorsFromCountry(countryName: String): Unit = {
    if (game.isMuslim(countryName)) {
      var m = game.getMuslim(countryName)
      val advisors = List.fill(m.numAdvisors)(Advisors)
      removeEventMarkersFromCountry(countryName, advisors:_*)
    }
  }

  // Set up the extra cells for Training Camps/al-Baghdadi
  // It is caller's responsibility to avoid calling this
  // if the when the other event (Training Camp/al-Baghdadi) is still in play.
  def playExtraCellsEvent(event: String, targetCountry: String = ""): Unit = {

    val priorGameState = game
    //  Place the appropriate event marker on the board
    event match {
      case TrainingCamps =>
        game.trainingCamp match {
          case Some(existing) if existing == targetCountry =>
            log(s"Training Camps marker remains in $targetCountry")
          case Some(existing) =>
            removeEventMarkersFromCountry(existing, TrainingCamps)
            addEventMarkersToCountry(targetCountry, TrainingCamps)
          case None =>
            addEventMarkersToCountry(targetCountry, TrainingCamps)
        }
      case AlBaghdadi =>
        addGlobalEventMarker(AlBaghdadi)

      case e =>
        throw new IllegalArgumentException("placeExtraCells() invalid event")
    }

    logExtraCellCapacityChange(priorGameState)
  }

  // This is called whenever something happens that can change
  // the capacity for extra cells.
  //    Caliphate declared/displaced
  //    Training Camps and AlBaghdadi events no longer in play
  def logExtraCellCapacityChange(priorGameState: GameState): Unit = {
    if (game.extraCellCapacity != priorGameState.extraCellCapacity) {
      val alBaghdadi = (game.markers contains AlBaghdadi)
      val msg = game.extraCellCapacity match {
        case 0                => s"There is no longer an event in play that grants extra cells"
        case 5 if alBaghdadi  => s"$AlBaghdadi is in play and a Caliphate has been declared"
        case 5                => s"$TrainingCamps is in a Caliphate country"
        case 3 if alBaghdadi  => s"$AlBaghdadi is in play but no Caliphate has been declared"
        case 3                => s"$TrainingCamps is in play in a non-Caliphate country"
        case x => throw new IllegalStateException(s"Invalid training camp capacity: $x")
      }

      log(msg, Color.Info)
      log(s"The \"extra cells\" area now has a capacity of ${amountOf(game.extraCellCapacity, "cell")}", Color.Info)

      val delta = game.extraCellsAvailable - priorGameState.extraCellsAvailable
      if (delta > 0)
        log(s"Add ${amountOf(delta, "out of play cell")} to the right of the Ample Funding Box", Color.MapPieces)
      else if (delta < 0)
        log(
          s"Remove the ${amountOf(delta.abs, "cell")} cells from the \"extra cells\" area to out of play",
          Color.MapPieces)
    }
  }

  // Check to see if the given country has the "Training Camps".
  // If it does, but no longer meets the requirements for housing
  // the "Training Camps", the remove it and log all necessary
  // changes to the game.
  def removeTrainingCamp_?(name: String, endOfTurn: Boolean = false): Unit = {
    if (game.isTrainingCamp(name)) {
      val m = game.getMuslim(name)
      if (m.isGood || (m.totalCells == 0 && !m.hasCadre)) {
        val priorGameState = game
        removeEventMarkersFromCountry(name, TrainingCamps)

        if (!endOfTurn)
          logExtraCellCapacityChange(priorGameState)
      }
    }
  }


  def startCivilWar(name: String): Unit = {
    val wasCaliphateMember = game.isCaliphateMember(name)
    val orig = game getMuslim name
    if (!orig.civilWar) {
      testCountry(name)
      if (orig.isGood)
        worsenGovernance(name, levels = 1, canShiftToIR = true)
      else if (orig.isIslamistRule)
        improveGovernance(name, 1, canShiftToGood = true)
      val m = game.getMuslim(name)
      game = game.updateCountry(m.copy(civilWar = true, regimeChange = NoRegimeChange))
      log(s"Add civil war marker to $name", Color.MapPieces)
      if (m.inRegimeChange)
        log(s"Remove regime change marker from $name", Color.MapPieces)
      if (!wasCaliphateMember && game.isCaliphateMember(name))
        log(s"Place a Caliphate Country marker in $name", Color.MapPieces)
      removeAwakeningMarker(name, m.awakening)
      addMilitiaToCountry(name, m.awakening min game.militiaAvailable)
      removeReactionMarker(name, m.reaction)
      val newSleepers = m.reaction min game.cellsAvailable
      addSleeperCellsToCountry(name, newSleepers)
      flipCaliphateSleepers()

      //  This civil war may disrupt the Tehran Beirut Land Corridor
      checkTehranBeirutLandCorridor()
    }
  }

  def endCivilWar(name: String, noDisplacement: Boolean = false): Unit = {
    val m = game.getMuslim(name)
    val priorGameState = game

    if (m.civilWar) {
      game = game.updateCountry(m.copy(civilWar = false))
      log(s"Remove civil war marker from $name", Color.MapPieces)
      removeMilitiaFromCountry(name, m.militia)
      removeEventMarkersFromCountry(name, UNSCR_1973)
      removeAllAdvisorsFromCountry(name)
      if (m.caliphateCapital && !m.isIslamistRule && !m.inRegimeChange) {
        game = game.updateCountry(game.getMuslim(name).copy(caliphateCapital = false))
        log(s"Remove the Caliphate Capital marker from $name", Color.MapPieces)
        if (noDisplacement == false) {
          // During end of turn we defer this until all countries have been adjusted
          displaceCaliphateCapital(m.name)
          logExtraCellCapacityChange(priorGameState)
        }
      }
    }
  }

  def endRegimeChange(name: String, noDisplacement: Boolean = false): Unit = {
    val m = game.getMuslim(name)
    val priorGameState = game

    if (m.inRegimeChange) {
      game = game.updateCountry(m.copy(regimeChange = NoRegimeChange))
      log(s"Remove regime change marker from $name", Color.MapPieces)
      if (m.caliphateCapital && !m.isIslamistRule && !m.civilWar) {
        game = game.updateCountry(game.getMuslim(name).copy(caliphateCapital = false))
        log(s"Remove the Caliphate Capital marker from $name", Color.MapPieces)
        if (noDisplacement == false) {
          // During end of turn we defer this until all countries have been adjusted
          displaceCaliphateCapital(m.name)
          logExtraCellCapacityChange(priorGameState)
        }
      }
    }
  }

  // Source/dest may be "track" or a muslim country.
  // The source cannot be the same as the dest or an exception is thrown.
  // There must be enough troops available in the source or an exception is thrown
  // It is OK to specify zero troops, in which case nothing happens.
  def moveTroops(source: String, dest: String, num: Int): Unit = {
    if (num > 0) {
      assert(source != dest, "The source and destination for moveTroops() cannot be the same.")
      val startingCommitment = game.troopCommitment
      def disp(name: String) = if (name == "track") "the troops track" else name
      log(s"Move ${amountOf(num, "troop")} from ${disp(source)} to ${disp(dest)}", Color.MapPieces)
      if (source == "track")
        assert(game.troopsAvailable >= num, "moveTroop(): Not enough troops available on track")
      else {
        val c = game.getCountry(source)
        assert(c.troops >= num, s"moveTroop(): Not enough troops available in $source")
        c match {
          case m: MuslimCountry    => game = game.updateCountry(m.copy(troops = m.troops - num))
          case n: NonMuslimCountry => game = game.updateCountry(n.copy(troops = n.troops - num))
        }
      }
      if (dest != "track") {
        game.getCountry(dest) match {
          case m: MuslimCountry    => game = game.updateCountry(m.copy(troops = m.troops + num))
          case n: NonMuslimCountry => game = game.updateCountry(n.copy(troops = n.troops + num))
        }
        removeAllAdvisorsFromCountry(dest)
      }
      if (game.troopCommitment != startingCommitment)
        log(s"Move the troop commitment marker from ${startingCommitment} to ${game.troopCommitment}", Color.MapMarker)
    }
  }

  def removeAllTroopsMarkers(name: String): Unit = {
    val markers = game.getMuslim(name).troopsMarkers.map(_.name)
    removeEventMarkersFromCountry(name: String, markers:_*)
  }

  //  Move all troops cubes in the country to the track
  //  and remove all troops markers
  def removeAllTroopsFromCountry(name: String): Unit = {
    val c = game getCountry name
    moveTroops(name, "track", c.troops)
    removeAllTroopsMarkers(name)
  }

  def putTroopsInOffMapBox(source: String, num: Int): Unit = {
    if (num > 0) {
      val startingCommitment = game.troopCommitment
      def disp(name: String) = if (name == "track") "the troops track" else name

      log(s"Move ${amountOf(num, "troop")} from ${disp(source)} to the off-map box", Color.MapPieces)
      // Note: No need to "remove" them from the track as the number on the track is
      // calculated based on those on the map and in the off map box.
      if (source == "track")
        assert(game.troopsAvailable >= num, "putTroopsInOffMapBox(): Not enough troops available on track")
      else {
        val m = game.getMuslim(source)
        assert(m.troops >= num, s"putTroopsInOffMapBox(): Not enough troops available in $source")
        game  = game.updateCountry(m.copy(troops = m.troops - num))
      }
      game = game.copy(offMapTroops = game.offMapTroops + num)

      if (game.troopCommitment != startingCommitment)
        log(s"Move the troop commitment marker from ${startingCommitment} to ${game.troopCommitment}", Color.MapMarker)
    }
  }

  def moveOfMapTroopsToTrack(num: Int): Unit = {
    if (num > 0) {
      assert(game.offMapTroops >= num, "moveOfMapTroopsToTrack() num too high")
      val startingCommitment = game.troopCommitment

      log(s"Move ${amountOf(num, "troop")} from the off-map box to the troops track", Color.MapPieces)
      // Note: No need to "add" them from the track as the number on the track is
      // calculated based on those on the map and in the off-map box.
      game = game.copy(offMapTroops = game.offMapTroops - num)

      if (game.troopCommitment != startingCommitment)
        log(s"Move the troop commitment marker from ${startingCommitment} to ${game.troopCommitment}", Color.MapMarker)
    }
  }

  // Must be enough available militia on track or an exception is thrown.
  def addMilitiaToCountry(name: String, num: Int): Unit = {
    if (num > 0) {
      assert(game.militiaAvailable >= num, "addMilitiaToCountry(): Not enough militia available on track")
      val m = game.getMuslim(name)
      game = game.updateCountry(m.copy(militia = m.militia + num))
      log(s"Add $num militia to $name from the track", Color.MapPieces)
    }
  }

  // Must be enough militia in country or an exception is thrown.
  def removeMilitiaFromCountry(name: String, num: Int): Unit = {
    if (num > 0) {
      val m = game.getMuslim(name)
      assert(m.militia >= num, s"removeMilitiaFromCountry(): Not enough militia in $name")
      game = game.updateCountry(m.copy(militia = m.militia - num))
      log(s"Remove $num militia from $name to the track", Color.MapPieces)
    }
  }

  def addActiveCellsToCountry(name: String, num: Int, logPrefix: String = "") =
    addCellsToCountry(name, true, num, logPrefix)

  def addSleeperCellsToCountry(name: String, num: Int, logPrefix: String = "") =
    addCellsToCountry(name, false, num, logPrefix)

  // Move cells from the track (or training camp) to a country on the map.
  // Caller should ensure there are enough available cells to satisfy the move.
  // otherwise the function will throw an exception!
  def addCellsToCountry(name: String, active: Boolean, num: Int, logPrefix: String = ""): Unit = {
    if (num > 0) {
      val isActive  = active || game.isCaliphateMember(name)
      val cellType  = if (isActive) "active cell" else "sleeper cell"
      val available = game.cellsAvailable
      val hasCadre  = game.getCountry(name).hasCadre

      assert(available >= num, s"not enough available cells have: $available, need $num")

      val fromTrack = num min game.cellsOnTrack  // Take as many as possible from the track first
      val fromCamp  = num - fromTrack            // Any left over come from the camp

      testCountry(name)
      val updated = game.getCountry(name) match {
        case m: MuslimCountry    if isActive => m.copy(activeCells  = m.activeCells  + num)
        case m: MuslimCountry                => m.copy(sleeperCells = m.sleeperCells + num)
        case n: NonMuslimCountry if isActive => n.copy(activeCells  = n.activeCells  + num)
        case n: NonMuslimCountry             => n.copy(sleeperCells = n.sleeperCells + num)
      }
      game = game.updateCountry(updated)

      if (hasCadre)
        removeCadresFromCountry(name, 1)
      if (fromTrack > 0)
        log(
          "%sAdd %s to %s from the funding track".format(logPrefix, amountOf(fromTrack, cellType), name),
          Color.MapPieces)
      if (fromCamp > 0)
        log(
          "%sAdd %s to %s from the \"extra cells\" available area".format(logPrefix, amountOf(fromCamp, cellType), name),
          Color.MapPieces)
    }
  }

  // Remove cells from the a country.
  // The removed cells will moved to:
  // 1. Out of play if there are cells on the map that came from the training camp and
  //    the training camp is currently at capacity.  This can happen if the training camp
  //    was previously in play then subsequently removed from play. (Or was in a Caliphate
  //    Country that is no longer part of the Caliphate)
  // 2. The training camp if it is in play and not a full capacity
  // 3. The funding track.  This is a calculated value, so in effect the cell are simply
  //    removed from the country and no further fields are updated.
  // Caller should ensure there are enough cells of the requested type to satisfy
  // the removal, otherwise the function will throw an exception!
  // Removing zero is OK and will not do anything.
  def removeCellsFromCountry(
    name: String,
    actives: Int,
    sleepers: Int,
    sadr: Boolean,
    addCadre: Boolean,
    logPrefix: String = ""): Unit = {
    val c = game.getCountry(name)
    if (actives + sleepers > 0 || sadr) {
      assert(c.activeCells >= actives, s"removeCellsFromCountry(): not enough active cells present")
      assert(c.sleeperCells >= sleepers, s"removeCellsFromCountry(): not enough sleeper cells present")

      val cadreAdded =
        addCadre &&
        c.cadres == 0 &&
        c.cells == (actives + sleepers) &&
        (sadr || !c.hasMarker(Sadr))
      for ((num, active) <- List((actives, true), (sleepers, false)); if num > 0) {
        val cellType    = if (active) "active cell" else "sleeper cell"
        val toOutOfPlay = num min game.excessExtraCellsOnMap
        val toExtra     = (num - toOutOfPlay) min (game.extraCellCapacity - game.extraCellsAvailable)
        val toTrack     = (num - toOutOfPlay - toExtra)

        val updated = game.getCountry(name) match {
          case m: MuslimCountry    if active => m.copy(activeCells  = m.activeCells  - num)
          case m: MuslimCountry              => m.copy(sleeperCells = m.sleeperCells - num)
          case n: NonMuslimCountry if active => n.copy(activeCells  = n.activeCells  - num)
          case n: NonMuslimCountry           => n.copy(sleeperCells = n.sleeperCells - num)
        }
        game = game.updateCountry(updated)

        if (toOutOfPlay > 0)
           log(
            "%sRemove %s from %s to out of play".format(logPrefix, amountOf(toOutOfPlay, cellType), name),
            Color.MapPieces)
        if (toExtra > 0)
          log(
            "%sRemove %s from %s to the \"extra cells\" area".format(logPrefix, amountOf(toExtra, cellType), name),
            Color.MapPieces)
        if (toTrack > 0)
          log(
            "%sRemove %s from %s to the funding track".format(logPrefix, amountOf(toTrack, cellType), name),
            Color.MapPieces)
      }
      if (sadr)
        removeEventMarkersFromCountry(name, Sadr)
      if (cadreAdded)
        addCadreToCountry(name)
      removeTrainingCamp_?(name)
      if (name == Nigeria)
        makeNigeriaNonMuslim_?() // rule 11.3.3.3
    }
  }


  def moveCellsBetweenCountries(
    fromName: String,
    toName: String,
    num: Int,
    active: Boolean,
    forTravel: Boolean): Unit = {
    if (num > 0) {
      testCountry(toName)
      val makeActive =
        game.isCaliphateMember(toName) ||
        (forTravel && toName == UnitedStates && globalEventInPlay(TravelBan))
      val fromType = if (active)     "active cell" else "sleeper cell"
      val toType1  = if (makeActive) "an active cell" else "a sleeper cell"
      val toType   = if (makeActive) "active cells" else "sleeper cells"
      val (from, to) = (game.getCountry(fromName), game.getCountry(toName))
      if (active)
        assert(from.activeCells >= num, s"moveCellsBetweenCountries(): $fromName does not have $num active cells")
      else
        assert(from.sleeperCells >= num, s"moveCellsBetweenCountries(): $fromName does not have $num sleeper cells")

      if (fromName == toName)
        num match {
          case 1 => log(s"1 $fromType in $fromName travels in place and becomes $toType1", Color.MapPieces)
          case n => log(s"$n ${fromType}s in $fromName travel in place and become $toType", Color.MapPieces)
        }
      else {
        num match {
          case 1 => log(s"Move 1 $fromType from $fromName to $toName as $toType1", Color.MapPieces)
          case n => log(s"Move $n ${fromType}s from $fromName to $toName as $toType", Color.MapPieces)
        }
      }

      from match {
        case m: MuslimCountry    if active => game = game.updateCountry(m.copy(activeCells  = m.activeCells  - num))
        case m: MuslimCountry              => game = game.updateCountry(m.copy(sleeperCells = m.sleeperCells - num))
        case n: NonMuslimCountry if active => game = game.updateCountry(n.copy(activeCells  = n.activeCells  - num))
        case n: NonMuslimCountry           => game = game.updateCountry(n.copy(sleeperCells = n.sleeperCells - num))
      }
      to match {
        case m: MuslimCountry    if makeActive => game = game.updateCountry(m.copy(activeCells = m.activeCells + num))
        case m: MuslimCountry                  => game = game.updateCountry(m.copy(sleeperCells = m.sleeperCells + num))
        case n: NonMuslimCountry if makeActive => game = game.updateCountry(n.copy(activeCells = n.activeCells + num))
        case n: NonMuslimCountry               => game = game.updateCountry(n.copy(sleeperCells = n.sleeperCells + num))
      }
      removeTrainingCamp_?(fromName)
      if (fromName == Nigeria)
        makeNigeriaNonMuslim_?() // rule 11.3.3.3
    }
  }


  def flipSleeperCells(name: String, num: Int): Unit = {
    if (num > 0) {
      val c = game.getCountry(name)
      assert(c.sleeperCells >= num, s"Cannot flip $num sleepers cells in $name, only ${c.sleeperCells} present")
      log("Flip %s in %s to active".format(amountOf(num, "sleeper cell"), name), Color.FlipPieces)
      c match {
        case m: MuslimCountry    => game = game.updateCountry(
          m.copy(sleeperCells = m.sleeperCells - num, activeCells = m.activeCells  + num))
        case n: NonMuslimCountry => game = game.updateCountry(
          n.copy(sleeperCells = n.sleeperCells - num, activeCells = n.activeCells  + num))
      }
    }
  }

  def flipAllSleepersCells(name: String): Unit = {
    val c = game.getCountry(name)
    if (c.sleeperCells > 0) {
      log(s"Flip all sleeper cells in $name to active", Color.FlipPieces)
      c match {
        case m: MuslimCountry    => game = game.updateCountry(
          m.copy(sleeperCells = 0, activeCells  = m.activeCells + m.sleeperCells))
        case n: NonMuslimCountry => game = game.updateCountry(
          n.copy(sleeperCells = 0, activeCells  = n.activeCells + n.sleeperCells))
      }
    }
  }

  def hideActiveCells(name: String, num: Int): Unit = {
    if (num > 0) {
      val c = game.getCountry(name)
      assert(c.activeCells >= num, s"Cannot hide $num active cells in $name, only ${c.activeCells} present")
      if (num == 1)
        log(s"Flip 1 active cell in $name to a sleeper cell", Color.FlipPieces)
      else
        log(s"Flip $num active cells in $name to sleeper cells", Color.FlipPieces)
      c match {
        case m: MuslimCountry    => game = game.updateCountry(
          m.copy(sleeperCells = m.sleeperCells + num, activeCells = m.activeCells - num))
        case n: NonMuslimCountry => game = game.updateCountry(
          n.copy(sleeperCells = n.sleeperCells + num, activeCells = n.activeCells - num))
      }
    }
  }

  def addCadreToCountry(name: String): Unit = {
    testCountry(name)
    val c = game.getCountry(name)
    log(s"Add cadre marker to $name.", Color.MapPieces)
    c match {
      case m: MuslimCountry    => game = game.updateCountry(m.copy(cadres = m.cadres + 1))
      case n: NonMuslimCountry => game = game.updateCountry(n.copy(cadres = n.cadres + 1))
    }
  }


  def removeCadresFromCountry(name: String, num: Int): Unit = {
    val c = game.getCountry(name)
    val numToRemove = num min c.cadres
    if (numToRemove > 0) {
      log(s"Remove ${amountOf(numToRemove, "cadre")} marker from $name.", Color.MapPieces)
      c match {
        case m: MuslimCountry    =>
          game = game.updateCountry(m.copy(cadres = m.cadres - numToRemove))
          removeTrainingCamp_?(name)
        case n: NonMuslimCountry =>
          game = game.updateCountry(n.copy(cadres = n.cadres - numToRemove))
      }
    }
  }

  def addBesiegedRegimeMarker(name: String): Unit = {
    val m = game.getMuslim(name)
    if (m.besiegedRegime)
      log(s"$name already has a besieged regime marker")
    else {
      log(s"Add a besieged regime marker to $name", Color.MapPieces)
      game = game.updateCountry(m.copy(besiegedRegime = true))
    }
  }

  def removeBesiegedRegimeMarker(name: String): Unit = {
    val m = game.getMuslim(name)
    if (m.besiegedRegime) {
      log(s"Remove besieged regime marker from $name", Color.MapPieces)
      game = game.updateCountry(m.copy(besiegedRegime = false))
    }
  }

  def addAidMarker(target: String, num: Int = 1): Unit = {
    if (num > 0) {
      val m = game.getMuslim(target)
      if (m.canTakeAidMarker) {
        game = game.updateCountry(m.copy(aidMarkers = m.aidMarkers + num))
        log(s"Add ${amountOf(num, "aid marker")} to $target", Color.MapPieces)
      }
      else
        log(s"$target cannot take an aid marker")
    }
  }

  def removeAidMarker(target: String, num: Int = 1): Unit = {
    if (num > 0) {
      val m = game.getMuslim(target)
      assert(m.aidMarkers >= num, "removeAidMarker() not enough markers")
      game = game.updateCountry(m.copy(aidMarkers = m.aidMarkers - num))
      log(s"Remove ${amountOf(num, "aid marker")} from $target", Color.MapPieces)
    }
  }

  def addAwakeningMarker(target: String, num: Int = 1): Unit = {
    if (num > 0) {
      game.getCountry(target) match {
        case m: MuslimCountry =>
          if (lapsingEventInPlay(ArabWinter))
            log(s"\nAwakening markers cannot be placed in $target because \"Arab Winter\" is in effect", Color.Event)
          else if (m.canTakeAwakeningOrReactionMarker) {
            testCountry(target)
            // Need a fresh copy after testing the country.
            val fresh = game.getMuslim(target)
            game = game.updateCountry(fresh.copy(awakening = fresh.awakening + num))
            log(s"Add ${amountOf(num, "awakening marker")} to $target", Color.MapPieces)
          }
          else
            log(s"\n$target cannot take an awakening marker.", Color.Event)
        case _ =>
            log(s"$target cannot take an awakening marker because it is Non-Muslim.", Color.Event)
      }
    }
  }

  def removeAwakeningMarker(target: String, num: Int = 1): Unit = {
    if (num > 0) {
      val m = game.getMuslim(target)
      assert(m.awakening >= num, "removeAwakeningMarker() not enough markers")
      game = game.updateCountry(m.copy(awakening = m.awakening - num))
      log(s"Remove ${amountOf(num, "awakening marker")} from $target", Color.MapPieces)
    }
  }

  def addReactionMarker(target: String, num: Int = 1): Unit = {
    if (num > 0) {
      game.getCountry(target) match {
        case m: MuslimCountry =>
          if (lapsingEventInPlay(ArabWinter))
            log(s"\nReaction markers cannot be placed in $target because \"Arab Winter\" is in effect", Color.Event)
          else if (m.canTakeAwakeningOrReactionMarker) {
            testCountry(target)
            // Need a fresh copy after testing the country.
            val fresh = game.getMuslim(target)
            game = game.updateCountry(fresh.copy(reaction = fresh.reaction + num))
            log(s"Add ${amountOf(num, "reaction marker")} to $target", Color.MapPieces)
          }
          else
            log(s"\n$target cannot take a reaction marker.", Color.Event)

        case _ =>
            log(s"$target cannot take a reaction marker because it is Non-Muslim.", Color.Event)
      }
    }
  }

  def removeReactionMarker(target: String, num: Int = 1): Unit = {
    if (num > 0) {
      val m = game.getMuslim(target)
      assert(m.reaction >= num, "removeReactionMarker() not enough markers")
      game = game.updateCountry(m.copy(reaction = m.reaction - num))
      log(s"Remove ${amountOf(num, "reaction marker")} from $target", Color.MapPieces)
    }
  }

  def moveWMDCacheToAvailable(name: String, num: Int): Unit = {
    if (num > 0) {
      val c = game.getCountry(name)
      assert(c.wmdCache >= num, s"moveWMDCacheToAvailable(): not enough WMD in $name")
      val  newCache = c.wmdCache - num;
      c match {
        case m: MuslimCountry    => game = game.updateCountry(m.copy(wmdCache = newCache))
        case n: NonMuslimCountry => game = game.updateCountry(n.copy(wmdCache = newCache))
      }
      log(s"Move ${amountOf(num, "unavailable WMD Plot")} from $name to the available plots box", Color.MapPieces)
      val updatedPlots = game.plotData.copy(availablePlots = List.fill(num)(PlotWMD) ::: game.availablePlots)
      game = game.copy(plotData = updatedPlots)
    }
  }

  def addAvailableWMDToCache(name: String, num: Int): Unit = {
    if (num > 0) {
      assert(num <= game.plotData.numAvailWMD, s"addAvailableWMDToCache($name, $num): not enough available WMD")
      val c = game.getCountry(name)

      // Sorting the plot ensures the PlotWMDs come first.
      game = game.copy(
        plotData = game.plotData.copy(removedPlots = game.plotData.availablePlots.sorted.drop(num))
      )

      c match {
        case m: MuslimCountry    => game = game.updateCountry(m.copy(wmdCache = m.wmdCache + num))
        case n: NonMuslimCountry => game = game.updateCountry(n.copy(wmdCache = n.wmdCache + num))
      }
      log(s"Add ${amountOf(num, "out of play WMD plot")} to the cache in $name", Color.MapPieces)
    }
  }

  def removeCachedWMD(name: String, num: Int, bumpPrestige: Boolean = true): Unit = {
    if (num > 0) {
      val c = game.getCountry(name)
      assert(c.wmdCache >= num, s"removeCachedWMD($name, $num): not enough WMD in $name")
      c match {
        case m: MuslimCountry    => game = game.updateCountry(m.copy(wmdCache = m.wmdCache - num))
        case n: NonMuslimCountry => game = game.updateCountry(n.copy(wmdCache = n.wmdCache - num))
      }
      game = game.copy(
        plotData = game.plotData.copy(removedPlots = List.fill(num)(PlotWMD) ::: game.removedPlots)
      )
      log(s"Remove ${amountOf(num, "WMD plot")} from the cache in $name", Color.MapPieces)
      if (bumpPrestige) {
        log(s"Increase prestige for removing WMD plots")
        increasePrestige(num)
      }
    }
  }

  def removePlacedWMD(name: String, num: Int, bumpPrestige: Boolean = true): Unit = {
    if (num > 0) {
      val c = game.getCountry(name)
      val wmd = c.plots.count(_.plot == PlotWMD)
      assert(wmd >= num, s"removePlacedWMD($name, $num): not enough WMD in $name")
      log(s"Remove ${amountOf(num, "placed WMD plot")} from $name", Color.MapPieces)

      c match {
        case m: MuslimCountry    => game = game.updateCountry(m.copy(plots = m.plots.sorted.drop(num)))
        case n: NonMuslimCountry => game = game.updateCountry(n.copy(plots = n.plots.sorted.drop(num)))
      }
      game = game.copy(
        plotData = game.plotData.copy(removedPlots = List.fill(num)(PlotWMD) ::: game.removedPlots)
      )

      if (bumpPrestige) {
        log(s"Increase prestige for removing WMD plots")
        increasePrestige(num)
      }
    }
  }

  def addRemovedWMDToCache(name: String, num: Int): Unit = {
    if (num > 0) {
      assert(game.plotData.numRemovedWMD >= num, s"addRemovedWMDToCache($name, $num): not enough removed WMD")
      val c = game.getCountry(name)
      game = game.copy(
        plotData = game.plotData.copy(removedPlots = game.plotData.removedPlots.drop(num))
      )
      c match {
        case m: MuslimCountry    => game = game.updateCountry(m.copy(wmdCache = m.wmdCache + num))
        case n: NonMuslimCountry => game = game.updateCountry(n.copy(wmdCache = n.wmdCache + num))
      }
      log(s"Add ${amountOf(num, "out of play WMD plot")} to the cache in $name", Color.MapPieces)
    }
  }

  def addRemovedWMDToAvailable(num: Int): Unit = {
    if (num > 0) {
      assert(game.plotData.numRemovedWMD >= num, s"addRemovedWMDToAvailable($num): not enough removed WMD")
      game = game.copy(
        plotData = game.plotData.copy(
          removedPlots = game.plotData.removedPlots.drop(num),
          availablePlots = List.fill(num)(PlotWMD) ::: game.plotData.availablePlots
        )
      )
      log(s"Add ${amountOf(num, "out of play WMD plot")} to the available box", Color.MapPieces)
    }
  }

  def removeAvailableWMD(num: Int, bumpPrestige: Boolean = true): Unit = {
    if (num > 0) {
      assert(game.plotData.numAvailWMD >= num, s"removeAvailableWMD($num): not enough WMD plots available")
      val updatedPlots = game.plotData.copy(
        availablePlots = game.availablePlots.sorted.drop(num),
        removedPlots   = List.fill(num)(PlotWMD) ::: game.removedPlots
      )
      game = game.copy(plotData = updatedPlots)
      log(s"Remove ${amountOf(num, "WMD plot")} from the available plots box", Color.MapPieces)
      if (bumpPrestige) {
        log(s"Increase prestige for removing WMD plots")
        increasePrestige(num)
      }
    }
  }

  def increaseFunding(amount: Int): Unit = {
    if (amount > 0) {
      val newValue = (game.funding + amount) min MaxFunding
      val prefix = if (newValue == game.funding) s"remains" else s"now"
      game = game.copy(funding = newValue)
      log(s"+$amount to funding ($prefix $newValue)", Color.MapMarker)
    }
  }

  def decreaseFunding(amount: Int): Unit = {
    if (amount > 0) {
      val newValue = (game.funding - amount) max 1
      val prefix = if (newValue == game.funding) s"remains" else s"now"
      game = game.copy(funding = newValue)
      log(s"-$amount to funding ($prefix $newValue)", Color.MapMarker)
    }
  }

  def increasePrestige(amount: Int): Unit = {
    if (amount > 0) {
      val newValue = (game.prestige + amount) min MaxPrestige
      val prefix = if (newValue == game.prestige) s"remains" else s"now"
      game = game.copy(prestige = newValue)
      log(s"+$amount to prestige ($prefix $newValue)", Color.MapMarker)
    }
  }

  def decreasePrestige(amount: Int): Unit = {
    if (amount > 0) {
      val newValue = (game.prestige - amount) max 1
      val prefix = if (newValue == game.prestige) s"remains" else s"now"
      game = game.copy(prestige = newValue)
      log(s"-$amount to prestige ($prefix $newValue)", Color.MapMarker)
    }
  }

  def makeNigeriaNonMuslim_?(): Unit = {
    if ((game isMuslim Nigeria) &&
        (game getMuslim Nigeria).isAlly &&
        (game getMuslim Nigeria).totalCells == 0) {
      log("Nigeria is an Ally with no cells, so it becomes a non-Muslim country")
      endCivilWar(Nigeria)
      endRegimeChange(Nigeria)
      val m = game getMuslim Nigeria
      removeCadresFromCountry(Nigeria, m.cadres)
      if (m.militia > 0)
        removeMilitiaFromCountry(Nigeria, m.militia)
      if (m.troops > 0)
        moveTroops(Nigeria, "track", m.troops)
      if (m.aidMarkers > 0)
        removeAidMarker(Nigeria, m.aidMarkers)
      if (m.awakening > 0)
        removeAwakeningMarker(Nigeria, m.awakening)
      if (m.reaction > 0)
        removeReactionMarker(Nigeria, m.reaction)
      for (plotOnMap <- m.plots)
        removePlotFromCountry(Nigeria, plotOnMap, toAvailable = true)
      for (marker <- m.markers)
        removeEventMarkersFromCountry(Nigeria, m.markers: _*)
      log("Flip Nigeria over to its non-Muslim side", Color.Event)
      if (m.governance != Poor)
        log(s"Place a ${govToString(m.governance)} governance marker in Nigeria", Color.MapPieces)

      game = game.updateCountry(DefaultNigeria.copy(governance = m.governance))
    }
  }

  // Returns the number of unblock plots that are resolved
  def resolvePlots(): Int = {
    case class Unblocked(name: String, isMuslim: Boolean, mapPlot: PlotOnMap)
    def chng(amt: Int) = if (amt > 0) "Increase" else "Decrease"

    // Order plots so that backlashed plots are resolved first
    // This will ensure that beneficial plots do not increase
    // funding while already at nine before backlashed plots
    // decrease funding.
    implicit val UnblockedOrdering: Ordering[Unblocked] = new Ordering[Unblocked] {
      def compare(x: Unblocked, y: Unblocked) = {
        if (x.mapPlot.backlashed && !y.mapPlot.backlashed)
          -1
        else if (!x.mapPlot.backlashed && y.mapPlot.backlashed)
          1
        else
          x.name.compare(y.name)
      }
    }

    def adjustFundingByDelta(delta: Int): Unit =
      if (delta > 0)
        increaseFunding(delta)
      else
        decreaseFunding(delta.abs)

    val unblocked = for (c <- game.countries.filter(_.hasPlots); p <- c.plots)
      yield Unblocked(c.name, c.isMuslim, p)
    val greenOnBlue = game.muslims.exists { m =>
      m.hasPlots &&
      (m.inRegimeChange || m.civilWar) &&
      (m.totalTroops > 0 || m.numAdvisors > 0)
    }
    var wmdsInCivilWars = Set.empty[String]
    var wmdInUS = unblocked
      .exists(ub => ub.name == UnitedStates && ub.mapPlot.plot == PlotWMD)

    if (wmdInUS && !game.ignoreVictory) {
      // If there is a WMD in the United States resolve it first as it will end the game.
      log(separator())
      log("An unblocked WMD plot was resolved in the United States", Color.Info)
      log("Game Over - Jihadist automatic victory!", Color.Info)

      if (askExitAfterWin()) {
        saveGameState(Some("Game Over - Jihadist automatic victory!"))
        throw QuitGame
      }
      else {
        log("Ignoring instant victory conditions.", Color.Info)
        game = game.copy(ignoreVictory = true)
      }
    }

    log()
    log(separator(char='='), Color.Info)
    log("Resolve plots", Color.Info)
    if (unblocked.isEmpty) {
      log()
      log("There are no unblocked plots on the map", Color.Info)
    }
    else {
      for (Unblocked(name, _, mapPlot) <- unblocked.sorted) {
        val country = game.getCountry(name)
        log(separator())
        log(s"Unblocked $mapPlot in $name", Color.Info)
        if (name == Israel)
          removeGlobalEventMarker("Abbas")
        if (name == India)
          removeEventMarkersFromCountry(Pakistan, "Indo-Pakistani Talks")


        country match {
          //------------------------------------------------------------------
          case m: MuslimCountry =>
            // Funding
            if (mapPlot.plot == PlotWMD && mapPlot.backlashed) {
              game = game.copy(funding = 1)
              log(s"Set funding to 1 (WMD Plot that was backlashed)", Color.MapMarker)
            }
            else if (m.isGood) {
              val delta = if (mapPlot.backlashed) -2 else 2
              log(s"${chng(delta)} funding by ${delta.abs} (Muslim country at Good governance)")
              adjustFundingByDelta(delta)
            }
            else {
              val delta = if (mapPlot.backlashed) -1 else 1
              log(s"${chng(delta)} funding by ${delta.abs} (Muslim country at worse than Good governance)")
              adjustFundingByDelta(delta)
            }
            // Prestige
            if (m.totalTroopsThatAffectPrestige > 0 && mapPlot.plot == PlotWMD) {
              game = game.copy(prestige = 1)
              log(s"Set prestige to 1 (Troops present with WMD)", Color.MapMarker)
            }
            else if (m.totalTroopsThatAffectPrestige > 0) {
              log(s"Decrease prestige by -1 (Troops present)")
              decreasePrestige(1)
            }
            // Sequestration
            if (mapPlot.plot == PlotWMD && game.sequestrationTroops) {
              log("\nWMD plot resolved while Sequestration in effect", Color.Event)
              removeGlobalEventMarker(Sequestration)
            }

            // rule 11.2.6    (WMD in Civil War)
            if (mapPlot.plot == PlotWMD && m.civilWar && wmdsInCivilWars.contains(m.name)) {
              // If second WMD in the same civil war.  Shift immediately to Islamist Rule.
              worsenGovernance(m.name, levels = 3, canShiftToIR = true) // 3 levels guarantees shift to IR
            }
            else {
              if (mapPlot.plot == PlotWMD && m.civilWar) {
                // First WMD in civil war, remove a militia.
                wmdsInCivilWars += m.name
                if (m.militia > 0) {
                  log("A WMD plot in a civil war country removes a militia", Color.Event)
                  removeMilitiaFromCountry(m.name, 1)
                }
              }

              // No roll for governance if the country is Islamist Rule,
              // but one Aid marker is removed for each plot roll that
              // would have been made.
              if (m.isIslamistRule && m.aidMarkers > 0)
                removeAidMarker(name, mapPlot.plot.number min m.aidMarkers)
              else if (m.isGood || m.isFair || m.aidMarkers > 0) {
                // Rare for a plot to exist in an IR country.  Jihadist would have to
                // place the plot with one card, then do Major Jihad with the second
                def isSuccess(die: Int) = m.isIslamistRule || die <= m.governance
                // roll plot dice
                val dice = List.fill(mapPlot.plot.number)(getDieRoll(s"Enter die roll plot placement: "))
                val successes = dice.count(isSuccess)
                val diceStr = dice.map(d => s"$d (${if (isSuccess(d)) "success" else "failure"})")
                log(s"Dice rolls to degrade governance: ${diceStr.mkString(", ")}")
                // Remove 1 aid marker for each successful die roll
                removeAidMarker(name, successes min m.aidMarkers)
                if (!m.isIslamistRule)
                  worsenGovernance(name, levels = successes, canShiftToIR = false)
              }
            }

          //------------------------------------------------------------------
          case n: NonMuslimCountry =>

            if (name == UnitedStates && mapPlot.plot == PlotWMD) {
              // Special case where the user has opted to continue playing after a Win
              // we will treat this as any other plot resolved in the US.
              log(separator())
              log("An unblocked WMD plot was resolved in the United States", Color.Info)
              log("This results in a Jihadist automatic victory!", Color.Info)
              log("But since you have opted to continue playing after Victory has been achieved", Color.Info)
              log("the plot will be treated as a normal WMD plot resolved in a NonMuslim country.\n", Color.Info)
            }

            // Funding
            if (n.iranSpecialCase) {
              log(s"Increase funding by 1 (Iran at Fair governance)")
              increaseFunding(1)
            }
            else if (name == UnitedStates) {
              game = game.copy(funding = 9)
              log(s"Set funding to 9.  (Plot in the United States)", Color.MapMarker)
            }
            else if (mapPlot.plot == PlotWMD) {
              game = game.copy(funding = 9)
              log(s"Set funding to 9.  (WMD plot in a non-Muslim country)", Color.MapMarker)
            }
            else if (n.isGood) {
              val delta = mapPlot.plot.number * 2
              log(s"Increase funding by $delta (Plot number times 2, non-Muslim Good country)")
              increaseFunding(delta)
            }
            else {
              val delta = mapPlot.plot.number
              log(s"Increase funding by $delta (Plot number, non-Muslim country)")
              increaseFunding(delta)
            }

            // Posture
            if (name == UnitedStates)
              rollUSPosture()
            else if (n.name == Nigeria && mapPlot.plot != Plot1) {
              // rule 11.3.3.3  (Nigeria)
              game = game.updateCountry(DefaultMuslimNigeria.copy(
                sleeperCells = n.sleeperCells,
                activeCells  = n.activeCells,
                cadres       = n.cadres,
                plots        = n.plots,
                markers      = n.markers,
                wmdCache     = n.wmdCache
              ))
              log("Flip Nigeria over to its Muslim side", Color.Event)
              log("Nigeria is now a Poor Neutral Muslim country", Color.Event)
              logWorldPosture()
            }
            else if (n.canChangePosture) {
              rollCountryPosture(n.name)
              if (n.isSchengen)
                if (game.botRole == Jihadist) {
                  val omit = Set(n.name)
                  val s1 = JihadistBot.posturePriority(Schengen.filterNot(_ == n.name)).get
                  val s2 = JihadistBot.posturePriority(Schengen.filterNot(x => x == n.name || x == s1)).get
                  log(s"Jihadist selects two other Schengen countries: $s1 and $s2")
                  rollCountryPosture(s1)
                  rollCountryPosture(s2)
                }
                else {
                  println("Select two other Schengen countries for posture rolls")
                  val s1 = askCountry("First Schengen country: ", Schengen, allowAbort = false)
                  val s2 = askCountry("Second Schengen country: ", Schengen.filterNot(_ == s1), allowAbort = false)
                  log(s"Jihadist selects two other Schengen countries: $s1 and $s2")
                  rollCountryPosture(s1)
                  rollCountryPosture(s2)
                }
            }

            // Sequestration
            if (mapPlot.plot == PlotWMD && game.sequestrationTroops) {
              log("\nWMD plot resolved while Sequestration in effect", Color.Event)
              removeGlobalEventMarker(Sequestration)
            }
            // Prestige
            if (name == UnitedStates)
              rollPrestige()
        } // match
        log() // blank line before next one
      } // for
      // Move all of the plots to the resolved plots box.

      val wmdCount   = game.countries.foldLeft(0) { (num, c) => num + (c.plots count (_.plot == PlotWMD)) }
      val otherCount = game.countries.foldLeft(0) { (num, c) => num + (c.plots count (_.plot != PlotWMD)) }
      if (wmdCount > 0)
        displayLine(s"Remove the ${amountOf(wmdCount, "resolved WMD plot")} from the game", Color.MapPieces)

      if (otherCount > 0)
        if (game.useExpansionRules)
          displayLine(
            s"Put the ${amountOf(otherCount, "resolved non-WMD plot")} in the resolved plots box",
            Color.MapPieces)
        else
          displayLine(
            s"Put the ${amountOf(otherCount, "resolved non-WMD plot")} in the available plots box",
            Color.MapPieces)

      (game.countries.filter(_.hasPlots)) foreach {
        case m: MuslimCountry =>
          val (removed, resolved) = m.plots
            .map(_.plot)
            .partition(_ == PlotWMD)
          val updatedPlots = if (game.useExpansionRules)
            game.plotData.copy(
              resolvedPlots = resolved ::: game.resolvedPlots,
              removedPlots  = removed  ::: game.removedPlots)
          else
            game.plotData.copy(
              availablePlots = resolved ::: game.availablePlots,
              removedPlots  = removed   ::: game.removedPlots)

          game = game.updateCountry(m.copy(plots = Nil)).copy(plotData = updatedPlots)
        case n: NonMuslimCountry =>
          val (removed, resolved) = n.plots
            .map(_.plot)
            .partition (_ == PlotWMD)
          val updatedPlots = if (game.useExpansionRules)
            game.plotData.copy(
              resolvedPlots = resolved ::: game.resolvedPlots,
              removedPlots  = removed  ::: game.removedPlots)
          else
            game.plotData.copy(
              availablePlots = resolved ::: game.availablePlots,
              removedPlots  = removed   ::: game.removedPlots)
          game = game.updateCountry(n.copy(plots = Nil)).copy(plotData = updatedPlots)
      }
      pause()
    }
    val targets = unblocked.map(u => PlotTarget(u.name, u.isMuslim)).toSet
    game = game.copy(
      plotData = game.plotData.copy(
        resolvedTargets       = targets,
        resolvedInGreenOnBlue = greenOnBlue
      )
    )
    return unblocked.size
  }

  // Pirates from the base game
  def pirates1ConditionsInEffect: Boolean = List(Somalia, Yemen)
    .map(game.getMuslim)
    .exists(_.isIslamistRule)

  // Pirates from the awakening expansion
  def pirates2ConditionsInEffect: Boolean = List(Somalia, Yemen)
    .map(game.getMuslim)
    .exists(m => (m.isPoor && m.isAdversary) || m.isIslamistRule)


  // Calculate and display the number of cards
  // each side will draw for the turn.
  def drawCardsForTurn(): Unit = {
    val jihadistCards = new ListBuffer[(String, Int)]()
    val usCards = new ListBuffer[(String, Int)]()

    jihadistCards.append((s"Funding level is ${game.fundingLevel}", JihadistCardDraw(game.fundingLevel)))

    usCards.append((s"Troop commitment is ${game.troopCommitment}", USCardDraw(game.troopCommitment)))

    // The number of cards drawn by the US player
    // can be affected by several events.
    if (lapsingEventInPlay(FullyResourcedCOIN))
      usCards.append((deck(FullyResourcedCOIN).cardName, 2))

    if (globalEventInPlay(USChinaTradeWar)) {
      val modifier = game.worldPosture match {
        case Even => 0
        case world if world == game.usPosture => 1
        case _ => -1
      }
      usCards.append((USChinaTradeWar, modifier))
    }

    val jihadistNum = jihadistCards.map(_._2).sum
    val usNum = usCards.map(_._2).sum
    val totalCards = jihadistNum + usNum

    log()
    log("Draw Cards", Color.Info)
    log(separator(char = '='), Color.Info)

    // Check to see if we have reached the end of the current draw pile
    if (numCardsInDrawPile() < totalCards) {
      log("There are not enough cards in the draw pile to fill both hands.", Color.Info)
      handleEmptyDrawPile(atEndOfTurn = true)
      log()
    }

    // for ((role, cardDraw) <- List((Jihadist, jihadistCards, US, usCards)))
    log(Jihadist.toString)
    log(separator(length = 40))
    for ((name, amt) <- jihadistCards)
      log(f"$amt%2d - $name")
    log()
    log(US.toString)
    log(separator(length = 40))
    for ((name, amt) <- usCards)
      log(f"$amt%2d - $name")

    log(s"\nDraw cards alternately into the $Jihadist and $US hands", Color.Info)
    log(s"drawing $jihadistNum cards and $usNum cards respectively.", Color.Info)

    increaseCardsInHand(Jihadist, jihadistNum)
    increaseCardsInHand(US, usNum)
  }

  def removeLapsingAnd1stPLot(): Unit = {
    game.firstPlotEntry foreach {
      case LapsingEntry(num, discarded) =>
        log("\nFirst Plot")
        log(separator())
        if (discarded) {
          log(s"Remove first plot marker", Color.Event)
          game = game.copy(firstPlotEntry = None)
        }
        else  {
          log(s"Discard ${cardNumAndName(num)}", Color.Event)
          game = game.copy(firstPlotEntry = None, cardsDiscarded = num :: game.cardsDiscarded)
        }
    }

    if (game.eventsLapsing.nonEmpty) {
      log()
      log("Lapsing Events")
      log(separator())
      removeLapsingEvents(game.eventsLapsing.map(_.cardNumber), endOfTurn = true)
    }
  }


  // Note that the end of turn sequence is different depending on whether
  // or not the Awakening expansion rules are in effect.
  def endTurn(): Unit = {
    def returnUsedPlotsToAvailable(): Unit = {
      if (game.resolvedPlots.nonEmpty) {
        val num = game.resolvedPlots.size
        val updatedPlots = game.plotData.copy(
          resolvedPlots  = Nil,
          availablePlots = game.availablePlots ::: game.resolvedPlots)
        game = game.copy(plotData = updatedPlots)
        log()
        log(s"Return ${amountOf(num, "resolved plot")} to the available plots box", Color.MapPieces)
      }
    }

    def flipGreenRegimeChangeMarkers(): Unit = {
      for (rc <- game.muslims if rc.regimeChange == GreenRegimeChange) {
        game = game.updateCountry(rc.copy(regimeChange = TanRegimeChange))
        log(s"\nFlip green regime change marker in ${rc.name} to its tan side", Color.FlipPieces)
      }
    }

    // Check active Gobal events that affect off map troops
    // Troops affected by Lapsing events will have already been
    // taken care of when the Lapsing event were removed.
    def returnOffMapTroopsToTrack(): Unit = {
      // If Sequestration troops are off map and there is a 3 Resource country at IslamistRule
      // then return the troops to available.
      if (game.sequestrationTroops) {
        game.muslims
          .find(m => m.resourceValue >= 3 && m.isIslamistRule)
          .map(_.name)
          .foreach { name =>
            log(s"\nThere is a 3 Resource Muslim country at Islamist Rule ($name)", Color.Event)
            removeGlobalEventMarker(Sequestration)
          }
      }

      if (globalEventInPlay(SouthChinaSeaCrisis) && game.usPosture == game.getNonMuslim(China).posture) {
        log("\nChina and the US have the same Posture", Color.Event)
        removeGlobalEventMarker(SouthChinaSeaCrisis)
      }
    }

    val labyrinthOrder = !game.useExpansionRules
    val awakeningOrder = game.useExpansionRules

    log()
    log(s"End of turn ${game.turn}", Color.Info)
    log(separator(char='='), Color.Info)

    if ((globalEventInPlay(Pirates1) && pirates1ConditionsInEffect) ||
        (globalEventInPlay(Pirates2) && pirates2ConditionsInEffect)) {
      log("No funding drop because Pirates is in effect")
    }
    else {
      log("Jihadist funding drops by 1")
      decreaseFunding(1)
    }
    if (globalEventInPlay(Fracking)) {
      log(s"Jihadist funding drops by 1 because Fracking is in effect")
      decreaseFunding(1)
    }
    if (game.numIslamistRule > 0) {
      log(s"US prestige drops by 1 (At least 1 country is under Islamist Rule)")
      decreasePrestige(1)
    }
    else
      log(s"US prestige stays at ${game.prestige} (No countries under Islamist Rule)", Color.MapMarker)

    val (worldPosture, level) = game.gwot
    if (game.usPosture == worldPosture && level == 3) {
      log(s"World posture is $worldPosture $level and US posture is ${game.usPosture}")
      log(s"US prestige increases by 1")
      increasePrestige(1)
    }

    if (game.useExpansionRules) {
      // Awakening Rule End of Turn order
      clearReserves(game.humanRole)  // The Bot's reserves are not cleared
      returnUsedPlotsToAvailable()
      polarization()
      endTurnCivilWarAttrition()
      // Polarization/Attrition could affect the score
      // Will Exit game if auto victory has been achieved
      checkAutomaticVictory()
      drawCardsForTurn()
      removeLapsingAnd1stPLot()
      returnOffMapTroopsToTrack()
      flipGreenRegimeChangeMarkers()
    }
    else {
      // Labyrinth rule end of turn order
      removeLapsingAnd1stPLot()
      clearReserves(game.humanRole)  // The Bot's reserves are not cleared
      drawCardsForTurn()
      flipGreenRegimeChangeMarkers()
    }

    // Reset history list of plays. They are not stored in turn files.
    val turnNum = game.turn
    game = game.copy(turnActions = Nil)
    game = game.copy(turn = game.turn + 1)
    saveGameState(Some(s"End of turn $turnNum"), endOfTurn = true)
    pause()
  }

  // Take troops from available if possible, otherwise we must
  // ask the user where to take them from.
  def selectTroopsToPutOffMap(numToRemove: Int): List[MapItem] = {
    val numFromTrack = numToRemove min game.troopsAvailable
    val numFromMap   = numToRemove - numFromTrack
    val items = new ListBuffer[MapItem]()

    if (numFromTrack > 0)
      items += MapItem("track", numFromTrack)

    if (numFromMap > 0) {
      val candidates = game.countries
        .filter(_.troops > 0)
        .map(c => MapItem(c.name, c.troops))
        .sortBy(_.country)

      if (numFromTrack == 0)
        println("\nThere are no troops on the troop track to remove to out of play")
      else
        println(s"\n${amountOf(numFromTrack, "troop")} will be removed from the troops track to out of play")

      println(s"\nSelect ${amountOf(numFromMap, "troop")} from the map to remove to the out of play box")
      items ++= askMapItems(candidates, numFromMap, "troop")
    }

    items.toList
  }


  def returnOffMapTroopsForLapsingCard(lapsingCard: Int): Unit = {
    lapsingCard match {
      case EbolaScare =>
        log(s"\nEbola Scare ends", Color.Event)
        moveOfMapTroopsToTrack(1)
      case KoreanCrisis =>
        log(s"\nKorean Crisis ends", Color.Event)
        moveOfMapTroopsToTrack(2)
      case USBorderCrisis =>
        log(s"\nUS Border Crisis ends", Color.Event)
        moveOfMapTroopsToTrack(1)
      case _ =>
    }
  }

  //  If a Lapsing card that removed troops to the off map box is removed by an event
  //  (eg. 353 Bowling Green Massacre) then the troops should  be moved back to the
  //  troops track.
  //  When called by the end of turn code, this should be false because the timing of the
  //  removal affects the US card draw and so the troop restoration must be deferred until
  // after the card draw.
  def removeLapsingEvents(targets: List[Int], endOfTurn: Boolean = false): Unit = {
    val candidates = game.eventsLapsing
      .filter(e => targets.contains(e.cardNumber))
    val (remove, discards) = candidates
      .partition(e => deck(e.cardNumber).remove != NoRemove)

    def eventNames(events: List[LapsingEntry]) = events
      .sorted
      .map(e => cardNumAndName(e.cardNumber))

    if (remove.nonEmpty)
      wrap(s"Remove lapsing cards from the game: ", remove)
        .foreach(line => log(line, Color.Event))

    if (discards.nonEmpty) {
      val (markers, cards) = discards.partition(_.discarded)
      if (cards.nonEmpty)
        wrap(s"Discard lapsing cards: ", cards)
          .foreach(line => log(line, Color.Event))

      if (markers.nonEmpty)
        wrap(s"Remove lapsing markers: ", markers)
          .foreach(line => log(line, Color.Event))
    }

    for (cardNum <- targets)
      cardNum match {
        case TheDoorOfItjihad if !endOfTurn =>
          if (game.humanRole == Jihadist)
            log(s"\nThe $Jihadist player no longer plays cards at random.", Color.Event)
          else
            log(s"\nThe $Jihadist Bot may again play Non-US associated events.", Color.Event)

        case _ =>
          returnOffMapTroopsForLapsingCard(cardNum)
      }

    val candidateSet = candidates.map(_.cardNumber).toSet
    val toDiscarded = discards.filterNot(_.discarded).map(_.cardNumber)
    val toRemoved = remove.map(_.cardNumber)
    game = game.copy(
      eventsLapsing = game.eventsLapsing.filterNot(e => candidateSet(e.cardNumber)),
      cardsDiscarded = toDiscarded ::: game.cardsDiscarded,
      cardsRemoved = toRemoved ::: game.cardsRemoved
    )
  }

  // Remove a single lapsing event
  def removeLapsingEvent(target: Int): Unit =
    removeLapsingEvents(target::Nil)


  val scenarios = ListMap[String, Scenario](
    "LetsRoll"            -> LetsRoll,
    "YouCanCallMeAl"      -> YouCanCallMeAl,
    "Anaconda"            -> Anaconda,
    "MissionAccomplished" -> MissionAccomplished,
    "Awakening"           -> Awakening,
    "MittsTurn"           -> MittsTurn,
    "StatusOfForces"      -> StatusOfForces,
    "IslamicStateOfIraq"  -> IslamicStateOfIraq,
    "FallOfISIL"          -> FallOfISIL,
    "TrumpTakesCommand"   -> TrumpTakesCommand,
    "HillaryWins"         -> HillaryWins,
    // Commented out because the Surge scenario would require a lot of
    // special case code to ignore Civil War, Militia, etc.
    // "Surge"               -> Surge,
  )

  def askScenarioName(): Option[String] = {
    val gameChoices = List(
      Some(LabyrinthMode)  -> "Labyrinth: The War on Terror, 2001 - ?",
      Some(AwakeningMode)  -> "Labyrinth The Awakening, 2010 - ?",
      Some(ForeverWarMode) -> "Labyrinth The Forever War, 2015 - ?",
      None                 -> "Cancel"
    )

    askMenu("Play a scenario from which game:", gameChoices, allowAbort = false).head.flatMap { mode =>
      val scenarioChoices = scenarios.toList
        .filter(_._2.startingMode == mode)
        .map { case (key, scenario) => Some(key) -> scenario.name }
      val choices = scenarioChoices :+ (None -> "Cancel")

      askMenu("Choose a scenario:", choices, allowAbort = false).head
    }
  }
  // Case sensitive
  def isValidScenario(name: String) = scenarios contains name

  // Returns (gameLength, campaign)
  def askGameStyle(scenario: Scenario): Option[(Int, Boolean)] = {
    val campLen = (scenario.allowsCampaign, scenario.startingMode) match {
      case (true, LabyrinthMode) => 3
      case (true, AwakeningMode) => 2
      case _ => 0
    }

    val choices = List(
      choice(true,        Some(1 -> false),        "Single scenario (1 deck)"),
      choice(true,        Some(2 -> false),        "Single scenario (2 decks)"),
      choice(true,        Some(3 -> false),        "Single scenario (3 decks)"),
      choice(campLen > 0, Some(campLen -> true),  s"Campaign game   ($campLen decks)"),
      choice(true,        None,                    "Cancel"),
    ).flatten

    askMenu("Choose game style:", choices, allowAbort = false).head
  }

  // ask which side the user wishes to play
  def askHumanSide(): Option[Role] = {
    val choices = List(
      Some(US)       -> "Play as US",
      Some(Jihadist) -> "Play as Jihadist",
      None           -> "Cancel")
    askMenu("Choose a side:", choices, allowAbort = false).head
  }

  // ask which side the user wishes to play
  def askEnhBotDifficulty(bot: Role): EnhBotDifficulty = {
    val human = bot.opponent
    val easyDesc = Seq(
      s"$human associated events are triggered during the $bot turn when the",
      s"event conditions are met.  When an event triggers, the # of Ops",
      s"on the $human associated card are added to $bot reserves.")
    val mediumDesc = Seq(
      s"When $bot reserves = 0 $human associated events are triggered during the $bot turn",
      s"when the event conditions are met.  When an event triggers, the # of Ops",
      s"on the $human associated card are added to $bot reserves.",
      s"When $bot reserves > 0 $human associated events are NOT triggered during the $bot turn.",
      s"Instead the # of Ops on the $human associated card are subtracted from $bot reserves.")
    val hardDesc = Seq(
      s"$human associated events are never triggered during the $bot turn.")
    val choices = List(
      EnhBotEasy   -> ("Easy", easyDesc),
      EnhBotMedium -> ("Medium", mediumDesc),
      EnhBotHard   -> ("Hard", hardDesc))
    askMenuWithWrap(s"\nSelect enhanced $bot Bot difficulty level:", choices, delim = "  ", sameLine = false, allowAbort = false)
  }


  def programMainMenu(params: UserParams): Unit = {

    def gameChoices(): List[(Option[String], (String, Seq[String]))] = {
      val choices = savedGames()
        .toList
        .map { name =>
          val summary = loadGameDescription(name)
            .split(",")
            .toSeq
            .map(_.trim)
            .dropWhile(_ == "")
          Some(name) -> (name, summary)
        }
      choices :+ (None -> ("Cancel", Seq.empty))
    }
    val games = savedGames()
    sealed trait MenuChoice
    case object New extends MenuChoice
    case object Resume extends MenuChoice
    case object Delete extends MenuChoice
    case object Exit extends MenuChoice

    val choices = List(
      choice(true, New, "Start a new game"),
      choice(games.nonEmpty, Resume, "Resume a saved game"),
      choice(games.nonEmpty, Delete, "Delete a saved game"),
      choice(true, Exit, "Exit the program"),
    ).flatten

    askMenu("Game Menu", choices).head match {
      case New =>
        startNewGame(params)
        programMainMenu(params)

      case Resume =>
        askMenuWithWrap("Resume which game:", gameChoices(), sameLine = false, allowAbort = false)
          .foreach { name =>
            game = loadMostRecent(name)
            printSummary(game.actionSummary)
            playGame()
          }
        programMainMenu(params)

      case Delete =>
        askMenuWithWrap("Delete which game:", gameChoices(), sameLine = false, allowAbort = false)
          .foreach { name =>
            if (askYorN(s"\nReally delete game [$name] (y/n)? "))
              (gamesDir/name).rmtree()
          }
        programMainMenu(params)

      case Exit =>
        throw ExitProgram
    }
  }

  def askEnhancedBot(): Option[Boolean] = {
    val choices = List(
      Some(false) -> "Standard Awakening Jihadist Bot",
      Some(true)  -> "Enhanced Jihadist Bot",
      None        -> "Cancel"
    )
    askMenu("\nChoose Bot opponent:", choices).head
  }

  // Prompt user for needed info and begin a new game.
  def startNewGame(params: UserParams): Unit = {
    case object CancelNewGame  extends Exception
    try {
      val scenarioName = params.scenarioName
            .orElse(askScenarioName())
            .getOrElse(throw CancelNewGame)
      val scenario = scenarios(scenarioName)
      val (gameLength, campaign) = askGameStyle(scenario)
        .getOrElse(throw CancelNewGame)
      val humanRole = params.side
        .orElse(askHumanSide())
        .getOrElse(throw CancelNewGame)

      val enhancedBot = if (humanRole == US)
        params.enhancedBot
          .orElse(askEnhancedBot())  // TODO: true for testing, This should be false
          .getOrElse(throw CancelNewGame)
      else
        false  // No enhanced US Bot so don't bother asking

      val difficulties = (humanRole, enhancedBot) match {
        case (US, true) =>
          List(Muddled)
        case (Jihadist, true) =>
          List(OffGuard)
        case (US, false) =>
          params.jihadistBotDifficulties
            .getOrElse(askDifficulties(Jihadist))
        case (Jihadist, false) =>
          params.usBotDifficulties
            .getOrElse(askDifficulties(US))
      }

      val enhBotDifficulty = (humanRole, enhancedBot) match {
        case (US, true) =>
          params.enhBotDifficulty
            .getOrElse(askEnhBotDifficulty(Jihadist))
        case (Jihadist, true) =>
          params.enhBotDifficulty
            .getOrElse(askEnhBotDifficulty(US))
        case (_, false) =>
          EnhBotHard
      }

      val humanAutoRoll = params.autoDice
        .getOrElse(!askYorN("\nDo you wish to roll your own dice (y/n)? "))

      val saveName = askGameName("\nEnter a name for your new game (blank to cancel): ")
        .getOrElse(throw CancelNewGame)

      val showColor = params.showColor
        .getOrElse(!scala.util.Properties.isWin)

      game = initialGameState(
        saveName,
        scenario,
        gameLength,
        campaign,
        humanRole,
        humanAutoRoll,
        difficulties,
        showColor,
        enhancedBot,
        enhBotDifficulty)

      logSummary(game.scenarioSummary)
      printSummary(game.scoringSummary)

      if (scenario.cardsRemoved.nonEmpty) {
        log()
        log("The following cards are removed for this scenario")
        log(separator())
        scenario
          .cardsRemoved
          .map(deck(_).toString)
          .foreach(log(_))
      }

      scenario.additionalSetup()

      game = game.copy(turn = 1)
      drawCardsForTurn()  // Display and save card draw for first turn
      saveGameState(Some("Beginning of game"))
      pause()
      playGame()
    }
    catch {
      case CancelNewGame =>
    }
  }

  val AbortCard = "abort card"
  case object ExitProgram extends Exception
  case object QuitGame    extends Exception
  case object AbortAction extends Exception
  case object CancelledByFerguson extends Exception

  def versionString = {
    val versionSuffix  = if (SOFTWARE_VERSION.startsWith("0")) " - BETA" else ""
    s"Labyrinth Awakening: Bot Software (version $SOFTWARE_VERSION$versionSuffix)"
  }
  def displayVersion() = displayLine(s"\n$versionString", Color.Info)

  // def doWarOfIdeas(country: Country)
  def main(args: Array[String]): Unit = {
    try {

      gamesDir.mkpath()  // Make sure the /games directory exists
      displayVersion()

      var configParams   = loadParamsFile(UserParams())
      var cmdLineParams  = parseCommandLine(args.toIndexedSeq, configParams)

      if (cmdLineParams.listGames) {
        // List the saved games to the console and exit
        val saved = savedGames()
        if (saved.isEmpty)
          println("You do not have any saved games")
        else {
          val fmt = "%%-%ds - %%s".format(longestString(saved))
          for (s <- saved)
            println(fmt.format(s, loadGameDescription(s)))
        }
      }
      else if (cmdLineParams.resumeName.nonEmpty) {
        game = loadMostRecent(cmdLineParams.resumeName.get)
        printSummary(game.actionSummary)
        playGame()
      }
      else if (cmdLineParams.deleteName.nonEmpty) {
        (gamesDir/cmdLineParams.deleteName.get).rmtree()
      }
      else
        programMainMenu(cmdLineParams)
    }
    catch {
      case ExitProgram =>
      case t: Throwable =>
        System.err.println(t.stackTrace)
        pause(Some("Press Enter to exit the program..."))
    }
  }

  def parseCommandLine(args: Seq[String], userParams: UserParams): UserParams = {
    import org.sellmerfud.optparse._
    def diffHelp(diffs: Seq[BotDifficulty]): Seq[String] = {
      val maxLen = longestString(diffs.map(_.name))
      val fmt = "%%-%ds - %%s".format(maxLen)
      diffs.map(d => fmt.format(d.name, d.description))
    }
    case class JihadDiff(diff: BotDifficulty)
    case class USDiff(diff: BotDifficulty)
    try {
      new OptionParser[UserParams] {
        addArgumentParser[JihadDiff] { arg =>
          if (isValidIdeology(arg))
            JihadDiff(BotDifficulty(arg))
          else
            throw new InvalidArgumentException(s"  Invalid Jihadist ideology value '$arg'")
        }
        addArgumentParser[USDiff] { arg =>
          if (isValidUsResolve(arg))
            USDiff(BotDifficulty(arg))
          else
            throw new InvalidArgumentException(s"  Invalid US resolve value '$arg'")
        }
        banner = "usage: awakening [options]"
        this.separator("")
        this.separator("Options:")
        val saved = savedGames()
        if (saved.isEmpty)
          reqd[String]("-g", "--game=name", "Resume a game in progress")
            { (v, c) => throw new InvalidArgumentException("  You do not have any saved games") }
        else
          reqd[String]("-g", "--game=name", saved, "Resume a game in progress")
            { (v, c) => c.copy(resumeName = Some(v)) }

        flag("-l", "--list", "Display a list of saved games")
          { (c) => c.copy(listGames = true) }

        if (saved.isEmpty)
          reqd[String]("", "--delete=name", "Delete a game in progress")
            { (v, c) => throw new InvalidArgumentException("  You do not have any saved games") }
        else
          reqd[String]("", "--delete=name", saved, "Delete a game in progress")
            { (v, c) => c.copy(deleteName = Some(v)) }

        val scenarioHelp = "Select a scenario" +: scenarios.keys.toSeq
        reqd[String]("", "--scenario=name", scenarios.keys.toSeq, scenarioHelp: _*)
          { (v, c) => c.copy(scenarioName = Some(v)) }

        reqd[String]("", "--side=us|jihadist", Seq("us","jihadist"), "Select a side to play")
          { (v, c) => c.copy(side = Some(if (v == "us") US else Jihadist)) }

        reqd[Int]("", "--level=n", Seq.range(1, 7), "Select difficulty level (1 - 6)")
          { (v, c) => c.copy(level = Some(v)) }

        reqd[String]("", "--dice=auto|human", Seq("auto", "human"), "How to roll the human player's dice",
                                                             "auto  - the program rolls them automatically",
                                                             "human - you enter your dice rolls manually")
          { (v, c) => c.copy(autoDice = Some(v == "auto")) }

        list[JihadDiff]("", "--ideology=x,y,z",
                       ("Comma separated list of Jihadist ideology values" +: diffHelp(AllJihadistLevels)): _*) {
          (v, c) =>
            val values = v.map { case JihadDiff(diff) => diff }.sorted.distinct
            c.copy(ideology = values)
        }
        list[USDiff]("", "--us-resolve=x,y,z",
                     ("Comma separated list of US resolve values" +: diffHelp(AllUSLevels)): _*) {
          (v, c) =>
          val values = v.map { case USDiff(diff) => diff }.sorted.distinct
          c.copy(usResolve = values)
        }
        val enhBotDiffValues = EnhBotDifficulty.All.map(d => d.name -> d).toMap
        reqd[EnhBotDifficulty]("", "--enh-bot-difficulty", enhBotDiffValues, "Enhanced Bot difficulty level")
          { (v, c) => c.copy(enhBotDifficulty = Some(v)) }
        bool("", "--color", "Show colored log messages")
          { (v, c) => c.copy(showColor = Some(v)) }
        bool("", "--enhanced-bot", "Use enhanced Bot (Jihadist Bot only)")
          { (v, c) => c.copy(enhancedBot = Some(v)) }
        flag("-v", "--version", "Display program version and exit") { (c) =>
          println(versionString)
          System.exit(0)
          c // To keep compiler happy
        }
      }.parse(args, userParams)
    }
    catch { case e: OptionParserException => println(e.getMessage); sys.exit(1) }
  }



  case class UserParams(
    val resumeName: Option[String] = None,
    val deleteName: Option[String] = None,
    val listGames: Boolean = false,
    val scenarioName: Option[String] = None,
    val campaign: Option[Boolean] = None,
    val gameLength: Int = 1,
    val side: Option[Role] = None,
    val level: Option[Int] = None,
    val autoDice: Option[Boolean] = None,
    val ideology: List[BotDifficulty] = Nil,
    val usResolve: List[BotDifficulty] = Nil,
    val enhBotDifficulty: Option[EnhBotDifficulty] = None,
    val showColor: Option[Boolean] = None,
    val enhancedBot: Option[Boolean] = None,
  ) {

    def jihadistBotDifficulties: Option[List[BotDifficulty]] = ideology match {
      case Nil => level.map(AllJihadistLevels.take(_))
      case xs  => Some(xs)
    }

    def usBotDifficulties: Option[List[BotDifficulty]] = usResolve match {
      case Nil => level.map(AllUSLevels.take(_))
      case xs  => Some(xs)
    }
  }

  def loadParamsFile(initialParams: UserParams): UserParams = {
    import java.util.Properties
    import scala.util.Properties.userHome

    def readConfig(path: Pathname): UserParams = {
      displayLine(s"using config file: [$path]")
      try {
        var params = initialParams
        val props = new Properties()
        def propValue(name: String): Option[String] =
          Option(props.getProperty(name)).map(_.trim) match {
            case Some("") => None  // Treat empty values a non-existent
            case x        => x
          }
        path.reader { r => props.load(r) }

        propValue("scenario") foreach { value =>
          if (isValidScenario(value))
            params = params.copy(scenarioName = Some(value))
          else
            println(s"Ignoring invalid scenario name ($value) in awakening_config file")
        }

        propValue("side") foreach { value =>
          value.toLowerCase match {
            case "jihadist" => params = params.copy(side = Some(Jihadist))
            case "us"       => params = params.copy(side = Some(US))
            case _ => println(s"Ignoring invalid side name ($value) in awakening_config file")
          }
        }

        propValue("level") foreach { value =>
          if (List("1","2","3","4","5","6") contains value)
            params = params.copy(level = Some(value.toInt))
          else
            println(s"Ignoring invalid level ($value) in awakening_config file")
        }

        propValue("dice") foreach { value =>
          value.toLowerCase match {
            case "auto"  => params = params.copy(autoDice = Some(true))
            case "human" => params = params.copy(autoDice = Some(false))
            case _ => println(s"Ignoring invalid dice value ($value) in awakening_config file")
          }
        }

        propValue("ideology") foreach { value =>
          val tokens = value.split(",")
            .toList
            .map(_.trim)
            .filterNot(_ == "")
          if (tokens forall isValidIdeology)
            params = params.copy(ideology = tokens.distinct.map(BotDifficulty.apply).sorted)
          else
            println(s"Ignoring invalid ideology value ($value) in awakening_config file")
        }

        propValue("us-resolve") foreach { value =>
          val tokens = value.split(",")
            .toList
            .map(_.trim)
            .filterNot(_ == "")
          if (tokens forall isValidUsResolve)
            params = params.copy(usResolve = tokens.distinct.map(BotDifficulty.apply).sorted)
          else
            println(s"Ignoring invalid us-resolve value ($value) in awakening_config file")
        }

        propValue("color") foreach { value =>
          value.toLowerCase match {
            case "yes" => params = params.copy(showColor = Some(true))
            case "no"  => params = params.copy(showColor = Some(false))
            case _ => println(s"Ignoring invalid color value ($value) in awakening_config file")
          }
        }

        propValue("enhanced-bot") foreach { value =>
          value.toLowerCase match {
            case "yes" => params = params.copy(enhancedBot = Some(true))
            case "no"  => params = params.copy(enhancedBot = Some(false))
            case _ => println(s"Ignoring invalid enhanced-bot value ($value) in awakening_config file")
          }
        }

        propValue("enhanced-bot-difficulty") foreach { value =>
          EnhBotDifficulty.fromStringOpt(value.toLowerCase) match {
            case Some(difficulty) => params = params.copy(enhBotDifficulty = Some(difficulty))
            case None =>println(s"Ignoring invalid enhanced-bot value ($value) in awakening_config file")
          }            
        }

        params
      }
      catch {
        case e: Throwable =>
          println(s"Error reading $path: ${e.getMessage}")
          initialParams
      }
    }

    // Use the first config file that we find or return the
    // initial params there isn't any.
    val xdg_config = sys.env.get("XDG_CONFIG_HOME")
      .map(s => Pathname(s))
      .getOrElse(Pathname(userHome)  / ".config")
    List(
      Pathname(".") / "test_config",
      Pathname(".") / "awakening_config",
      Pathname(userHome) / "awakening_config",
      Pathname(userHome) / ".awakening_config",
      xdg_config / "awakening" / "awakening_config")
      .find(path => path.exists && path.isReadable)
      .map(readConfig)
      .getOrElse(initialParams)
  }

  val AllUSLevels = List(OffGuard,Competent,Adept,Vigilant,Ruthless,NoMercy)
  val AllJihadistLevels  = List(Muddled,Coherent,Attractive,Potent,Infectious,Virulent)

  val USLevels = Map(
    OffGuard.name  -> List(OffGuard),
    Competent.name -> List(OffGuard,Competent),
    Adept.name     -> List(OffGuard,Competent,Adept),
    Vigilant.name  -> List(OffGuard,Competent,Adept,Vigilant),
    Ruthless.name  -> List(OffGuard,Competent,Adept,Vigilant,Ruthless),
    NoMercy.name   -> List(OffGuard,Competent,Adept,Vigilant,Ruthless,NoMercy))

  val JihadistLevels = Map(
    Muddled.name    -> List(Muddled),
    Coherent.name   -> List(Muddled,Coherent),
    Attractive.name -> List(Muddled,Coherent,Attractive),
    Potent.name     -> List(Muddled,Coherent,Attractive,Potent),
    Infectious.name -> List(Muddled,Coherent,Attractive,Potent,Infectious),
    Virulent.name   -> List(Muddled,Coherent,Attractive,Potent,Infectious,Virulent))

  def askDifficulties(botRole: Role): List[BotDifficulty] = {
    val levels = if (botRole == US) AllUSLevels else AllJihadistLevels
    val fmt = "%%-%ds".format(longestString(levels.map(_.name)))

    def nextChoice(num: Int, prev: Option[BotDifficulty], levels: List[BotDifficulty]): List[(String, String)] =
      (prev, levels) match {
        case (_, Nil) => Nil
        case (None, (d@BotDifficulty(_, name, desc)) :: rest) =>
          (num.toString -> s"${fmt.format(name)}: $desc") :: nextChoice(num+1, Some(d), rest)
        case (Some(BotDifficulty(_, pname, _)), (d@BotDifficulty(_, name, desc)) :: rest) =>
          (num.toString -> s"${fmt.format(name)}: $pname plus $desc") :: nextChoice(num+1, Some(d), rest)
      }

    levels.take(askMenu("Choose a difficulty level:", nextChoice(1, None, levels), allowAbort = false).head.toInt)
  }

  def isValidIdeology(name: String) =
    JihadistLevels.keys
      .exists(_.toLowerCase == name.toLowerCase)

  def isValidUsResolve(name: String) =
    USLevels.keys
      .exists(_.toLowerCase == name.toLowerCase)


  // Check to see if any automatic victory condition has been met.
  // Note: The WMD resolved in United States condition is checked by resolvePlots()
  def checkAutomaticVictory(): Unit = {
    def gameOver(victor: Role, reason: String): Unit = {
      val summary = s"Victory condition met - $victor automatic victory!"
      log()
      log(separator())
      log(summary, Color.Info)
      log(reason, Color.Info)

      if (askExitAfterWin()) {
        // game = game.copy(turnActions = Nil)
        saveGameState(Some(summary))
        throw QuitGame
      }
      else {
        game = game.copy(ignoreVictory = true)
        log(s"\nIgnoring $victor automatic victory.", Color.Info)
      }
    }

    if (!game.ignoreVictory) {
      if (game.goodResources >= 12)
        gameOver(US, s"${game.goodResources} resources controlled by countries with Good governance")
      else if (game.numGoodOrFair >= 15)
        gameOver(US, s"${game.numGoodOrFair} Muslim countries have Fair or Good governance")
      else if (game.totalCellsOnMap == 0 && game.humanRole == Jihadist)
        gameOver(US, s"There are no cells on the map")
      else if (game.islamistResources >= 6 && game.botRole == Jihadist)
        gameOver(Jihadist, s"${game.islamistResources} resources controlled by countries with Islamist Rule governance")
      else if (game.islamistResources >= 6 && game.islamistAdjacency) {
        val reason = s"${game.islamistResources} resources controlled by countries with Islamist Rule governance\n" +
                      "and at least two of the countries are adjacent"
        gameOver(Jihadist, reason)
      }
      else if (game.numPoorOrIslamic >= 15 && game.prestige == 1) {
        val reason = s"${game.numPoorOrIslamic} Muslim countries have Poor or Islamist rule governance " +
                      "and US prestige is 1"
        gameOver(Jihadist, reason)
      }
    }
  }

  def numUnresolvedPlots: Int = game.countries.foldLeft(0) { (sum, c) => sum + c.plots.size }


  sealed trait UserAction {
    def perform(param: Option[String]): Unit
  }

  case object PlayCard extends UserAction {
    override def perform(param: Option[String]): Unit =
      getActiveRole() match {
        case US => usCardPlay(param)
        case Jihadist => jihadistCardPlay(param)
      }
  }

  // This action is only take by the Human US player
  case object DiscardLast extends UserAction {
    override def perform(param: Option[String]): Unit = {
      askCardNumber(FromRole(US)::Nil, "Card # to discard (blank to cancel): ", param, allowAbort = false)
        .foreach { cardNumber =>
          decreaseCardsInHand(US, 1)
          addCardToDiscardPile(cardNumber)
          game = game.copy(turnActions = USDiscardedLastCard(cardNumber)::game.turnActions)
          saveGameState(Some(s"US player discards last card [${cardNumAndName(cardNumber)}]"))
      }
    }
  }

  case class EndPhase(holdLastCard: Boolean) extends UserAction {
    override def perform(param: Option[String]): Unit = {
      val activeRole = getActiveRole()
      val phaseNum = actionPhaseCount(activeRole) + 1
      val bothHandsEmpty = numCardsInHand(US) == 0 && numCardsInHand(Jihadist) == 0
      val holdMsg = if (holdLastCard)
        ", holding last card"
      else
        ""
      val endMsg = s"End of ${ordinal(phaseNum)} $activeRole action phase$holdMsg"

      log(s"\n$endMsg", Color.Info)
      // We resolve plots at the end of Jihadist action phase
      // only if both players have no cards left
      val numPlots = if (activeRole == US || bothHandsEmpty)
        resolvePlots()
      else
        0

      val endPhase = EndOfActionPhase(activeRole, phaseNum, numPlots)
      game = game.copy(
        targetsLastPhase = game.targetsThisPhase,
        targetsThisPhase = PhaseTargets(),
        turnActions = endPhase::game.turnActions
      )
      saveGameState(Some(endPhase.toString))


      if (bothHandsEmpty || (numCardsInHand(Jihadist) == 0 && holdLastCard)) {
        pause()  // So the user can see the plot resolution results
        endTurn()
      }
    }
  }

  case object RemoveCadre extends UserAction {
    override def perform(param: Option[String]): Unit = {
      val numRemoved = humanVoluntarilyRemoveCadre()
      if (numRemoved > 0) {
        game = game.copy(turnActions = VoluntaryCadreRemoval(numRemoved)::game.turnActions)
        saveGameState()
      }
    }
  }

  case object ShowState extends UserAction {
    override def perform(param: Option[String]): Unit = {
      showCommand(param)
    }
  }

  case object History extends UserAction {
    override def perform(param: Option[String]): Unit = {
      historyCommand(param)
    }
  }

  case object Rollback extends UserAction {
    override def perform(param: Option[String]): Unit = {
      rollback()
    }
  }

  case object Inspect extends UserAction {
    override def perform(param: Option[String]): Unit = {
      inspect()
    }
  }

  case object Adjust extends UserAction {
    override def perform(param: Option[String]): Unit = {
      adjustCommand(param)
    }
  }

  case object Quit extends UserAction {
    override def perform(param: Option[String]): Unit =
      throw QuitGame
  }

  case object Help extends UserAction {
    override def perform(param: Option[String]): Unit = {
      val help = """|
        |Action prompt help
        |-----------------------------------------------------------------------------
        |This prompt shows the actions that can be taken during the current
        |action phase.  The action(s) above the dividing line pertain to game play
        |and those below the line allow you to view and manage the state of the game.
        |
        |To execute an action you simply type the name of the action.
        |For simplcity you actually only have to type the first character of the name.
        |After entering an action you will be prompted for the information needed to
        |complete that action.
        |
        |To avoid these extra prompts you can follow the action with an argument.
        |For example to play card #35 you would type:
        |p 35<enter>
        |
        |And because playing a card is done so often, for this action only you can
        |omit the action name and simply enter the card number:
        |35<enter>
        |
        |Entering Show, History, or Adjust will display a menu of options.
        |Once you become familiar with these actions you may want to skip the menu
        |by following the action with an argument.
        |
        |Use the argument 'help' or '?' to see help for the specific action.
        |For example, to see help for the show command, you would enter:
        |s ?<enter>
        |-----------------------------------------------------------------------------
        """.stripMargin
      displayLine(help)
    }
  }

  def getActiveRole(): Role = {
    game.turnActions
      .collect { case eoap: EndOfActionPhase => eoap.role.opponent }
      .headOption
      .getOrElse(Jihadist)
  }

  def getCurrentPhaseCardPlays(): List[CardPlay] =
    game.turnActions
      .takeWhile {
        case _: EndOfActionPhase => false
        case _ => true
      }
      .collect { case a: CardPlay => a }

  def numCardsPlayedInCurrentPhase() = {
    getCurrentPhaseCardPlays()
      .map(_.numCards)
      .sum
  }

  def actionPhaseCount(role: Role) = {
    game.turnActions
      .count {
        case EndOfActionPhase(`role`, _, _) => true
        case _ => false
      }
  }


  // ---------------------------------------------------
  // This is the main prompt for taking an action during
  // the game.
  def actionPhasePrompt(): (UserAction, Option[String]) = {
    val PlayCardName = "Play card"
    def getResponse(prompt: String, options: Seq[String], allowInt: Boolean): Option[(String, Option[String])] = {
      val HELP = raw"(?i)(?:\?|he|hel|help)".r

      readLine(prompt) match {
        case null | "" => None
        case input =>
          val (cmd::param) = input.trim.split(" ", 2).toList
          cmd match {
            case "" => None
            case INTEGER(i) if allowInt => Some((PlayCardName, Some(i)))
            case HELP() => Some(("?", None))
            case s =>
              matchOne(s.trim, options) match {
                case Some(c) => Some((c, param.lastOption))
                case _ => getResponse(prompt, options, allowInt)
              }
          }
      }
    }

    val activeRole = getActiveRole()
    val numCardsPlayed = numCardsPlayedInCurrentPhase()
    val numInHand = numCardsInHand(activeRole)
    val isUSHuman = activeRole == US && isHuman(US)
    val isJihadistHuman = activeRole == Jihadist && isHuman(Jihadist)
    val canPlay = numCardsPlayed < 2 && numInHand > 0
    val canDiscard = isUSHuman && numInHand == 1 && !game.jihadistIdeology(Infectious)
    val canEndPhase = numCardsPlayed == 2 || numInHand == 0 ||
      (isUSHuman && numInHand == 1 && !game.jihadistIdeology(Infectious))
    val canCadre = isJihadistHuman && game.hasCountry(_.hasCadre)
    val canRoll = mostRecentSaveNumber(game.saveName).getOrElse(0) > 0

    def choice(test: Boolean, name: String, action: UserAction) = if (test)
      Some((name, action))
    else
      None
    val (holdLast, holdMsg) = if (isUSHuman && numInHand == 1)
      (true, " (holding last card)")
    else
      (false, "")
    val mainChoices = ListMap(List(
      choice(canPlay,     PlayCardName, PlayCard),
      choice(canDiscard,  "Discard last card (with no effect)", DiscardLast),
      choice(canCadre,    "Volunatary cadre removal", RemoveCadre),
      choice(canEndPhase, s"End action phase$holdMsg", EndPhase(holdLast)),
    ).flatten:_*)

    val otherChoices = ListMap(List(
      choice(true,    "Show", ShowState),
      choice(true,    "History", History),
      choice(canRoll, "Rollback", Rollback),
      choice(canRoll, "Inspect", Inspect),
      choice(true,    "Adjust", Adjust),
      choice(true,    "Quit", Quit),
      choice(true,    "?", Help),
    ).flatten:_*)

    val options = (mainChoices.keysIterator ++ otherChoices.keysIterator).toSeq
    val reserves = if (activeRole == Jihadist)
      s"${amountOf(game.reserves.jihadist, "Op")} in reserve"
    else
        s"${amountOf(game.reserves.us, "Op")} in reserve"
    val phaseNum = actionPhaseCount(activeRole) + 1
    val roleType = if (isHuman(activeRole)) "Player's" else "Bot's"
    val phase = s"$activeRole $roleType ${ordinal(phaseNum)} action phase"
    val turn = s"Turn ${game.turn}"
    val played = s"${amountOf(numCardsPlayed, "Card")} played"
    val drawPile = s"${numCardsInDrawPile()} in draw pile"
    val inHand = s"$numInHand in hand"
    val lineLen = 56

    def formatLine(left: String, right: String): String = {
      val padLen = lineLen - 4 - left.length - right.length
      val pad = " " * padLen
      s"| $left$pad$right |"

    }

    displayLine()
    displayLine(separator(lineLen, char = '='), Color.Info)
    displayLine(formatLine(phase, turn), Color.Info)
    displayLine(formatLine(played, inHand), Color.Info)
    displayLine(formatLine(reserves, drawPile), Color.Info)
    displayLine(separator(lineLen, char = '='), Color.Info)
    displayLine()
    for (display <- mainChoices.keysIterator)
      displayLine(display)
    displayLine(separator(lineLen), Color.Info)
    displayLine(otherChoices.keysIterator.mkString(" | "))
    displayLine(separator(lineLen), Color.Info)
    val numericItem = if (canPlay) Some(PlayCard) else None
    getResponse("Action: ", options, allowInt = canPlay) match {
      case Some((actionName, param)) =>
        val action = mainChoices.getOrElse(actionName, otherChoices(actionName))
        (action, param)
      case None => actionPhasePrompt()
    }
  }

  // ---------------------------------------------
  // Process user actions until the user quits the game
  // Assumes the global `game` variable has been initialized!
  def playGame(): Unit = {

    @tailrec def actionLoop(lastAction: Option[UserAction]): Unit = {
      checkAutomaticVictory() // Will Exit game if auto victory has been achieved
      if (getActiveRole() == Jihadist && isBot(Jihadist) && game.botEnhancements)
        if (JihadistBot.voluntaryCadreRemoval())
          saveGameState()


      // If we are at the end of the action phase allow the user
      // to bypass the menu
      val activeRole = getActiveRole()
      val numCardsPlayed = numCardsPlayedInCurrentPhase()
      val numInHand = numCardsInHand(activeRole)
      val owner = if (isHuman(activeRole))
        "your"
      else
        s"$activeRole Bot's"
      val isUSHuman = activeRole == US && isHuman(US)
      val canEndPhase = numCardsPlayed == 2 || numInHand == 0
      val promptCanFollowAction =
        lastAction == Some(PlayCard) ||
        lastAction == Some(DiscardLast) ||
        lastAction == Some(EndPhase(false))

      val promptLine1 = if (numCardsPlayed == 2)
        "Two cards have been played."
      else
        s"No cards remaining in $owner hand."
      val endPrompt =
        s"""|
            |$promptLine1
            |End the current $activeRole action phase? (y/n) """.stripMargin

      if (canEndPhase && promptCanFollowAction && askYorN(endPrompt)) {
        val action = EndPhase(false)
        action.perform(None)
        actionLoop(Some(action))
      }
      else {
        val (action, param) = actionPhasePrompt()
        action.perform(param)
        actionLoop(Some(action))
      }
    }

    try actionLoop(None)
    catch {
      // Expected exception, all others bubble up
      case QuitGame =>
    }
  }

  // The Jihadist play can voluntarily remove cadre markers on the map
  // (To avoid giving the US an easy prestige bump)
  // Returns number of cadres removed
  def humanVoluntarilyRemoveCadre(): Int = {
    var firstOne = true
    def nextCadre(): Int = {
      val candidates = countryNames(game.countries.filter(_.hasCadre))
      if (candidates.nonEmpty) {
        sealed trait Choice
        case class Target(name: String) extends Choice
        case object Cancel extends Choice
        val choices = candidates.map(n => Target(n) -> n) :+
          (Cancel -> "Finished removing cadres")
        askMenu(s"Remove cadre in which country: ", choices, allowAbort = false).head match {
          case Cancel =>
            0
          case Target(target) =>
            val num = askInt(s"\nRemove how many cadres from $target:", 0, game.getCountry(target).cadres)
            if (num > 0) {
              if (firstOne)
                log(s"\n$Jihadist voluntarily cadre removal.", Color.Info)
              firstOne = false
              removeCadresFromCountry(target, num)
            }
            num + nextCadre()
        }
      }
      else
        0
    }

    if (game.hasCountry(_.hasCadre))
      nextCadre()
    else {
      displayLine("\nThere are no cadres on the map.", Color.Info)
      pause()
      0
    }
  }

  // If there is a param, then we process the param and return.
  // If there is no param, then we loop showing a menu of options
  // until the user exits the menu with no action.
  def showCommand(param: Option[String]): Unit = {
    def displayHelp(): Unit = {
      val help = """|
        |Show command help
        |-----------------------------------------------------------------------------
        |If you enter the 'show' command with no arguments you will see a menu.
        |
        |You can bypass the menu by following the command with an argument.
        |The argument can be shortend to it a unique prefix of the argument.
        |For example: 's sum'  is equivalent to 'show summary'
        |
        |s summary   -- Game summary including score
        |s <country> -- Show status of a the named country
        |s actions   -- The list of actions for the current turn
        |s scenario  -- Scenario information and difficulty level
        |s caliphate -- Countries that are part of the caliphate
        |s civil war -- Countries in civil war
        |s targets   -- Countries targeted this/last action phase
        |s draw pile -- Cards in the draw pile (or in US/Jihadist hand)
        |s discarded -- Cards in the discard pile
        |s removed   -- Cards removed from the game
        |s all       -- Entire game state
        |s help      -- Show this help message
        |
        |When showing the status of countries you can specify multiple names
        |separated by commas.
        |For example: s syr,egy,iraq  would show the status Syria, Egypt and Iraq.
        |-----------------------------------------------------------------------------
      """.stripMargin
      displayLine(help)
  }

    val options = List(
      "all", "actions", "summary", "scenario", "caliphate", "civil war",
      "targets", "draw pile", "discarded", "removed", "help"
    ) ::: countryNames(game.countries)
    val menuChoices = List(
      choice(true, Some("summary"),    "Game summary and score"),
      choice(true, Some("country"),    "Countries"),
      choice(true, Some("actions"),    "Actions this turn"),
      choice(true, Some("scenario"),   "Scenario information"),
      choice(true, Some("caliphate"),  "Calipate countries"),
      choice(true, Some("civil war"),  "Countries in Civil War"),
      choice(true, Some("targets"),    "Countries targeted this/last action phase"),
      choice(true, Some("draw pile"),  "Cards in draw pile (or hands)"),
      choice(true, Some("discarded"),  "Cards in discard pile"),
      choice(true, Some("removed"),    "Cards removed from the game"),
      choice(true, Some("all"),        "Entire game state"),
      choice(true, None,               "Finished"),
    ).flatten

    // This function assumes entity is valid!
    def showEntity(entity: String): Unit = {
      entity match {
        case "summary"    => printSummary(game.scoringSummary); printSummary(game.statusSummary)
        case "actions"    => printSummary(game.actionSummary)
        case "scenario"   => printSummary(game.scenarioSummary)
        case "caliphate"  => printSummary(game.caliphateSummary)
        case "civil war"  => printSummary(game.civilWarSummary)
        case "targets"     => printSummary(game.targetSummary)
        case "draw pile"  => printSummary(game.deckSummary)
        case "discarded"  => printSummary(game.discardedCardsSummary)
        case "removed"    => printSummary(game.removedCardsSummary)
        case "all"        => printGameState()
        case "help"       => displayHelp()
        case name         => printSummary(game.countrySummary(name))
      }
    }

    def showCountries(): Unit = {
      val prompt = "\nShow which country (blank to cancel): "
      val candidates = countryNames(game.countries)
      askOneOf(prompt, candidates, None, true, false, CountryAbbreviations)
        .foreach { name =>
          printSummary(game.countrySummary(name))
          showCountries()
        }
    }


    def showMenu(): Unit = {
      askMenu("Show Game Information", menuChoices, allowAbort = false).head match {
        case Some("country") =>
          showCountries()
          showMenu()

        case Some(option) =>
          showEntity(option)
          pause()
          showMenu()

        case None =>
      }
    }

    val HELP  = """(?:\?|--help|-h)""".r
    param.map(_.trim) match {
      case Some(param) if HELP.matches(param) =>
        showEntity("help")

      case Some(param) if param.contains(",") =>
        param.split(raw"\s*,\s*").filter(_ != "").toList match {
          case Nil =>
            displayLine("Invalid input.")
          case paramEntries =>
            paramEntries
              .foreach { possibleName =>
                matchOneCL(possibleName, countryNames(game.countries), CountryAbbreviations) match {
                  case Right(name) =>
                    printSummary(game.countrySummary(name))
                  case Left(MatchOneError(msg, ambiguous)) =>
                    displayLine()
                    displayLine(msg)
                    if (ambiguous.nonEmpty) {
                      displayLine(s"Can be one of: ${ambiguous.mkString(", ")}")
                    }
                }
              }
        }

      case Some(param) if param != "" =>
        askOneOf("Show: ", options, Some(param), true, false, CountryAbbreviations)
          .foreach(showEntity)

      case _ =>
        showMenu()
    }
  }

  // Print the entire game state to stdout
  def printGameState(): Unit = {
    def printCountries(title: String, countries: List[String]): Unit = {
      println()
      displayLine(title, Color.Info)
      displayLine(separator(char = '='), Color.Info)
      if (countries.isEmpty)
        println("none")
      else
        for (name <- countries; SummaryEntry(text, color) <- game.countrySummary(name).entries)
          displayLine(text, color)
    }

    printSummary(game.scenarioSummary)
    printCountries("Muslim Countries with Good Governance",  countryNames(game.muslims.filter(_.isGood)))
    printCountries("Muslim Countries with Fair Governance",  countryNames(game.muslims.filter(_.isFair)))
    printCountries("Muslim Countries with Poor Governance",  countryNames(game.muslims.filter(_.isPoor)))
    printCountries("Muslim Countries under Islamic Rule",    countryNames(game.muslims.filter(_.isIslamistRule)))
    printCountries("Untested Muslim Countries",    countryNames(game.muslims.filter(_.isUntested)))
    printCountries("Non-Muslim Countries with Hard Posture", countryNames(game.nonMuslims.filter(_.isHard)))
    printCountries("Non-Muslim Countries with Soft Posture", countryNames(game.nonMuslims.filter(_.isSoft)))
    printCountries("Untested Non-Muslim Countries", countryNames(game.nonMuslims.filter(_.isUntested)))
    val iranSpecial = game.nonMuslims.find(_.iranSpecialCase).map(_.name)
    if (iranSpecial.nonEmpty)
      printCountries("Iran Special Case", iranSpecial.toList)
    printSummary(game.scoringSummary)
    printSummary(game.statusSummary)
    printSummary(game.targetSummary)
    printSummary(game.removedCardsSummary)
    if (game.useExpansionRules) {
      printSummary(game.civilWarSummary)
      printSummary(game.caliphateSummary)
    }
    printSummary(game.removedCardsSummary)
  }



  // Test to see if the event should trigger and if so
  // perform the event.
  def performTriggeredEvent(opponentRole: Role, card: Card): Unit = {
    if (card.autoTrigger)
      performCardEvent(card, opponentRole, triggered = true)
    else if (card.eventWillTrigger(opponentRole))
      opponentRole match {
        case Jihadist => JihadistBot.performTriggeredEvent(card)
        case US       => USBot.performTriggeredEvent(card)
      }
    else
      log("\n%s event \"%s\" has no effect".format(card.association, card.cardName), Color.Event)
    pause()
  }

  // The Intel Community event allows the US to play an additional card during
  // the action phase.  Hence the `additional` parameter.
  def usCardPlay(param: Option[String], additional: Boolean = false): Unit = {

    askCardNumber(FromRole(US)::Nil, "Card # ", param, allowAbort = false) foreach { cardNumber =>
      val card = deck(cardNumber)
      val savedState = game

      decreaseCardsInHand(US, 1)
      if (additional)
        addAdditionalCardToPlayedCard(card.number)  // Add card to most recent PlayedCard
      else
        addPlayedCard(US, card.number)  // Add the card to the list of plays for the turn.

      cachedEventPlayableAnswer = None

      logCardPlay(US, card)
      try {
        // When the Ferguson event is in effect, the Jihadist player
        // may cancel the play of any US associated card.
        // If the JihadistBot is playing it will cancel the next one played by the US.
        if (lapsingEventInPlay(Ferguson)    &&
            card.association == US          &&
            (game.botRole == Jihadist ||
             askYorN("Do you wish to cancel the play of this US associated card? (y/n) "))) {

          log(s"${card.numAndName} is discarded without effect due to Ferguson being in effect", Color.Event)
          addCardToDiscardPile(card.number)
          removeLapsingEvent(Ferguson)
        }
        else
          game.humanRole match {
            case US => humanUsCardPlay(card, ignoreEvent = false)
            case _  => USBot.cardPlay(card, ignoreEvent = false)
          }

        // Note: There currently no events that allow the US player to return
        // the card to hand. But we account for this in case it becomes
        //  possible in future expansion
        if (ignoreDiscardAtEndOfTurn()) {
          increaseCardsInHand(US, 1)
        }
        else {
          // The discard process is a bit complicated because
          // some when an event is played the card may have been removed from the game
          // moved to the lapsing box/1st plot box, kept in the user's hand, etc.
          if (!game.cardsRemoved.contains(cardNumber) &&
              !game.cardsLapsing().contains(cardNumber) &&
              !game.isFirstPlot(cardNumber)) {
            // If Reassessment was done, or if a seond card was used by the evnet
            // then there is a second card to discard.
            // Note: A second card being used by an event is NOT the same
            // as an event allowing an additional card to be played.
            // An additional card will come through here on its own and be
            // discarded at that time.
            // If we are processing an "additional" card then there will
            // never be another card to discard.
            val toDiscard = if (additional)
              List(cardNumber)
            else
              cardsInPlay(ignoreAdditional = true)

            for (n <- toDiscard)
              addCardToDiscardPile(n)
          }
        }


        setIgnoreDiscardAtEndOfTurn(false)  // Reset for next turn
        // When an additional card is played it will be saved as part
        // of the primary card play.
        if (!additional)
          saveGameState()
      }
      catch {
        case AbortAction if additional => throw AbortAction // Abort back to the original card.
        case AbortAction =>
          println("\n>>>> Aborting the current card play <<<<")
          println(separator())
          displayGameStateDifferences(game, savedState)
          game = savedState
      }
    }
  }


  // Once the user enters a valid command (other than using reserves), then in order to
  // abort the command in progress they must type 'abort' at any prompt during the turn.
  // We will then roll back to the game state as it was before the card play.
  def humanUsCardPlay(card: Card, ignoreEvent: Boolean): Unit = {
    val eventPlayable = !ignoreEvent && card.eventIsPlayable(US)
    val ExecuteEvent = "Execute event"
    val WarOfIdeas   = "War of ideas"
    val Deploy       = "Deploy"
    val RegimeChg    = "Regime change"
    val Withdraw     = "Withdraw"
    val Disrupt      = "Disrupt"
    val Alert        = "Alert"
    val Reassess     = "Reassessment"
    val AddReserves  = "Add to reserves"
    val UseReserves  = "Expend reserves"
    val AbortCard    = "Abort card"
    var reservesUsed = 0
    var card1EventValid = false
    var cardEventTriggered = false
    def inReserve    = game.reserves.us
    def opsAvailable = (card.ops + reservesUsed) min 3

    val firstCardCanTrigger = card.autoTrigger || card.association  == Jihadist
    val canReassess = firstCardOfPhase(US) && card.ops == 3 && hasCardInHand(US)

    sealed trait PerformOption {
      def menuText: String
    }
    case object PerformCardActivity extends PerformOption {
      override def menuText = "Operations"
    }
    case object Play2ndCard extends PerformOption {
      override def menuText = "Play 2nd card to perform Reassessment"
    }
    case object PerformReassess extends PerformOption {
      override def menuText = "Perform Reassessment"
    }
    case class TriggerOpponentEvent(card: Card) extends PerformOption {
      override def menuText = s"Trigger event [${card.cardName}]"

    }

    def promptForSecondCard(): Card = {
      println("\nYou must play a second 3 Ops card.")
      askCardNumber(FromRole(US)::Nil, "Enter # of second 3 Ops card: ", allowNone = true, opsRequired = Set(3)) match {
        case None =>
          throw AbortAction
        case Some(cardNum) =>
          val card2 = deck(cardNum)
          if (lapsingEventInPlay(Ferguson) && card2.association == US) {
            displayLine("\nFerguson is in effect, so the Jihadist will cancel any US associated card.", Color.Info)
            sealed trait Choice
            case object Another extends Choice
            case object Abort extends Choice
            case object Proceed extends Choice
            val choices = List(
              Another -> "Choose another 3 Ops card",
              Abort   -> "Abort the Reassessment action",
              Proceed -> "Play this card anyway")
            askMenu("What do you wish to do:", choices).head match {
              case Another => promptForSecondCard()
              case Abort   => throw AbortAction
              case Proceed =>
                log(s"${card2.numAndName} is discarded without effect due to Ferguson being in effect", Color.Event)
                log(s"The reassessment action is cancelled", Color.Event)
                addPlayedCard(US, cardNum)
                addCardToDiscardPile(cardNum)
                removeLapsingEvent(Ferguson)
                throw CancelledByFerguson
            }
          }

          decreaseCardsInHand(US, 1)
          // Replace the head card play with a reassessment
          game = game.copy(turnActions = PlayedReassement(card.number, card2.number) :: game.turnActions.tail)
          logCardPlay(US, card2, opsOnly = true)
          card2
      }
    }


    @tailrec def getUsActivity(): Either[String, List[PerformOption]] = {
      // The US must announce reassessment before triggering an event
      // so if the event was triggered then reassessment is not allowed.
      val showReassess = canReassess && !cardEventTriggered && reservesUsed == 0
      val actions = List(
        choice(eventPlayable && reservesUsed == 0,         ExecuteEvent, ExecuteEvent),
        choice(true,                                       WarOfIdeas, WarOfIdeas),
        choice(game.deployPossible(opsAvailable),          Deploy, Deploy),
        choice(game.regimeChangePossible(opsAvailable),    RegimeChg, RegimeChg),
        choice(game.withdrawPossible(opsAvailable),        Withdraw, Withdraw),
        choice(game.disruptTargets(opsAvailable).nonEmpty, Disrupt, Disrupt),
        choice(game.alertPossible(opsAvailable),           Alert, Alert),
        choice(showReassess,                               Reassess, Reassess),
        choice(card.ops < 3 && inReserve < 2,              AddReserves, AddReserves),
        choice(opsAvailable < 3 && inReserve > 0,          UseReserves, UseReserves),
        choice(true,                                       AbortCard, AbortCard),
      ).flatten

      println(s"\nYou have ${opsString(opsAvailable)} available and ${opsString(inReserve)} in reserve")

      askMenu(s"$US action: ", actions).head match {
        case AbortCard =>
          if (askYorN("Really abort (y/n)? "))
            throw AbortAction
          else
            getUsActivity()

        case UseReserves =>
          reservesUsed = inReserve
          log(s"$US player expends their reserves of ${opsString(reservesUsed)}", Color.Info)
          game = game.copy(reserves = game.reserves.copy(us = 0))
          getUsActivity()

        case Reassess =>
            val card2 = promptForSecondCard()
            var newOptions = List(PerformReassess)
            val trigger1 = if (firstCardCanTrigger)
              List(TriggerOpponentEvent(card))
            else
              Nil
            val trigger2 = if (card2.autoTrigger || card2.association == Jihadist)
              List(TriggerOpponentEvent(card2))
            else
              Nil
              
            if (trigger1.nonEmpty || trigger2.nonEmpty)
              Right(PerformReassess :: trigger1 ::: trigger2)
            else
              Left(Reassess)

        case action =>
          Left(action)
      }
    }

    def performCardActivity(activity: String): Unit = {
      activity match {
        case AddReserves  => addToReserves(US, card.ops)
        case ExecuteEvent => performCardEvent(card, US)
        case WarOfIdeas   => humanWarOfIdeas(opsAvailable)
        case Deploy       => humanDeploy(opsAvailable)
        case Disrupt      => humanDisrupt(opsAvailable)
        case Reassess     => humanReassess()
        case RegimeChg    => humanRegimeChange()
        case Withdraw     => humanWithdraw()
        case Alert        => humanAlert()
        case _ => throw new IllegalStateException(s"performCardActivity() - Invalid US activity: $activity")
      }
      pause()
    }

    def processOptions(options: List[PerformOption]): Unit = options match {
      case Nil =>
        // End the processOptions recursive loop
      case PerformCardActivity::Nil =>
        getUsActivity() match {
          case Left(activity) =>
            performCardActivity(activity)

          case Right(moreOptions) =>
            processOptions(options.filterNot(_ == PerformCardActivity):::moreOptions)
        }

      case PerformReassess::Nil =>
        performCardActivity(Reassess)

      case TriggerOpponentEvent(c)::Nil =>
        performTriggeredEvent(Jihadist, c)
        cardEventTriggered = true

      case options =>
        // Ask what happens next
        val choices = options.map(o => Some(o) -> o.menuText) :+
          (None, s"Abort card")

        val newOptions = askMenu("What happens next:", choices).head match {
          case None =>
            if (askYorN("Really abort (y/n)? "))
              throw AbortAction
            else
              options

          case Some(PerformCardActivity) =>
            getUsActivity() match {
              case Left(activity) =>
                performCardActivity(activity)
                options
                  .filter {
                    case PerformCardActivity => false
                    case Play2ndCard => false  // Not longer an option!
                    case _ => true
                  }

              case Right(moreOptions) =>
                // Reassess and TriggerCard replace the perform option
                // So the user can chooose the order
                options.filterNot(_ == PerformCardActivity):::moreOptions
            }

          case Some(Play2ndCard) =>
            val card2 = promptForSecondCard()
            val triggerCard2 = if (card2.autoTrigger || card2.association == Jihadist)
              List(TriggerOpponentEvent(card2))
            else
              Nil
            PerformReassess :: options
              .filter {
                case Play2ndCard => false
                case PerformCardActivity => false
                case _ => true
              } ::: triggerCard2

          case Some(PerformReassess) =>
            performCardActivity(Reassess)
            options.filterNot(_ == PerformReassess)

          case Some(TriggerOpponentEvent(c)) =>
            performTriggeredEvent(Jihadist, c)
            cardEventTriggered = true
            options
              .filter {
                case TriggerOpponentEvent(x) => x.number != c.number
                case Play2ndCard => false // No longer an option!
                case _ => true
              }
        }
        processOptions(newOptions)
    }

    // If the the event will auto trigger or if it is a Jihadist associated event
    // that will trigger, then Ask if user up front if the wish to perform
    // Reassessment.  This is done because the player must "announce" that they
    // wish to do Reassessment before seeing the results of any triggered event form
    // the first 3 Ops card played.
    val initialOptions = (firstCardCanTrigger, canReassess) match {
      case (false, _)    => List(PerformCardActivity)
      case (true, false) => List(PerformCardActivity, TriggerOpponentEvent(card))
      case (true, true)  => List(PerformCardActivity, TriggerOpponentEvent(card), Play2ndCard)
    }

    try processOptions(initialOptions)
    catch {
      // If cancelled then we just return from this function.
      case CancelledByFerguson =>
    }
  }

  // This method allows the human player to execute an operation with the
  // given number of Ops.  This is called by some event card actions.
  def humanExecuteOperation(ops: Int): Unit = {
    val WarOfIdeas   = "War of Ideas"
    val Deploy       = "Deploy"
    val RegimeChg    = "Regime change"
    val Withdraw     = "Withdraw"
    val Disrupt      = "Disrupt"
    val Alert        = "Alert"
    val Reserves     = "Add 1 OP to reserves"
    val actions = List(
      choice(true,                              WarOfIdeas, WarOfIdeas),
      choice(game.deployPossible(ops),          Deploy, Deploy),
      choice(game.regimeChangePossible(ops),    RegimeChg, RegimeChg),
      choice(game.withdrawPossible(ops),        Withdraw, Withdraw),
      choice(game.disruptTargets(ops).nonEmpty, Disrupt, Disrupt),
      choice(game.alertPossible(ops),           Alert, Alert),
      choice(game.reserves.us < 2,              Reserves, Reserves)
    ).flatten
    askMenu(s"$US action:", actions).head match {
      case WarOfIdeas => humanWarOfIdeas(ops)
      case Deploy     => humanDeploy(ops)
      case RegimeChg  => humanRegimeChange()
      case Withdraw   => humanWithdraw()
      case Disrupt    => humanDisrupt(ops)
      case Alert      => humanAlert()
      case _          => addToReserves(US, 1)
    }
  }

  def humanWarOfIdeas(ops: Int): Unit = {
    log()
    log(s"$US attempts War of Ideas operation with ${opsString(ops)}")
    val target = askCountry("War of Ideas in which country: ", game.warOfIdeasTargets(ops))
    addOpsTarget(target)
    performWarOfIdeas(target, ops)
  }

  // Troops can always deploy to the track.

  def humanDeploy(ops: Int): Unit = {
    log()
    log(s"\n$US performs a Deploy operation")
    log(separator())
    // If the only
    val (from, to) = game.deployTargets(ops).get
    val source     = askCountry("Deploy troops from: ", from)
    val (maxCanLeave, troopCubesPresent) = if (source == "track")
      (game.troopsAvailable, game.troopsAvailable)
    else
      (game.getCountry(source).maxDeployFrom, game.getCountry(source).troops)

    // If th NATO or NATO-2 marker is present, the player has the option
    // of deploying it out of the country and removing it from the game.
    val (markersRemoved, markersValue): (List[String], Int) = if (source == "track")
      (Nil, 0)
    else {
      val src = game.getCountry(source)
      val markers = src.troopsMarkers.filter(m => m.canDeploy)
      // If the source is in regime change then we must take care not to allow too many
      // markers to be removed so that we do not leave enough troops/militia behind.
      val regimeChange = game.isMuslim(source) && game.getMuslim(source).inRegimeChange
      val canRemoveAll: List[TroopsMarker] => Boolean = if (regimeChange)
        markerList => markerList.map(_.num).sum <= maxCanLeave
      else
        _ => true  // Always possible if not in regime change

      val combos = for {
        i <- 1 to markers.size
        combo <- markers.combinations(i).toList if canRemoveAll(combo)
        names = combo.map(_.name)
      } yield (names.mkString(",") -> andList(names))

      if (combos.isEmpty)
        (Nil, 0)
      else {
        val choices = ("none" -> "Do not deploy any markers") :: combos.toList
        val markerNames = askMenu(s"Which troop markers will deploy out of $source", choices).head match {
          case "none" => Nil
          case str    => str.split(",").toList
        }
        val value = markerNames
          .foldLeft(0) { (sum, name) =>
            sum + markers.find(_.name == name).map(_.num).getOrElse(0)
          }
        (markerNames, value)
      }
    }

    val maxTroops  = (maxCanLeave - markersValue) min troopCubesPresent
    val minTroops  = if (markersRemoved.isEmpty) 1 min maxTroops else 0
    val numTroops  = askInt("Deploy how many troops: ", minTroops, maxTroops)

    if (numTroops > 0) {
      val dest = askCountry("Deploy troops to: ", to.filterNot(_ == source))
      addOpsTarget(dest)
      moveTroops(source, dest, numTroops)
    }
    // Troops markers that deploy are simply removed
    removeEventMarkersFromCountry(source, markersRemoved:_*)
  }

  def humanRegimeChange(): Unit = {
    log(s"\n$US performs a Regime Change operation")
    log(separator())
    val dest      = askCountry("Regime change in which country: ", game.regimeChangeTargets)
    val source    = askCountry("Deploy troops from: ", game.regimeChangeSourcesFor(dest))
    val maxTroops = if (source == "track") game.troopsAvailable
                    else game.getCountry(source).maxDeployFrom
    val numTroops = askInt("How many troops: ", 6, maxTroops)
    addOpsTarget(dest)
    performRegimeChange(source, dest, numTroops)
  }

  def humanWithdraw(): Unit = {
    log(s"\n$US performs a Withdraw operation")
    log(separator())
    val source = askCountry("Withdraw troops from which country: ", game.withdrawFromTargets)
    val dest   = askCountry("Deploy withdrawn troops to: ", game.withdrawToTargets.filter(_ != source))
    val numTroops = askInt("How many troops: ", 1, game.getCountry(source).troops)
    addOpsTarget(source)
    performWithdraw(source, dest, numTroops)
  }

  def humanDisrupt(ops: Int): Unit = {
    log(s"\n$US performs a Disrupt operation")
    log(separator())
    val target = askCountry("Disrupt in which country: ", game.disruptTargets(ops))
    addOpsTarget(target)
    performDisrupt(target)
  }

  // Pick a random plot in the country.
  // If any are backlashed, then don't add them to the list unless that is all there is.
  def humanPickPlotToAlert(countryName: String): PlotOnMap = {
    val c = game.getCountry(countryName)
    assert(c.hasPlots, s"humanPickPlotToAlert(): $countryName has no plots")
    if (c.plots.exists(_.backlashed) && c.plots.exists(p => !p.backlashed))
      shuffle(c.plots.filterNot(_.backlashed)).head
    else
      shuffle(c.plots).head
  }

  def humanAlert(): Unit = {
    log(s"\n$US performs an Alert operation")
    log(separator())
    val name = askCountry("Alert plot in which country: ", game.alertTargets)
    addOpsTarget(name)
    performAlert(name, humanPickPlotToAlert(name))
  }

  def humanReassess(): Unit = {
    log(s"\n$US performs a Reassessment operation")
    log(separator())

    setUSPosture(oppositePosture(game.usPosture))
}

  // Return true if this is the first card played by the give role in the
  // current action phase.
  // IMPORTANT: This function assumes that the current card has already been added to
  //            the list of plays for the turn.
  def firstCardOfPhase(role: Role): Boolean =
    role == getActiveRole() && numCardsPlayedInCurrentPhase() == 1

  def jihadistCardPlay(param: Option[String]): Unit = {

    askCardNumber(FromRole(Jihadist)::Nil, "Card # ", param, allowAbort = false) foreach { cardNumber =>
      val card = deck(cardNumber)
      val savedState = game

      decreaseCardsInHand(Jihadist, 1)
      // Add the card to the list of plays for the turn.
      addPlayedCard(Jihadist, card.number)

      cachedEventPlayableAnswer = None
      // If TheDoorOfItjihad lapsing card is in effect,
      // then Jihadist cannot play any events (except autoTrigger events)
      logCardPlay(Jihadist, card)
      try {
        // When the Ferguson event is in effect, the Jihadist player
        // may cancel the play of any US associated card.
        // If the JihadistBot is playing it will only cancel those played by the US.
        if (lapsingEventInPlay(Ferguson)    &&
            card.association == US          &&
            game.humanRole == Jihadist      &&
            askYorN("Do you wish to cancel the play of this US associated card? (y/n) ")) {
          log(s"${card.numAndName} is discarded without effect due to Ferguson being in effect", Color.Event)
          removeLapsingEvent(Ferguson)
        }
        else
          game.humanRole match {
            case Jihadist => humanJihadistCardPlay(card, ignoreEvent = false)
            case _        => JihadistBot.cardPlay(card, ignoreEvent = false)
          }

        // Boko Haram allows the card to be returned to hand
        if (ignoreDiscardAtEndOfTurn()) {
          increaseCardsInHand(Jihadist, 1)
        }
        else {
          // The discard process is a bit complicated because
          // some when an event is played the card may have been removed from the game
          // moved to the lapsing box/1st plot box, kept in the user's hand, etc.
          if (!game.cardsRemoved.contains(cardNumber) &&
              !game.cardsLapsing().contains(cardNumber) &&
              !game.isFirstPlot(cardNumber)) {
                // There may be a second card to discard be cause
                // some eventa allow the use of a second card
                // so we discard all cards in play for the current action
                // rather than just use the `cardNumber` variable.
                // Note: A `second` card is not the same as an `additional` card.
                //        Curently no event playablle by the Jihadist allows
                //        for an `additional` card to be played.
                for (n <- cardsInPlay(ignoreAdditional = true))
                  addCardToDiscardPile(n)
              }
        }
        setIgnoreDiscardAtEndOfTurn(false)  // Reset for next turn
        saveGameState()
      }
      catch {
        case AbortAction =>
          println("\n>>>> Aborting the current card play <<<<")
          println(separator())
          displayGameStateDifferences(game, savedState)
          game = savedState
      }
    }
  }

  // Once the user enters a valid command (other than using reserves), then in order to
  // abort the command in progress they must type 'abort' at any prompt during the turn.
  // We will then roll back to the game state as it was before the card play.
  def humanJihadistCardPlay(card: Card, ignoreEvent: Boolean): Unit = {
    val eventPlayable =
      !ignoreEvent &&
      lapsingEventNotInPlay(TheDoorOfItjihad) &&  // Blocks al Non-US events
      card.eventIsPlayable(Jihadist)
    val ExecuteEvent = "Execute event"
    val Recruit      = "Recruit"
    val Travel       = "Travel"
    val Jihad        = "Jihad"
    val PlotAction   = "Plot"
    val AddReserves  = "Add to reserves"
    val UseReserves  = "Expend reserves"
    val RemoveCadre  = "Remove cadre"
    val AbortCard    = "Abort Card"
    var reservesUsed = 0
    var firstPlotUsed = false
    def inReserve    = game.reserves.jihadist
    def opsAvailable = (card.ops + reservesUsed) min 3

    @tailrec def getJihadistActivity(): String = {
      val actions = List(
        choice(eventPlayable && reservesUsed == 0, ExecuteEvent, ExecuteEvent),
        choice(game.recruitPossible,               Recruit, Recruit),
        choice(true,                               Travel, Travel), // Travel must be possible or the Jihadist has lost
        choice(game.jihadPossible,                 Jihad, Jihad),
        choice(game.plotPossible(opsAvailable),    PlotAction, PlotAction),
        choice(card.ops < 3 && inReserve < 2,      AddReserves, AddReserves),
        choice(opsAvailable < 3 && inReserve > 0,  UseReserves, UseReserves),
        choice(game.hasCountry(_.hasCadre),        RemoveCadre, RemoveCadre),
        choice(true,                               AbortCard, AbortCard),
      ).flatten

      println(s"\nYou have ${opsString(opsAvailable)} available and ${opsString(inReserve)} in reserve")
      askMenu(s"$Jihadist action: ", actions).head match {
        case UseReserves =>
          reservesUsed = inReserve
          log(s"$Jihadist player expends their reserves of ${opsString(reservesUsed)}", Color.Info)
          game = game.copy(reserves = game.reserves.copy(jihadist = 0))
          getJihadistActivity()

        case RemoveCadre =>
          humanVoluntarilyRemoveCadre()
          getJihadistActivity()

        case AbortCard =>
            if (askYorN("Really abort (y/n)? "))
              throw AbortAction
            else
              getJihadistActivity()
        case action => action
      }
    }

    def performCardActivity(activity: String): Unit = {
      activity match {
        case AddReserves  => addToReserves(Jihadist, card.ops)
        case ExecuteEvent => performCardEvent(card, Jihadist)
        case Recruit      => humanRecruit(opsAvailable)
        case Travel       => humanTravel(opsAvailable)
        case Jihad        => humanJihad(opsAvailable)
        case PlotAction   =>
          humanPlot(opsAvailable)
          // The first plot used in a turn is placed in the 1st plot box
          // The Ruthless US bot resolve does not allow this.
          if (game.firstPlotEntry.isEmpty && !game.usResolve(Ruthless)) {
            firstPlotUsed = true
            log(s"\nPlace the $card card in the first plot box", Color.Info)
            game = game.copy(firstPlotEntry = Some(LapsingEntry(card.number)))
          }
            

        case other => throw new IllegalStateException(s"Invalid Jihadist action: $other")
      }
      pause()
    }


    if (card.autoTrigger || card.association == US) {
      sealed trait Choice
      case object Ops extends Choice
      case object Trigger extends Choice
      case object Abort extends Choice
      val choices = List(
        Ops     -> "Operations",
        Trigger -> s"Trigger event [${card.cardName}]",
        Abort   -> "Abort card",
      )

      def performActions(): Unit = {
        askMenu("What happens next:", choices).head match {
          case Ops =>
            performCardActivity(getJihadistActivity())
            if (card.association == US && firstPlotUsed)
              log(s"\nThe First plot option prevents the US associated \"${card.cardName}\" event from triggering", Color.Info)
            else
              performTriggeredEvent(US, card)

          case Trigger =>
            performTriggeredEvent(US, card)
            performCardActivity(getJihadistActivity())

          case Abort =>
            if (askYorN("Really abort (y/n)? "))
              throw AbortAction
            else
              performActions()
        }
      }
      performActions()
    }
    else
      performCardActivity(getJihadistActivity())
  }

  def humanRecruit(ops: Int, ignoreFunding: Boolean = false, madrassas: Boolean = false): Unit = {
    val recruitCells = if (ignoreFunding) game.cellsAvailable else game.cellsToRecruit
    log(s"\n$Jihadist performs a Recruit operation with ${opsString(ops)}")
    log(separator())
    if (recruitCells == 1)
      log(s"There is 1 cell available for recruitment")
    else
      log(s"There are $recruitCells cells available for recruitment")
    // All of the target destinations must be declared before rolling any dice.
    val targets = for (i <- 1 to ops) yield {
      val ord  = ordinal(i)
      val dest = askCountry(s"$ord Recruit destination: ", game.recruitTargets(madrassas))
      (ord, dest)
    }

    val results = for ((ord, dest) <- targets) yield {
      testCountry(dest)  // It is possible that the country was reverted to untested by an event.
      addOpsTarget(dest)
      val c = game.getCountry(dest)
      if (c.autoRecruit) {
        log(s"$ord Recruit automatically succeeds in $dest")
        (dest, true)
      }
      else {
        if (game.usResolve(Vigilant) && (game isMuslim dest)) {
          val m = game getMuslim dest
          if (m.civilWar)
            log(s"$US Bot with Vigilant prevents auto recruit in Civil War country", Color.Event)
          else if (m.inRegimeChange)
            log(s"$US Bot with Vigilant prevents auto recruit in Regime Change country", Color.Event)
        }
        val die     = getDieRoll(s"Enter $ord recruit die roll in $dest: ", Some(Jihadist))
        val success = c.recruitSucceeds(die)
        val result  = if (success) "succeeds" else "fails"
        log(s"$ord Recruit $result in $dest with a roll of $die")
        (dest, success)
      }
    }
    println()
    val successes = (for ((dest, success) <- results; if success) yield dest).toList
    def allTheSame(l: List[String]) = l match {
      case Nil => false
      case x::xs => xs forall (_ == x)
    }
    // If we have more successes than there are available cells, then we must
    // allow the user to choose where to put the cells.
    if (successes.size > recruitCells) {
        // But if all of the success targets are the same country, then
        // just add all available cell there without asking.
      if (allTheSame(successes))
        addSleeperCellsToCountry(successes.head, recruitCells)
      else {
        log(s"${successes.size} successful recruits, but only $recruitCells available cells")
        println("Specify where to put the cells:")
        def ask(dest: String) = askOneOf(s"1 cell in $dest (place or skip): ", Seq("place", "skip")).get
        // Keep asking until all of the available cells have been placed or until
        // the remaining target destination are all the same.
        @tailrec def placeNext(dests: List[String], skipped: List[String], cellsLeft: Int): Unit =
          if (cellsLeft != 0)
            (dests, skipped) match {
              case (Nil, Nil)                  =>  // should not get here
              case (Nil, sk) if allTheSame(sk) => addSleeperCellsToCountry(sk.head, cellsLeft)
              case (Nil, sk)                   => placeNext(sk.reverse, Nil, cellsLeft) // Go around again
              case (ds, _) if allTheSame(ds)   => addSleeperCellsToCountry(ds.head, cellsLeft)
              case (d :: ds, sk)               => ask(d) match {
                                                    case "place" => addSleeperCellsToCountry(d, 1)
                                                                    placeNext(ds, sk, cellsLeft - 1)
                                                    case _       => placeNext(ds, d :: sk, cellsLeft)
                                                  }
            }
        placeNext(successes, Nil, recruitCells)
      }
    }
    else // We have enough cells to place one in all destinations
      for ((dest, successesInDest) <- successes.groupBy(dest => dest))
        addSleeperCellsToCountry(dest, successesInDest.size)
  }

  def travelIsAutomatic(src: String, dest: String): Boolean = {
    if (lapsingEventInPlay(IslamicMaghreb) && Schengen.contains(dest))
      false
    else if (game.getCountry(dest).isIslamistRule || src == dest)
      true
    else if (lapsingEventInPlay(Biometrics))
      (areAdjacent(src, dest) && !game.getCountry(dest).isGood)
    else
      areAdjacent(src, dest)
  }

  def humanTravel(ops: Int): Unit = {
    log(s"\n$Jihadist performs a Travel operation with ${opsString(ops)}")
    log(separator())
    log(s"There are ${{amountOf(game.cellsOnMap, "cell")}} on the map")
    val maxRolls    = ops min game.cellsOnMap
    val numAttempts = askInt("How many attempts do you wish to make?", 1, maxRolls, Some(maxRolls))
    // Keep track of where cells have been selected. They can only be selected once!
    case class Source(name: String, sleepers: Int, actives: Int)
    var sourceCountries = game.countries
      .filter(_.cells > 0)
      .map(c => c.name -> Source(c.name, c.sleeperCells, c.activeCells))
      .toMap

    // All of the travelling cells must be declared before rolling any dice.
    val attempts = for (i <- 1 to numAttempts) yield {
      println()
      val ord        = ordinal(i)
      val candidates = sourceCountries.keys.toList.sorted
      val src        = sourceCountries(askCountry(s"$ord Travel from which country: ", candidates))
      val active     = if (src.sleepers > 0 && src.actives > 0) {
        askCellsNotSadr(src.name, 1, sleeperFocus = false) match {
          case (1, _) => true
          case _      => false
        }
      }
      else {
        println(s"Travel cell will be ${if (src.actives > 0) "an active cell" else "a sleeper cell"}")
        src.actives > 0
      }

      val destCandidates = {
        val uk = game.getNonMuslim(UnitedKingdom)
        var names = if (lapsingEventInPlay(Biometrics))
          src.name :: getAdjacent(src.name)
        else
          countryNames(game.countries)

        if (Schengen.contains(src.name) && uk.hasMarker(BREXIT) && uk.isHard)
          names = names.filterNot(_ == UnitedKingdom)

        if (isTravelBanCountry(src.name))
          names = names.filterNot(_ == UnitedStates)

        names
      }

      val dest = askCountry(s"$ord Travel to which destination country: ", destCandidates.sorted)
      // Remove the selected cell so that it cannot be selected twice.
      if (src.sleepers + src.actives == 1)
        sourceCountries -= src.name
      else if (active)
        sourceCountries += src.name -> src.copy(actives = src.actives - 1)
      else
        sourceCountries += src.name -> src.copy(sleepers = src.sleepers - 1)

      TravelAttempt(src.name, dest, active)
    }
    for (a <- attempts)
      addOpsTarget(a.to)
    performTravels(attempts.toList)
  }

      // All of the jihad cells must be declared before rolling any dice.
  def getHumanJihadTargets(diceLeft: Int, candidates: List[String]): List[JihadTarget] = {
    if (diceLeft == 0 || candidates.isEmpty)
      Nil
    else {
      println(s"You have ${diceString(diceLeft)} remaining")
      val name = askCountry(s"Jihad in which country: ", candidates)
      val m    = game.getMuslim(name)
      val majorJihad = if (m.majorJihadOK(diceLeft))
        askYorN(s"Conduct Major Jihad in $name (y/n)? ")
      else
        false
      val numRolls = if (majorJihad) {
        val minRolls = if (m.besiegedRegime) 1 else 2
        val maxRolls = diceLeft  // We know there are at least 5 cells present
        askInt(s"Roll how many dice in $name?", minRolls, maxRolls, Some(maxRolls))
      }
      else {
        val maxRolls = diceLeft min m.totalCells
        askInt(s"Roll how many dice in $name?", 1, maxRolls, Some(maxRolls))
      }
      val (actives, sleepers, sadr) = if (majorJihad) (numRolls, 0, m.hasSadr)
      else if (m.totalCells == numRolls) (m.activeCells, m.sleeperCells, m.hasSadr)
      else if (m.activeCells == 0 && !m.hasSadr)  (0, numRolls, false)
      else if (m.sleeperCells == 0 && !m.hasSadr) (numRolls, 0, false)
      else askCells(name, numRolls, sleeperFocus = false)
      val target = JihadTarget(name, actives, sleepers, sadr, majorJihad)
      target :: getHumanJihadTargets(diceLeft - numRolls, candidates.filterNot(_ == name))
    }
  }
  // When ever a country is targeted and is a candidate for Major Jihad
  // ask the user if they want to do Major Jihad.
  def humanJihad(ops: Int): Unit = {
    val Active  = true
    log(s"\n$Jihadist performs a Jihad operation with ${opsString(ops)}")
    log(separator())
    val jihadCountries = game.muslims.filter(_.jihadOK)
    val maxDice = ops min jihadCountries.map(_.totalCells).sum
    var candidates = countryNames(jihadCountries)

    val targets = getHumanJihadTargets(maxDice, candidates)
    for (t <- targets)
      addOpsTarget(t.name)
    performJihads(targets)
  }

  // Can only Plots with number <= ops or WMD Plot
  def humanPlot(ops: Int): Unit = {
    val numPlots = game.plotsAvailableWith(ops).size
    val maxCells = game.plotTargets.map(name => game.getCountry(name).totalCells).sum
    val maxRolls = ops min maxCells
    log(s"\n$Jihadist performs a Plot operation with ${opsString(ops)}")
    log(separator())
    val cellDisp = if (maxCells == 1) "There is one cell on the map that can plot"
                   else              s"There are $maxCells cells on the map that can plot"
    val plotDisp = if (numPlots == 1) s"There is 1 plot available for this operation"
    else                              s"There are $numPlots plots available for this operation"
    println(cellDisp)
    println(plotDisp)
    val numAttempts = askInt("How many attempts do you wish to make?", 1, maxRolls, Some(maxRolls))
    // Keep track of where cells have been selected. They can only be selected once!
    case class Target(name: String, actives: Int, sleepers: Int)
    var targetCountries = (for (name <- game.plotTargets; c = game.getCountry(name))
      yield name -> Target(name, c.activeCells + (if (c.hasSadr) 1 else 0), c.sleeperCells)).toMap
    // All of the plotting cells must be declared before rolling any dice.
    val attempts = for (i <- (1 to numAttempts).toList) yield {
      println()
      val ord        = ordinal(i)
      val candidates = targetCountries.keys.toList.sorted
      val target     = targetCountries(askCountry(s"$ord Plot attempt in which country: ", candidates))
      val active     = if (target.sleepers > 0 && target.actives > 0) {
        askCells(target.name, 1, sleeperFocus = false) match {
          case (_, _, true) => true
          case (1, _, _)    => true
          case _            => false
        }
      }
      else {
        println(s"$ord Plot cell will be ${if (target.actives > 0) "an active cell" else "a sleeper cell"}")
        target.actives > 0
      }
      // Remove the selected cell so that it cannot be selected twice.
      if (target.sleepers + target.actives == 1)
        targetCountries -= target.name
      else if (active)
        targetCountries += target.name -> target.copy(actives = target.actives - 1)
      else
        targetCountries += target.name -> target.copy(sleepers = target.sleepers - 1)
      PlotAttempt(target.name, active)
    }
    for (a <- attempts)
      addOpsTarget(a.name)
    performPlots(ops, attempts)
  }

  def addAvailablePlotToCountry(name: String, plot: Plot, visible: Boolean = false, ignoreAbuSayyaf: Boolean = false): Unit = {
    val index = game.availablePlots.indexOf(plot)
    assert(index >= 0, s"addAvailablePlotToCountry(): $plot is not available")
    val updatedPlots = game.plotData.copy(
      availablePlots = game.availablePlots.take(index) ::: game.availablePlots.drop(index + 1)
    )
    testCountry(name)
    game = game.copy(plotData = updatedPlots)
    game.getCountry(name) match {
      case m: MuslimCountry    => game = game.updateCountry(m.copy(plots = PlotOnMap(plot) :: m.plots))
      case n: NonMuslimCountry => game = game.updateCountry(n.copy(plots = PlotOnMap(plot) :: n.plots))
    }
    if (game.humanRole == Jihadist ||
       visible ||
       (name == UnitedStates && (game getCountry UnitedStates).hasMarker(NEST)))
      log(s"Add $plot to $name", Color.MapPieces)
    else
      log(s"Add a hidden plot to $name", Color.MapPieces)

    if (!ignoreAbuSayyaf && name == Philippines && countryEventInPlay(Philippines, AbuSayyaf)) {
      val p = game getNonMuslim Philippines
      if (p.totalCells >= p.totalTroops) {
        log(s"Decrease prestige by 1 because Abu Sayyaf is in effect", Color.Event)
        decreasePrestige(1)
      }
    }
  }

  // For adjustment only
  def addResolvledPlotToCountry(name: String, plot: Plot): Unit = {
    val index = game.resolvedPlots.indexOf(plot)
    assert(index >= 0, s"addResolvledPlotToCountry(): $plot is not in the resolved box")
    val updatedPlots = game.plotData.copy(
      resolvedPlots = game.resolvedPlots.take(index) ::: game.resolvedPlots.drop(index + 1)
    )
    testCountry(name)
    game = game.copy(plotData = updatedPlots)
    game.getCountry(name) match {
      case m: MuslimCountry    => game = game.updateCountry(m.copy(plots = PlotOnMap(plot) :: m.plots))
      case n: NonMuslimCountry => game = game.updateCountry(n.copy(plots = PlotOnMap(plot) :: n.plots))
    }
    if (game.humanRole == Jihadist ||
       (name == UnitedStates && (game getCountry UnitedStates).hasMarker(NEST)))
      log(s"Add $plot to $name", Color.MapPieces)
    else
      log(s"Add a $plot to $name (hidden)", Color.MapPieces)
  }

  // For adjustment only
  def addRemovedPlotToCountry(name: String, plot: Plot): Unit = {
    val index = game.removedPlots.indexOf(plot)
    assert(game.removedPlots.nonEmpty, s"addRemovedPlotToCountry(): $plot is not in the removed box")
    val updatedPlots = game.plotData.copy(
      removedPlots = game.removedPlots.tail
    )
    testCountry(name)
    game = game.copy(plotData = updatedPlots)
    game.getCountry(name) match {
      case m: MuslimCountry    => game = game.updateCountry(m.copy(plots = PlotOnMap(plot) :: m.plots))
      case n: NonMuslimCountry => game = game.updateCountry(n.copy(plots = PlotOnMap(plot) :: n.plots))
    }
    if (game.humanRole == Jihadist ||
       (name == UnitedStates && (game getCountry UnitedStates).hasMarker(NEST)))
      log(s"Add $plot to $name", Color.MapPieces)
    else
      log(s"Add a $plot to $name (hidden)", Color.MapPieces)
  }
  // The plot will be moved to the resolved plots box unless toAvailable is true
  def removePlotFromCountry(name: String, mapPlot: PlotOnMap, toAvailable: Boolean = false): Unit = {
    val c = game.getCountry(name)
    val index = c.plots.indexOf(mapPlot)
    assert(index >= 0, s"removePlotFromCountry(): $mapPlot is not present in $name")
    val newPlots = c.plots.take(index) ::: c.plots.drop(index + 1)
    c match {
      case m: MuslimCountry    => game = game.updateCountry(m.copy(plots = newPlots))
      case n: NonMuslimCountry => game = game.updateCountry(n.copy(plots = newPlots))
    }
    if (toAvailable) {
      val updatedPlots = game.plotData.copy(availablePlots = mapPlot.plot :: game.availablePlots)
      game = game.copy(plotData = updatedPlots)
      log(s"Move $mapPlot from $name to the available plots box", Color.MapPieces)
    }
    else {
      val updatedPlots = game.plotData.copy(resolvedPlots = mapPlot.plot :: game.resolvedPlots)
      game = game.copy(plotData = updatedPlots)
      log(s"Move $mapPlot from $name to the resolved plots box", Color.MapPieces)
    }
  }

    // For adjustment only
  def removeWMDPlotFromCountry(name: String, mapPlot: PlotOnMap): Unit = {
    val c = game.getCountry(name)
    val index = c.plots.indexOf(mapPlot)
    assert(index >= 0, s"removeWMDPlotFromCountry(): $mapPlot is not present in $name")
    val newPlots = c.plots.take(index) ::: c.plots.drop(index + 1)
    c match {
      case m: MuslimCountry    => game = game.updateCountry(m.copy(plots = newPlots))
      case n: NonMuslimCountry => game = game.updateCountry(n.copy(plots = newPlots))
    }

    val updatedPlots = game.plotData.copy(removedPlots = mapPlot.plot :: game.removedPlots)
    game = game.copy(plotData = updatedPlots)
    log(s"Move $mapPlot from $name to out of play", Color.MapPieces)
  }

  def jihadistChoosesToDeclareCaliphate(countryName: String, numCells: Int): Boolean =
    numCells >= 3 &&
    canDeclareCaliphate(countryName) &&
    ((isHuman(Jihadist) && askDeclareCaliphate(countryName)) ||
     (isBot(Jihadist)   && JihadistBot.willDeclareCaliphate(countryName)))

  def saveAdjustment(desc: String): Unit = {
    game = game.copy(turnActions = AdjustmentMade(desc) :: game.turnActions)
    saveGameState()
  }
  def saveAdjustment(country: String, desc: String): Unit = {
    game = game.copy(turnActions = AdjustmentMade(s"$country -> $desc") :: game.turnActions)
    saveGameState()
  }

  def adjustCommand(param: Option[String]): Unit = {
    def displayHelp(): Unit = {
      val help = """|
        |Adjust command help
        |-----------------------------------------------------------------------------
        |There is no menu for the adjust command.
        |
        |To adjust some state within the game use the 'adjust' command followed by
        |a argument specifying what you want to adjust.
        |The argument can be shortend to it a unique prefix of the argument.
        |For example: 'a pre'  is equivalent to 'adjust prestige'
        |
        |a <country>   -- Settings for the named country
        |a prestige    -- US prestige level
        |a posture     -- US posture
        |a funding     -- Jihadist funding level
        |a reserves    -- US/Jihadist reserves
        |a cards       -- Move cards between draw pile/discard/removed
        |                 and the number of cards in player/bot hand
        |a troops      -- Number of troops in the Offmap box
        |a plots       -- Available/resolved plots
        |a resolved    -- Countries where a plot was resolved in the plot resolution
        |a events      -- Global events that are in play
        |a markers     -- Marker for discarded Lapsing event/1st plot
        |a difficulty  -- Jihadist ideology/US resolve
        |a game length -- Game length in decks (not available in campaign game)
        |a color       -- Toggle color in logs
        |                 (Does not work with Windows 10.0 or older)
        |a enhanced    -- Toggle enhanced Bot implementation (Jihadist Bot only)
        |a enhlevel    -- Enhanced Bot difficultly level
        |a auto roll   -- Toggle auto rolling for Player dice rolls
        |a manual roll -- Toggle manual dice rolls for all rolls
        |a bot log     -- Toggle bot log entries for debugging
        |a help        -- Show this help message
        |-----------------------------------------------------------------------------
        """.stripMargin
      displayLine(help)
    }

    val options = (List(
      "prestige", "funding", "difficulty", "cards", "game length",
      "markers", "events" , "reserves",
      "plots", "troops", "posture", "auto roll",
      "bot logging", "enhanced", "enhlevel", "manual roll", "color",
      "resolved", "help"
    ).sorted:::countryNames(game.countries).sorted)
      .filter {
        case "game length" => !game.campaign && game.deckNumber < 3
        case _ => true
      }

    def adjustEntity(entity: String): Unit = {
      entity match {
        case "prestige"    => adjustPrestige()
        case "posture"     => adjustUsPosture()
        case "funding"     => adjustFunding()
        case "troops"      => adjustOffmapTroops()
        case "auto roll"   => adjustAutoRoll()
        case "resolved"    => adjustPlotTargets()
        case "difficulty"  => adjustDifficulty()
        case "game length" => adjustGameLength()
        case "bot logging" => adjustBotLogging()
        case "enhanced"    => adjustBotEnhancements()
        case "enhlevel"    => adjustEnhBotDifficulty()
        case "manual roll" => adjustBotManualDieRolls()
        case "color"       => adjustShowColor()
        case "cards"       => adjustCardLocations()
        case "markers"     => adjustLapsingMarkers()
        case "events"      => adjustEventMarkers()
        case "reserves"    => adjustReserves()
        case "plots"       => adjustPlots()
        case "help" =>
          displayHelp()
          adjustCommand(None)
        case name          => adjustCountry(name)
      }
    }

    val prompt = "[Adjust] (h for help, blank to cancel): "
    val HELP  = """(?:\?|--help|-h)""".r
    param.map(_.trim) match {
      case Some(param) if HELP.matches(param) =>
        displayHelp()

      case _ =>
        askOneOf(prompt, options, param, true, false, CountryAbbreviations)
          .foreach(adjustEntity)
    }
  }

  def adjustInt(name: String, current: Int, range: Range): Option[Int] = {
    val prompt = s"$name is $current.  Enter new value (${range.min} - ${range.max}) "
    @tailrec def getResponse(): Option[Int] =
      readLine(prompt) match {
        case null | "" => None
        case INTEGER(x) if range contains x.toInt => Some(x.toInt)
        case input =>
          println(s"$input is not valid")
          getResponse()
      }
    getResponse()
  }

  def adjustPrestige(): Unit = {
    adjustInt("Prestige", game.prestige, 1 to 12) foreach { value =>
      logAdjustment("Prestige", game.prestige, value)
      game = game.copy(prestige = value)
      saveAdjustment("Prestige")
    }
  }

  def adjustUsPosture(): Unit = {
    val newValue = oppositePosture(game.usPosture)
    logAdjustment("US posture", game.usPosture, newValue)
    game = game.copy(usPosture = newValue)
    saveAdjustment("US posture")
  }

  def adjustFunding(): Unit = {
    adjustInt("Funding", game.funding, 1 to 9) foreach { value =>
      logAdjustment("Funding", game.funding, value)
      game = game.copy(funding = value)
      saveAdjustment("Funding")
    }
  }

  def adjustOffmapTroops(): Unit = {
    adjustInt("Offmap troops", game.offMapTroops, 0 to (game.offMapTroops + game.troopsAvailable)) foreach { value =>
      logAdjustment("Offmap troops", game.offMapTroops, value)
      game = game.copy(offMapTroops = value)
      saveAdjustment("Offmap troops")
    }
  }

  def adjustAutoRoll(): Unit = {
    logAdjustment("Human auto roll", game.humanAutoRoll, !game.humanAutoRoll)
    game = game.copy(humanAutoRoll = !game.humanAutoRoll)
    saveAdjustment("Human auto roll")
  }

  def adjustReserves(): Unit = {
    val choices = List(US.toString, Jihadist.toString)
    for {
      roleName <- askOneOf(s"Which side (${orList(choices)}): ", choices, allowNone = true, allowAbort = false)
      role     = Role(roleName)
      current  = if (role == US) game.reserves.us else game.reserves.jihadist
      value    <- adjustInt(s"$role reserves", current, 0 to 2)
    } {
      logAdjustment(s"$role reserves", current, value)
      role match {
        case US       => game = game.copy(reserves = game.reserves.copy(us = value))
        case Jihadist => game = game.copy(reserves = game.reserves.copy(jihadist = value))
      }
      saveAdjustment(s"$role reserves")
    }
  }

  def adjustDifficulty(): Unit = {
    val AllLevels = if (game.botRole == US) AllUSLevels
    else AllJihadistLevels
    val AllNames = AllLevels.map(_.name)
    var inEffect = game.botDifficulties.map(_.name)

    val label = if (game.botRole == US) "US resolve" else "Jihadist ideology"
    val standard = for (num <- 1 to AllNames.size; included = AllNames.take(num))
      yield s"$num) ${included.mkString(", ")}"
    // Don't allow the first name as a choice, as it represents the base
    // line difficulty and cannot be removed.
    val choices = List.range(1, 7) ::: AllNames.drop(1)

    @tailrec def getNextResponse(): Unit = {
      val current = s"Current $label: ${inEffect.mkString(", ")}"
      val help1 = "" :: current :: separator(current.length) ::
                  s"Select a number for standard $label combinations" :: standard.toList

      val help2 = "" :: s"Or enter the name of a $label to toggle its inclusion" ::
                     AllLevels.map(_.toString)

      help1 foreach println
      help2 foreach println
      askOneOf(s"$label: ", choices, allowNone = true, allowAbort = false) match {
        case None =>
        case Some(INTEGER(x)) =>
          inEffect = AllNames.take(x.toInt)
          getNextResponse()
        case Some(name) if inEffect contains name =>
          inEffect = inEffect.filterNot(_ == name)
          getNextResponse()
        case Some(name) =>
          inEffect = AllNames.filter(n => (inEffect contains n) || n == name) // Maintain standard order.
          getNextResponse()
      }
    }
    getNextResponse()
    val updated = inEffect.map(BotDifficulty.apply)
    if (updated != game.botDifficulties) {
      logAdjustment(s"$label", game.botDifficulties.map(_.name), updated.map(_.name))
      game = game.copy(botDifficulties = updated)
      saveAdjustment("Bot difficulty")
    }
  }

  def adjustGameLength(): Unit = {
    adjustInt("Game length", game.gameLength, game.deckNumber to 3) foreach { value =>
      logAdjustment("Game length", game.gameLength, value)
      game = game.copy(gameLength = value)
      saveAdjustment("Game length")
    }
  }

  //  Whether or not to log dice rolls in Trung decisions
  def adjustShowColor(): Unit = {
    val newValue = !game.showColor
    logAdjustment("Show color", game.showColor, newValue)
    game = game.copy(showColor = newValue)
    saveAdjustment("Show Color")
  }

  def adjustBotLogging(): Unit = {
    val newValue = !game.botLogging
    logAdjustment("Bot logging", game.botLogging, newValue)
    game = game.copy(botLogging = newValue)
    saveAdjustment("Bot logging")
  }

  def adjustBotEnhancements(): Unit = {
    val newValue = !game.botEnhancements
    logAdjustment("Bot enhancements", game.botEnhancements, newValue)
    game = game.copy(botEnhancements = newValue)
    saveAdjustment("Bot enhancements")
  }

  def adjustEnhBotDifficulty(): Unit = {
    val choices = EnhBotDifficulty.All.map(d => d -> d.name)
    val newValue = askMenu("Enhanced Bot difficulty level:", choices).head
    logAdjustment("Enh Bot difficulty", game.enhBotDifficulty, newValue)
    game = game.copy(enhBotDifficulty = newValue)
    saveAdjustment("Enh Bot difficulty")
  }

  def adjustBotManualDieRolls(): Unit = {
    val newValue = !game.manualDieRolls
    logAdjustment("Manual die rolls", game.manualDieRolls, newValue)
    game = game.copy(manualDieRolls = newValue)
    saveAdjustment("Manual die rolls")
  }

  // This is a helper function for adjustCardLocations()
  // For a card in the first plot box there is no need to
  // prompt.
  // For the others we present a menu if the number of availble
  // cards is less than 25,
  // otherwise we prompt for the card number.
  // Returns None if the use changes their mind.
  private def getCardToMove(source: CardDrawSource): Option[Int] = {
    source match {
      case From1stPlot => game.firstPlotEntry.map(_.cardNumber)
      case source =>
        val candidates = cardsInSource(source)
        candidates.size match {
          case 0 => None
          case 1 => candidates.headOption
          case n if n < 25 =>
            val choices = candidates.sorted.map(n => Some(n) -> cardNumAndName(n)) :+
              (None -> "Do not select a card.")
            askMenu(s"Select which card in the ${source.name}:", choices).head
          case _ =>
            println(s"\nThese cards are in the ${source.name}:")
            println(separator())
            wrapInColumns("", candidates.sorted.map(cardNumAndName))
              .foreach(println)
            askCardNumber(source::Nil, s"Enter # of card in the ${source.name} (blank for none): ", allowAbort = false)
        }
    }
  }

  val OutOfPlayTroopsLapsingEvents = Map(
    KoreanCrisis -> 2, EbolaScare -> 1, USBorderCrisis -> 1
  ).withDefaultValue(0)

  def countLapsingOopTroops(lapsingEvents: List[LapsingEntry]): Int =
    lapsingEvents.foldLeft(0)((sum, e) => sum + OutOfPlayTroopsLapsingEvents(e.cardNumber))

  // This function allows the user to move a card between one of the
  // following locations:
  // the deck, the discard pile, lapsing box, 1st plot box, or removed
  // Note: Currently we do not track the card numbers of cards in hand
  // (we only track the number of cards in hand)
  def adjustCardLocations(): Unit = {
    // Some lapsing cards affect the number of out of play troops that can be on the map.
    val sources = FromDrawPile::FromDiscard::FromLapsing::From1stPlot::FromRemoved::Nil
    val movedCards = new ListBuffer[Int]()
    val origOopTroops = countLapsingOopTroops(game.eventsLapsing)
    val origCardsInHand = game.cardsInHand

    def nextAdjustment(): Unit = {
      val choices = List(
        choice(cardsInSource(FromDrawPile).nonEmpty, Some(Left(FromDrawPile)), "Move card from the draw pile"),
        choice(cardsInSource(FromDiscard).nonEmpty,  Some(Left(FromDiscard)),  "Move card from the discard pile"),
        choice(cardsInSource(FromLapsing).nonEmpty,  Some(Left(FromLapsing)),  "Move card from the lapsing box"),
        choice(cardsInSource(From1stPlot).nonEmpty,  Some(Left(From1stPlot)),  "Move card from the 1st plot box"),
        choice(cardsInSource(FromRemoved).nonEmpty,  Some(Left(FromRemoved)),  "Move card from the removed pile"),
        choice(true,                                 Some(Right(US)),          "Change # of cards in US hand"),
        choice(true,                                 Some(Right(Jihadist)),    "Change # of cards in Jihadist hand"),
        choice(true, None, "Finished moving cards"),
      ).flatten

      askMenu("Adjust cards:", choices, allowAbort = false).head match {
        case None => // Exit function
        case Some(Left(from)) =>
          getCardToMove(from) match {
          case None =>
            nextAdjustment()
          case Some(cardNum) =>
            val canLapse = deck(cardNum).lapsing != NoLapsing
            val canRemove = deck(cardNum).remove != NoRemove
            val destChoices = sources
              .filter { s =>
                s != from &&
                (s != FromLapsing || canLapse) &&
                (s != From1stPlot || game.firstPlotEntry.isEmpty) &&
                (s != FromRemoved || canRemove)
              }
              .map(s => s -> s.name) :+ (from -> "Do not move the card")
            val to = askMenu(s"Choose new location for [${cardNumAndName(cardNum)}]:", destChoices).head
            if (from != to) {
              movedCards += cardNum
              // Remove the card from its current location
              from match {
                case FromDiscard =>
                  game = game.copy(cardsDiscarded = game.cardsDiscarded.filterNot(_ == cardNum))
                case FromLapsing =>
                  val newLapsing = game.eventsLapsing.filterNot(_.cardNumber == cardNum)
                  game = game.copy(eventsLapsing = newLapsing)
                case From1stPlot =>
                  game = game.copy(firstPlotEntry = None)
                case FromRemoved =>
                  game = game.copy(cardsRemoved = game.cardsRemoved.filterNot(_ == cardNum))
                case _ => // FromDrawPile nothing to do
              }
              // Put it in its new location.
              to match {
                case FromDiscard =>
                  game = game.copy(cardsDiscarded = cardNum::game.cardsDiscarded)
                case FromLapsing =>
                  game = game.copy(eventsLapsing = LapsingEntry(cardNum)::game.eventsLapsing)
                case From1stPlot =>
                  game = game.copy(firstPlotEntry = Some(LapsingEntry(cardNum)))
                case FromRemoved =>
                  game = game.copy(cardsRemoved = cardNum::game.cardsRemoved)
                case _ => // FromDrawPile nothing to do
              }
            }
            nextAdjustment()
        }

      case Some(Right(US)) =>
        val maximum = 20 min (numCardsInDrawPile() + game.cardsInHand.us)
        displayLine(s"\nCards in US hand: ${game.cardsInHand.us}, Cards in Draw pile: ${numCardsInDrawPile()}")
        val newNum = askInt("\nNew number of cards in hand: ", 0, maximum)
        game = game.copy(cardsInHand = game.cardsInHand.copy(us = newNum))
        nextAdjustment()

      case Some(Right(Jihadist)) =>
        val maximum = 20 min (numCardsInDrawPile() + game.cardsInHand.jihadist)
        displayLine(s"\nCards in Jihadist hand: ${game.cardsInHand.jihadist}, Cards in Draw pile: ${numCardsInDrawPile()}")
        val newNum = askInt("\nNew number of cards in hand: ", 0, maximum)
        game = game.copy(cardsInHand = game.cardsInHand.copy(jihadist = newNum))
        nextAdjustment()
    }

    }

    nextAdjustment()

    val moved = movedCards.sorted.distinct
    if (moved.nonEmpty || origCardsInHand != game.cardsInHand) {
      val desc = new ListBuffer[String]
      if (moved.nonEmpty) {
        logAdjustment(s"Location of cards [${moved.map(cardNumAndName).mkString(", ")}]")
        desc += "card locations"
      }
      if (origCardsInHand.us != game.cardsInHand.us) {
        logAdjustment(s"Number of cards in US hand changed from ${origCardsInHand.us} to ${game.cardsInHand.us}")
        desc += "cards in US hand"
      }
      if (origCardsInHand.jihadist != game.cardsInHand.jihadist) {
        logAdjustment(s"Number of cards in Jihadist hand changed from ${origCardsInHand.jihadist} to ${game.cardsInHand.jihadist}")
        desc += "cards in Jihadist hand"
      }
      // Check to see if we need to move troops to/from the out of play box
      val newOopTroops = countLapsingOopTroops(game.eventsLapsing)
      val delta = newOopTroops - origOopTroops
      if (delta > 0) {
        val items = selectTroopsToPutOffMap(newOopTroops - newOopTroops)
        for (MapItem(name, num) <- items)
          putTroopsInOffMapBox(name, num)
      }
      else if (delta < 0)
        moveOfMapTroopsToTrack(newOopTroops - newOopTroops)
      saveAdjustment(desc.toList.mkString(", "))
    }
  }


  // Lapsing cards and the first plot card can be removed by events.
  // When this happens, the lapsing event/first plot is marked because
  // its effect remains until the end of the current turn.
  def adjustLapsingMarkers(): Unit = {
    val movedLapsing = new ListBuffer[Int]()
    val canChangeFirstPlot = game.firstPlotEntry.map(_.discarded).getOrElse(true)
    val origFirstPlot = game.firstPlotEntry
    val origOopTroops = countLapsingOopTroops(game.eventsLapsing)

    def nextAdjustment(): Unit = {
      val (lapsing, notLapsing) = LapsingCards
        .filter(n =>
            game.cardsInUse.contains(n) &&
            !game.isFirstPlot(n) &&
            !game.cardsRemoved.contains(n)
        )
        .partition(lapsingEventInPlay)
      val canAdd1stPlot = canChangeFirstPlot && game.firstPlotEntry.isEmpty
      val canRemove1stPlot = canChangeFirstPlot && game.firstPlotEntry.nonEmpty
      sealed trait Choice
      case object AddLapsing extends Choice
      case object RemoveLapsing extends Choice
      case object Add1stPlot extends Choice
      case object Remove1stPlot extends Choice
      case object Finished extends Choice
      val choices = List(
        choice(notLapsing.nonEmpty, AddLapsing,     "Add lapsing marker"),
        choice(lapsing.nonEmpty,    RemoveLapsing,  "Remove lapsing marker"),
        choice(canAdd1stPlot,       Add1stPlot,    "Add 1st plot marker"),
        choice(canRemove1stPlot,    Remove1stPlot, "Remove 1st plot marker"),
        choice(true,                Finished,        "Finished adjusting lapsing/1st plot markers")
      ).flatten

      askMenu("Choose one:", choices).head match {
        case AddLapsing =>
          val choices = notLapsing.map(n => n -> cardNumAndName(n)) :+
            (0 -> "Do not add a lapsing marker")

          askMenu("Chose the card associated with the marker to add:", choices).head match {
            case 0 =>
            case n =>
              movedLapsing += n
              val marker = LapsingEntry(n, true)
              game = game.copy(eventsLapsing = marker::game.eventsLapsing)
              displayLine(s"\n$marker added", Color.Info)
          }
          nextAdjustment()

        case RemoveLapsing =>
          val choices = lapsing.map(n => n -> cardNumAndName(n)) :+
            (0 -> "Do not remove a lapsing marker")
          askMenu("Chose the card associated with the marker to remove:", choices).head match {
            case 0 =>
            case n =>
              movedLapsing += n
              val marker = LapsingEntry(n, true)
              game = game.copy(eventsLapsing = game.eventsLapsing.filterNot(_ == marker))
              displayLine(s"\n$marker removed", Color.Info)
          }
          nextAdjustment()

        case Add1stPlot =>
          // The card number is irrelevant
            // Card number of zero used for 1st Plot marker
            game = game.copy(firstPlotEntry = Some(LapsingEntry(0, true)))
            displayLine(s"\nFirst plot marker added", Color.Info)
            nextAdjustment()

        case Remove1stPlot =>
            game = game.copy(firstPlotEntry = None)
            displayLine(s"\nFirst plot marker removed", Color.Info)
            nextAdjustment()

        case Finished =>
      }
    }

    displayLine("\nAdjust which event markers are in lapsing boxes and whether or not", Color.Info)
    displayLine("there is a marker in the 1st plot box.", Color.Info)
    displayLine("\nIf you want to place/remove the actual cards in these boxes then", Color.Info)
    displayLine("you should use the 'adjust cards' command.", Color.Info)

    nextAdjustment()

    val firstPlotChanged = canChangeFirstPlot && origFirstPlot.isEmpty != game.firstPlotEntry.isEmpty
    val moved = movedLapsing.sorted.distinct

    if (firstPlotChanged || moved.nonEmpty) {
      if (firstPlotChanged)
        logAdjustment(s"1st plot marker")
      if (moved.nonEmpty) {
        logAdjustment(s"Location of lapsing/1st plot markers for cards [${moved.map(cardNumAndName).mkString(", ")}]")
        // Check to see if we need to move troops to/from the out of play box
        val newOopTroops = countLapsingOopTroops(game.eventsLapsing)
        val delta = newOopTroops - origOopTroops
        if (delta > 0) {
          val items = selectTroopsToPutOffMap(delta)
          for (MapItem(name, num) <- items)
            putTroopsInOffMapBox(name, num)
        }
        else if (delta < 0)
          moveOfMapTroopsToTrack(delta.abs)
      }
      saveAdjustment("lapsing/1st plot markers")
    }
  }

  def adjustEventMarkers(): Unit = {
    case class OopTroops(marker: String, numTroops: Int)
    val OutOfPlayTroops = List(OopTroops(Sequestration, 3), OopTroops(SouthChinaSeaCrisis, 2))
    def countOopTrrops(inplayMarkers: List[String]): Int =
      inplayMarkers.foldLeft(0) { (sum, marker) =>
        sum + OutOfPlayTroops
          .find(_.marker == marker)
          .map(_.numTroops)
          .getOrElse(0)
      }

    val AllMarkers = GlobalMarkers.keys.toList.sorted
    var inPlay = game.markers
    val priorGameState = game
    def available = AllMarkers.filterNot(inPlay.contains)
    def showColums(xs: List[String]): Unit = {
      if (xs.isEmpty) println("none")
      else columnFormat(xs, 4) foreach println
    }
    @tailrec def getNextResponse(): Unit = {
      println()
      println(separator())
      println("Global markers that are currently in play:")
      showColums(inPlay)
      println()
      println("Global markers that are out of play:")
      showColums(available)
      println()
      println("Enter a marker name to move it between in play and out of play.")
      askOneOf("Marker: ", AllMarkers, allowNone = true, allowAbort = false) match {
        case None =>
        case Some(name) if inPlay contains name =>
          inPlay = inPlay.filterNot(_ == name)
          getNextResponse()
        case Some(name) =>
          inPlay = name :: inPlay
          if (name == TrumpTweetsON)
            inPlay = inPlay.filterNot(_ == TrumpTweetsOFF)
          else if (name == TrumpTweetsOFF)
            inPlay = inPlay.filterNot(_ == TrumpTweetsON)
          getNextResponse()
      }
    }
    getNextResponse()
    inPlay = inPlay.sorted
    if (inPlay != game.markers) {
      logAdjustment("Global Markers", game.markers, inPlay)
      // Check to see if we need to move troops to/from the out of play box
      val oldOop = countOopTrrops(game.markers)
      val newOop = countOopTrrops(inPlay)
      if (oldOop > newOop)
        moveOfMapTroopsToTrack(oldOop - newOop)
      else if (newOop > oldOop) {
        val items = selectTroopsToPutOffMap(newOop - oldOop)
        for (MapItem(name, num) <- items)
          putTroopsInOffMapBox(name, num)
      }

      game = game.copy(markers = inPlay)
      saveAdjustment("Global Markers")

      logExtraCellCapacityChange(priorGameState)
    }
  }

  def adjustPlotTargets(): Unit = {
    val origTargetNames = game.plotData.resolvedTargets.map(_.name)
    var targetNames = origTargetNames

    def nextAction(): Unit = {
      val nonTargets = countryNames(game.countries).filterNot(targetNames.apply)
      sealed trait Choice
      case object Add extends Choice
      case object Delete extends Choice
      case object Done extends Choice
      val choices = List(
        choice(nonTargets.nonEmpty,  Add,    "Add a country to the resolved plot targets"),
        choice(targetNames.nonEmpty, Delete, "Remove a country from the resolved plot targets"),
        choice(true,                 Done,   "Finished")
      ).flatten
      println("\nCountries where plots were resolved in the last plot resolution phase:")
      if (targetNames.isEmpty)
        println("none")
      else
        println(targetNames.toList.sorted.mkString(", "))
      askMenu("Choose one: ", choices, allowAbort = false).head  match {
        case Add  =>
          val name = askCountry("Select country to add: ", nonTargets.sorted, allowAbort = false)
          targetNames = targetNames + name
          nextAction()
        case Delete =>
          val name = askCountry("Select country to remove: ", targetNames.toList.sorted, allowAbort = false)
          targetNames = targetNames - name
          nextAction()
        case Done =>
      }
    }

    nextAction()
    if (targetNames != origTargetNames) {
      val targets = targetNames.map(name => PlotTarget(name, game.getCountry(name).isMuslim))
      logAdjustment(
        "Resolved plot targets", origTargetNames.toList.sorted, targetNames.toList.sorted)
      val updatedPlots = game.plotData.copy(resolvedTargets = targets)
      game = game.copy(plotData = updatedPlots)
      saveAdjustment("Resolved plot targets")
    }
  }

  def adjustPlots(): Unit = {
    class Location(val name: String, var plots: Vector[Plot])

    val Avail = new Location("Available Box", game.availablePlots.toVector)
    val Resolved = new Location("Resolved Box", game.resolvedPlots.toVector)
    val Removed = new Location("Removed WMD", game.removedPlots.toVector)
    val locations = List(Avail, Resolved, Removed)

    def showAll(): Unit = {
      val width = longestString(locations.map(_.name))
      println()
      for (loc <- locations)
        println(s"${padLeft(loc.name,  width)}: ${plotsDisplay(loc.plots.toList)}")
    }

    def askPlot(source: Location): Option[Plot] =
      source.plots match {
        case Nil => None
        case _ =>
          val choices = source.plots.toList.sorted.map(p => Some(p) -> p.toString) :+
            (None, "Cancel")
          askMenu(s"Move which plot:", choices, allowAbort = false).head
      }

    def moveFrom(source: Location): Unit = {
      askPlot(source) match {
        case None =>
        case Some(plot) =>
          val dests = if (plot == PlotWMD)
            List(Avail, Removed).filter(l => l != source)
          else
            List(Avail, Resolved).filter(l => l != source)
          val locChoices = dests.map(l => Some(l) -> l.name)
          val choices = locChoices ::: List(None ->"Cancel")
          askMenu(s"Move $plot to: ", choices, allowAbort = false).head match {
            case None =>
            case Some(dest) =>
              val index = source.plots.indexOf(plot)
              source.plots = source.plots.patch(index, Vector.empty, 1)
              dest.plots = dest.plots :+ plot
          }
      }
    }


    def nextAction(): Unit = {
      val locChoices = locations
        .filter(_.plots.nonEmpty)
        .map(l => Some(l) -> l.name)
      val choices = locChoices ::: List(None ->"Finished")

      showAll()
      askMenu("Move a plot from: ", choices, allowAbort = false).head match {
        case None =>
        case Some(source)  =>
          moveFrom(source)
          nextAction()
      }
    }

    if (locations.map(_.plots.size).sum == 0)
      println("Nothing to adjust, all plots are on the map")
    else {
      nextAction()

      val availableChanged = Avail.plots.toList.sorted != game.plotData.availablePlots.sorted
      val resolvedChanged = Resolved.plots.toList.sorted != game.plotData.resolvedPlots.sorted
      val removedChanged = Removed.plots.toList.sorted != game.plotData.removedPlots.sorted

      if (availableChanged || resolvedChanged || removedChanged) {
        if (availableChanged)
          logAdjustment("Available plots", plotsDisplay(game.availablePlots), plotsDisplay(Avail.plots.toList))
        if (resolvedChanged)
          logAdjustment("Resolved plots", plotsDisplay(game.resolvedPlots), plotsDisplay(Resolved.plots.toList))
        if (removedChanged)
          logAdjustment("Removed plots", plotsDisplay(game.removedPlots), plotsDisplay(Removed.plots.toList))

        val updatedPlots = game.plotData.copy(
         availablePlots = Avail.plots.toList,
         resolvedPlots  = Resolved.plots.toList,
         removedPlots   = Removed.plots.toList)
        game = game.copy(plotData = updatedPlots)
        saveAdjustment("Available, resolved, removed plots")
      }
    }
  }


  def adjustCountry(name: String): Unit = {
    @tailrec def getNextResponse(): Unit = {
      val (actions, choices) = if (game.isMuslim(name)) {
        val actions = ListMap(
          "active cells"       -> adjustActiveCells _,
          "advisors"           -> adjustCountryAdvisors _,
          "aid"                -> adjustAid _,
          "awakening"          -> adjustAwakening _,
          "alignment"          -> adjustAlignment _,
          "besieged regime"    -> adjustBesiegedRegime _,
          "cadre"              -> adjustCadres _,
          "caliphate capital"  -> adjustCaliphateCapital _,
          "civil war"          -> adjustCivilWar _,
          "flip to non-Muslim" -> adjustToNonMuslim _,
          "governance"         -> adjustGovernance _,
          "markers"            -> adjustCountryMarkers _,
          "militia"            -> adjustMilitia _,
          "plots"              -> adJustCountryPlots _,
          "reaction"           -> adjustReaction _,
          "regime change"      -> adjustRegimeChange _,
          "sleeper cells"      -> adjustSleeperCells _,
          "troops"             -> adjustTroops _,
          "wmd cache"          -> adjustCountryWMDCache _,
        )
        var choices = actions.keysIterator.toList
          .filter {
            case "flip to non-Muslim" => (name == Iran || name == Nigeria)
            case _ => true
          }
        (actions, choices)
      }
      else {
        val actions = ListMap(
          "active cells"   -> adjustActiveCells _,
          "cadre"          -> adjustCadres _,
          "flip to muslim" -> adjustToMuslim _,
          "markers"        -> adjustCountryMarkers _,
          "plots"          -> adJustCountryPlots _,
          "posture"        -> adjustCountryPosture _,
          "sleeper cells"  -> adjustSleeperCells _,
          "troops"         -> adjustTroops _, // Valid in Philippines if Abu Sayyaf
        )
        var choices = actions.keysIterator.toList
          .filter {
            case "posture" => !List(UnitedStates, Israel, Iran).contains(name)
            case "flip to muslim" => (name == Iran || name == Nigeria)
            case _ => true
          }
        (actions, choices)
      }

      for (SummaryEntry(text, color) <- game.countrySummary(name).entries)
        displayLine(text, color)
      displayLine(separator(54))

      askOneOf(s"[$name attribute] (? for list): ", choices, allowNone = true, allowAbort = false) match {
        case None        =>
        case Some(selected) =>
          actions(selected)(name)
          getNextResponse()
      }

    }
    // Start response loop
    getNextResponse()
  }


  def adjustCountryPosture(name: String): Unit = {
    game.getCountry(name) match {
      case _: MuslimCountry =>
        throw new IllegalArgumentException(s"Cannot set posture of Muslim country: $name")
      case n: NonMuslimCountry if n.iranSpecialCase =>
        println("Iran is a special case that does not have a posture.")
        pause()
      case n: NonMuslimCountry =>
        val choices = (PostureUntested::Soft::Hard::Nil).filterNot(_ == n.posture)
        val prompt = s"New posture (${orList(choices)}): "
        askOneOf(prompt, choices, allowNone = true, allowAbort = false) foreach { newPosture =>
          logAdjustment(name, "Posture", n.posture, newPosture)
          game = game.updateCountry(n.copy(postureValue = newPosture))
          saveAdjustment(name, "Posture")
        }
    }
  }

  def adjustAlignment(name: String): Unit = {
    game.getCountry(name) match {
      case _: NonMuslimCountry =>
        throw new IllegalArgumentException(s"Cannot set alignment of non-Muslim country: $name")
      case m: MuslimCountry if m.isUntested =>
        println(s"$name is untested. Set the governance first.")
        pause()
      case m: MuslimCountry =>
        val choices = (Ally::Neutral::Adversary::Nil).filterNot(_ == m.alignment)
        val prompt = s"New alignment (${orList(choices)}): "
        askOneOf(prompt, choices, allowNone = true, allowAbort = false) foreach { newAlignment =>
          logAdjustment(name, "Alignment", m.alignment, newAlignment)
          game = game.updateCountry(m.copy(alignment = newAlignment))
          checkTehranBeirutLandCorridor()
          saveAdjustment(name, "Alignment")
        }
    }
  }

  def adjustGovernance(name: String): Unit = {
    game.getCountry(name) match {
      case _: NonMuslimCountry =>
        throw new IllegalArgumentException(s"Cannot set governance of non-Muslim country: $name")
      case m: MuslimCountry =>
        val choices = List(GovernanceUntested, Good, Fair, Poor, IslamistRule)
          .filterNot(_ == m.governance)
          .map(govToString)
        val prompt = s"New governance (${orList(choices)}): "
        val choice = askOneOf(prompt, choices, allowNone = true, allowAbort = false)
        choice.map(govFromString).foreach { newGov =>
          // When a country becomes Good or Islamist Rule, the country cannot contain:
          //  aid, besiege regime, civil war, regime change, awakening, reaction
          // Further when the country becomes Good, it cannot be the Caliphate Capital.
          val goodOrIslamist = newGov == Good || newGov == IslamistRule
          val nixCapital      = newGov == IslamistRule && m.caliphateCapital
          val nixAid          = goodOrIslamist && m.aidMarkers != 0
          val nixBesieged     = goodOrIslamist && m.besiegedRegime
          val nixCivilWar     = goodOrIslamist && m.civilWar
          val nixRegimeChange = goodOrIslamist && m.inRegimeChange
          val nixAwakening    = goodOrIslamist && m.awakening != 0
          val nixReaction     = goodOrIslamist && m.reaction != 0
          val nixTrainingCamps= newGov == Good && m.hasMarker(TrainingCamps)
          val anyWarnings     = nixCapital || nixAid || nixBesieged || nixCivilWar ||
                                nixRegimeChange || nixAwakening || nixRegimeChange || nixTrainingCamps
          def warn(condition: Boolean, message: String): Unit = if (condition) println(message)
          warn(nixCapital, s"$name will no longer be the Caliphate Capital.\n" +
                           "The Caliphate will be removed completely.")
          warn(nixAid, "The aid markers will be removed.")
          warn(nixBesieged, "The besieged regime marker will be removed.")
          warn(nixCivilWar, "The civil war marker will be removed.")
          warn(nixRegimeChange, "The regime change marker will be removed.")
          warn(nixAwakening, "The awakening markers will be removed.")
          warn(nixReaction, "The reaction markers will be removed.")
          warn(nixTrainingCamps, "The Training Camps marker will be removed")
          if (!anyWarnings || askYorN(s"Do you wish to continue (y/n)? ")) {
            var updated = m
            logAdjustment(name, "Governance", govToString(updated.governance), govToString(newGov))
            updated = updated.copy(governance = newGov)
            if (updated.governance == IslamistRule && updated.alignment != Adversary) {
              logAdjustment(name, "Alignment", updated.alignment, Adversary)
              updated = updated.copy(alignment = Adversary)
            }
            if (nixCapital) {
              log(s"$name lost Caliphate Capital status.  Caliphate no longer declared.")
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
              endCivilWar(name)
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
            removeTrainingCamp_?(name)
            saveAdjustment(name, "Governance")
          }
        }
    }
  }

  def adjustActiveCells(name: String): Unit = {
    val c = game.getCountry(name)
    val maxCells = c.activeCells + game.cellsAvailable
    if (maxCells == 0) {
      println("There are no cells available to add to this country.")
      pause()
    }
    else
      adjustInt("Active cells", c.activeCells, 0 to maxCells) foreach { value =>
        if (value < c.activeCells)
          removeCellsFromCountry(name, c.activeCells - value, 0, false, addCadre = false, s"$name adjusted: ")
        else if (value > c.activeCells)
          addActiveCellsToCountry(name, value - c.activeCells, s"$name adjusted: ")
        saveAdjustment(name, "Active cells")
      }
  }

  def adjustSleeperCells(name: String): Unit = {
    val c = game.getCountry(name)
    val maxCells = c.sleeperCells + game.cellsAvailable
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
        if (value < c.sleeperCells)
          removeCellsFromCountry(name, 0, c.sleeperCells - value, false, addCadre = false, s"$name adjusted: ")
        else if (value > c.sleeperCells)
          addSleeperCellsToCountry(name, value - c.sleeperCells, s"$name adjusted: ")
        saveAdjustment(name, "Sleeper cells")
      }
  }

  def adjustCadres(name: String): Unit = {
    val c = game.getCountry(name)
    adjustInt("Cadres", c.cadres, 0 to 5) foreach { value =>
      logAdjustment(name, "Cadres", c.cadres, value)
      c match {
        case m: MuslimCountry    => game = game.updateCountry(m.copy(cadres = value))
        case n: NonMuslimCountry => game = game.updateCountry(n.copy(cadres = value))
      }
      saveAdjustment(name, "Cadres")
    }
  }

  def adjustTroops(name: String): Unit = {
    val c = game getCountry name
    val maxTroops = c.troops + game.troopsAvailable
    if (maxTroops == 0) {
      println("There are no troops available to add to this country.")
      pause()
    }
    else {
      adjustInt("Troops", c.troops, 0 to maxTroops) foreach { value =>
        logAdjustment(name, "Troops", c.troops, value)
        c match {
          case m: MuslimCountry    => game = game.updateCountry(m.copy(troops = value))
          case n: NonMuslimCountry => game = game.updateCountry(n.copy(troops = value))
        }
        saveAdjustment(name, "Troops")
      }
    }
  }

  def adjustMilitia(name: String): Unit = {
    game.getCountry(name) match {
      case _: NonMuslimCountry =>
        throw new IllegalArgumentException(s"Cannot add militia to non-Muslim country: $name")
      case m: MuslimCountry =>
        val maxMilitia = m.militia + game.militiaAvailable
        if (maxMilitia == 0) {
          println("There are no troops available to add to this country.")
          pause()
        }
        else {
          adjustInt("Militia", m.militia, 0 to maxMilitia) foreach { value =>
            logAdjustment(name, "Militia", m.militia, value)
            game = game.updateCountry(m.copy(militia = value))
          }
          saveAdjustment(name, "Militia")
        }
    }
  }

  def adjustAid(name: String): Unit = {
    game.getCountry(name) match {
      case _: NonMuslimCountry =>
        throw new IllegalArgumentException(s"Cannot add aid to non-Muslim country: $name")
      case m: MuslimCountry if m.isGood =>
        println("Cannot add aid to a country with Good governance")
        pause()
      case m: MuslimCountry if m.isIslamistRule =>
        println("Cannot add aid to a country under Islamist Rule")
        pause()
      case m: MuslimCountry =>
        adjustInt("Aid", m.aidMarkers, 0 to 10) foreach { value =>
          logAdjustment(name, "Aid", m.aidMarkers, value)
          game = game.updateCountry(m.copy(aidMarkers = value))
          saveAdjustment(name, "Aid")
        }
    }
  }

  def adjustAwakening(name: String): Unit = {
    game.getCountry(name) match {
      case _: NonMuslimCountry =>
        throw new IllegalArgumentException(s"Cannot add awakening markers to non-Muslim country: $name")
      case m: MuslimCountry if m.isGood =>
        println("Cannot add awakening markers to a country with Good governance")
        pause()
      case m: MuslimCountry if m.isIslamistRule =>
        println("Cannot add awakening markers to a country under Islamist Rule")
        pause()
      case m: MuslimCountry if m.civilWar =>
        println("Cannot add awakening markers to a country in Civil War")
        pause()
      case m: MuslimCountry =>
        adjustInt("Awakening markers", m.awakening, 0 to 10) foreach { value =>
          logAdjustment(name, "Awakening markers", m.awakening, value)
          game = game.updateCountry(m.copy(awakening = value))
          saveAdjustment(name, "Awakening markers")
        }
    }
  }

  def adjustReaction(name: String): Unit = {
    game.getCountry(name) match {
      case _: NonMuslimCountry =>
        throw new IllegalArgumentException(s"Cannot add reaction markers to non-Muslim country: $name")
      case m: MuslimCountry if m.isGood =>
        println("Cannot add reaction markers to a country with Good governance")
        pause()
      case m: MuslimCountry if m.isIslamistRule =>
        println("Cannot add reaction markers to a country under Islamist Rule")
        pause()
      case m: MuslimCountry if m.civilWar =>
        println("Cannot add reaction markers to a country in Civil War")
        pause()
      case m: MuslimCountry =>
        adjustInt("Reaction markers", m.reaction, 0 to 10) foreach { value =>
          logAdjustment(name, "Reaction markers", m.reaction, value)
          game = game.updateCountry(m.copy(reaction = value))
          saveAdjustment(name, "Reaction markers")
        }
    }
  }

  def adjustBesiegedRegime(name: String): Unit = {
    game.getCountry(name) match {
      case _: NonMuslimCountry =>
        throw new IllegalArgumentException(s"Cannot add besieged regime to non-Muslim country: $name")
      case m: MuslimCountry if m.isGood =>
        println("Cannot add besieged regime to a country with Good governance")
        pause()
      case m: MuslimCountry if m.isIslamistRule =>
        println("Cannot add besieged regime to a country under Islamist Rule")
        pause()
      case m: MuslimCountry =>
        val newValue = !m.besiegedRegime
        logAdjustment(name, "Besieged regime", m.besiegedRegime, newValue)
        game = game.updateCountry(m.copy(besiegedRegime = newValue))
        saveAdjustment(name, "Besieged regime")
    }
  }

  def adjustRegimeChange(name: String): Unit = {
    game.getCountry(name) match {
      case _: NonMuslimCountry =>
        throw new IllegalArgumentException(s"Cannot add Regime Change to non-Muslim country: $name")
      case m: MuslimCountry if m.isGood =>
        println("Cannot add Regime Change to a country with Good governance")
        pause()
      case m: MuslimCountry if m.isIslamistRule =>
        println("Cannot add Regime Change to a country under Islamist Rule")
        pause()
      case m: MuslimCountry =>
        val choices = (NoRegimeChange::GreenRegimeChange::TanRegimeChange::Nil).filterNot(_ == m.regimeChange)
        val prompt = s"New regime change value (${orList(choices)}): "
        askOneOf(prompt, choices, allowNone = true, allowAbort = false) foreach { newValue =>
          val nixCapital = newValue == NoRegimeChange && m.caliphateCapital
          if (nixCapital)
            println(s"$name will no longer be the Caliphate Capital.\n" +
                     "The Caliphate will be removed completely.")
          if (!nixCapital || askYorN(s"Do you wish continue (y/n)? ")) {
            logAdjustment(name, "Regime change", m.regimeChange, newValue)
            var updated = m.copy(regimeChange = newValue)
            if (nixCapital) {
              log(s"$name lost Caliphate Capital status.  Caliphate no longer declared.")
              updated = updated.copy(caliphateCapital = false)
            }
            game = game.updateCountry(updated)
            saveAdjustment(name, "Regime Change")
          }
        }
    }
  }

  def adjustCivilWar(name: String): Unit = {
    game.getCountry(name) match {
      case _: NonMuslimCountry =>
        throw new IllegalArgumentException(s"Cannot add Civil War to non-Muslim country: $name")
      case m: MuslimCountry if m.isGood =>
        println("Cannot add Civil War to a country with Good governance")
        pause()
      case m: MuslimCountry if m.isIslamistRule =>
        println("Cannot add Civil War to a country under Islamist Rule")
        pause()
      case m: MuslimCountry =>
        val newValue = !m.civilWar
        logAdjustment(name, "Civil War", m.civilWar, newValue)
        if (newValue)
          startCivilWar(name)
        else
          endCivilWar(name)
        saveAdjustment(name, "Civil War")
    }
  }

  def adjustCaliphateCapital(name: String): Unit = {
    val priorGameState = game
    game.getCountry(name) match {
      case _: NonMuslimCountry =>
        throw new IllegalArgumentException(s"Non-Muslim cannot be the Caliphate capital: $name")
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
        saveAdjustment(name, "Caliphate Capital")
        logExtraCellCapacityChange(priorGameState)
    }
  }

  // Move plots between country and available/resolved
  def adJustCountryPlots(name: String): Unit = {
    sealed trait Action {
      def perform(): Unit
    }
    sealed trait AddAction extends Action {
      def candidates: List[Plot]
      def ok = candidates.nonEmpty
    }
    sealed trait RemoveAction extends Action {
      def candidates: List[PlotOnMap]
      def ok = candidates.nonEmpty
    }
    case object FromAvail extends AddAction {
      def candidates = game.availablePlots.sorted
      def perform(): Unit = {
        val plot = askPlots(candidates, 1, allowAbort = false).head
        addAvailablePlotToCountry(name, plot, ignoreAbuSayyaf = true)
      }
    }
    case object FromResolved extends AddAction {
      def candidates = game.resolvedPlots.sorted
      def perform(): Unit = {
        val plot = askPlots(candidates, 1, allowAbort = false).head
        addResolvledPlotToCountry(name, plot)
      }
    }
    case object FromRemoved extends AddAction {
      def candidates = game.removedPlots.sorted
      def perform(): Unit = {
        addRemovedPlotToCountry(name, PlotWMD)
      }
    }
    case object ToAvail extends RemoveAction {
      def candidates = game.getCountry(name).plots.sorted
      def perform(): Unit = {
        val plot = askMapPlots(candidates, 1, allowAbort = false).head
        removePlotFromCountry(name, plot, toAvailable = true)
      }
    }
    case object ToResolved extends RemoveAction {
      def candidates = game.getCountry(name).plots.filter(_.plot != PlotWMD).sorted
      def perform(): Unit = {
        val plot = askMapPlots(candidates, 1, allowAbort = false).head
        removePlotFromCountry(name, plot, toAvailable = false)
      }
    }
    case object ToRemoved extends RemoveAction {
      def candidates = game.getCountry(name).plots.filter(_.plot == PlotWMD).sorted
      def perform(): Unit = {
        // Unless one is backlashed, then all WMD are identical
        val plot = if (candidates.forall(_ == candidates.head))
          candidates.head
        else
          askMapPlots(candidates, 1, allowAbort = false).head
        removeWMDPlotFromCountry(name, plot)
      }
    }

    def nextAction(): Unit = {
      val choices = List(
        choice(ToAvail.ok,      Some(ToAvail),   s"Remove to available"),
        choice(ToResolved.ok,   Some(ToResolved), s"Remove to resolved"),
        choice(ToRemoved.ok,    Some(ToRemoved),  s"Remove WMD to out of play"),
        choice(FromAvail.ok,    Some(FromAvail),  s"Add from available"),
        choice(FromResolved.ok, Some(FromResolved),  s"Add from resolved"),
        choice(FromRemoved.ok,  Some(FromRemoved),  s"Add WMD from out of play"),
        choice(true,            None, "Finished")
      ).flatten

      displayLine(s"\n$name")
      displayLine(separator())
      displayLine(s"In country   : ${mapPlotsDisplay(ToAvail.candidates)}")
      displayLine(s"Available box: ${plotsDisplay(FromAvail.candidates)}")
      displayLine(s"Resolved box : ${plotsDisplay(FromResolved.candidates)}")
      displayLine(s"Out of play  : ${plotsDisplay(FromRemoved.candidates)}")

      askMenu("Choose plot adjustment: ", choices, allowAbort = false).head  match {
        case Some(action) =>
          action.perform()
          nextAction()
        case None =>
      }
    }

    val origCountry  = ToAvail.candidates
    val origAvail    = FromAvail.candidates
    val origResolved = FromResolved.candidates
    val origRemoved  = FromRemoved.candidates

    if (origCountry.isEmpty && origAvail.isEmpty && origResolved.isEmpty && origRemoved.isEmpty)
      displayLine("Nothing to adjust, there are no plots", Color.Info) // Should never happen!
    else {
      nextAction()

      val countryChanged = origCountry != ToAvail.candidates
      val availableChanged = origAvail != FromAvail.candidates
      val resolvedChanged = origResolved != ToResolved.candidates
      val removedChanged = origRemoved != ToRemoved.candidates
      if (countryChanged || availableChanged || resolvedChanged || removedChanged) {
        if (countryChanged)
          logAdjustment(name, "plots", mapPlotsDisplay(origCountry), mapPlotsDisplay(ToAvail.candidates))
        if (availableChanged)
          logAdjustment("Available plots", plotsDisplay(origAvail), plotsDisplay(FromAvail.candidates))
        if (resolvedChanged)
          logAdjustment("Resolved plots", plotsDisplay(origResolved), plotsDisplay(FromResolved.candidates))
        if (removedChanged)
          logAdjustment("Removed plots", plotsDisplay(origRemoved), plotsDisplay(FromRemoved.candidates))

        saveAdjustment(name, "Plots")
      }
    }
  }

  // Note: Advisors are not handled by this function because a country can contain
  //       multiple Advisors markers.  Set adjustCountryAdvisors()
  def adjustCountryMarkers(name: String): Unit = {
    val country = game.getCountry(name)
    val priorGameState = game
    var inPlay = country.markers
    val AllMarkers = CountryMarkers.keys.toList.sorted
    def available = AllMarkers.filterNot(inPlay.contains)
    def showColums(xs: List[String]): Unit = {
      if (xs.isEmpty) println("none")
      else columnFormat(xs, 4) foreach println
    }
    @tailrec def getNextResponse(): Unit = {
      println()
      println(s"$name markers:")
      showColums(inPlay)
      println()
      println("Country markers that are out of play:")
      showColums(available)
      println()
      println(s"Enter a marker name to move it between $name and out of play.")
      askOneOf("Marker: ", AllMarkers, allowNone = true, allowAbort = false) match {
        case None =>
        case Some(Advisors) =>
          println("use 'adjust advisors' to adjust Advisors Markers")
          getNextResponse()
        case Some(name) if inPlay contains name =>
          inPlay = inPlay.filterNot(_ == name)
          getNextResponse()
        case Some(name) =>
          inPlay = name :: inPlay
          getNextResponse()
      }
    }
    getNextResponse()
    inPlay = inPlay.sorted
    if (inPlay != country.markers) {

      logAdjustment(name, "Markers", country.markers, inPlay)
      country match {
        case m: MuslimCountry    => game = game.updateCountry(m.copy(markers = inPlay))
        case n: NonMuslimCountry => game = game.updateCountry(n.copy(markers = inPlay))
      }

      logExtraCellCapacityChange(priorGameState)
      saveAdjustment(name, "Markers")
    }
  }

  def adjustCountryAdvisors(name: String): Unit = {
    val numInOtherCountries = game.muslims.map(m => if (m.name == name) 0 else m.numAdvisors).sum
    val m = game.getMuslim(name)
    var inPlace = m.markers.filter(_ == Advisors)
    val origNumAdvisors = inPlace.size
    var available = List.fill(3 - numInOtherCountries - inPlace.size)(Advisors)

    if (inPlace.isEmpty && available.isEmpty)
      println("All 3 of the Advisors markers are in other countries")
    else {
      def getNextResponse(): Unit = {
        println()
        println(s"$name now contains ${amountOf(inPlace.size, "Advisors marker")}")
        println(s"There are ${amountOf(available.size, "available Advisors marker")}")
        sealed trait Choice
        case object Place extends Choice
        case object Remove extends Choice
        case object Done extends Choice
        val choices = List(
          choice(available.nonEmpty, Place,  "Add an Advisors marker"),
          choice(inPlace.nonEmpty,   Remove, "Remove an Advisors marker"),
          choice(true,               Done,   "Finished")
        ).flatten
        askMenu("Choose one:", choices, allowAbort = false).head match {
          case Place =>
            inPlace = Advisors :: inPlace
            available = available.tail
            getNextResponse()

          case Remove =>
            available = Advisors :: available
            inPlace = inPlace.tail
            getNextResponse()

          case Done =>
        }
      }
      getNextResponse()

      if (inPlace.size != origNumAdvisors) {
        val newMarkers = inPlace ::: (m.markers.filterNot(_ == Advisors))
        logAdjustment(name, "Markers", m.markers, newMarkers)
        game = game.updateCountry(m.copy(markers = newMarkers))
        saveAdjustment(name, "Markers")
      }
    }
  }

  def adjustCountryWMDCache(name: String): Unit = {
    val origCache = game.getCountry(name).wmdCache
    val origAvail = game.plotData.numAvailWMD
    val origRemoved = game.plotData.numRemovedWMD

    def nextAction(): Unit = {
      val numCache = game.getCountry(name).wmdCache
      val numAvail = game.plotData.numAvailWMD
      val numRemoved = game.plotData.numRemovedWMD
      sealed trait Choice
      case object DelAvail extends Choice
      case object DelRemoved extends Choice
      case object AddAvail extends Choice
      case object AddRemoved extends Choice
      case object Done extends Choice
      val choices = List(
        choice(numCache > 0,   DelAvail,   "Remove to available box"),
        choice(numCache > 0,   DelRemoved, "Remove out of the game"),
        choice(numAvail > 0,   AddAvail,   "Add from available box"),
        choice(numRemoved > 0, AddRemoved, "Add from out of play"),
        choice(true,           Done,       "Finished")
      ).flatten

      displayLine(s"\n$name")
      displayLine(separator())
      displayLine(s"WMD Cache   : $numCache")
      displayLine(s"Available   : $numAvail")
      displayLine(s"Out of play : $numRemoved")
      askMenu(s"Adjust WMD cache in $name:", choices).head match {
        case DelAvail   =>
          val num = askInt("\nRemove how many to available?", 0, numCache)
          moveWMDCacheToAvailable(name, num)
          nextAction()
        case DelRemoved =>
          val num = askInt("\nRemove how many out of the game?", 0, numCache)
          removeCachedWMD(name, num, bumpPrestige = false)
          nextAction()
        case AddAvail   =>
          val num = askInt("\nAdd how many from available?", 0, numAvail)
          addAvailableWMDToCache(name, num)
          nextAction()
        case AddRemoved =>
          val num = askInt("\nAdd how many from out of play?", 0, numRemoved)
          addRemovedWMDToCache(name, num)
          nextAction()
        case Done =>
      }
    }

    nextAction()

    val cacheChanged = origCache != game.getCountry(name).wmdCache
    val availChanged = origAvail != game.plotData.numAvailWMD
    val removedChanged = origRemoved != game.plotData.numRemovedWMD

    if (cacheChanged || availChanged || removedChanged) {
      if (cacheChanged)
        logAdjustment(name, "WMD cache", origCache, game.getCountry(name).wmdCache)
      if (availChanged)
        logAdjustment(name, "Available WMD", origAvail, game.plotData.numAvailWMD)
      if (removedChanged)
        logAdjustment(name, "Removed WMD", removedChanged, game.plotData.numRemovedWMD)
      saveAdjustment(name, "WMD cache")
    }
  }

  def adjustToNonMuslim(name: String): Unit = {
    assert(name == Iran || name == Nigeria, s"Cannot flip $name to non-Muslim")
    if (game isMuslim name) {
      game = game.updateCountry(if (name == Iran) DefaultIran else DefaultNigeria)
      logAdjustment(name, "Flipped", "Muslim", "non-Muslim")
      saveAdjustment(name, "Flipped")
    }
  }

  def adjustToMuslim(name: String): Unit = {
    assert(name == Iran || name == Nigeria, s"Cannot flip $name to muslim")
    if (game isNonMuslim name) {
      game = game.updateCountry(if (name == Iran) DefaultMuslimIran else DefaultMuslimNigeria)
      logAdjustment(name, "Flipped", "non-Muslim", "Muslim")
      saveAdjustment(name, "Flipped")
    }
  }
}

