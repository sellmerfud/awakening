
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

    Option(classLoader.getResourceAsStream("version")) match {
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
  }

  def dieRoll = nextInt(6) + 1
  def humanDieRoll(prompt: String = "Enter die roll: ", allowAbort: Boolean = true) =
    if (game.humanAutoRoll)
      dieRoll
    else
      (askOneOf(prompt, 1 to 6, allowAbort = allowAbort) map (_.toInt)).get

  // If the given role is human, then prompt if necessary, otherwise produce the roll automatically
  def getDieRoll(role: Role, prompt: String = "Enter die roll: ", allowAbort: Boolean = true): Int = {
    if (role == game.humanRole)
      humanDieRoll(prompt, allowAbort)
    else
      dieRoll
  }

  val INTEGER = """(\d+)""".r

  sealed trait GameMode {
    val orderValue: Int
  }
  case object LabyrinthMode  extends GameMode { val orderValue = 1; override def toString() = "Labyrinth"   }
  case object AwakeningMode  extends GameMode { val orderValue = 2; override def toString() = "Awakening"   }
  case object ForeverWarMode extends GameMode { val orderValue = 3; override def toString() = "Forever War" }
  object GameMode {
    def apply(name: String): GameMode = name.toLowerCase match {
      case "labyrinth"   => LabyrinthMode
      case "awakening"   => AwakeningMode
      case "forever war" => ForeverWarMode
      case _ => throw new IllegalArgumentException(s"Invalid game mode name: $name")
    }
  }

  implicit val GameModeOrdering: Ordering[GameMode] = Ordering.by { m: GameMode => m.orderValue }


  sealed trait CardAssociation
  case object Unassociated extends CardAssociation { override def toString() = "Unassociated" }

  sealed trait Role extends CardAssociation
  case object US extends Role       { override def toString() = "US" }
  case object Jihadist extends Role { override def toString() = "Jihadist" }
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

  def oppositeRole(role: Role) = if (role == US) Jihadist else US

  implicit val BotDifficultyOrdering: Ordering[BotDifficulty] = Ordering.by { x: BotDifficulty => x.order }

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
  implicit val PlotOnMapOrdering: Ordering[PlotOnMap] = Ordering.by { x: PlotOnMap => x.plot }

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

  val DefaultMuslimIran        = MuslimCountry(Iran, isSunni = false, resources = 2, oilExporter = true,
                                                governance = Fair, alignment = Adversary)
  val DefaultMuslimNigeria     = MuslimCountry(Nigeria, isSunni = true, resources = 2, oilExporter = true,
                                               governance = Poor, alignment = Neutral)

  val DefaultMorocco           = MuslimCountry(Morocco, resources = 2)
  val DefaultAlgeriaTunisia    = MuslimCountry(AlgeriaTunisia, resources = 2, oilExporter = true)
  val DefaultLibya             = MuslimCountry(Libya, resources = 1, oilExporter = true)
  val DefaultEgypt             = MuslimCountry(Egypt, resources = 3)
  val DefaultSudan             = MuslimCountry(Sudan, resources = 1, oilExporter = true)
  val DefaultSomalia           = MuslimCountry(Somalia, resources = 1)
  val DefaultJordan            = MuslimCountry(Jordan, resources = 1)
  val DefaultSyria             = MuslimCountry(Syria, resources = 2)
  val DefaultCentralAsia       = MuslimCountry(CentralAsia, resources = 2)
  val DefaultTurkey            = MuslimCountry(Turkey, isSunni = false, resources = 2)
  val DefaultLebanon           = MuslimCountry(Lebanon, isSunni = false, resources = 1)
  val DefaultYemen             = MuslimCountry(Yemen, isSunni = false, resources = 1)
  val DefaultIraq              = MuslimCountry(Iraq, isSunni = false, resources = 3, oilExporter = true)
  val DefaultSaudiArabia       = MuslimCountry(SaudiArabia, isSunni = false, resources = 3, oilExporter = true)
  val DefaultGulfStates        = MuslimCountry(GulfStates, isSunni = false, resources = 3, oilExporter = true)
  val DefaultPakistan          = MuslimCountry(Pakistan, isSunni = false, resources = 2, wmdCache = 3)
  val DefaultAfghanistan       = MuslimCountry(Afghanistan, isSunni = false, resources = 1)
  val DefaultIndonesiaMalaysia = MuslimCountry(IndonesiaMalaysia, resources = 3, oilExporter = true)
  val DefaultMali              = MuslimCountry(Mali, resources = 1)

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
   Scandinavia       -> ((Schengen ::: SchengenLinks) filterNot (_ == Scandinavia)),
   EasternEurope     -> ((Schengen ::: SchengenLinks) filterNot (_ == EasternEurope)),
   Benelux           -> ((Schengen ::: SchengenLinks) filterNot (_ == Benelux)),
   Germany           -> ((Schengen ::: SchengenLinks) filterNot (_ == Germany)),
   France            -> ((Schengen ::: SchengenLinks) filterNot (_ == France)),
   Italy             -> ((Schengen ::: SchengenLinks) filterNot (_ == Italy)),
   Spain             -> ((Schengen ::: SchengenLinks) filterNot (_ == Spain)),
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
      (game hasCountry(_.name == adjName)) &&
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
    val names = if (globalEventInPlay(QatariCrisis) && (qatariCrisisAdjacencyMap contains name))
      qatariCrisisAdjacencyMap(name)
    else
      adjacencyMap(name)
    names filter adjFilter
  }
  def areAdjacent(name1: String, name2: String) = getAdjacent(name1) contains name2

  // Shortest distance between countries
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

  //  Return a list of countries with at least 1 cell that are closest to the
  //  target country
  def closestWithCells(target: String, allowSadr: Boolean = false): List[String] = {
    val candidates = countryNames(game.countries filter (c => c.name != target && (if (allowSadr) c.totalCells > 0 else c.cells > 0)))
    val ordered = (candidates map (name => (name, distance(name, target)))).sortWith((a, b) => a._2 < b._2)
    if (ordered.isEmpty)
      Nil
    else
      (ordered takeWhile (_._2 == ordered.head._2)) map (_._1)
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
      PersionGulfExporterNames filterNot (_ == Iran)
    else
      PersionGulfExporterNames
    game.getMuslims(names)
  }

  def nonPersianGulfExporters = game.muslims filter (m => m.oilExporter && !PersionGulfExporterNames.contains(m.name))

  def isPersionGulflExporter(name: String) = countryNames(persianGulfExporters) contains name
  def isNonPersionGulflExporter(name: String) = countryNames(nonPersianGulfExporters) contains name

  def plotsToStrings(plots: List[Plot], visible: Boolean = true): List[String] =
    (plots.size, visible) match {
      case (0, _)     => List("none")
      case (n, false) => List(amountOf(n, "hidden plot"))
      case (_, true)  => plots.sorted map (_.name)
    }

  def plotsDisplay(plots: List[Plot], visible: Boolean = true): String = (plots.size, visible) match {
    case (0, _)     => "none"
    case (n, false) => amountOf(n, "hidden plot")
    case (_, true)  => plots.sorted map (_.name) mkString ", "
  }

  def mapPlotsDisplay(plots: List[PlotOnMap], visible: Boolean = true): String = {
    val lashed = plots count (_.backlashed)
    (plots.size, visible) match {
      case (0, _)                   => "none"
      case (n, false) if lashed > 0 => s"${amountOf(n, "hidden plot")}, $lashed backlashed"
      case (n, false)               => amountOf(n, "hidden plot")
      case (_, true)                => plots.sorted map (_.toString) mkString ", "
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

  val AvengerCard = 242

  type CardEvent            = Role => Unit
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
    val name: String,
    val association: CardAssociation,
    val printedOps: Int,
    val remove: CardRemoval,
    val lapsing: CardLapsing,
    val autoTrigger: Boolean,
    val eventAlertsPlot: EventAlertsPlot,
    val eventRemovesLastCell: EventRemovesLastCell,
    val eventConditions: EventConditions,
    val executeEvent: CardEvent) {

    def ops: Int = printedOps

    def numAndName = s"#$number $name"
    override def toString() = s"${numAndName} (${opsString(ops)})"

    def eventIsPlayable(role: Role): Boolean =
      (association == Unassociated || association == role) && eventConditions(role, false)

    def eventWillTrigger(opponentRole: Role): Boolean = {
      association  == opponentRole &&
      opponentRole == game.botRole &&
      eventConditions(opponentRole, true)
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

  sealed trait Play {
    def name: String  // Used for quantifying type in save game files
    def numCards: Int
  }
  sealed trait CardPlay extends Play {
    val role: Role
  }

  // Used to keep track of cards played during the current turn
  // for display purposes only.  This is stored in the game state.
  case class PlayedCard(role: Role, cardNum: Int) extends CardPlay {
    override def name = "PlayedCard"
    override def numCards = 1
    override def toString() = s"$role played ${cardNumAndName(cardNum)}"
  }
  case class PlayedReassement(card1: Int, card2: Int) extends CardPlay {
    val role = US
    override def name = "PlayedReassessment"
    override def numCards = 2
    override def toString() = s"$role played ${cardNumAndName(card1)} and ${cardNumAndName(card2)}"
  }
  case class AdditionalCard(role: Role, cardNum: Int) extends CardPlay {
    override def name = "AdditionalCard"
    override def numCards = 1
    override def toString() = s"$role played additional card ${cardNumAndName(cardNum)}"
  }
  case class PlotsResolved(num: Int) extends Play {
    override def name = "PlotsResolved"
    override def numCards = 0
    override def toString() = s"$num Plots were resolved"
  }
  case class AdjustmentMade(desc: String) extends Play {
    override def name = "AdjustmentMade"
    override def numCards = 0
    override def toString() = s"Adjustment made: $desc"
  }

  class CardDeck(val cardMap: Map[Int, Card]) {
    def isValidCardNumber(num: Int): Boolean = cardMap contains num
    def apply(num: Int): Card      = cardMap(num)  // Allows deck(4) to get a specific card
    def cards: List[Card]          = cardMap.valuesIterator.toList.sorted
    def lapsing: List[Card]        = cards filter (_.lapsing != NoLapsing)
    def removable: List[Card]      = cards filter (_.remove != NoRemove)

  }

  val deck = new CardDeck(AwakeningCards.deckMap ++ LabyrinthCards.deckMap ++ ForeverWarCards.deckMap)
  def cardNumAndName(number: Int): String = deck(number).numAndName
  def cardNumsAndNames(xs: List[Int]): String = xs.sorted map cardNumAndName mkString ", "

  sealed trait Country {
    val name: String
    val governance: Int
    val sleeperCells: Int
    val activeCells: Int
    val hasCadre: Boolean
    val troops: Int
    val plots: List[PlotOnMap]
    val markers: List[String]
    val wmdCache: Int        // Number of WMD plots cached

    def isMuslim: Boolean
    def isNonMuslim: Boolean = !isMuslim

    def isUntested: Boolean
    def isGood         = governance == Good
    def isFair         = governance == Fair
    def isPoor         = governance == Poor
    def isIslamistRule = governance == IslamistRule

    def hasMarker(name: String) = markers contains name
    def countMarker(name: String) = markers count (_ == name)

    def cells      = sleeperCells + activeCells
    def hasSadr    = hasMarker(Sadr)
    def totalCells = cells + (if (hasSadr) 1 else 0)

    def troopsMarkers: List[TroopsMarker] = markers collect {
      case NATO            => TroopsMarker(NATO,            2, canDeploy = true,  prestigeLoss = true)
      case NATO2           => TroopsMarker(NATO2,           2, canDeploy = true,  prestigeLoss = true)
      case UNSCR_1973      => TroopsMarker(UNSCR_1973,      1, canDeploy = false, prestigeLoss = false)
      case OperationServal => TroopsMarker(OperationServal, 1, canDeploy = false,  prestigeLoss = true)
    }

    def markerTroops: Int = troopsMarkers.foldLeft(0) { (total, tm) => total + tm.num }
    def deployableMarkerTroops = (troopsMarkers filter (_.canDeploy)).foldLeft(0) { (total, tm) => total + tm.num }
    def markerTroopsThatAffectPrestige: Int =
      troopsMarkers.foldLeft(0) { (total, tm) => total + (if (tm.prestigeLoss) tm.num else 0) }
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
  }

  case class NonMuslimCountry(
    name: String,
    governance: Int            = Good,
    sleeperCells: Int          = 0,
    activeCells: Int           = 0,
    hasCadre: Boolean          = false,
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
    def isOppositeUsPosture = !isUntested && posture != game.usPosture
    def canRemovePosture = !isUntested && !(iranSpecialCase || name == UnitedStates || name == Israel)
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
    hasCadre: Boolean          = false,
    plots: List[PlotOnMap] = Nil,
    markers: List[String]      = Nil,
    isSunni: Boolean           = true,
    resources: Int             = 0,
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
    def isAlly      = alignment == Ally
    def isNeutral   = alignment == Neutral
    def isAdversary = alignment == Adversary

    def canExportOil = oilExporter && !hasMarker(TradeEmbargoJihadist)
    def resourceValue = {
      val corridorPlus = if (name == Iran && hasMarker(TehranBeirutLandCorridor)) 1 else 0
      val opecCutMinus = if (canExportOil && game.cardLapsing(OPECProductionCut)) 1 else 0
      val spikePlus    = if (canExportOil) (game.oilPriceSpikes) else 0
      val hormuzPlus   = if (game.cardLapsing(StraitofHormuz) && isNonPersionGulflExporter(name)) 1 else 0
      val hormuzMinus  = if (game.cardLapsing(StraitofHormuz) && isPersionGulflExporter(name)) 1 else 0
      resources + corridorPlus + spikePlus + hormuzPlus - hormuzMinus - opecCutMinus
    }

    def isShiaMix = !isSunni
    def inRegimeChange = regimeChange != NoRegimeChange

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
    def canTakeBesiegedRegimeMarker = !(isIslamistRule || besiegedRegime)
    def canTakeAidMarker = !(isGood || isIslamistRule)
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

    def jihadDRM = awakening - reaction
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
    def wasOpsTarget(name: String)        = ops contains name
    def wasEventTarget(name: String)      = event contains name
    def wasOpsOrEventTarget(name: String) = wasOpsTarget(name) || wasEventTarget(name)
    def wasTestedOrImprovedToFairOrGood(name: String) = testedOrImprovedToFairOrGood contains name
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
  )

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
  }

  case class LogEntry(text: String, color: Option[Color])

  // A game segment containing a file name for the segment,
  // a short description and the log messages that were generated
  // during the game segment.
  case class GameSegment(save_number: Int, summary: Seq[String])

  case class GameState(
    scenarioName: String,
    startingMode: GameMode,
    campaign: Boolean,
    scenarioNotes: Seq[String],
    currentMode: GameMode,
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
    sequestrationTroops: Boolean   = false,  // true if 3 troops off map due to Sequestration event
    offMapTroops: Int              = 0,
    reserves: Reserves             = Reserves(0, 0),
    plays: List[Play]              = Nil,               // Cards plays/plot resolutions during current turn (most recent first).
    firstPlotCard: Option[Int]     = None,     // Card number
    cardsLapsing: List[Int]        = Nil,         // Card numbers currently lapsing
    cardsRemoved: List[Int]        = Nil,         // Card numbers removed from the game.
    targetsThisPhase: PhaseTargets = PhaseTargets(),
    targetsLastPhase: PhaseTargets = PhaseTargets(),
    exitAfterWin: Boolean          = true,
    botLogging: Boolean = false,
    history: Vector[GameSegment]   = Vector.empty,
    description: String            = "",
    showColor: Boolean             = !scala.util.Properties.isWin, // Default true except on Windows
    log: Vector[LogEntry]          = Vector.empty) {  // Log of the cuurent game segment

    def useExpansionRules   = currentMode == AwakeningMode || currentMode == ForeverWarMode
    def scenarioNameDisplay = if (campaign) s"$scenarioName -- Campaign" else scenarioName

    def botRole = if (humanRole == US) Jihadist else US

    def usResolve(name: BotDifficulty) = botRole == US && (botDifficulties contains name)
    def jihadistIdeology(name: BotDifficulty) = botRole == Jihadist && (botDifficulties contains name)

    def cardRemoved(num: Int) = cardsRemoved contains num
    def cardLapsing(num: Int) = cardsLapsing contains num

    def isFirstPlot(num: Int) = firstPlotCard == Some(num)
    def muslims    = countries filter (_.isInstanceOf[MuslimCountry]) map (_.asInstanceOf[MuslimCountry])
    def nonMuslims = countries filter (_.isInstanceOf[NonMuslimCountry]) map (_.asInstanceOf[NonMuslimCountry])

    def availablePlots      = plotData.availablePlots
    def resolvedPlots       = plotData.resolvedPlots
    def removedPlots        = plotData.removedPlots
    def resolvedPlotTargets = plotData.resolvedTargets

    // The methods assume a valid name and will throw an exception if an invalid name is used!
    def getCountry(name: String) = {
      if (currentMode == LabyrinthMode && !game.campaign && (name == Nigeria || name == Mali))
        throw new IllegalArgumentException(s"getCountry() '$name' is not available in Labyrinth mode")
      (countries find (_.name == name)).get
    }
    def getMuslim(name: String)    = (muslims find (_.name == name)).get
    def getNonMuslim(name: String) = (nonMuslims find (_.name == name)).get

    def getCountries(names: List[String]):  List[Country]          = names map getCountry
    def getMuslims(names: List[String]):    List[MuslimCountry]    = names map getMuslim
    def getNonMuslims(names: List[String]): List[NonMuslimCountry] = names map getNonMuslim

    def hasCountry(test: (Country) => Boolean)            = countries  exists test
    def hasMuslim(test: (MuslimCountry) => Boolean)       = muslims    exists test
    def hasNonMuslim(test: (NonMuslimCountry) => Boolean) = nonMuslims exists test

    def isMuslim(name: String)    = hasMuslim(_.name == name)
    def isNonMuslim(name: String) = hasNonMuslim(_.name == name)

    def adjacentCountries(name: String)   = getCountries(getAdjacent(name))
    def adjacentMuslims(name: String)     = getMuslims(getAdjacent(name) filter isMuslim)
    def adjacentNonMuslims(name: String)  = getNonMuslims(getAdjacent(name) filter isNonMuslim)
    def adjacentCountriesWithCells(name: String) = getCountries(getAdjacent(name)) filter (_.cells > 0)

    def adjacentToGoodAlly(name: String)     = game.adjacentMuslims(name) exists (m => m.isGood && m.isAlly)
    def adjacentToIslamistRule(name: String) = game.adjacentMuslims(name) exists (_.isIslamistRule)
    def adjacentToCivilWar(name: String)     = game.adjacentMuslims(name) exists (_.civilWar)

    // Return true if the given country is adjacent to at least on Sunni Muslim Country
    def adjacentToSunni(name: String) = adjacentMuslims(name) exists (_.isSunni)
    // Return true if the given country is adjacent to at least on Shi Mix Muslim Country
    def adjacentToShiaMix(name: String) = adjacentMuslims(name) exists (_.isShiaMix)

    def caliphateCapital: Option[String] = muslims find (_.caliphateCapital) map (_.name)
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
      this.copy(countries = changed :: (countries filterNot (_.name == changed.name)))

    def updateCountries(changed: List[Country]): GameState = {
      val updates = (changed map (c => (c.name -> c))).toMap
      this.copy(countries = countries map (c => updates.getOrElse(c.name, c)))
    }

    def adjustPrestige(amt: Int): GameState = this.copy(prestige = (prestige + amt) max 1 min 12)
    def adjustFunding(amt: Int): GameState  = this.copy(funding = (funding + amt) max 1 min 9)

    def addMarker(name: String): GameState = this.copy(markers = name :: markers)
    def removeMarker(name: String): GameState = this.copy(markers = markers filterNot (_ == name))

    // List of countries that are valid targets for War of Ideas
    def woiTargets: List[Country] = countries filter {
      case n: NonMuslimCountry => !(n.name == UnitedStates || n.name == Israel)
      case m: MuslimCountry    => m.isUntested || m.isNeutral || (m.isAlly && !m.isGood)
    }


    def prestigeModifier = prestige match {
      case x if x <  4 => -1
      case x if x <  7 =>  0
      case x if x < 10 =>  1
      case _           =>  2
    }

    // Returns the current GWOT
    // posture (Soft, Even, Hard)
    // value 0, 1, 2, 3
    def gwot: (String, Int) = {
      val value = (nonMuslims.filterNot(_.name == UnitedStates).foldLeft(0) {
        case (v, c) if c.isHard => v + 1
        case (v, c) if c.isSoft => v - 1
        case (v, _) => v // Untested
      })
      val posture = if (value == 0) Even else if (value < 0) Soft else Hard
      (posture, value.abs min 3)
    }

    def worldPosture = gwot._1

    // 0, 1, 2, 3
    def gwotPenalty: Int = {
      gwot match {
        case (posture, value)  if posture != usPosture => value min 3
        case _ => 0
      }
    }

    def troopCommitment = troopsAvailable match {
      case x if x <  5 => Overstretch
      case x if x < 10 => War
      case _           => LowIntensity
    }

    def prestigeLevel = prestige match {
      case x if x <  4 => Low
      case x if x <  7 => Medium
      case x if x < 10 => High
      case _           => VeryHigh
    }

    def fundingLevel = funding match {
      case x if x < 4 => Tight
      case x if x < 7 => Moderate
      case _          => Ample
    }

    def troopsOnMap   = countries.foldLeft(0) { (a, c) => a + c.troops }
    def militiaOnMap  = muslims.foldLeft(0) { (a, m) => a + m.militia }

    def troopsAvailable  = 15 - offMapTroops - troopsOnMap
    def militiaAvailable = 15 - militiaOnMap

    // Extra cells are only usable when funding is at 9 or by event or civil war attrition.
    // If some of those cells are on the map and the training camp/al-Baghdadai is removed, those
    // extra cells remain on the map until eliminated.
    // Training camp cells are always the last to be placed on the map and the first
    // to be removed.

    def trainingCamp        = muslims find (_.hasMarker(TrainingCamps)) map (_.name)
    def isTrainingCamp(name: String) = trainingCamp == Some(name)
    // def trainingCampCapacity= trainingCamp map (tc => if (isCaliphateMember(tc)) 5 else 3) getOrElse 0
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
    def oilPriceSpikes   = cardsLapsing count OilSpikeCards.contains
    def goodResources =
      muslims.filter(_.isGood).foldLeft(0) { (a, c) => a + c.resourceValue }
    def islamistResources =
      muslims.filter(_.isIslamistRule).foldLeft(0) { (a, c) => a + c.resourceValue } + (if (caliphateDeclared) 1 else 0)
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
      val fromCountries = countryNames(countries filter (_.canDeployFrom(ops)))
      val toCountries   = countryNames(countries filter (_.canDeployTo(ops)))
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
      val ms = countryNames(countries filter (_.maxDeployFrom > 5)) filterNot (_ == target)
      if (troopsAvailable > 5) "track" :: ms else ms
    }

    def regimeChangeTargets: List[String] = {
      val haveSource     = (target: String) => regimeChangeSourcesFor(target).nonEmpty
      val iranStrict     = game.currentMode == ForeverWarMode || game.campaign
      val (world, value) = gwot

      def isAllowed(m: MuslimCountry) = if (m.name == Iran && iranStrict)
        (game.prestigeLevel == High || game.prestigeLevel == VeryHigh) && world == Hard && value == 3
      else
        true

      val targets = countryNames(muslims filter { m =>
        (m.isIslamistRule && isAllowed(m))         ||
        (m.name == Iraq  && m.hasMarker(IraqiWMD)) ||
        (m.name == Libya && m.hasMarker(LibyanWMD))
      })
      // Only a valid target if there is a source of troops available
      targets filter haveSource
    }

    def regimeChangePossible(ops: Int) = {
      ops >= 3 && usPosture == Hard && regimeChangeTargets.nonEmpty
    }

    def withdrawFromTargets: List[String] = countryNames(muslims filter (m => m.inRegimeChange && m.troops > 0))

    def withdrawToTargets: List[String] = "track" :: countryNames(countries filter (_.canDeployTo(3)))

    def withdrawPossible(ops: Int) =
        ops >= 3 && usPosture == Soft && withdrawFromTargets.nonEmpty

    // Returns the losses that would occur if this country is the
    // target of a disrupt operation.
    // Some(Either(cells, ())) or None
    def disruptLosses(name: String): Option[Either[Int, Unit]] = {
      val c = getCountry(name)
      val alAnbar = globalEventInPlay(AlAnbar)
      val numLosses = c match {
        case m: MuslimCountry if alAnbar && (m.name == Iraq || m.name == Syria) => 1
        case m: MuslimCountry =>
          if ((m.totalTroopsAndMilitia) > 1 && (m.totalTroops > 0 || m.hasMarker(Advisors))) 2 else 1
        case n: NonMuslimCountry =>
          // Note only Philippines can have troops if AbuSayyaf marker is there
          if (n.isHard || n.totalTroops > 1) 2 else 1
      }
      if (c.cells > 0)
        Some(Left(numLosses min c.cells))
      else if (c.hasCadre)
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

    def alertTargets: List[String] = countryNames(countries filter (_.hasPlots))

    def warOfIdeasMuslimTargets(ops: Int): List[String] =
      countryNames(muslims filter (_.warOfIdeasOK(ops)))

    def warOfIdeasNonMuslimTargets(ops: Int): List[String] =
      countryNames(nonMuslims filter (_.warOfIdeasOK(ops)))

    def warOfIdeasTargets(ops: Int): List[String] =
      warOfIdeasMuslimTargets(ops) ::: warOfIdeasNonMuslimTargets(ops)


    def recruitTargets(madrassas: Boolean): List[String] =
      countryNames(countries filter (_.recruitOK(madrassas)))

    def recruitPossible = lapsingEventNotInPlay(GTMO) &&
                          cellsToRecruit > 0 &&
                          recruitTargets(madrassas = false).nonEmpty

    def jihadTargets: List[String] = countryNames(muslims filter (_.jihadOK))
    def jihadPossible = jihadTargets.nonEmpty

    def majorJihadTargets(ops: Int) = countryNames(muslims filter (_.majorJihadOK(ops)))
    def majorJihadPossible(ops: Int) = majorJihadTargets(ops).nonEmpty

    def plotTargets: List[String] = {
      val muslimTargets = muslims filter (m => !m.isIslamistRule && m.totalCells > 0)
      val nonMuslimTargets = nonMuslims filter (_.totalCells > 0)
      countryNames(muslimTargets ::: nonMuslimTargets)
    }
    def plotsAvailableWith(ops: Int) = availablePlots filter (_.opsToPlace <= ops)
    def plotPossible(ops: Int) = plotsAvailableWith(ops).nonEmpty && plotTargets.nonEmpty

    // --- Summaries --------------------------------------
    def playSummary: Seq[String] = {
      val b = new ListBuffer[String]
      b += s"Plays so far this turn"
      b += separator()
      if (plays.isEmpty)
        b += "none"
      else
        b ++= plays.reverse map (_.toString)
      b.toList
    }

    def scenarioSummary: Seq[String] = {
      val b = new ListBuffer[String]
      b += s"Scenario: ${scenarioNameDisplay}"
      b += separator()
      b += s"The Bot is playing the $botRole"
      b += (if (botRole == US) "US Resolve" else "Jihadist Ideology")
      for (difficulty <- botDifficulties)
        b += s"  $difficulty"
      if (scenarioNotes.nonEmpty) {
        b += ""
        b += "Scenario Notes:"
        b += separator()
        scenarioNotes foreach (line => b += line)
      }
      b += ""
      b += "Options:"
      b += separator()
      b += s"Exit game after victory: ${if (exitAfterWin) "yes" else "no"}"
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
      val activePlotCountries = countries filter (_.hasPlots)
      val b = new ListBuffer[String]
      b += s"Status"
      b += separator()
      b += s"Game Mode       : ${game.currentMode}"
      b += separator()
      b += f"US posture      : $usPosture | World posture     : ${worldPostureDisplay}  (GWOT penalty $gwotPenalty)"
      b += f"US prestige     : $prestige%2d   | Jihadist funding  : $funding%2d"
      b += f"US reserves     : ${reserves.us}%2d   | Jihadist reserves : ${reserves.jihadist}%2d"
      b += separator()
      if (game.useExpansionRules) {
        b += f"Troops on track : $troopsAvailable%2d   | Troops off map    : $offMapTroops%2d"
        b += s"Troop commitment: $troopCommitment"
        b += separator()
        b += f"Cells on track  : $cellsOnTrack%2d   | Militia on track  : $militiaAvailable%2d"
      }
      else {
        b += f"Troops on track : $troopsAvailable%2d   | Troop commitment  : $troopCommitment"
        b += separator()
        b += f"Cells on track  : $cellsOnTrack%2d"
      }
      b += f"Cells to recruit: ${cellsToRecruit}%2d   | Funding level     : ${fundingLevel}"
      if (game.useExpansionRules) {
        val albaghdadi = (globalEventInPlay(AlBaghdadi), game.caliphateDeclared) match {
          case (true, true) => Some(s"$AlBaghdadi with Caliphate")
          case (true, false) => Some(s"$AlBaghdadi without Caliphate")
          case (false, _) => None
        }
        val camp = game.trainingCamp.map(name => s"$TrainingCamps in $name")
        val events = albaghdadi.toList ::: camp.toList
        val eventDisplay = events match {
          case Nil => ""
          case xs  => xs.mkString(" (", ", ", ")")
        }
        b += separator()
        b += s"Extra cells$eventDisplay"
        val extraOnMap = (cellsOnMap - 15) max 0
        b += f"Available       : ${extraCellsAvailable}%2d   | On map            : ${extraOnMap}%2d"
        b += f"Capacity        : ${extraCellCapacity}%2d"
      }
      val ms = markers ::: (for (c <- countries; m <- c.markers) yield (s"$m (${c.name})"))
      b += separator()
      wrap( "Markers         : ", ms) foreach (l => b += l)
      wrap( "Lapsing         : ", cardsLapsing.sorted map cardNumAndName) foreach (l => b += l)
      b += s"1st plot        : ${firstPlotCard map cardNumAndName getOrElse "none"}"


      wrap( "Available plots : ", plotsToStrings(availablePlots, humanRole == Jihadist)) foreach (l => b += l)
      if (game.useExpansionRules)
        wrap( "Resolved plots  : ", plotsToStrings(resolvedPlots)) foreach (l => b += l)
      wrap( "Removed plots   : ", plotsToStrings(removedPlots)) foreach (l => b += l)
      if (activePlotCountries.isEmpty)
        b += s"Active plots    : none"
      else {
        b += s"Active plots"
        val fmt = "  %%-%ds : %%s".format(activePlotCountries.map(_.name.length).max)
        for (c <- activePlotCountries) {
          val visible = humanRole == Jihadist || (c.name == UnitedStates && c.hasMarker(NEST))
          b += fmt.format(c.name, mapPlotsDisplay(c.plots, visible))
        }
      }
      b.toList
    }

    // If show all is false, then some attributes will not be displayed
    // if they have value of zero.
    def countrySummary(name: String, showAll: Boolean = false): Seq[String] = {
      val b = new ListBuffer[String]
      val items = new ListBuffer[String]
      def item(num: Int, label: String, pluralLabel: Option[String] = None): Unit = {
        if (showAll || num > 0)
          items += amountOf(num, label, pluralLabel)
      }
      def addItems(): Unit = if (items.nonEmpty) b += s"  ${items mkString ", "}"

      getCountry(name) match {
        case n: NonMuslimCountry =>
          val posture = if (n.iranSpecialCase) "(Special Case)" else n.posture
          b += s"$name -- ${govToString(n.governance)}, $posture, Recruit ${n.recruitNumber}"
          item(n.activeCells, "Active cell")
          item(n.sleeperCells, "Sleeper cell")
          if (n.hasCadre)
            items += "Cadre marker"
          else if (showAll)
            items += "No Cadre marker"
          item(n.troops, "Troop")
          addItems()
          if (showAll || n.hasPlots) {
            val visible = humanRole == Jihadist || (name == UnitedStates && n.hasMarker(NEST))
            b += s"  Plots: ${mapPlotsDisplay(n.plots, visible)}"
          }
          if (showAll || n.markers.size > 0)
            b += s"  Markers: ${markersString(n.markers)}"
          if (n.wmdCache > 0)
            b += s"  WMD cache: ${amountOf(n.wmdCache, "WMD plot")}"


        case m: MuslimCountry =>
          val gov = if (m.isUntested) "Untested" else s"${govToString(m.governance)} ${m.alignment}"
          val res = amountOf(m.resourceValue, "resource")
          val oil = if (m.oilExporter && !m.hasMarker(TradeEmbargoJihadist)) List("Oil exporter") else Nil
          val autoRecruit = if (m.autoRecruit) List("Auto-Recruit") else Nil
          val desc = (gov :: res :: oil ::: autoRecruit).mkString(", ")
          b += s"$name -- $desc"
          item(m.activeCells, "Active cell")
          item(m.sleeperCells, "Sleeper cell")
          if (m.hasCadre)
            items += "Cadre marker"
          else if (showAll)
            items += "No Cadre marker"
          item(m.troops, "Troop")
          item(m.militia, "Militia", Some("Militia"))
          addItems()

          items.clear()
          item(m.aidMarkers, "Aid marker")
          item(m.awakening, "Awakening marker")
          item(m.reaction, "Reaction marker")
          if (m.besiegedRegime)
            items += "Besieged regime"
          else if (showAll)
            items += "No Besieged regime"
          addItems()

          items.clear()
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
            b += s"  Plots: ${mapPlotsDisplay(m.plots, humanRole == Jihadist)}"
          if (showAll || m.markers.size > 0)
            b += s"  Markers: ${markersString(m.markers)}"
          if (m.wmdCache > 0)
            b += s"  WMD cache: ${amountOf(m.wmdCache, "WMD plot")}"
      }
      b.toList
    }

    def removedCardsSummary: Seq[String] = {
      val b = new ListBuffer[String]
      b += "Cards removed from the game"
      b += separator()
      wrap("", game.cardsRemoved map (deck(_).numAndName)) foreach (l => b += l)
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
        case None =>
          b += "There is no Caliphate declared"
      }
      b.toList
    }

    def civilWarSummary: Seq[String] = {
      val b = new ListBuffer[String]
      b += "Civil Wars"
      b += separator()
      val civilWars = (muslims filter (_.civilWar)).toList
      if (civilWars.isEmpty)
        b += "There are no counties in civil war"
      else
        b += civilWars map (_.name) mkString ", "
      b.toList
    }
  }
  def initialGameState(
    scenario: Scenario,
    campaign: Boolean,
    humanRole: Role,
    humanAutoRoll: Boolean,
    botDifficulties: List[BotDifficulty],
    showColor: Boolean) = {

    var countries = if (scenario.startingMode == LabyrinthMode && !campaign)
      LabyrinthDefaultCountries
    else
      ExpansionDefaultCountries

    // Apply scenario overrides to countries.
    for (c <- scenario.countries)
      countries = c :: (countries filterNot (_.name == c.name))

    GameState(
      scenario.name,
      scenario.startingMode,
      campaign,
      scenario.notes,
      scenario.startingMode,
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
      cardsRemoved = scenario.cardsRemoved,
      showColor = showColor,
      offMapTroops = scenario.offMapTroops)
  }


  // Global variables
  var game = initialGameState(Awakening, false, US, true, Muddled :: Nil, !scala.util.Properties.isWin)

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

  // If num is 1 use the name as is
  // otherwise either use the plural if given or add an 's' to the name.
  def amountOf(num: Int, name: String, plural: Option[String] = None) =
    (num, plural) match {
      case (1, _)            => s"$num $name"
      case (_, Some(plural)) => s"$num $plural"
      case _                 => s"$num ${name}s"
    }

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

  // Find a match for the given string in the list of options.
  // Any unique prefix of the given options will succeed.
  def matchOne(s: String, options: Seq[String], abbreviations: Map[String, String] = Map.empty): Option[String] = {
    // Filter out any abbreviations that do not have a match with one of the options.
    val abbr = abbreviations filter { case (_, name) => options contains name }
    // When showing the list of options to the user, we want to group
    // all abbreviations with the word that they represent.
    val displayList = {
      var associations = Map.empty[String, Set[String]].withDefaultValue(Set.empty)
      for ((a, o) <- abbr)
        associations += o -> (associations(o) + a)
      options map {
        case o if associations(o).nonEmpty => o + associations(o).toList.sorted.mkString(" (", ",", ")")
        case o => o
      }
    }
    if (s == "?") {
      println(s"Enter one of:\n${orList(displayList)}")
      None
    }
    else {
      val normalizedAbbreviations = for ((a, v) <- abbr) yield (a.toLowerCase, v)
      val normalized = (options ++ abbr.keys) map (_.toLowerCase)
      (normalized.distinct filter (_ startsWith s.toLowerCase)) match {
        case Seq() =>
          println(s"'$s' is not valid. Must be one of:\n${orList(displayList)}")
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
  }

  def askOneOf(prompt: String,
               options: Seq[Any],
               initial: Option[String] = None,
               allowNone: Boolean = false,
               allowAbort: Boolean = true,
               abbr: Map[String, String] = Map.empty): Option[String] = {
    val choices = if (allowAbort) options ++ List(AbortCard) else options
    def testResponse(response: Option[String]): Option[String] = {
      response flatMap (s => matchOne(s.trim, choices map (_.toString), abbr)) match {
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
          askOneOf(p, choices, allowNone = true, allowAbort = allowAbort) map (_.toInt) match {
            case None    => d
            case Some(x) => x
          }
        case None =>
          val p = if (choices.size > 6)
            "%s (%d - %d): ".format(prompt, choices.head, choices.last)
          else
            "%s (%s): ".format(prompt, orList(choices))
          (askOneOf(p, choices, None, allowAbort = allowAbort) map (_.toInt)).get
      }
    }
  }

  def askCountry(prompt: String, candidates: List[String], allowAbort: Boolean = true): String = {
    assert(candidates.nonEmpty, s"askCountry(): list of candidates cannot be empty")
    // If only one candidate then don't bother to ask
    candidates match {
      case x :: Nil => println(s"$prompt $x"); x
      case xs       => askOneOf(prompt, xs, allowAbort = allowAbort, abbr = CountryAbbreviations).get
    }
  }

  // Ask the user to select multiple countries from the given candidates.
  def askCountries(num: Int, candidates: List[String], allowDuplicates: Boolean = false): List[String] = {
    def nextCountry(n: Int, targets: List[String]): List[String] = {
      if (n <= num && targets.nonEmpty) {
        val name = askCountry(s"Select ${ordinal(n)} country: ", targets)
        val newTargets = if (allowDuplicates) targets else (targets filterNot (_ == name))
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
    val entries   = plots.sorted.zipWithIndex map { case (p, i) => i.toString -> p }
    val choices   = plots.sorted.zipWithIndex map { case (p, i) => i.toString -> p.toString }
    val plotMap   = Map(entries:_*)
    askMenu(s"Choose ${amountOf(maxPlots, "plot")}:", choices, maxPlots, allowAbort = allowAbort) map plotMap
  }

  // Used when the user must select 1 or more available plots to place in a country.
  def askPlots(plots: List[Plot], num: Int, allowAbort: Boolean = true): List[Plot] = {
    val maxPlots  = num min plots.size
    val entries   = plots.sorted.zipWithIndex map { case (p, i) => i.toString -> p }
    val choices   = plots.sorted.zipWithIndex map { case (p, i) => i.toString -> p.toString }
    val plotMap   = Map(entries:_*)
    askMenu(s"Choose ${amountOf(maxPlots, "plot")}:", choices, maxPlots, allowAbort = allowAbort) map plotMap
  }

  // Ask the user to select a number of available plots
  def askAvailablePlots(num: Int, ops: Int): List[Plot] = {
    askPlots(game.availablePlots filter (p => ops >= p.opsToPlace), num)
  }

  // Return true for Awakening Marker, false for Reaction Marker
  def askPlaceAwakeningOrReactionMarker: Boolean = {
    val choices = List(true -> "Place awakening marker", false -> "Place reaction marker")
    askMenu("\nChoose one:", choices).head
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
      println(s"$countryName has $a and $s")

      if (maxCells == 1) {
        val prompt = if (sleeperFocus) "Which cell (sleeper or active)? "
        else "Which cell (active or sleeper)? "
        askOneOf(prompt, List("active", "sleeper")) match {
          case Some("active") => (1, 0)
          case _              => (0, 1)
        }
      }
      else {
        if (sleeperFocus) {
          val smax     = maxCells min sleeperCells
          val prompt   = "How many sleeper cells? "
          val sleepers = askInt(prompt, 1, smax, Some(smax))
          (maxCells - sleepers , sleepers)
        }
        else {
          val amax    = maxCells min activeCells
          val prompt  = "How many active cells? "
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
        println(s"$countryName has $a and $s and Sadr is present")
        val sadr = askYorN("Do you want to select Sadr? (y/n) ")
        if (maxCells == 1 && sadr)
          (0, 0, true)
        else if (maxCells == 1) {
          val prompt = if (sleeperFocus) "Which cell (sleeper or active)? "
                       else "Which cell (active or sleeper)? "
          askOneOf(prompt, List("active", "sleeper")) match {
            case Some("active") => (1, 0, false)
            case _              => (0, 1, false)
          }
        }
        else {
          if (sleeperFocus) {
            val smax     = maxCells min sleeperCells
            val prompt   = "How many sleeper cells? "
            val sleepers = askInt(prompt, 1, smax, Some(smax))
            (maxCells - sleepers , sleepers, sadr)
          }
          else {
            val amax    = maxCells min activeCells
            val prompt  = "How many active cells? "
            val actives = askInt(prompt, 1, amax, Some(amax))
            (actives, maxCells - actives, sadr)
          }
        }
      }
    }
  }

  // Returns "troop-cube", "militia-cube", or the name of a troop marker
  def askTroopOrMilitia(target: String): String = {
    val c = game.getCountry(target)
    val militia = if (game.isMuslim(target)) game.getMuslim(target).militia else 0

    assert(c.totalTroops + militia > 0, s"askTroopOrMilitia($target) called but no units present")

    if (c.troops == 0 && c.troopsMarkers.isEmpty)
      "militia-cube"
    else if (militia == 0 && c.troopsMarkers.isEmpty)
      "troop-cube"
    else {
      val choices = List(
        choice(c.troops > 0, "troop-cube",   s"Troop cube (${c.troops} present)"),
        choice(militia > 0,  "militia-cube", s"Militia cube (${militia} present)")
      ).flatten ++ (c.troopsMarkers.sorted map (x => x.name -> x.name))
      askMenu("\nChoose one:", choices).head
    }
  }

  def askCardNumber(prompt: String,
                    initial: Option[String] = None,
                    allowNone: Boolean = true,
                    only3Ops: Boolean = false,
                    removedLapsingOK: Boolean = false): Option[Int] = {
    def checkNumber(input: String): Boolean = input match {
      case INTEGER(num) if deck.isValidCardNumber(num.toInt) =>
        if (only3Ops && deck(num.toInt).ops != 3) {
          println("You must enter a 3 Ops card")
          false
        }
        else
          true
      case _ =>
        println(s"'$input' is not a valid card number")
        false
    }
    def testResponse(response: Option[String]): Option[Int] = {
      def outOfPlay(x: String): Boolean = {
        val num  = x.trim.toInt
        val name = cardNumAndName(num)
        removedLapsingOK match {
          case false if game.cardRemoved(num) =>
            println(s"$name has been removed from the game")
            true
          case false if game.cardLapsing(num) =>
            println(s"$name is currently lapsing")
            true
          case false if game.isFirstPlot(num) =>
            println(s"$name is currently the first plot card")
            true
          case _ => false
        }
      }
      response filter checkNumber match {
        case None =>
          readLine(prompt) match {
            case null | "" if allowNone => None
            case null | ""              => testResponse(None)
            case input                  => testResponse(Some(input))
          }
        case Some(n) if outOfPlay(n) =>
          if (askYorN("Play it anyway (y/n)? ")) Some(n.trim.toInt) else None
        case n => n map (_.trim.toInt)
      }
    }
    testResponse(initial)
  }

  // The Avenger card has a Special clause that its event
  // triggers whenever it is randomly drawn.
  def checkIfAvengerDrawn(numberDrawn: Int): Unit = {
    // No need to ask unless we are using the Forever War cards
    if (numberDrawn > 0 && GameModeOrdering.gt(game.currentMode, AwakeningMode)) {
      val card = deck(AvengerCard)
      val name = card.name
      val prompt = if (numberDrawn == 1)
        s"""Was #$AvengerCard "$name" the card drawn? (y/n) """
      else
        s"""Was #$AvengerCard "$name" one of the cards drawn? (y/n) """

      if (askYorN(prompt)) {
        log()
        log(s"""The "$name" card was randomly drawn, so the event triggers""")
        log(separator())
        card.executeEvent(US)
        log()
        log(s"""Place the "$name" card in the discard pile.""")
      }
    }
  }
  // Convenience method for creating choices for the askMenu() function.
  def choice[T](condition: Boolean, value: T, desc: String): Option[(T, String)] =
    if (condition) Some(value -> desc) else None

  def choice[T](condition: Boolean, value: T): Option[T] = if (condition) Some(value) else None

  def choice[T](condition: Boolean, value: T, desc: String, detail: Seq[String]): Option[(T, (String, Seq[String]))] =
    if (condition) Some(value -> (desc, detail)) else None

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
        println(separator())
        val indexMap = (itemsRemaining.keys.zipWithIndex map (_.swap)).toMap
        for ((key, i) <- itemsRemaining.keysIterator.zipWithIndex)
          println(s"${i+1}) ${itemsRemaining(key)}")
        val prompt = if (numChoices > 1) s"${ordinal(num)} Selection: "
        else "Selection: "
        val choice = askOneOf(prompt, 1 to itemsRemaining.size, allowAbort = allowAbort).get.toInt
        val index  = choice - 1
        val key    = indexMap(index)
        val remain = if (repeatsOK) itemsRemaining else itemsRemaining - key
        indexMap(index) :: nextChoice(num + 1, remain)
      }
    }
    if (items.size > 1 &&  prompt != "")
      println(prompt)
    nextChoice(1, ListMap(items:_*))
  }

    // Present a numbered menu of choices
  // Allow the user to choose 1 or more choices and return
  // a list of keys to the chosen items.
  // Caller should println() a brief description of what is being chosen.
  // items is a list of (key -> display) for each item in the menu.
  def askMenuWithWrap[T](
    items: List[(T, (String, Seq[String]))],
    menuPrompt: String = "",
    numChoices: Int = 1,
    repeatsOK: Boolean = false,
    allowAbort: Boolean = true): List[T] = {

    def nextChoice(num: Int, itemsRemaining: ListMap[T, (String, Seq[String])]): List[T] = {
      if (itemsRemaining.isEmpty || num > numChoices)
        Nil
      else if (itemsRemaining.size == 1)
        itemsRemaining.keys.head :: Nil
      else {
        val width = itemsRemaining.size.toString.size
        println(menuPrompt)
        println(separator(char = '='))
        val indexMap = (itemsRemaining.keys.zipWithIndex map (_.swap)).toMap
        for ((key, i) <- itemsRemaining.keysIterator.zipWithIndex) {
          val number = String.format(s"%${width}d) ", i+1)
          val (desc, detail) = itemsRemaining(key)
          val prefix = s"${number}${desc} "
          wrap(prefix, detail, showNone = false) foreach println
        }
        val prompt = if (numChoices > 1) s"${ordinal(num)} Selection: "
        else "Selection: "
        println(separator())
        val choice = askOneOf(prompt, 1 to itemsRemaining.size, allowAbort = allowAbort).get.toInt
        val index  = choice - 1
        val key    = indexMap(index)
        val remain = if (repeatsOK) itemsRemaining else itemsRemaining - key
        indexMap(index) :: nextChoice(num + 1, remain)
      }
    }
    nextChoice(1, ListMap(items:_*))
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
        val maxLen = (targets map (_.country.length)).max
        val fmt = "%%-%ds  (%%s)".format(maxLen)
        def disp(t: MapItem) = {fmt.format(t.country, amountOf(t.num, name))}
        val choices = targets.map(t => t.country -> disp(t))
        println()
        val country = askMenu(s"${amountOf(numItems, name)} remaining, choose country", choices).head
        val limit   = (targets find (_.country == country)).get.num min numItems
        val num     = askInt("How many", 1, limit)
        val newTargets = targets map { t =>
          if (t.country == country) t.copy(num = t.num - num) else t
        } filterNot (_.num == 0)
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
          val len = (sources map (_.name.length)).max
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
                val choices = sources map { i => i.name -> disp(i) }
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
            sources filterNot (_.name == src.name)
          else
            sources map { x =>
              if (x.name == src.name) x.copy(actives = x.actives - a, sleepers = x.sleepers - s)
              else x
            }
          CellsItem(src.name, a, s) :: nextChoice(numRemaining - a - s, newSources)
        }
      }

      // First create a CellsItem for all potential sources that have cells.
      val trackSrc = if (trackOK && game.cellsAvailable > 0)
        List(CellsItem("track", 0, game.cellsAvailable))
      else
        Nil
      val countrySrcs = (game getCountries names filter (_.totalCells > 0)
                           map (c => CellsItem(c.name, c.activeCells, c.sleeperCells)))
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

  def askToRemoveCells(maxCells: Int, countryNames: List[String], sleeperFocus: Boolean): List[CellsToRemove] = {

    @tailrec def askNext(numLeft: Int, countries: List[String], removed: List[CellsToRemove]): List[CellsToRemove] = {
      if (numLeft == 0 || countries.isEmpty)
        removed.reverse
      else {
        val name = askCountry("Remove cells from which country: ", countries)
        val maxNum = game.getMuslim(name).totalCells min numLeft
        val num  = if (countries.size == 1)
          maxNum
        else
          askInt(s"Remove how many cells from $name", 1, maxNum)
        val (actives, sleepers, sadr) = askCells(name, num, sleeperFocus)
        askNext(numLeft - num, countries filterNot (_ == name), CellsToRemove(name, (actives, sleepers, sadr)) :: removed)
      }
    }

    if (countryNames.isEmpty)
      Nil
    else {
      println("Cells in target countries")
      println(separator())
      for (name <- countryNames) {
        val b = new ListBuffer[String]
        val m = game.getMuslim(name)
        if (m.sleeperCells > 0)
          b += amountOf(m.sleeperCells, "sleeper")
        if (m.activeCells > 0)
          b += amountOf(m.activeCells, "active")
        if (m.hasSadr)
          b += "Sadr"

        println(s"  $name ${b.toList.mkString("(", ", ", ")")}")
      }
      askNext(maxCells, countryNames, List.empty)
    }
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
    val row = dieRoll - 1        // tan die
    val col = (dieRoll - 1) / 2  // black die
    if (row == 0 && col == 2)
      secondaryRandomMuslimCountry
    else
      game.getMuslim(randomMuslimTable(row)(col))
  }

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

  def randomShiaMixList: List[MuslimCountry] = {
    val xs = List(Syria, SaudiArabia, Turkey, Iraq, GulfStates, Yemen, Pakistan, Lebanon, Afghanistan)
    (if (game.isMuslim(Iran)) Iran :: xs else xs) map game.getMuslim
  }

  def randomShiaMixCountry: MuslimCountry = {
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
          val newGov = if (dieRoll < 5) Poor else Fair
          if (newGov == Fair)
            addTestedOrImprovedToFairOrGood(name)
          game = game.updateCountry(m.copy(governance = newGov, alignment = Neutral))
          log(s"${m.name} tested: Set to ${govToString(newGov)} Neutral", Color.MapPieces)

        case n: NonMuslimCountry =>
          val newPosture = if (dieRoll < 5) Soft else Hard
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
    val die = dieRoll
    log(s"Governance die roll: $die")
    val newGov = if (die < 5) Poor else Fair
    game = game.updateCountry(m.copy(governance = newGov))
    log(s"Set the governance of $name to ${govToString(newGov)}", Color.MapPieces)
  }

  // The default drm when rolling US posture is +1
  def rollUSPosture(drms: List[(Int, String)] = Nil): Unit = {
    val die = dieRoll
    val allDrms = (1, "Rolling US Posture") :: drms
    val modifiedDie = die + (allDrms map (_._1)).sum
    val newPosture = if (modifiedDie < 5) Soft else Hard
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
    val die = dieRoll
    val modifiedDie = die + drm
    log(s"\nRoll posture of $name")
    log(s"Die roll: $die")
    if (drm != 0) {
      log(f"$drm%+d")
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
    game.useExpansionRules       &&
    !game.caliphateDeclared      &&
    (game isMuslim capital)      &&
    (game getMuslim capital).caliphateCandidate

  def askDeclareCaliphate(capital: String): Boolean =
    askYorN(s"Do you wish to declare a Caliphate with $capital as the Capital (y/n)? ")


  // Throws an exception if a Caliphate already exists
  def declareCaliphate(capital: String): Unit = {
    assert(!game.caliphateDeclared, "declareCaliphate() called and a Caliphate Capital already on the map")
    log(s"A Caliphate is declared with $capital as the Capital", Color.Event)
    setCaliphateCapital(capital)
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
    // Caliphate capital displaced.  Attempt to move it to adjacent caliphate country.
    val adjacents = game.adjacentMuslims(previousCapital) filter (_.caliphateCandidate)
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
        val choices = adjacents map (_.name)
        askCountry(s"Choose new Caliphate capital (${orList(choices)}): ", choices, allowAbort = false)
      }
      else {
        // The Bot pick the best candidate for the new capital based on
        // the set of conditions outlined by compareCapitalCandidates().
        // We sort the best to worst.  If more than one has the best score, then
        // we choose randomly among them.
        val sorted = adjacents.map(CaliphateCapitalCandidate).sorted
        val best   = sorted.takeWhile(compareCapitalCandidates(_, sorted.head) == 0) map (_.m.name)
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

  // Convergence
  def performConvergence(forCountry: String, awakening: Boolean): Unit = {

    if (game.useExpansionRules) {
      if (lapsingEventInPlay(ArabWinter))
        log("No convergence performed because \"Arab Winter\" is in effect", Color.Info)
      else {
        val target = randomConvergenceTarget.name
        testCountry(target)
        val m = game getMuslim target
        if (awakening) {
          game = game.updateCountry(m.copy(awakening = m.awakening + 1))
          log(s"Convergence for ${forCountry}: Add 1 awakening marker to ${target}", Color.MapPieces)
        }
        else {
          game = game.updateCountry(m.copy(reaction = m.reaction + 1))
          log(s"Convergence for ${forCountry}: Add 1 reaction marker to ${target}", Color.MapPieces)
        }
      }
    }
  }

  def inspect[T](name: String, value: T): T = {
    println(s"DEBUG: $name == ${value.toString}")
    value
  }

  // Format the given sequence of strings in a comma separated list
  // such that we do not exceed the given number of columns.
  def wrap[T](prefix: String, values: Seq[T], columns: Int = 100, showNone: Boolean = true): Seq[String] = {
    val stringValues = values map (_.toString)
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
        s.append(", ")
        if (s.length + v.length < columns)
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


  //  Return true if the user enters skip.
  //  This is a hidden feature to skip future pauses when showing
  //  game state differences.
  def pause(): Boolean = {
    val (color, reset) = if (game.showColor)
      (Console.GREEN, Console.RESET)
    else
      ("", "")
    readLine(s"\n${color}>>>>> [ Press Enter to continue... ] <<<<<${reset}") == "skip"
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


  def logAdjustment(name: String, oldValue: Any, newValue: Any): Unit = {
    def normalize(value: Any) = value match {
      case None                       => "none"
      case Some(x)                    => x.toString.trim
      case true                       => "yes"
      case false                      => "no"
      case s: Seq[_] if s.isEmpty     => "none"
      case s: Seq[_]                  => s map (_.toString) mkString ", "
      case x if x.toString.trim == "" => "none"
      case x                          => x.toString.trim
    }
    log(s"$name adjusted from [${normalize(oldValue)}] to [${normalize(newValue)}]")
  }

  def logAdjustment(countryName: String, attributeName: String, oldValue: Any, newValue: Any): Unit =
    logAdjustment(s"$countryName: $attributeName", oldValue, newValue)

  def logCardPlay(player: Role, card: Card, playable: Boolean): Unit = {
    val fakeNews = if (lapsingEventInPlay(FakeNews))
     """ but will be cancelled by "Fake News""""
    else
      ""
    log()
    log(separator(char = '='), echo = false)
    log(s"$player plays $card")
    if (card.autoTrigger)
      log(s"  (The ${card.association} event will automatically trigger)")
    else if ((card.association == player || card.association == Unassociated) && playable)
      log(s"  (The ${card.association} event is playable)$fakeNews")
    else if ((card.association == player || card.association == Unassociated) && !playable)
      log(s"  (The ${card.association} event is not playable)")
  }

  def separator(length: Int = 52, char: Char = '-'): String = char.toString * length

  def markersString(markers: List[String]): String = if (markers.isEmpty)
    "none"
  else
    markers mkString ", "

  // Return a sorted list of the names of the given countries
  def countryNames(candidates: List[Country]) = (candidates map (_.name)).sorted.distinct

  // Get ordinal number.
  def ordinal(i: Int): String = i match {
    case x if x != 11 && x % 10 == 1 => s"${x}st"
    case x if x != 12 && x % 10 == 2 => s"${x}nd"
    case x if x != 13 && x % 10 == 3 => s"${x}rd"
    case x => s"${x}th"
  }

  // Sorts a list column wise.  Returns a list of rows where
  // each row is a string with the items of that row lined up
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

    rows.toIndexedSeq map { entries =>
      (entries.toList.zipWithIndex map { case (entry, col) =>
        padLeft(entry, colWidths(col))
      }).mkString("  ")
    }
  }


  def logWorldPosture(): Unit = {
    log(s"World Posture is ${game.worldPostureDisplay}  (GWOT penalty ${game.gwotPenalty})", Color.Info)
  }


  def printSummary(summary: Seq[String]): Unit = {
    println()
    summary foreach println
  }

  def logSummary(summary: Seq[String]): Unit = {
    log()
    summary foreach (log(_))
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
  def showHistory(input: Option[String]): Unit = {
    case class Error(msg: String) extends Exception
    try {
      def redirect(tokens: List[String]): Option[Pathname] = {
        tokens match {
          case Nil => None
          case x::_  if !(x startsWith ">") => None
          case ">":: Nil => throw Error("No filename specified after '>'")
          case ">"::file::_ => Some(Pathname(file))
          case file::_ => Some(Pathname(file drop 1))
        }
      }

      def printSegment(save_number: Int, last_save_number: Int, path: Option[Pathname]): Unit = {
        if (save_number <= last_save_number) {
          val header   = s"\n>>> History of save point $save_number <<<"
          val log_path = gamesDir/gameName.get/getLogName(save_number)

          val entries = if (log_path.exists)
            SavedGame.loadLog(log_path)
          else
            Vector.empty

          path match {
            case None =>
              displayLine(header, Some(Color.Green))
              displayLine(separator(char = '='), Some(Color.Green))
              for (LogEntry(line, color) <- entries)
                displayLine(line, color)

            case Some(path) =>
              path.appender { stream =>
                stream.write(header + lineSeparator)
                stream.write(separator() + lineSeparator)
                for (LogEntry(line, _) <- entries)
                  stream.write(line + lineSeparator)
              }
          }
          printSegment(save_number + 1, last_save_number, path)
        }
      }

      val maxIndex = game.history.size - 1
      val NUM      = """(-?\d+)""".r
      val ALL      = """al{0,2}""".r
      val REDIR    = """>.*""".r
      val tokens   = (input getOrElse "" split "\\s+").toList map (_.toLowerCase) dropWhile (_ == "")

      def indexVal(str: String): Int = str.toInt match {
        case x if x < 0 => (game.history.size + x) max 0
        case x          => x min maxIndex
      }

      val (startIndex, count, redirect_path) = tokens match {
        case Nil                => (maxIndex, 0, None)
        case NUM(x)::NUM(y)::xs => (indexVal(x), y.toInt, redirect(xs))
        case NUM(x)::xs         => (indexVal(x), 0, redirect(xs))
        case ALL()::xs          => (0, 0, redirect(xs))
        case REDIR()::_         => (maxIndex, 0, redirect(tokens))
        case p::_               => throw Error(s"Invalid parameter: $p")
      }
      val endIndex = count match {
        case c if c < 1 => maxIndex
        case c          => (startIndex + (c - 1)) min maxIndex
      }

      // Delete any previous file before we start appending to it.
      redirect_path foreach { p =>
        if (p.isDirectory)
          throw new IllegalArgumentException(s"Cannot redirect to a directory ($p)!")
        p.delete()
      }

      printSegment(startIndex, endIndex, redirect_path)

      redirect_path foreach { p =>
        println(s"\nHistory was written to file: $p")
      }
    }
    catch {
      case e: IOException => println(s"IOException: ${e.getMessage}")
      case Error(msg) => println(msg)
    }
  }


  // We assume that the current working directory
  // set as the installed directory and thus the game directory
  // is in ./games.  The script used to invoke the program will
  // ensure that is the case.
  val gamesDir = Pathname("./games")


  // Save a brief description of the game.
  // The descriptions are used by the askWhichGame() function.
  def saveGameDescription(desc: String): Unit = {
    assert(gameName.nonEmpty, "saveGameDescription(): called with gameName not set!")
    val path = gamesDir/gameName.get/"description"
    path.writeFile(desc)
  }

  def loadGameDescription(name: String): String = {
    val path = gamesDir/name/"description"
    if (path.exists)
      path.readFile().trim
    else
      ""
  }

  def getSaveName(save_number: Int) = f"save-$save_number%03d"

  def getLogName(save_number: Int)  = f"log-$save_number%03d"

  def getSaveFileNumber(filename: Pathname): Option[Int] = {
    val SAVE_FILE = """save-(\d+)""".r
    filename.basename.toString match {
      case SAVE_FILE(n) => Some(n.toInt)
      case _            => None
    }
  }

  def saveGameState(desc: Option[String] = None): Unit = {
    assert(gameName.nonEmpty, "saveGameState(): called with gameName not set!")

    val save_number = game.history.size
    val save_path   = gamesDir/gameName.get/getSaveName(save_number)
    val log_path    = gamesDir/gameName.get/getLogName(save_number)
    val segmentDesc = desc orElse game.plays.headOption.map(_.toString) getOrElse ""
    val cardsPlayed = (game.plays map (_.numCards)).sum
    val turnInfo = game.plays.headOption match {
      case Some(_: PlayedReassement) =>
        s"(turn ${game.turn} - ${ordinal(cardsPlayed - 1)} & ${ordinal(cardsPlayed)} card play)"
      case Some(_: PlayedCard) =>
          s"(turn ${game.turn} - ${ordinal(cardsPlayed)} card play)"
      case _ =>
        s"(turn ${game.turn} - ${amountOf(cardsPlayed, "card")} played)"
    }
    val gameDesc    = Seq(
      s"scenario: ${game.scenarioName}",
      s"playing: ${game.humanRole}",
      s"last action: $segmentDesc",
      turnInfo
    ).mkString(", ")
    val segment = GameSegment(save_number, Seq(segmentDesc, turnInfo))

    // Make sure that the game directory exists
    save_path.dirname.mkpath()
    SavedGame.saveLog(log_path, game.log)
    game = game.copy(
      description = gameDesc,
      history = game.history :+ segment,
      log = Vector.empty,
    )
    SavedGame.save(save_path, game)
    saveGameDescription(gameDesc)
  }

  def loadGameState(name: String, save_number: Int): Unit = {
    val save_path = gamesDir/name/getSaveName(save_number)
    gameName = Some(name)
    game = SavedGame.load(save_path).copy(log = Vector.empty)
  }

  // Given a directory for a saved game finds the most recent save file.
  def mostRecentSaveNumber(name: String): Option[Int] = {
    val dir = gamesDir/name
    if (dir.isDirectory) {
      val entries = dir.children(withDirectory = false) flatMap { child =>
        getSaveFileNumber(child)
      }
      entries.sortBy(num => -num).headOption
    }
    else
      None
  }

  def loadMostRecent(name: String): Unit = {
    val save_number = mostRecentSaveNumber(name) getOrElse {
      throw new IllegalStateException(s"No saved file found for game '$name'")
    }
    loadGameState(name, save_number)
  }
    // Return the list of saved games
  def savedGames: List[String] = {
    gamesDir.children(withDirectory = false).toList map (_.toString) filter { name =>
      mostRecentSaveNumber(name).nonEmpty
    }
  }

  sealed trait GameFile
  case class TurnFile(num: Int) extends GameFile
  case class PlayFile(num: Int) extends GameFile



  val VALID_NAME = """([-A-Za-z0-9_ ]+)""".r
  var gameName: Option[String] = None // The name of sub-directory containing the game files

  // Ask the user for a name for their new game.
  def askGameName(prompt: String): String = {
    def getName: String = {
      readLine(prompt) match {
        case null => getName
        case VALID_NAME(name) =>
          if ((gamesDir/name).exists) {
            println(s"A game called '$name' already exists.")
            if (askYorN(s"Do you want to overwrite the existing game (y/n)? ")) {
              (gamesDir/name).rmtree()
              name
            }
            else
              getName
          }
          else
            name
        case name =>
          println("Game names must consist of one or more letters, numbers, spaces, dashes or underscores")
          getName
      }
    }
    getName
  }

  // Allows the user to roll back to the beginning of any turn.
  def rollback(input: Option[String]): Unit = {
    try {
      val pages = game.history.drop(1).reverse.sliding(25, 25).toList
      val firstPage = 0
      val lastPage  = pages.size -1
      val PAGE_UP   = -1
      val PAGE_DOWN = -2
      val CANCEL    = -3

      def showPage(pageNum: Int): Unit = {
        val saveChoices: List[(Int, (String, Seq[String]))] = pages(pageNum).toList map {
          case GameSegment(save_number, summary) => save_number -> (s"Save point ${save_number}:", summary)
        }
        val otherChoices: List[(Int, (String, Seq[String]))] = List(
          choice(pageNum > firstPage, PAGE_UP,   "Page up, show newer save points ", Seq.empty),
          choice(pageNum < lastPage,  PAGE_DOWN, "Page down, show older save points ", Seq.empty),
          choice(true,                CANCEL,    "Cancel, do not roll back ", Seq.empty)
        ).flatten

        val current = game.history.last
        println()
        wrap(s"Current save point: ${current.save_number} ", current.summary) foreach (l => println(l))
        println("\nRollback to the beginning of a previous save point.")
        println("The save points are displayed with the most recent first.")

        askMenuWithWrap(saveChoices:::otherChoices, "\nChoose a save point:").head match {
          case CANCEL      =>
          case PAGE_UP     => showPage(pageNum - 1)
          case PAGE_DOWN   => showPage(pageNum + 1)
          case save_number =>
            if (askYorN(s"Are you sure you want to rollback to this save point? (y/n) ")) {
              // Games are saved at the end of the turn, so we actually want
              // to load the file with turnNumber -1.
              val target       = save_number - 1
              val name         = gameName.get
              val oldGameState = game
              loadGameState(name, target)
              saveGameDescription(game.description)  // Update the description file

              // Remove all safe files that succeed this one.
              // We are exploring anew
              removeSaveFiles(name, target + 1)
              displayGameStateDifferences(oldGameState, game)
            }
            else
              showPage(pageNum)
        }
      }

      if (game.history.size > 1)
        showPage(0)
      else
        println("\nThere are no previous save points")
    }
    catch {
      case AbortAction =>
    }
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
        "Set the bot difficulties to %s".format(to.botDifficulties map (_.name) mkString ", "))
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
    show(from.extraCellsAvailable, to.extraCellsAvailable, s"Set cells in training camp to ${to.extraCellsAvailable}")
    show(from.resolvedPlots.sorted, to.resolvedPlots.sorted,
          s"Set resolvedPlots plots to ${plotsDisplay(to.resolvedPlots)}")
    show(from.availablePlots.sorted, to.availablePlots.sorted,
          s"Set available plots to ${plotsDisplay(to.availablePlots, to.humanRole == Jihadist)}")
    show(from.markers.sorted,  to.markers.sorted,
            s"Set global event markers to: ${markersString(to.markers)}" )
    (from.firstPlotCard, to.firstPlotCard) match {
      case (x, y) if x == y =>  // No change
      case (_, Some(c))     => b += s"Set ${cardNumAndName(c)} as the first plot card"
      case (_, None)        => b += "There should be no first plot card"
    }
    if (from.cardsLapsing.sorted != to.cardsLapsing.sorted) {
      b += "The following cards are lapsing:"
        to.cardsLapsing.sorted foreach (c => b += s"  ${cardNumAndName(c)}")
    }
    if (from.cardsRemoved.sorted != to.cardsRemoved.sorted) {
      b += "The following cards have been removed from play:"
      to.cardsRemoved.sorted foreach (c => b += s"  ${cardNumAndName(c)}")
    }

    if (b.nonEmpty) {
      showHeader()
      b foreach println
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
      showC(fromM, toM orElse toN, _.governance, "governance")
      showM(fromM, toM, _.alignment, "alignment")
      showC(fromM, toM orElse toN, _.sleeperCells, "sleeper cells")
      showC(fromM, toM orElse toN, _.activeCells, "active cells")
      showC(fromM, toM orElse toN, _.hasCadre, "cadre marker")
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
          b += s"Set plots to ${mapPlotsDisplay(newVal, to.humanRole == Jihadist)}"
      }
      (toM orElse toN) foreach { t =>
        val newVal = t.markers.sorted
        if (fromM.markers.sorted != newVal)
          b += s"Set markers to ${markersString(newVal)}"
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
      showC(fromN, toN orElse toM, _.hasCadre, "cadre marker")
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
          log(s"$US adds ${opsString(opsAdded)} to reserves.  Reserves now ${opsString(game.reserves.us)}", Color.MapMarker)
        }
      case Jihadist =>
        val opsAdded = ops min (2 - game.reserves.jihadist)
        if (opsAdded > 0) {
          game = game.copy(reserves = game.reserves.copy(jihadist = game.reserves.jihadist + opsAdded))
          log(s"$Jihadist adds ${opsString(opsAdded)} to reserves.  Reserves now ${opsString(game.reserves.jihadist)}", Color.MapMarker)
        }
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

  def polarization(): Unit = {
    val candidates = game.muslims filter (m => (m.awakening - m.reaction).abs > 1)
    log()
    log("Polarization")
    log(separator())
    if (candidates.isEmpty)
      log("No countries affected", Color.Info)
    else {
      // Remember any Caliphate capital in case it is displaced.
      val caliphateCapital = candidates find (_.caliphateCapital) map (_.name)
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
        (m.awakening - m.reaction) match {
          case 0 =>  // Nothing happens
          case 2 =>
            game = game.updateCountry(m.copy(awakening = m.awakening + 1))
            log()
            log(s"Add an awakening marker to ${name}", Color.MapPieces)
          case -2 =>
            game = game.updateCountry(m.copy(reaction = m.reaction + 1))
            log()
            log(s"Add a reaction marker to ${name}", Color.MapPieces)
          case x if x > 2 =>
            log()
            if (m.isAlly) {
              improveGovernance(name, 1, canShiftToGood = true, endOfTurn = true, convergenceOK = false)
              if (game.getMuslim(name).isGood)
                convergers = Converger(name, awakening = true) :: convergers
            }
            else
              shiftAlignmentLeft(m.name)


          case _ => // x < -2
            log()
            if (m.isAdversary) {
              worsenGovernance(name, levels = 1, canShiftToIR = true, endOfTurn = true, convergenceOK = false)
              if (game.getMuslim(name).isIslamistRule)
                convergers = Converger(name, awakening = false) :: convergers
            }
            else
              shiftAlignmentRight(m.name)
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
      val maxAbsorb     = m.troops + m.militia + (m.troopsMarkers map (_.num)).sum
      val multiplier    = if (hamaOffensive) 2 else 1
      val losses        = hits * multiplier
      val unfulfilled   = (losses - maxAbsorb) max 0
      val hitsRemaining =  (unfulfilled + multiplier - 1) / multiplier

      log()
      log(s"$US must absorb ${amountOf(losses, "loss", Some("losses"))}")

      val (markersLost, troopsLost, militiaLost) = if (game.humanRole == US) {
        // If there are any markers representing troops or
        // if there is a mix of multiple type of cubes that can take losses (troops and militia),
        // and the hits are not sufficient to eliminate all forces present,
        // then allow the user to choose which units absorb the losses.
        val mixedCubes = m.troops > 0 && m.militia > 0
        if (losses >= m.totalTroopsAndMilitia)
           (m.troopsMarkers map (_.name), m.troops, m.militia)
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
                choice(troops > 0,  "troop",  "Troop cube"),
                choice(militia > 0, "militia","Militia cube")
              ).flatten ++ (markers.sorted map (m => m.name -> s"${m.name}  (absorbs ${amountOf(m.num, "loss", Some("losses"))})"))
              println(s"$US must take ${amountOf(lossesRemaining, "more loss", Some("more losses"))}")
              askMenu("Which unit will take the next loss:", choices).head match {
                case "troop"   => troopsLost  += 1; nextHit(lossesRemaining - 1, markers, troops - 1, militia)
                case "militia" => militiaLost += 1; nextHit(lossesRemaining - 1, markers, troops, militia - 1)
                case name      =>
                  val marker = (markers find (_.name == name)).get
                  val lossesAbsorbed = marker.num
                  markersLost = name :: markersLost
                  nextHit(lossesRemaining - lossesAbsorbed, markers filterNot (_.name == name), troops, militia)
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
      val multiplier    = (if (m.totalTroops > 0 || m.hasMarker(Advisors)) 2 else 1) +
                          (if (lapsingEventInPlay(ExpandedROE)) 1 else 0)
      val losses        = hits * multiplier
      val unfulfilled   = (losses - m.totalCells) max 0
      val hitsRemaining =  (unfulfilled + multiplier - 1) / multiplier

      log()
      log(s"$Jihadist must absorb ${amountOf(losses, "loss", Some("losses"))}")

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
    val effectsList = List(
      if (hamaOffensive) Some("Hama Offensive") else None,
      if (siegeOfMosul)  Some("Siege of Mosul") else None
    ).flatten
    val effects = if (effectsList.nonEmpty) effectsList.mkString(" (", ", ", ")") else ""

    log()
    log(s"Attrition: $name$effects")
    log(separator())

    // Get the fresh instance of the Muslim country
    val m = game.getMuslim(name)
    // If Siege of Mosul in play then cells are halved and (troops + militia)
    // are doubled
    val jihadDie = dieRoll
    val usDie    = dieRoll
    val totalCells = if (siegeOfMosul) m.totalCells / 2 else m.totalCells
    val totalTroopsAndMilitia = if (siegeOfMosul) m.totalTroopsAndMilitia * 2 else m.totalTroopsAndMilitia
    val jihadHits = totalCells            / 6 + (if (jihadDie <= totalCells % 6) 1 else 0)
    val usHits    = totalTroopsAndMilitia / 6 + (if (usDie <= totalTroopsAndMilitia % 6) 1 else 0)

    if (totalCells == 0 && totalTroopsAndMilitia == 0)
      log("No cells, troops or militia present")
    else {
      if (totalCells == 0)
        log("No Jihadist cells to inflict hits")
      else {
        if (totalCells > 0 && totalCells % 6 != 0)
          log(s"Jihadist die roll: $jihadDie")
        log(s"The Jihadist inflicts ${amountOf(jihadHits, "hit")} on the US")
      }

      if (totalTroopsAndMilitia == 0)
        log("No US troops or militia present to inflict hits")
      else {
        if (totalTroopsAndMilitia > 0 && totalTroopsAndMilitia % 6 != 0)
          log(s"US die roll : $usDie")
        log(s"The US inflicts ${amountOf(usHits, "hit")} on the Jihadist")
      }

      if (jihadHits + usHits > 0) {
        val unfulfilledJihadHits = usCivilWarLosses(m, jihadHits, hamaOffensive)
        val unfulfilledUSHits    = jihadistCivilWarLosses(m, usHits)

        //  Make sure the country (ie. Nigeria) is still a Non Muslim country
        if (game.isMuslim(name)) {
          val delta = unfulfilledJihadHits - unfulfilledUSHits
          if (delta == 0) {
            if (unfulfilledJihadHits != 0)
              log(s"Both sides have ${amountOf(unfulfilledJihadHits, "unfulfilled hit")}.  No further effects.")
          }
          else {
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
    def civilWars = game.muslims filter (m => (m.civilWar))
    log()
    log("Civil War Attrition")
    log(separator())
    if (civilWars.isEmpty)
      log("No countries in civil war")
    else {
      val caliphateCapital = civilWars find (_.caliphateCapital) map (_.name)
      val priorGameState = game
      val totalAdvisors = (civilWars map (_.numAdvisors)).sum
      // Add militia for any Advisors present
      if (civilWars exists (_.numAdvisors > 0)) {
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
            var advisorMap: Map[String, Int] = (civilWars filter (_.numAdvisors > 0) map (m => (m.name, m.numAdvisors))).toMap
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
              nextMilita(numLeft - num, candidates filterNot (_ == target))
            }
            nextMilita(game.militiaAvailable, countryNames(civilWars filter (_.numAdvisors > 0)))
          }

        }

      }

      for (name <- civilWars map (_.name))
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
        if (ops < 3 && tested.isPoor)
          log(s"Not enough Ops to complete War of Ideas in $name")
        else {
          log(s"$US performs War of Ideas in $name")
          log(separator())
          val die = getDieRoll(US)
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
        val die = getDieRoll(US)
        val newPosture = if (die > 4) Hard else Soft
        game = game.updateCountry(n.copy(postureValue = newPosture))
        log(s"Die roll: $die")
        if (newPosture == n.posture)
          log(s"Posture of $name remains $newPosture", Color.MapPieces)
        else
          log(s"Change posture of $name from ${n.posture} to $newPosture", Color.MapPieces)
        if (newPosture == game.usPosture && game.prestige < 12) {
          game = game.adjustPrestige(1)
          log(s"New posture matches US posture, increase US prestige by +1 to ${game.prestige}", Color.MapMarker)
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
    val die = getDieRoll(US, "Enter governance die roll: ")
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
    val bumpPrestige = game.getCountry(target).disruptAffectsPrestige
    addOpsTarget(target)
    addDisruptedTarget(target)
    game.disruptLosses(target) match {
      case Some(Left(numCells)) =>
        val (actives, sleepers) = if (game.humanRole == US)
          askCellsNotSadr(target, numCells, sleeperFocus = false)
        else
          USBot.chooseCellsToDisrupt(target, numCells)
        flipSleeperCells(target, sleepers)
        removeCellsFromCountry(target, actives, 0, false, addCadre = true)
      case Some(Right(_)) =>
        removeCadreFromCountry(target)
      case None =>
        throw new IllegalStateException(s"performDisrupt(): $target has no cells or cadre")
    }
    if (bumpPrestige && game.usResolve(Adept)) {
      log(s"$US Bot with Adept resolve alerts increases prestige by +2", Color.MapMarker)
      increasePrestige(2)
    }
    else if (bumpPrestige)
      increasePrestige(1)
  }

  def performAlert(countryName: String, plotOnMap: PlotOnMap): Unit = {
    val c = game.getCountry(countryName)
    assert(c.plots contains plotOnMap, s"performAlert(): $countryName does not contain $plotOnMap")
    addOpsTarget(countryName)

    val plot = plotOnMap.plot
    val prestigeDelta = if (plot == PlotWMD) 1 else 0
    val i = c.plots.indexOf(plotOnMap)
    val remaining = (c.plots take i) ::: (c.plots drop (i + 1))
    val updated = c match {
      case m: MuslimCountry    => game = game.updateCountry(m.copy(plots = remaining))
      case n: NonMuslimCountry => game = game.updateCountry(n.copy(plots = remaining))
    }
    if (plot == PlotWMD) {
      val updatedPlots = game.plotData.copy(removedPlots = plot :: game.removedPlots)
      game = game.copy(plotData = updatedPlots)
      log(s"$plot alerted in $countryName, remove it from the game.", Color.MapPieces)
      if (game.useExpansionRules) {
        game = game.adjustPrestige(1)
        log(s"Increase prestige by +1 to ${game.prestige} for alerting a WMD plot", Color.MapMarker)
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

  def performReassessment(): Unit = {
    val newPosture = oppositePosture(game.usPosture)
    setUSPosture(newPosture)
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
          log(s"Travel from $from to $to succeeds automatically")
          handleResult(true, from, to, active)
        }
        else {
          val die = getDieRoll(Jihadist, prompt = s"Enter die roll: ")
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
        if (active)
          log("Using an already active cell")
        else
          flipSleeperCells(name, 1)
        val die = getDieRoll(Jihadist, prompt = s"Enter die roll: ")
        log(s"Die roll: $die")
        val success = die <= game.getCountry(name).governance
        log(if (success) "Success" else "Failure")

        if (success) {
          // Ask the user which plot to place.  The Bot takes a random plot regardless of ops spent.
          val plots = if (game.humanRole == Jihadist)
            askAvailablePlots(1, ops)
          else if (game.jihadistIdeology(Attractive)) {
            log(s"$Jihadist Bot with Attractive Ideology places two plots")
            shuffle(game.availablePlots) take 2
          }
          else
            shuffle(game.availablePlots) take 1

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
        log(s"Conduct $jihad in $name, rolling ${diceString(numAttempts)}")
        log(separator())
        if (major && m.sleeperCells > 0)
          flipAllSleepersCells(name)
        else if (!major && sleepers > 0)
          flipSleeperCells(name, sleepers)
        def nextAttempt(num: Int): Int = num match {
          case n if n <= numAttempts =>
            val ord = if (numAttempts == 1) "" else s"${ordinal(num)} "
            val die = getDieRoll(Jihadist, prompt = s"Enter ${ord}die roll: ")
            log(s"${ord}Die roll: $die")
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


  def performCardEvent(card: Card, role: Role, triggered: Boolean = false): Unit = {
    if (!card.autoTrigger && lapsingEventInPlay(FakeNews)) {
      log("\n%s event \"%s\" is cancelled by \"Fake News\"".format(card.association, card.name), Color.Event)
      removeLapsingCards(FakeNews::Nil)
    }
    else {
      if (card.autoTrigger)
        log("\n%s automatic event \"%s\" triggers".format(card.association, card.name))
      else if (triggered)
        log("\n%s event \"%s\" triggers".format(card.association, card.name))
      else
        log("\n%s executes the \"%s\" event".format(role, card.name))
      log(separator())
      card.executeEvent(role)
  
      if (card.markLapsingAfterExecutingEvent(role))
        markCardAsLapsing(card.number)
      else if (card.removeAfterExecutingEvent(role))
        removeCardFromGame(card.number)
    }
  }

  def removeCardFromGame(cardNumber: Int): Unit = {
    log("Remove the \"%s\" card from the game".format(deck(cardNumber).name), Color.Event)
    game = game.copy(cardsRemoved = cardNumber :: game.cardsRemoved)
  }

  def markCardAsLapsing(cardNumber: Int): Unit = {
    log("Mark the \"%s\" card as lapsing".format(deck(cardNumber).name), Color.Event)
    game = game.copy(cardsLapsing = cardNumber :: game.cardsLapsing)
  }

  def removeCardFromLapsing(cardNumber: Int): Unit = {
    if (game.cardLapsing(cardNumber)) {
      log("The \"%s\" card is no longer lapsing".format(deck(cardNumber).name), Color.Event)
      game = game.copy(cardsLapsing = game.cardsLapsing filterNot (_ == cardNumber))
    }
  }

  // Prestige roll used
  //   After regime change, withdraw, unblocked plot in the US, or by event.
  def rollPrestige(): Unit = {
    log("Roll Prestige...")
    val dirDie      = dieRoll
    val shiftDice   = List(dieRoll, dieRoll)
    val shiftAmount = if (dirDie + (if (game.gwotPenalty > 0) -1 else 0) < 5)
       -shiftDice.min
    else
      shiftDice.min

    log(s"Direction roll: $dirDie")
    if (game.gwotPenalty > 0) {
      log(s"-1: GWOT penalty is not zero")
      log(s"Modified roll: ${dirDie - 1}")
    }
    log(s"Rolls for shift amount: ${shiftDice.head} and ${shiftDice.last} (lowest value is used)")
    game = game.adjustPrestige(shiftAmount)
    val desc = if (shiftAmount < 0) "drops" else "rises"
    log(s"$US prestige $desc by $shiftAmount to ${game.prestige}", Color.MapMarker)
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
          if (m.besiegedRegime  ) log(s"Remove besieged regime marker from $name", Color.MapPieces)
          if (m.aidMarkers > 0  ) log(s"Remove ${amountOf(m.aidMarkers, "aid marker")} from $name", Color.MapPieces)
          if (m.awakening > 0   ) log(s"Remove ${amountOf(m.awakening, "awakening marker")} from $name", Color.MapPieces)
          if (m.reaction > 0    ) log(s"Remove ${amountOf(m.reaction, "reaction marker")} from $name", Color.MapPieces)
          if (m.militia > 0     ) log(s"Remove ${m.militia} militia from $name", Color.MapPieces)

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
                 awakening  = (m.awakening - delta) max 0) // One awakening for each level actually improved
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
      case _         =>
    }
  }

  def shiftAlignmentRight(name: String): Unit = {
    var m = game.getMuslim(name)
    m.alignment match {
      case Ally    => setAlignment(name, Neutral)
      case Neutral => setAlignment(name, Adversary)
      case _       =>
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
      removeCadreFromCountry(name)
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
      removeCadreFromCountry(name)
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
      hasCadre     = iran.hasCadre,
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

    (List(Iran, Syria, Lebanon) forall tehranBeirutCandidate) &&
    (List(Iraq, Turkey)         exists tehranBeirutCandidate)
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
    game.countries exists (c => countryEventInPlay(c.name, markerName))

  def globalEventInPlay(name: String)     = game.markers contains name
  def globalEventNotInPlay(name: String)  = !globalEventInPlay(name)
  def lapsingEventInPlay(cardNum: Int)    = game.cardsLapsing contains cardNum
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
     game.countries filter (c => countryEventInPlay(c.name, marker)) foreach { c =>
       removeEventMarkersFromCountry(c.name, marker)
     }
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
        assert(targetCountry.nonEmpty, "placeExtraCells() called with empty targetCountry")
        addEventMarkersToCountry(targetCountry, TrainingCamps)

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
        case 3 if alBaghdadi  => s"$AlBaghdadi is in play but no Caliphate has been displaced"
        case 3                => s"$TrainingCamps is in play in a non-Caliphate country"
        case x => throw new IllegalStateException(s"Invalid training camp capacity: $x")
      }

      log(msg, Color.Info)
      log(s"The extra cells area now has a capacity of ${amountOf(game.extraCellCapacity, "cell")}", Color.Info)

      val delta = game.extraCellsAvailable - priorGameState.extraCellsAvailable
      if (delta > 0)
        log(s"Add ${amountOf(delta, "out of play cell")} to the right of the Ample Funding Box", Color.MapPieces)
      else if (delta < 0)
        log(s"Remove the ${amountOf(delta.abs, "cell")} cells from the extra cells area to out of play", Color.MapPieces)
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
    val markers = game.getMuslim(name).troopsMarkers map (_.name)
    removeEventMarkersFromCountry(name: String, markers:_*)
  }

  //  Move all troops cubes in the country to the track
  //  and remove all troops markers
  def removeAllTroopsFromCountry(name: String): Unit = {
    val c = game getCountry name
    moveTroops(name, "track", c.troops)
    removeAllTroopsMarkers(name)
  }

  def takeTroopsOffMap(source: String, num: Int): Unit = {
    if (num > 0) {
      val startingCommitment = game.troopCommitment
      def disp(name: String) = if (name == "track") "the troops track" else name

      log(s"Move ${amountOf(num, "troop")} from ${disp(source)} to the off-map box", Color.MapPieces)
      // Note: No need to "remove" them from the track as the number on the track is
      // calculated based on those on the map and in the off map box.
      if (source == "track")
        assert(game.troopsAvailable >= num, "takeTroopsOffMap(): Not enough troops available on track")
      else {
        val m = game.getMuslim(source)
        assert(m.troops >= num, s"takeTroopsOffMap(): Not enough troops available in $source")
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

      def disp(name: String) = if (name == "track") "the troops track" else name
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

      val updated = game.getCountry(name) match {
        case m: MuslimCountry    if isActive => m.copy(activeCells  = m.activeCells  + num)
        case m: MuslimCountry                => m.copy(sleeperCells = m.sleeperCells + num)
        case n: NonMuslimCountry if isActive => n.copy(activeCells  = n.activeCells  + num)
        case n: NonMuslimCountry             => n.copy(sleeperCells = n.sleeperCells + num)
      }
      game = game.updateCountry(updated)

      if (hasCadre)
        removeCadreFromCountry(name)
      if (fromTrack > 0)
        log("%sAdd %s to %s from the funding track".format(logPrefix, amountOf(fromTrack, cellType), name), Color.MapPieces)
      if (fromCamp > 0)
        log("%sAdd %s to %s from the training camp available area".format(logPrefix, amountOf(fromCamp, cellType), name), Color.MapPieces)
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
  def removeCellsFromCountry(name: String, actives: Int, sleepers: Int, sadr: Boolean, addCadre: Boolean, logPrefix: String = ""): Unit = {
    val c = game.getCountry(name)
    if (actives + sleepers > 0 || sadr) {
      assert(c.activeCells >= actives, s"removeCellsFromCountry(): not enough active cells present")
      assert(c.sleeperCells >= sleepers, s"removeCellsFromCountry(): not enough sleeper cells present")

      val cadreAdded = addCadre && c.hasCadre == false && c.cells == (actives + sleepers) && (sadr || !c.hasMarker(Sadr))
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
           log("%sRemove %s from %s to out of play".format(logPrefix, amountOf(toOutOfPlay, cellType), name), Color.MapPieces)
        if (toExtra > 0)
          log("%sRemove %s from %s to the extra cell available area".format(logPrefix, amountOf(toExtra, cellType), name), Color.MapPieces)
        if (toTrack > 0)
          log("%sRemove %s from %s to the funding track".format(logPrefix, amountOf(toTrack, cellType), name), Color.MapPieces)
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


  def moveCellsBetweenCountries(fromName: String, toName: String, num: Int, active: Boolean, forTravel: Boolean): Unit = {
    if (num > 0) {
      val makeActive = game.isCaliphateMember(toName) || (forTravel && toName == UnitedStates && globalEventInPlay(TravelBan))
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
    val c = game.getCountry(name)
    if (c.hasCadre)
      log(s"$name already has a cadre marker")
    else {
      log(s"Add cadre marker to $name.", Color.MapPieces)
      c match {
        case m: MuslimCountry    => game = game.updateCountry(m.copy(hasCadre = true))
        case n: NonMuslimCountry => game = game.updateCountry(n.copy(hasCadre = true))
      }
    }

  }


  def removeCadreFromCountry(name: String): Unit = {
    val c = game.getCountry(name)
    if (c.hasCadre) {
      log(s"Remove cadre marker from $name.", Color.MapPieces)
      c match {
        case m: MuslimCountry    => game = game.updateCountry(m.copy(hasCadre = false))
        case n: NonMuslimCountry => game = game.updateCountry(n.copy(hasCadre = false))
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
      val m = game.getMuslim(target)
      if (lapsingEventInPlay(ArabWinter))
        log("Awakening markers cannot be placed because \"Arab Winter\" is in effect", Color.Event)
      else if (m.canTakeAwakeningOrReactionMarker) {
        game = game.updateCountry(m.copy(awakening = m.awakening + num))
        log(s"Add ${amountOf(num, "awakening marker")} to $target", Color.MapPieces)
      }
      else
        log(s"$target cannot take an awakening marker")
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
      val m = game.getMuslim(target)
      if (lapsingEventInPlay(ArabWinter))
        log("Reaction markers cannot be placed because \"Arab Winter\" is in effect", Color.Event)
      else if (m.canTakeAwakeningOrReactionMarker) {
        game = game.updateCountry(m.copy(reaction = m.reaction + num))
        log(s"Add ${amountOf(num, "reaction marker")} to $target", Color.MapPieces)
      }
      else
        log(s"$target cannot take a reaction marker")
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

  def removeCachedWMD(name: String, num: Int, bumpPrestige: Boolean = true): Unit = {
    val c = game.getCountry(name)
    assert(c.wmdCache >= num, s"removeCachedWMD(): not enough WMD in $name")
    c match {
      case m: MuslimCountry    => game = game.updateCountry(m.copy(wmdCache = m.wmdCache - num))
      case n: NonMuslimCountry => game = game.updateCountry(n.copy(wmdCache = n.wmdCache - num))
    }
    val updatedPlots = game.plotData.copy(removedPlots = List.fill(num)(PlotWMD) ::: game.removedPlots)
    game = game.copy(plotData = updatedPlots)

    log(s"Permanently remove ${amountOf(num, "unavailable WMD plot")} from $name", Color.MapPieces)
    if (bumpPrestige) {
      game = game.adjustPrestige(num)
      log(s"Increase prestige by +$num to ${game.prestige} for removing WMD plots", Color.MapMarker)
    }
  }

  def removePlacedWMD(name: String, num: Int, bumpPrestige: Boolean = true): Unit = {
    val c = game.getCountry(name)
    val wmd = (c.plots.sorted takeWhile (_.plot == PlotWMD)).size
    assert(wmd >= num, s"removePlacedWMD(): not enough WMD in $name")
    log(s"Remove ${amountOf(num, "placed WMD plot")} from $name", Color.MapPieces)

    c match {
      case m: MuslimCountry    => game = game.updateCountry(m.copy(plots = m.plots.sorted drop num))
      case n: NonMuslimCountry => game = game.updateCountry(n.copy(plots = n.plots.sorted drop num))
    }

    val updatedPlots = game.plotData.copy(removedPlots = List.fill(num)(PlotWMD) ::: game.removedPlots)
    game = game.copy(plotData = updatedPlots)

    if (bumpPrestige) {
      game = game.adjustPrestige(num)
      log(s"Increase prestige by +$num to ${game.prestige} for removing WMD plots", Color.MapMarker)
    }
  }

  def removeAvailableWMD(num: Int, bumpPrestige: Boolean = true): Unit = {
    val wmd = (game.availablePlots.sorted takeWhile (_ == PlotWMD)).size
    assert(wmd >= num, "removeAvailableWMD(): not enough WMD plots available")
    log(s"Permanently remove ${amountOf(num, "WMD plot")} from the available plots box", Color.MapPieces)
    val updatedPlots = game.plotData.copy(
      availablePlots = game.availablePlots.sorted drop num,
      removedPlots   = (game.availablePlots.sorted take num) ::: game.removedPlots
    )
    game = game.copy(plotData = updatedPlots)
    if (bumpPrestige) {
      game = game.adjustPrestige(num)
      log(s"Increase prestige by +$num to ${game.prestige} for removing WMD plots", Color.MapMarker)
    }
  }

  def increaseFunding(amount: Int): Unit = {
    if (amount > 0) {
      game = game.adjustFunding(amount)
      log(s"Increase funding by +$amount to ${game.funding}", Color.MapMarker)
    }
  }

  def decreaseFunding(amount: Int): Unit = {
    if (amount > 0) {
      game = game.adjustFunding(-amount)
      log(s"Decrease funding by -$amount to ${game.funding}", Color.MapMarker)
    }
  }

  def increasePrestige(amount: Int): Unit = {
    if (amount > 0) {
      game = game.adjustPrestige(amount)
      log(s"Increase prestige by +$amount to ${game.prestige}", Color.MapMarker)
    }
  }

  def decreasePrestige(amount: Int): Unit = {
    if (amount > 0) {
      game = game.adjustPrestige(-amount)
      log(s"Decrease prestige by -$amount to ${game.prestige}", Color.MapMarker)
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
      removeCadreFromCountry(Nigeria)
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

  def resolvePlots(): Unit = {
    case class Unblocked(name: String, isMuslim: Boolean, mapPlot: PlotOnMap)
    def chng(amt: Int) = if (amt > 0) "Increase" else "Decrease"
    val unblocked = for (c <- game.countries filter (_.hasPlots); p <- c.plots)
      yield Unblocked(c.name, c.isMuslim, p)
    val greenOnBlue = game.muslims exists (m => m.hasPlots &&
                                          (m.inRegimeChange || m.civilWar) &&
                                          (m.totalTroops > 0 || m.numAdvisors > 0))
    var wmdsInCivilWars = Set.empty[String]
    var wmdInUS = unblocked exists (ub => ub.name == UnitedStates && ub.mapPlot.plot == PlotWMD)

    log()
    log(separator(char='='))
    log("Resolve plots")
    if (unblocked.isEmpty) {
      log(separator())
      log("There are no unblocked plots on the map", Color.Info)
    }
    else if (wmdInUS && game.exitAfterWin) {
      // If there is a WMD in the United States resolve it first as it will end the game.
      log(separator())
      log("An unblocked WMD plot was resolved in the United States", Color.Info)
      log("Game Over - Jihadist automatic victory!", Color.Info)

      saveGameState(Some("Game Over - Jihadist automatic victory!"))
      throw ExitGame
    }
    else {
      for (Unblocked(name, _, mapPlot) <- unblocked) {
        val country = game getCountry name
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
              game = game.adjustFunding(delta)
              log(f"${chng(delta)} funding by $delta%+d to ${game.funding} (Muslim country at Good governance)", Color.MapMarker)
            }
            else {
              val delta = if (mapPlot.backlashed) -1 else 1
              game = game.adjustFunding(delta)
              log(f"${chng(delta)} funding by $delta%+d to ${game.funding} (Muslim country at worse than Good governance)", Color.MapMarker)
            }
            // Prestige
            if (m.totalTroopsThatAffectPrestige > 0 && mapPlot.plot == PlotWMD) {
              game = game.copy(prestige = 1)
              log(s"Set prestige to 1 (Troops present with WMD)", Color.MapMarker)
            }
            else if (m.totalTroopsThatAffectPrestige > 0) {
              game = game.adjustPrestige(-1)
              log(s"Decrease prestige by -1 to ${game.prestige} (Troops present)", Color.MapMarker)
            }
            // Sequestration
            if (mapPlot.plot == PlotWMD && game.sequestrationTroops) {
              returnSequestrationTroopsToAvailable("\nResolved WMD plot releases the off-map Sequestration troops")
            }

            // rule 11.2.6    (WMD in Civil War)
            if (mapPlot.plot == PlotWMD && m.civilWar && (wmdsInCivilWars contains m.name)) {
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

              if (m.isGood || m.isFair || m.aidMarkers > 0) {
                // Rare for a plot to exist in an IR country.  Jihadist would have to
                // place the plot with one card, then do Major Jihad with the second
                def isSuccess(die: Int) = m.isIslamistRule || die <= m.governance
                // roll plot dice
                val dice = List.fill(mapPlot.plot.number)(dieRoll)
                val successes = dice count isSuccess
                val diceStr = dice map (d => s"$d (${if (isSuccess(d)) "success" else "failure"})")
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
              game = game.adjustFunding(1)
              log(s"Increase funding by +1 to ${game.funding} (Iran at Fair governance)", Color.MapMarker)
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
              game = game.adjustFunding(delta)
              log(s"Increase funding by +$delta to ${game.funding} (Plot number times 2, non-Muslim Good country)", Color.MapMarker)
            }
            else {
              val delta = mapPlot.plot.number
              game = game.adjustFunding(delta)
              log(s"Increase funding by +$delta to ${game.funding} (Plot number, non-Muslim country)", Color.MapMarker)
            }

            // Posture
            if (name == UnitedStates)
              rollUSPosture()
            else if (n.name == Nigeria && mapPlot.plot != Plot1) {
              // rule 11.3.3.3  (Nigeria)
              game = game.updateCountry(DefaultMuslimNigeria.copy(
                sleeperCells = n.sleeperCells,
                activeCells  = n.activeCells,
                hasCadre     = n.hasCadre,
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
                  val s1 = JihadistBot.posturePriority(Schengen filterNot (_ == n.name)).get
                  val s2 = JihadistBot.posturePriority(Schengen filterNot (x => x == n.name || x == s1)).get
                  log(s"Jihadist selects two other Schengen countries: $s1 and $s2")
                  rollCountryPosture(s1)
                  rollCountryPosture(s2)
                }
                else {
                  println("Select two other Schengen countries for posture rolls")
                  val s1 = askCountry("First Schengen country: ", Schengen, allowAbort = false)
                  val s2 = askCountry("Second Schengen country: ", Schengen filterNot (_ == s1), allowAbort = false)
                  log(s"Jihadist selects two other Schengen countries: $s1 and $s2")
                  rollCountryPosture(s1)
                  rollCountryPosture(s2)
                }
            }

            // Sequestration
            if (mapPlot.plot == PlotWMD && game.sequestrationTroops) {
              returnSequestrationTroopsToAvailable("\nResolved WMD plot releases the off map Sequestration troops")
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
          displayLine(s"Put the ${amountOf(otherCount, "resolved non-WMD plot")} in the resolved plots box", Color.MapPieces)
        else
          displayLine(s"Put the ${amountOf(otherCount, "resolved non-WMD plot")} in the available plots box", Color.MapPieces)

      (game.countries filter (_.hasPlots)) foreach {
        case m: MuslimCountry =>
          val (removed, resolved) = m.plots map (_.plot) partition (_ == PlotWMD)
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
          val (removed, resolved) = n.plots map (_.plot) partition (_ == PlotWMD)
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
    }
    val targets = unblocked.map(u => PlotTarget(u.name, u.isMuslim)).toSet
    game = game.copy(
      plotData = game.plotData.copy(resolvedTargets       = targets,
                                    resolvedInGreenOnBlue = greenOnBlue),
      plays    = PlotsResolved(unblocked.size) :: game.plays
    )
    saveGameState()
  }

  // Pirates from the base game
  def pirates1ConditionsInEffect: Boolean = List(Somalia, Yemen) map game.getMuslim exists { m =>
    m.isIslamistRule
  }
  // Pirates from the awakening expansion
  def pirates2ConditionsInEffect: Boolean = List(Somalia, Yemen) map game.getMuslim exists { m =>
    (m.isPoor && m.isAdversary) || m.isIslamistRule
  }

    // If Sequestration troops are off map and there is a 3 Resource country at IslamistRule
    // then return the troops to available.
  def returnSequestrationTroopsToAvailable(msg: String): Unit = {
    if (game.sequestrationTroops) {
      log(msg)
      moveOfMapTroopsToTrack(3)
      game = game.copy(sequestrationTroops = false)
      removeGlobalEventMarker(Sequestration)
    }
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
    @tailrec def randomAwakeTarget: String = {
        randomMuslimCountry match {
          case m if m.canTakeAwakeningOrReactionMarker => m.name
          case _ => randomAwakeTarget
        }
    }

    log()
    log("The Awakening cards were added to the deck.", Color.Info)
    log("The Awakening expansion rules are now in effect.", Color.Info)
    log("The Bot will now use the Awakening priorities.", Color.Info)
    // If Syria is under Islamist rule then the WMD cache should be added to the available plots.
    val (updatedPlots, syriaCache) = if (syria.isIslamistRule)
      (game.plotData.copy(availablePlots = PlotWMD :: PlotWMD :: game.availablePlots), 0)
    else
      (game.plotData, 2)

    game = game.updateCountry(syria.copy(isSunni = false, wmdCache = syriaCache)).
                updateCountry(iran.copy(wmdCache = 1)).
                copy(plotData = updatedPlots, currentMode = AwakeningMode)
    log()
    if (syria.isIslamistRule) {
      log("Syria is now Shia-Mix country, place the Syria country mat on the board.", Color.Info)
      log("Because Syria is under Islamist Rule, add the two WMD plots", Color.Info)
      log("from the Syria cache to the available plots box.", Color.Info)
    }
    else {
      log("Syria is now Shia-Mix country, place the Syria country mat on the board.", Color.Info)
      log("Place two unavailable WMD plots in Syria.", Color.Info)
    }
    log("Iran now contains 1 unavailable WMD plot", Color.Info)

    //  Place an awakening marker in Algeria/Tunisia if possible
    //  otherwise in a random muslim country
    val awakeningTarget = if (algeria.canTakeAwakeningOrReactionMarker)
      AlgeriaTunisia
    else {
      log(s"$AlgeriaTunisia cannot take an awakening marker.", Color.Info)
      log("An awakening marker will be added to a random Muslim country.", Color.Info)
      randomAwakeTarget
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
    game = game.copy(currentMode = ForeverWarMode)
    log()
    log("The Forever War cards were added to the deck.", Color.Info)
    log("The Bot will now use the Forever War priorities.", Color.Info)
  }

  def endTurn(): Unit = {
    if (askYorN("Really end the turn (y/n)? ")) {
      if (numUnresolvedPlots > 0)
        resolvePlots()

      log()
      log("End of turn")
      log(separator(char='='))

      if ((globalEventInPlay(Pirates1) && pirates1ConditionsInEffect) ||
          (globalEventInPlay(Pirates2) && pirates2ConditionsInEffect)) {
        log("No funding drop because Pirates is in effect", Color.Info)
      }
      else {
        game = game.adjustFunding(-1)
        log(s"Jihadist funding drops -1 to ${game.funding}", Color.MapMarker)
      }
      if (globalEventInPlay(Fracking)) {
        game = game.adjustFunding(-1)
        log(s"Jihadist funding drops -1 to ${game.funding} because Fracking is in effect", Color.MapMarker)
      }
      if (game.numIslamistRule > 0) {
        game = game.adjustPrestige(-1)
        log(s"US prestige drops -1 to ${game.prestige} (At least 1 country is under Islamist Rule)", Color.MapMarker)
      }
      else
        log(s"US prestige stays at ${game.prestige} (No countries under Islamist Rule)", Color.Info)

      val (worldPosture, level) = game.gwot
      if (game.usPosture == worldPosture && level == 3) {
        game = game.adjustPrestige(1)
        log(s"US prestige increases +1 to ${game.prestige} (World posture is $worldPosture $level and US posture is ${game.usPosture})", Color.MapMarker)
      }

      // The Bot's reserves are not cleared
      clearReserves(game.humanRole)

      if (game.resolvedPlots.nonEmpty) {
        val num = game.resolvedPlots.size
        val updatedPlots = game.plotData.copy(
          resolvedPlots  = Nil,
          availablePlots = game.availablePlots ::: game.resolvedPlots)
        game = game.copy(plotData = updatedPlots)
        log()
        log(s"Return ${amountOf(num, "resolved plot")} to the available plots box", Color.MapPieces)
      }

      if (game.useExpansionRules) {
        polarization()
        endTurnCivilWarAttrition()
      }

      checkAutomaticVictory() // Will Exit game if auto victory has been achieved

      val extraUSCards      = if (lapsingEventInPlay(FullyResourcedCOIN)) 2 else 0
      val endEbolaScare     = lapsingEventInPlay(EbolaScare)
      val endKoreanCrisis   = lapsingEventInPlay(KoreanCrisis)
      val endUSBorderCrisis = lapsingEventInPlay(USBorderCrisis)
      val endSouthChinaSeasCrisis = globalEventInPlay(SouthChinaSeaCrisis) &&
                                    game.usPosture == game.getNonMuslim(China).posture

      if (game.cardsLapsing.nonEmpty) {
        if (game.cardsLapsing.nonEmpty) {
          log()
          log("Lapsing Cards")
          log(separator())
        }
        removeLapsingCards(game.cardsLapsing)
      }

      game.firstPlotCard foreach { num =>
        log()
        log("First Plot Card")
        log(separator())
        log(s"Discard : ${cardNumAndName(num)}")
        game = game.copy(firstPlotCard = None)
      }

      // Calculate number of cards drawn
      val usCards       = USCardDraw(game.troopCommitment) + extraUSCards
      val jihadistCards = JihadistCardDraw(game.fundingLevel)
      log()
      log("Draw Cards")
      log(separator())
      log(s"$US player will draw $usCards cards", Color.Info)
      log(s"Jihadist player will draw $jihadistCards cards", Color.Info)

      // If Sequestration troops are off map and there is a 3 Resource country at IslamistRule
      // then return the troops to available.

      val threeResIR = (game.muslims find (m => m.resourceValue >= 3 && m.isIslamistRule))
      (game.sequestrationTroops, threeResIR) match {
        case (true, Some(m)) =>
          returnSequestrationTroopsToAvailable(s"\nA 3 Resource Muslim country at Islamist Rule (${m.name})\nreleases the off map Sequestration troops")
        case _ =>
      }

      if (endEbolaScare) {
        log(s"\nEbola Scare ends", Color.Event)
        moveOfMapTroopsToTrack(1)
      }
      if (endKoreanCrisis) {
        log(s"\nKorean Crisis ends", Color.Event)
        moveOfMapTroopsToTrack(2)
      }
      if (endUSBorderCrisis) {
        log(s"\nUS Border Crisis ends", Color.Event)
        moveOfMapTroopsToTrack(1)
      }
      if (endSouthChinaSeasCrisis) {
        log(s"\nSouth China Seas Crisis ends", Color.Event)
        moveOfMapTroopsToTrack(2)
        removeGlobalEventMarker(SouthChinaSeaCrisis)
      }

      for (rc <- game.muslims filter (_.regimeChange == GreenRegimeChange)) {
        game = game.updateCountry(rc.copy(regimeChange = TanRegimeChange))
        log(s"\nFlip green regime change marker in ${rc.name} to its tan side", Color.FlipPieces)
      }

      // Reset history list of plays. They are not stored in turn files.
      game = game.copy(plays = Nil)
      game = game.copy(turn = game.turn + 1)
      saveGameState()
    }
  }

  def removeLapsingCards(targets: List[Int]): Unit = {
    val lapsing = game.cardsLapsing filter targets.contains
    val (remove, discard) = lapsing.partition(deck(_).remove != NoRemove)
    if (remove.nonEmpty) {
      val cards = if (remove.size == 1) "card" else "card"
      wrap(s"Remove lapsing $cards from the game: ", remove.sorted map cardNumAndName) foreach { line =>
        log(line, Color.Event)
      }

    }
    if (discard.nonEmpty) {
      val cards = if (discard.size == 1) "card" else "card"
      wrap(s"Discard lapsing $cards: ", discard.sorted map cardNumAndName) foreach { line =>
        log(line, Color.Event)
      }
    }

    game = game.copy(
      cardsLapsing = game.cardsLapsing filterNot (x => remove.contains(x) || discard.contains(x)),
      cardsRemoved = remove ::: game.cardsRemoved
    )
  }

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
  val scenarioChoices = scenarios.toList map { case (key, scenario) => key -> scenario.name }

  // Case sensitive
  def isValidScenario(name: String) = scenarios contains name

  val AbortCard = "abort card"
  case object ExitGame    extends Exception
  case object AbortAction extends Exception
  case object Adjustment  extends Exception

  // def doWarOfIdeas(country: Country)
  def main(args: Array[String]): Unit = {
    try {
      gamesDir.mkpath()
      val versionSuffix  = if (SOFTWARE_VERSION.startsWith("0")) " - BETA" else ""
      val versionDisplay = s"Labyrinth Awakening: Bot Software (version $SOFTWARE_VERSION$versionSuffix)"
      var configParams   = loadParamsFile(UserParams())
      var cmdLineParams  = parseCommandLine(args.toIndexedSeq, UserParams(), versionDisplay)

      println()
      println(versionDisplay)

      // If the user gave an explicit file name we must assign the gama a name.
      // This is mostly used for loading someone else's file for testing.
      if (cmdLineParams.gameFile.nonEmpty) {
        println()
        gameName = Some(askGameName("Enter a name for the game: "))
        game = SavedGame.load(Pathname(cmdLineParams.gameFile.get))
        printSummary(game.playSummary)
      }
      else if (cmdLineParams.listGames) {
        val saved = savedGames
        if (saved.isEmpty)
          println("You do not have any saved games")
        else {
          val fmt = "%%-%ds - %%s".format((saved map (_.length)).max)
          for (s <- saved)
            println(fmt.format(s, loadGameDescription(s)))
        }
        throw ExitGame
      }
      else if (cmdLineParams.gameName.nonEmpty) {
        val gameName = cmdLineParams.gameName.get
        if (cmdLineParams.deleteGame) {
          (gamesDir/gameName).rmtree()
          throw ExitGame
        }
        else {
          loadMostRecent(gameName)
          printSummary(game.playSummary)
        }
      }
      else {
        val existingGame = if (cmdLineParams.anyNewGameParams) None
                           else askWhichGame()
        existingGame match {
          case Some(name) =>
            loadMostRecent(name)
            printSummary(game.playSummary)

          case None => // Start a new game
            println()
            val scenarioName = cmdLineParams.scenarioName orElse
                               configParams.scenarioName getOrElse {
              // prompt for scenario
              val choices = scenarioChoices :+ ("quit" -> "Quit")
              askMenu("Choose a scenario:", choices, allowAbort = false).head match {
                case "quit"   => throw ExitGame
                case scenario => scenario
              }
            }
            val scenario = scenarios(scenarioName)
            //  campaign always false when starting with a scenario in the
            //  latest expansion
            val campaign = if (scenario.allowsCampaign) {
              cmdLineParams.campaign orElse configParams.campaign getOrElse {
                 val choices = List(
                   "single"   -> "Play single scenario",
                   "campaign" -> "Play a campaign game",
                   "quit"     -> "Quit")
                 askMenu("\nChoose one:", choices, allowAbort = false).head match {
                   case "quit"   => throw ExitGame
                   case "single" => false
                   case _        => true
                 }
              }
            }
            else
              false
              
            val humanRole = cmdLineParams.side orElse
                            configParams.side getOrElse {
              // ask which side the user wishes to play
              val choices = List(
                "US"       -> "Play as US",
                "Jihadist" -> "Play as Jihadist",
                "quit"     -> "Quit")
              askMenu("\nChoose one:", choices, allowAbort = false).head match {
                case "quit"   => throw ExitGame
                case side     => Role(side)
              }
            }
            val difficulties = if (humanRole == US)
              cmdLineParams.jihadistBotDifficulties orElse
              configParams.jihadistBotDifficulties getOrElse askDifficulties(Jihadist)
            else
              cmdLineParams.usBotDifficulties orElse
              configParams.usBotDifficulties getOrElse askDifficulties(US)
            val humanAutoRoll = cmdLineParams.autoDice orElse
                                configParams.autoDice getOrElse
                                !askYorN("Do you wish to roll your own dice (y/n)? ")

            gameName = Some(askGameName("Enter a name for your new game: "))

            val showColor = cmdLineParams.showColor orElse configParams.showColor getOrElse !scala.util.Properties.isWin
            game = initialGameState(scenario, campaign, humanRole, humanAutoRoll, difficulties, showColor)
            logSummary(game.scenarioSummary)
            printSummary(game.scoringSummary)
            if (scenario.cardsRemoved.nonEmpty) {
              log()
              log("The following cards are removed for this scenario")
              log(separator())
              scenario.cardsRemoved map (deck(_).toString) foreach (log(_))
            }
            log()
            scenario.additionalSetup()
            game = game.copy(turn = 1)
            saveGameState(Some("Beginning of game"))

            val usCards = USCardDraw(game.troopCommitment)
            val jihadistCards = JihadistCardDraw(game.fundingLevel)
            log()
            log("Draw Cards")
            log(separator())
            log(s"$US player will draw $usCards cards", Color.Info)
            log(s"Jihadist player will draw $jihadistCards cards", Color.Info)
        }
      }

      commandLoop()
    }
    catch {
      case ExitGame =>
    }
  }

  def parseCommandLine(args: Seq[String], userParams: UserParams, versionDisplay: String): UserParams = {
    import org.sellmerfud.optparse._
    def diffHelp(diffs: Seq[BotDifficulty]): Seq[String] = {
      val maxLen = (diffs map (_.name.length)).max
      val fmt = "%%-%ds - %%s".format(maxLen)
      diffs map (d => fmt.format(d.name, d.description))
    }
    case class JihadDiff(diff: BotDifficulty)
    case class USDiff(diff: BotDifficulty)
    try {
      new OptionParser[UserParams] {
        addArgumentParser[JihadDiff] { arg =>
          if (isValidIdeology(arg))
            JihadDiff(BotDifficulty(arg))
          else
            throw new InvalidArgumentException(s"Invalid Jihadist ideology value '$arg'")
        }
        addArgumentParser[USDiff] { arg =>
          if (isValidUsResolve(arg))
            USDiff(BotDifficulty(arg))
          else
            throw new InvalidArgumentException(s"Invalid US resolve value '$arg'")
        }
        banner = "awakening [options]"
        this.separator("")
        this.separator("Options:")
        val saved = savedGames
        if (saved.isEmpty)
          reqd[String]("-g", "--game=name", "Resume a game in progress")
            { (v, c) => throw new InvalidArgumentException("You do not have any saved games") }
        else
          reqd[String]("-g", "--game=name", saved, "Resume a game in progress")
            { (v, c) => c.copy(gameName = Some(v)) }

        flag("-l", "--list", "Display a list of saved games")
          { (c) => c.copy(listGames = true) }

        if (saved.isEmpty)
          reqd[String]("", "--delete=name", "Delete a game in progress")
            { (v, c) => throw new InvalidArgumentException("You do not have any saved games") }
        else
          reqd[String]("", "--delete=name", saved, "Delete a game in progress")
            { (v, c) => c.copy(gameName = Some(v), deleteGame = true) }

        val scenarioHelp = "Select a scenario" +: scenarios.keys.toSeq
        reqd[String]("", "--scenario=name", scenarios.keys.toSeq, scenarioHelp: _*)
          { (v, c) => c.copy(scenarioName = Some(v)) }

        reqd[String]("", "--campaign=yes|no", Seq("yes","no"), "Play a campaign game from selected scenario")
          { (v, c) => c.copy(campaign = Some(v == "yes")) }

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
          val values = (v map { case JihadDiff(diff) => diff }).sorted.distinct
          c.copy(ideology = values)
        }
        list[USDiff]("", "--us-resolve=x,y,z",
                     ("Comma separated list of US resolve values" +: diffHelp(AllUSLevels)): _*) {
          (v, c) =>
          val values = (v map { case USDiff(diff) => diff }).sorted.distinct
          c.copy(usResolve = values)
        }
        bool("", "--color", "Show colored log messages")
          { (v, c) => c.copy(showColor = Some(v)) }
        reqd[String]("", "--file=path", "Path to a saved game file")
          { (v, c) => c.copy(gameFile = Some(v)) }
        flag("-v", "--version", "Display program version and exit") { (c) =>
          println(versionDisplay)
          System.exit(0)
          c // To keep compiler happy
        }

      }.parse(args, userParams)
    }
    catch { case e: OptionParserException => println(e.getMessage); sys.exit(1) }
  }

  // Ask which saved game the user wants to load.
  // Return None if they with to start a new game.
  // previously saved game.
  def askWhichGame(): Option[String] = {
    val games = savedGames
    if (games.isEmpty)
      None
    else {
      val gameChoices = games.toList map { name =>
        val desc = loadGameDescription(name)
        val suffix = if (desc == "") "" else s", $desc"
        name -> s"Resume '$name'$suffix"
      }
      val choices = ("--new-game--" -> "Start a new game") :: gameChoices ::: List("--quit-game--" -> "Quit")
      askMenu("\nWhich game would you like to play:", choices, allowAbort = false).head match {
        case "--new-game--"  => None
        case "--quit-game--" => throw ExitGame
        case name            => Some(name)
      }
    }
  }

  case class UserParams(
    val gameName: Option[String] = None,
    val deleteGame: Boolean = false,
    val listGames: Boolean = false,
    val scenarioName: Option[String] = None,
    val campaign: Option[Boolean] = None,
    val side: Option[Role] = None,
    val level: Option[Int] = None,
    val autoDice: Option[Boolean] = None,
    val ideology: List[BotDifficulty] = Nil,
    val usResolve: List[BotDifficulty] = Nil,
    val showColor: Option[Boolean] = None,
    val gameFile: Option[String] = None) {

    def jihadistBotDifficulties: Option[List[BotDifficulty]] = ideology match {
      case Nil => level map (AllJihadistLevels take _)
      case xs  => Some(xs)
    }

    def usBotDifficulties: Option[List[BotDifficulty]] = usResolve match {
      case Nil => level map (AllUSLevels take _)
      case xs  => Some(xs)
    }

    def anyNewGameParams =
      (scenarioName orElse campaign orElse side orElse level orElse autoDice).nonEmpty ||
      ideology.nonEmpty || usResolve.nonEmpty
  }

  def loadParamsFile(initialParams: UserParams): UserParams = {
    import java.util.Properties
    val testpath = Some(Pathname("./test_config")) filter (_.exists)
    val path = testpath getOrElse Pathname("./awakening_config")
    if (path.exists && path.isReadable) {
      try {
        var params = initialParams
        val props = new Properties()
        def propValue(name: String): Option[String] =
          (Option(props.getProperty(name)) map (_.trim)) match {
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

        propValue("campaign") foreach { value =>
          value.toLowerCase match {
            case "yes" => params = params.copy(campaign = Some(true))
            case "no"  => params = params.copy(campaign = Some(false))
            case _ => println(s"Ignoring invalid campaign value ($value) in awakening_config file")
          }
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
          val tokens = value.split(",").toList map (_.trim) filterNot (_ == "")
          if (tokens forall isValidIdeology)
            params = params.copy(ideology = (tokens.distinct map BotDifficulty.apply).sorted)
          else
            println(s"Ignoring invalid ideology value ($value) in awakening_config file")
        }
        propValue("us-resolve") foreach { value =>
          val tokens = value.split(",").toList map (_.trim) filterNot (_ == "")
          if (tokens forall isValidUsResolve)
            params = params.copy(usResolve = (tokens.distinct map BotDifficulty.apply).sorted)
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
        params
      }
      catch {
        case e: Throwable =>
          println(s"Error reading $path: ${e.getMessage}")
          initialParams
      }
    }
    else
      initialParams
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
    val fmt = "%%-%ds".format((levels map (_.name.length)).max)

    def nextChoice(num: Int, prev: Option[BotDifficulty], levels: List[BotDifficulty]): List[(String, String)] =
      (prev, levels) match {
        case (_, Nil) => Nil
        case (None, (d@BotDifficulty(_, name, desc)) :: rest) =>
          (num.toString -> s"${fmt.format(name)}: $desc") :: nextChoice(num+1, Some(d), rest)
        case (Some(BotDifficulty(_, pname, _)), (d@BotDifficulty(_, name, desc)) :: rest) =>
          (num.toString -> s"${fmt.format(name)}: $pname plus $desc") :: nextChoice(num+1, Some(d), rest)
      }

    levels take askMenu("\nChoose a difficulty level:", nextChoice(1, None, levels), allowAbort = false).head.toInt
  }

  def isValidIdeology(name: String) =
    JihadistLevels.keys exists (_.toLowerCase == name.toLowerCase)

  def isValidUsResolve(name: String) =
    JihadistLevels.keys exists (_.toLowerCase == name.toLowerCase)


  // Check to see if any automatic victory condition has been met.
  // Note: The WMD resolved in United States condition is checked by resolvePlots()
  def checkAutomaticVictory(): Unit = {
    def gameOver(victor: Role, reason: String): Unit = {
      val summary = if (game.exitAfterWin)
        s"Game Over - $victor automatic victory!"
      else
        s"Victory condition met - $victor automatic victory!"
      log()
      log(separator())
      log(summary, Color.Info)
      log(reason, Color.Info)

      if (game.exitAfterWin) {
        game = game.copy(plays = Nil)
        saveGameState(Some(summary))
        throw ExitGame
      }
    }

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

  def numUnresolvedPlots: Int = game.countries.foldLeft(0) { (sum, c) => sum + c.plots.size }

  // ---------------------------------------------
  // Process all top level user commands.
  @tailrec def commandLoop(): Unit = {
    checkAutomaticVictory() // Will Exit game if auto victory has been achieved

    val cardsPlayed     = (game.plays map (_.numCards)).sum
    val cardsSincePlots = (game.plays takeWhile (!_.isInstanceOf[PlotsResolved])
                                      filterNot (_.isInstanceOf[AdditionalCard])
                                      map (_.numCards)).sum

    if (cardsSincePlots > 0 && cardsSincePlots % 4 == 0) {
      // If there are plots on the map call it to the users attention,
      // otherwise just do the resolvePlots() so that it is added to the log.
      if (numUnresolvedPlots > 0) {
        println()
        println(separator())
        println("4 cards have been played and there are unresolved plots on the map")
        pause()
      }
      resolvePlots()
    }

    val plots = numUnresolvedPlots
    val plotDisp = if (plots == 0) "" else s", ${amountOf(plots, "unresolved plot")}"
    val prompt = {
      s"""
         |>>> Turn ${game.turn}  (${amountOf(cardsPlayed, "card")} played$plotDisp) <<<
         |${separator()}
         |Command: """.stripMargin
    }
    readLine(prompt) match {
      case null =>
        println()
        commandLoop()
      case cmd =>
        doCommand(cmd.trim)
        commandLoop()
    }
  }


  // Parse the top level input and execute the appropriate command.
  def doCommand(input: String): Unit = {
    val cardsPlayed     = (game.plays map (_.numCards)).sum
    case class Command(val name: String, val help: String)
    val Commands = List(
      Command("us",           """Enter a card number for a US card play"""),
      Command("jihadist",     """Enter a card number for a Jihadist card play"""),
      Command("remove cadre", """Voluntarily remove a cadre from the map"""),
      Command("resolve plots","""Resolve any unblocked plots on the map"""),
      Command("end turn",     """End the current turn.
                                |This should be done after the last US card play.
                                |Any plots will be resolved and the end of turn
                                |actions will be conducted."""),
      Command("add awakening cards", """Add the Awakening cards to the draw deck and
                                |begin using the Awakening expansion rules.""".stripMargin),
      Command("add forever cards", """Add the Forever War cards to the draw deck and
                                |begin using the Forever War expansion rules.""".stripMargin),
      Command("show",         """Display the current game state
                                |  show all           - entire game state
                                |  show plays         - cards played during the current turn
                                |  show summary       - game summary including score
                                |  show scenario      - scenario and difficulty level
                                |  show removed cards - cards that have been removed from the game
                                |  show caliphate     - countries making up the Caliphate
                                |  show civil wars    - countries in civil war
                                |  show <country>     - state of a single country""".stripMargin),
      Command("adjust",       """Adjust game settings  (Minimal rule checking is applied)
                                |  adjust prestige      - US prestige level
                                |  adjust posture       - US posture
                                |  adjust funding       - Jihadist funding level
                                |  adjust difficulty    - Jihadist ideology/US resolve
                                |  adjust lapsing cards - Current lapsing cards
                                |  adjust removed cards - Cards removed from the game
                                |  adjust first plot    - Current first plot card
                                |  adjust markers       - Current global event markers
                                |  adjust reserves      - US and/or Jihadist reserves
                                |  adjust plots         - Available/resolved plots
                                |  adjust offmap troops - Number of troops in off map box
                                |  adjust auto roll     - Auto roll for human operations
                                |  adjust <country>     - Country specific settings""".stripMargin),
      Command("history",      """Display game history
                                |  history       - Shows the log from the beginning of the current turn
                                |  history -1    - Shows the log from the beginning of the previous turn
                                |  history -n    - Shows the log from the beginning of the turn n turns ago
                                |  history 1     - Shows the log for the first turn
                                |  history n     - Shows the log for the nth turn
                                |  history 1..3  - Shows the log for the first through third turns
                                |  history 5..   - Shows the log from the fifth turn through the end
                                |  history ..5   - Shows the log from the beginning through the fifth turn
                                |  history all   - Shows the entire log
                                |  Any of the above commands may be followed by >filename
                                |  to save the history in a file instead of echoing it to the console""".stripMargin),
      Command("rollback",     """Roll back card plays in the current turn or
                                |roll back to the start of any previous turn""".stripMargin),
      Command("help",         """List available commands"""),
      Command("quit",         """Quit the game.  All plays for the current turn will be saved.""")
    ) filter {
      case Command("rollback", _)            => mostRecentSaveNumber(gameName.get).getOrElse(0) > 0
      case Command("remove cadre", _)        => game.humanRole == Jihadist && (game.countries exists (_.hasCadre))
      case Command("add awakening cards", _) => game.currentMode == LabyrinthMode && game.campaign
      case Command("add forever cards", _)   => game.currentMode == AwakeningMode && game.campaign
      case _                                 => true
    }

    val CmdNames = (Commands map (_.name))

    def showCommandHelp(cmd: String) = Commands find (_.name == cmd) foreach (c => println(c.help))

    val tokens = input.split("\\s+").toList.dropWhile(_ == "")
    tokens.headOption foreach { verb =>
      val param = if (tokens.tail.nonEmpty) Some(tokens.tail.mkString(" ")) else None
      matchOne(verb, CmdNames) foreach {
        case "us"                    => usCardPlay(param)
        case "jihadist"              => jihadistCardPlay(param)
        case "remove cadre"          => humanRemoveCadre()
        case "resolve plots"         => resolvePlots()
        case "end turn"              => endTurn()
        case "add awakening cards"   => addAwakeningCards()
        case "add forever cards"     => addForeverWarCards()
        case "show"                  => showCommand(param)
        case "adjust"                => adjustSettings(param)
        case "history"               => showHistory(param)
        case "rollback"              => rollback(param)
        case "quit"                  => if (askYorN("Really quit (y/n)? ")) throw ExitGame
        case "help" if param.isEmpty =>
          println("Available commands: (type help <command> for more detail)")
          println(orList(CmdNames))
        case "help"     => matchOne(param.get, CmdNames) foreach showCommandHelp
        case cmd        => println(s"Internal error: Command '$cmd' is not valid")
      }
    }
  }

  // The Jihadist play can voluntarily remove cadre markers on the map
  // (To avoid giving the US an easy prestige bump)
  def humanRemoveCadre(): Unit = {
    val candidates = countryNames(game.countries filter (_.hasCadre))
    if (candidates.isEmpty)
      println("There are no cadres on the map")
    else {
      val target = askCountry(s"Remove cadre in which country: ", candidates)
      game.getCountry(target) match {
        case m: MuslimCountry    => game = game.updateCountry(m.copy(hasCadre = false))
        case n: NonMuslimCountry => game = game.updateCountry(n.copy(hasCadre = false))
      }
      log()
      log(separator())
      log(s"$Jihadist voluntarily removes a cadre from $target", Color.MapPieces)
    }
  }


  def showCommand(param: Option[String]): Unit = {
    val options = "all" :: "plays" :: "summary" :: "scenario" :: "caliphate" ::
                  "civil wars" :: "removed cards" :: countryNames(game.countries)
    val opts = if (game.useExpansionRules) options
               else options filterNot(o => o == "caliphate" || o == "civil wars")

    askOneOf("Show: ", options, param, allowNone = true, abbr = CountryAbbreviations, allowAbort = false) foreach {
      case "plays"         => printSummary(game.playSummary)
      case "summary"       => printSummary(game.scoringSummary); printSummary(game.statusSummary)
      case "scenario"      => printSummary(game.scenarioSummary)
      case "caliphate"     => printSummary(game.caliphateSummary)
      case "civil wars"    => printSummary(game.civilWarSummary)
      case "removed cards" => printSummary(game.removedCardsSummary)
      case "all"           => printGameState()
      case name            => printSummary(game.countrySummary(name))
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
    printCountries("Muslim Countries with Good Governance",  countryNames(game.muslims filter (_.isGood)))
    printCountries("Muslim Countries with Fair Governance",  countryNames(game.muslims filter (_.isFair)))
    printCountries("Muslim Countries with Poor Governance",  countryNames(game.muslims filter (_.isPoor)))
    printCountries("Muslim Countries under Islamic Rule",    countryNames(game.muslims filter (_.isIslamistRule)))
    printCountries("Untested Muslim Countries",    countryNames(game.muslims filter (_.isUntested)))
    printCountries("Non-Muslim Countries with Hard Posture", countryNames(game.nonMuslims filter (_.isHard)))
    printCountries("Non-Muslim Countries with Soft Posture", countryNames(game.nonMuslims filter (_.isSoft)))
    printCountries("Untested Non-Muslim Countries", countryNames(game.nonMuslims filter (_.isUntested)))
    val iranSpecial = game.nonMuslims find (_.iranSpecialCase) map (_.name)
    if (iranSpecial.nonEmpty)
      printCountries("Iran Special Case", iranSpecial.toList)
    printSummary(game.scoringSummary)
    printSummary(game.statusSummary)
    printSummary(game.removedCardsSummary)
    if (game.useExpansionRules) {
      printSummary(game.civilWarSummary)
      printSummary(game.caliphateSummary)
    }
  }

  sealed trait CardAction
  case class  TriggeredEvent(card: Card) extends CardAction
  case object Ops                        extends CardAction

  // Return a list of actions.
  // card2 is only used for US reassessment
  def getActionOrder(cards: List[Card], opponent: Role): List[CardAction] = {
    val triggeredCards = cards filter (c => c.autoTrigger || c.association == opponent)
    triggeredCards match {
      case c :: Nil =>
        val choices = List(
          "operations" -> "Operations",
          "event"      -> s"Event: ${c.name}")
        val prompt = if (c.association == opponent)
          s"\n$opponent associated event, which should happen first?"
        else
          s"\nThe event will trigger, which should happen first?"
        askMenu(prompt, choices).head  match {
          case "event" => List(TriggeredEvent(c), Ops)
          case _       => List(Ops, TriggeredEvent(c))
        }

      case c1 :: c2 :: Nil =>
        val choices = List(
          "operations" -> "Operations",
          "1st event"  -> s"Event: ${c1.name}",
          "2nd event"  -> s"Event: ${c2.name}")
        val first  = askMenu("\nWhich should happen first?", choices).head
        val second = askMenu("Which should happen second?", choices filterNot (_._1 == first)).head
        val third  = (choices map (_._1) filterNot (k => k == first || k == second)).head
        List(first, second, third) map {
          case "1st event" => TriggeredEvent(c1)
          case "2nd event" => TriggeredEvent(c2)
          case _           => Ops
        }
      case _ => // No events triggered
        List(Ops)
    }
  }

  // Test to see if the event should trigger and if so
  // perform the event.
  def attemptTriggeredEvent(opponentRole: Role, card: Card): Unit = {
    if (card.autoTrigger)
      performCardEvent(card, opponentRole, triggered = true)
    else if (card.association == opponentRole && card.eventWillTrigger(opponentRole))
      opponentRole match {
        case Jihadist => JihadistBot.performTriggeredEvent(card)
        case US       => USBot.performTriggeredEvent(card)
      }
    else
      log("\n%s event \"%s\" has no effect".format(card.association, card.name), Color.Event)
  }

  // The Intel Community event allows the US to play an additional card during
  // the action phase.  Hence the `additional` parameter.
  def usCardPlay(param: Option[String], additional: Boolean = false): Unit = {
    // If we are entering a new action phase,
    // resolve plots if necessary and reset the phase targets
    val (newPhase, skippedPlotResolution) = if (additional) (false, false)
                                            else newActionPhase(US)
    if (newPhase) {
      if (skippedPlotResolution) {
        if (numUnresolvedPlots > 0) {
          println()
          println(separator())
          println("A US card play starts a new action phase and there are unresolved plots")
          val choices = List("resolve" -> "Resolve plots", "cancel" -> "Abort the US card play")
          if (askMenu("", choices, allowAbort = false).head == "resolve")
            resolvePlots()
          return
        }
        else
          resolvePlots() // No plots so allow card play to continue
      }

      game = game.copy(
        targetsLastPhase = game.targetsThisPhase,
        targetsThisPhase = PhaseTargets()
      )
    }

    askCardNumber("Card # ", param) foreach { cardNumber =>
      val card = deck(cardNumber)
      val savedState = game

      // Add the card to the list of plays for the turn.
      val thisPlay = if (additional)
        AdditionalCard(US, card.number)
      else
        PlayedCard(US, card.number)
      game = game.copy(plays = thisPlay :: game.plays)

      cachedEventPlayableAnswer = None

      val playable = card.eventIsPlayable(US)
      logCardPlay(US, card, playable)
      try {

        // When the Ferguson event is in effect, the Jihadist player
        // may cancel the play of any US associated card.
        // If the JihadistBot is playing it will cancel the next one played by the US.
        if (lapsingEventInPlay(Ferguson)    &&
            card.association == US          &&
            (game.botRole == Jihadist ||
             askYorN("Do you wish to cancel the play of this US associated card? (y/n) "))) {

          log(s"${card.numAndName} is discarded without effect due to Ferguson being in effect", Color.Event)
          removeCardFromLapsing(Ferguson)
        }
        else
          game.humanRole match {
            case US => humanUsCardPlay(card, playable)
            case _  => USBot.cardPlay(card, playable)
          }

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
  def humanUsCardPlay(card: Card, playable: Boolean): Unit = {
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
    def inReserve    = game.reserves.us
    var secondCard: Option[Card] = None   // For reassessment only
    def opsAvailable = (card.ops + reservesUsed) min 3

    @tailrec def getAction(): String = {
      val canReassess = firstCardOfPhase(US) && card.ops == 3 && reservesUsed == 0
      val actions = List(
        choice(playable && reservesUsed == 0,              ExecuteEvent, ExecuteEvent),
        choice(true,                                       WarOfIdeas, WarOfIdeas),
        choice(game.deployPossible(opsAvailable),          Deploy, Deploy),
        choice(game.regimeChangePossible(opsAvailable),    RegimeChg, RegimeChg),
        choice(game.withdrawPossible(opsAvailable),        Withdraw, Withdraw),
        choice(game.disruptTargets(opsAvailable).nonEmpty, Disrupt, Disrupt),
        choice(game.alertPossible(opsAvailable),           Alert, Alert),
        choice(canReassess,                                Reassess, Reassess),
        choice(card.ops < 3 && inReserve < 2,              AddReserves, AddReserves),
        choice(opsAvailable < 3 && inReserve > 0,          UseReserves, UseReserves),
        choice(true,                                       AbortCard, AbortCard),
      ).flatten

      println(s"\nYou have ${opsString(opsAvailable)} available and ${opsString(inReserve)} in reserve")

      askMenu(s"\n$US action: ", actions).head match {
        case UseReserves =>
          reservesUsed = inReserve
          log(s"$US player expends their reserves of ${opsString(reservesUsed)}", Color.Info)
          game = game.copy(reserves = game.reserves.copy(us = 0))
          getAction()

        case Reassess =>
          println("You must play a second 3 Ops card")
          askCardNumber("Card # ", None, true, only3Ops = true) match {
            case None => throw AbortAction
            case Some(cardNum) =>
              val card2 = deck(cardNum)
              // Replace the head card play with a reassessment
              game = game.copy(plays = PlayedReassement(card.number, card2.number) :: game.plays.tail)
              secondCard = Some(card2)
              logCardPlay(US, card2, card2.eventIsPlayable(US))
              Reassess
          }
        case AbortCard =>
            if (askYorN("Really abort (y/n)? "))
              throw AbortAction
            else
              getAction()

        case action => action
      }
    }

    // If the the event is US elections or is associated with the Jihadist
    // Ask if user want so resolve event or operation first
    // If they choose to resolve the event first, do so before
    // prompting for the action because the ramifications of the event
    // may affect what actions are possible.
    val actionOrder = if (card.autoTrigger || card.association == Jihadist) {
      getActionOrder(card :: Nil, opponent = Jihadist) match {
        case Nil =>
          throw AbortAction
        case TriggeredEvent(c) :: _ =>
          attemptTriggeredEvent(Jihadist, c)
          List(Ops)
        case _ =>
          card1EventValid = true
          List(Ops, TriggeredEvent(card))
      }
    }
    else
      List(Ops)

    val action = getAction();

    // There will only be a second card if the reassessment action was chosen.
    // If so we must check to see if the event on the second card will trigger
    // an event.  (The first card event by still have to be triggered too.)
    val finalOrder = secondCard match {
      case Some(card2) if (card2.autoTrigger || card2.association == Jihadist) =>
        if (card1EventValid)
          getActionOrder(card :: card2 :: Nil, opponent = Jihadist)
        else
          getActionOrder(card2 :: Nil, opponent = Jihadist)
      case _ =>
        actionOrder
    }

    finalOrder foreach {
      case TriggeredEvent(c) =>
        attemptTriggeredEvent(Jihadist, c)

      case Ops =>
        action match {
          case AddReserves  => addToReserves(US, card.ops)
          case ExecuteEvent => performCardEvent(card, US)
          case WarOfIdeas   => humanWarOfIdeas(opsAvailable)
          case Deploy       => humanDeploy(opsAvailable)
          case Disrupt      => humanDisrupt(opsAvailable)
          case RegimeChg    => humanRegimeChange()
          case Withdraw     => humanWithdraw()
          case Alert        => humanAlert()
          case Reassess     => humanReassess()
          case _ => throw new IllegalStateException(s"Invalid US action: $action")
        }
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
    val actions = List(
      choice(true,                              WarOfIdeas, WarOfIdeas),
      choice(game.deployPossible(ops),          Deploy, Deploy),
      choice(game.regimeChangePossible(ops),    RegimeChg, RegimeChg),
      choice(game.withdrawPossible(ops),        Withdraw, Withdraw),
      choice(game.disruptTargets(ops).nonEmpty, Disrupt, Disrupt),
      choice(game.alertPossible(ops),           Alert, Alert)
    ).flatten
    askMenu(s"\n$US action:", actions).head match {
      case WarOfIdeas => humanWarOfIdeas(ops)
      case Deploy     => humanDeploy(ops)
      case RegimeChg  => humanRegimeChange()
      case Withdraw   => humanWithdraw()
      case Disrupt    => humanDisrupt(ops)
      case _          => humanAlert()
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
    log(s"$US performs a Deploy operation")
    log(separator())
    // If the only
    val (from, to) = game.deployTargets(ops).get
    val source     = askCountry("Deploy troops from: ", from)
    val maxCanLeave = if (source == "track")
      game.troopsAvailable
    else
      (game getCountry source).maxDeployFrom

    // If th NATO or NATO-2 marker is present, the player has the option
    // of deploying it out of the country and removing it from the game.
    val (markersRemoved, markersValue): (List[String], Int) = if (source == "track")
      (Nil, 0)
    else {
      val src = game getCountry source
      val markers = src.troopsMarkers filter (m => m.canDeploy)
      // If the source is in regime change then we must take care not to allow too many
      // markers to be removed so that we do not leave enough troops/militia behind.
      val regimeChange = (game isMuslim source) && (game getMuslim source).inRegimeChange
      val canRemoveAll: List[TroopsMarker] => Boolean = if (regimeChange)
        markerList => (markerList map (_.num)).sum <= maxCanLeave
      else
        _ => true  // Always possible if not in regime change

      val combos = for {
        i     <- 1 to markers.size
        combo <- markers.combinations(i).toList if canRemoveAll(combo)
        names = combo map (_.name)
      } yield (names.mkString(",") -> andList(names))

      if (combos.isEmpty)
        (Nil, 0)
      else {
        val choices = ("none" -> "Do not deploy any markers") :: combos.toList
        val markerNames = askMenu(s"Which troops markers will deploy out of $source", choices).head match {
          case "none" => Nil
          case str    => str.split(",").toList
        }
        val value = markerNames.foldLeft(0) { (sum, name) => sum + (markers find (_.name == name) map (_.num) getOrElse 0) }
        (markerNames, value)
      }
    }

    val maxTroops  = maxCanLeave - markersValue
    val minTroops  = if (markersRemoved.isEmpty) 1 else 0
    val numTroops  = if (maxTroops > 0)  // Could be zero if only markers could deploy out
      askInt("Deploy how many troops: ", minTroops, maxTroops)
    else
      0
    if (numTroops > 0) {
      val dest = askCountry("Deploy troops to: ", to filterNot (_ == source))
      addOpsTarget(dest)
      moveTroops(source, dest, numTroops)
    }
    // Troops markers that deploy are simply removed
    removeEventMarkersFromCountry(source, markersRemoved:_*)
  }

  def humanRegimeChange(): Unit = {
    log()
    log(s"$US performs a Regime Change operation")
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
    log()
    log(s"$US performs a Withdraw operation")
    log(separator())
    val source = askCountry("Withdraw troops from which country: ", game.withdrawFromTargets)
    val dest   = askCountry("Deploy withdrawn troops to: ", (game.withdrawToTargets filter (_ != source)))
    val numTroops = askInt("How many troops: ", 1, game.getCountry(source).troops)
    addOpsTarget(source)
    performWithdraw(source, dest, numTroops)
  }

  def humanDisrupt(ops: Int): Unit = {
    log()
    log(s"$US performs a Disrupt operation")
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
    if ((c.plots exists (_.backlashed)) && (c.plots exists (p => !p.backlashed)))
      shuffle(c.plots filterNot (_.backlashed)).head
    else
      shuffle(c.plots).head
  }

  def humanAlert(): Unit = {
    log()
    log(s"$US performs an Alert operation")
    log(separator())
    val name = askCountry("Alert plot in which country: ", game.alertTargets)
    addOpsTarget(name)
    performAlert(name, humanPickPlotToAlert(name))
  }

  def humanReassess(): Unit = {
    log()
    log(s"$US performs a Reassessment operation")
    log(separator())
    performReassessment()
  }

  // Attempt to detect a change of action phase.
  // Normally an action phase consists to two card plays by the same side.
  // There are exceptions to this because the hand sizes may have an odd number
  // of cards and each hand size may be different.
  // This function is called as a new card is being played by the indicated Role.
  // We use the follow heuristic:
  // - Looking at the most recent plays, count the number of cards that have been
  //   played by the current role since either the other role has played a card, or
  //   plots have been resolved or the beginning of the turn.
  //   If that number is even (including zero) then we are starting a new phase.
  //
  // Card #138, "Intel Community" allows a player to play an additional card
  // in the same action phase.  We do not include the additional card in the count.
  //
  // It is possible that we skipped the plot resolution phase (because there were no
  // plots on the map).  We detect that when going straight form a player phase
  // to another player phase, unless it is from Jihadist to US.
  // The second return value indicates whether the plot resolution was skipped.
  def newActionPhase(role: Role): (Boolean, Boolean) = {
    val cardPlays = (game.plays filterNot(_.isInstanceOf[AdjustmentMade])
                                takeWhile (_.isInstanceOf[CardPlay])
                                map (_.asInstanceOf[CardPlay]))
    val currentRolePlays = cardPlays takeWhile (_.role == role)

    // Skip any additional card plays in the count
    val newPhase = (currentRolePlays filterNot (_.isInstanceOf[AdditionalCard]) map (_.numCards)).sum % 2 == 0
    val skippedPlots = newPhase && cardPlays.nonEmpty &&
                       (cardPlays.head.role == role ||  // Always if same player is taking two actions in a row
                        role == Jihadist)               // If Jihadist is playing directly after US
    (newPhase, skippedPlots)
  }

  // Return true if this is the first card played by the give role in the
  // current action phase.
  // IMPORTANT: This function assumes that the current card has already been added to
  //            the list of plays for the turn.
  def firstCardOfPhase(role: Role): Boolean = {
    val cardPlays = game.plays.filterNot(_.isInstanceOf[AdjustmentMade]).takeWhile {
      case p: CardPlay if p.role == role => true
      case _ => false
    }
    // Skip any additional card plays
    (cardPlays filterNot (_.isInstanceOf[AdditionalCard]) map (_.numCards)).sum == 1
  }

  def jihadistCardPlay(param: Option[String]): Unit = {
    // If we are entering a new action phase,
    // resolve plots if necessary and reset the phase targets
    val (newPhase, skippedPlotResolution) = newActionPhase(Jihadist)
    if (newPhase) {
      if (skippedPlotResolution) {
        if (numUnresolvedPlots > 0) {
          println()
          println(separator())
          println("A Jihadist card play starts a new action phase and there are unresolved plots")
          val choices = List("resolve" -> "Resolve plots", "cancel" -> "Abort the Jihadist card play")
          if (askMenu("", choices, allowAbort = false).head == "resolve")
            resolvePlots()
          return
        }
        else
          resolvePlots() // No plots so allow card play to continue
      }

      game = game.copy(
        targetsLastPhase = game.targetsThisPhase,
        targetsThisPhase = PhaseTargets()
      )
    }
    askCardNumber("Card # ", param) foreach { cardNumber =>
      val card = deck(cardNumber)
      val savedState = game

      // Add the card to the list of plays for the turn.
      game = game.copy(plays = PlayedCard(Jihadist, card.number) :: game.plays)

      cachedEventPlayableAnswer = None
      // If TheDoorOfItjihad lapsing card is in effect,
      // then Jihadist cannot play any events (except autoTrigger events)
      val playable = lapsingEventNotInPlay(TheDoorOfItjihad) && card.eventIsPlayable(Jihadist)
      logCardPlay(Jihadist, card, playable)
      try {
        // When the Ferguson event is in effect, the Jihadist player
        // may cancel the play of any US associated card.
        // If the JihadistBot is playing it will only cancel those played by the US.
        if (lapsingEventInPlay(Ferguson)    &&
            card.association == US          &&
            game.humanRole == Jihadist      &&
            askYorN("Do you wish to cancel the play of this US associated card? (y/n) ")) {
          log(s"${card.numAndName} is discarded without effect due to Ferguson being in effect", Color.Event)
          removeCardFromLapsing(Ferguson)
        }
        else
          game.humanRole match {
            case Jihadist => humanJihadistCardPlay(card, playable)
            case _        => JihadistBot.cardPlay(card, playable)
          }

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
  def humanJihadistCardPlay(card: Card, playable: Boolean): Unit = {
    val ExecuteEvent = "Execute event"
    val Recruit      = "Recruit"
    val Travel       = "Travel"
    val Jihad        = "Jihad"
    val PlotAction   = "Plot"
    val AddReserves  = "Add to reserves"
    val UseReserves  = "Expend reserves"
    val RemoveCadre  = "Remove cadre"
    val AbortCard    = "Abort Card"
    var reservesUsed          = 0
    var opponentEventResolved = false
    var firstPlot             = false
    def inReserve    = game.reserves.jihadist
    def opsAvailable = (card.ops + reservesUsed) min 3

    @tailrec def getAction(): String = {
      val actions = List(
        choice(firstPlot,                                        PlotAction, PlotAction),
        choice(!firstPlot && playable && reservesUsed == 0,      ExecuteEvent, ExecuteEvent),
        choice(!firstPlot && game.recruitPossible,               Recruit, Recruit),
        choice(!firstPlot,                                       Travel, Travel), // Travel must be possible or the Jihadist has lost
        choice(!firstPlot && game.jihadPossible,                 Jihad, Jihad),
        choice(!firstPlot && game.plotPossible(opsAvailable),    PlotAction, PlotAction),
        choice(!firstPlot && card.ops < 3 && inReserve < 2,      AddReserves, AddReserves),
        choice(opsAvailable < 3 && inReserve > 0,                UseReserves, UseReserves),
        choice(game.hasCountry(_.hasCadre),                      RemoveCadre, RemoveCadre),
        choice(true,                                             AbortCard, AbortCard),
      ).flatten

      println(s"\nYou have ${opsString(opsAvailable)} available and ${opsString(inReserve)} in reserve")
      askMenu(s"\n$Jihadist action: ", actions).head match {
        case UseReserves =>
          reservesUsed = inReserve
          log(s"$Jihadist player expends their reserves of ${opsString(reservesUsed)}", Color.Info)
          game = game.copy(reserves = game.reserves.copy(jihadist = 0))
          getAction()
        case RemoveCadre =>
          humanRemoveCadre()
          getAction()
        case AbortCard =>
            if (askYorN("Really abort (y/n)? "))
              throw AbortAction
            else
              getAction()
        case action => action
      }
    }

    // If the the event is US elections or is associated with the US
    // Ask if user want so resolve event or operation first
    if (card.autoTrigger || card.association == US) {
      // Allow the user to use the first plot option to cancel the event.
      // The Ruthless US bot resolve does not allow this.
      if (!game.usResolve(Ruthless)       &&
          card.association == US          &&
          game.plotPossible(1)            &&
          game.firstPlotCard.isEmpty)
        firstPlot = askYorN(s"\nDo you wish to use your First Plot option to cancel the $US event (y/n)? ")

      if (firstPlot) {
        println()
        println(separator())
        log(s"Place the $card card in the first plot box", Color.Info)
        game = game.copy(firstPlotCard = Some(card.number))
        firstPlot = true
      }
      else
        getActionOrder(card :: Nil, opponent = US) match {
          case Nil =>
            throw AbortAction
          case TriggeredEvent(c) :: _ =>
            attemptTriggeredEvent(US, c)
            opponentEventResolved = true;
          case _ => // Ops selected, fall through
        }
    }

    val action = getAction()

    action match {
      case AddReserves  => addToReserves(Jihadist, card.ops)
      case ExecuteEvent => performCardEvent(card, Jihadist)
      case Recruit      => humanRecruit(opsAvailable)
      case Travel       => humanTravel(opsAvailable)
      case Jihad        => humanJihad(opsAvailable)
      case PlotAction   => humanPlot(opsAvailable)
      case _ => throw new IllegalStateException(s"Invalid Jihadist action: $action")
    }

    // If the opponent event was not triggered before the operation
    // See if it will be triggered now.
    if (card.autoTrigger && !opponentEventResolved)
      attemptTriggeredEvent(US, card);
    else if (card.association == US && !opponentEventResolved) {
      if (firstPlot)
        log("\nFirst plot prevents the %s event \"%s\" from triggering".format(US, card.name), Color.Event)
      else
        attemptTriggeredEvent(US, card)
    }
  }

  def humanRecruit(ops: Int, ignoreFunding: Boolean = false, madrassas: Boolean = false): Unit = {
    val recruitCells = if (ignoreFunding) game.cellsAvailable else game.cellsToRecruit
    log()
    log(s"$Jihadist performs a Recruit operation with ${opsString(ops)}")
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

    val results = for ((ord, dest) <- targets; c = game.getCountry(dest)) yield {
      addOpsTarget(dest)
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
        val die     = humanDieRoll(s"Die roll for $ord Recruit in $dest: ")
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
      for ((dest, num) <- successes groupBy (dest => dest) map { case (dest, xs) => (dest -> xs.size) })
        addSleeperCellsToCountry(dest, num)
  }

  def travelIsAutomatic(src: String, dest: String): Boolean = {
    if (lapsingEventInPlay(IslamicMaghreb) && (Schengen contains dest))
      false
    else if (lapsingEventInPlay(Biometrics))
      src == dest || ((areAdjacent(src, dest) && !(game getCountry dest).isGood))
    else
      src == dest || areAdjacent(src, dest) || (game getCountry dest).isIslamistRule
  }

  def humanTravel(ops: Int): Unit = {
    log()
    log(s"$Jihadist performs a Travel operation with ${opsString(ops)}")
    log(separator())
    log(s"There are ${{amountOf(game.cellsOnMap, "cell")}} on the map")
    val maxRolls    = ops min game.cellsOnMap
    val numAttempts = askInt("How many attempts do you wish to make?", 1, maxRolls, Some(maxRolls))
    // Keep track of where cells have been selected. They can only be selected once!
    case class Source(name: String, sleepers: Int, actives: Int)
    var sourceCountries =
      (game.countries
        filter (_.cells > 0)
        map (c => c.name -> Source(c.name, c.sleeperCells, c.activeCells))
      ).toMap

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
          names = names filterNot (_ == UnitedKingdom)

        if (isTravelBanCountry(src.name))
          names = names filterNot(_ == UnitedStates)

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

  // When ever a country is targeted and is a candidate for Major Jihad
  // ask the user if they want to do Major Jihad.
  def humanJihad(ops: Int): Unit = {
    val Active  = true
    log()
    log(s"$Jihadist performs a Jihad operation with ${opsString(ops)}")
    log(separator())
    val jihadCountries = game.muslims filter (_.jihadOK)
    val maxDice = ops min (jihadCountries map (_.totalCells)).sum
    var candidates = countryNames(jihadCountries)
    // All of the jihad cells must be declared before rolling any dice.
   def getJihadTargets(diceLeft: Int, candidates: List[String]): List[JihadTarget] = {
      if (diceLeft == 0 || candidates.isEmpty)
        Nil
      else {
        if (diceLeft < maxDice)
          println(s"You have ${diceString(diceLeft)} remaining")
        val name = askCountry(s"Jihad in which country: ", candidates)
        val m    = game.getMuslim(name)
        val majorJihad = if (m.majorJihadOK(diceLeft)) askYorN(s"Conduct Major Jihad in $name (y/n)? ")
        else false
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
        target :: getJihadTargets(diceLeft - numRolls, candidates filterNot (_ == name))
      }
    }
    val targets = getJihadTargets(maxDice, candidates)
    for (t <- targets)
      addOpsTarget(t.name)
    performJihads(targets)
  }

  // Can only Plots with number <= ops or WMD Plot
  def humanPlot(ops: Int): Unit = {
    val numPlots = game.plotsAvailableWith(ops).size
    val maxCells = (game.plotTargets map (name => game.getCountry(name).totalCells)).sum
    val maxRolls = ops min maxCells
    log()
    log(s"$Jihadist performs a Plot operation with ${opsString(ops)}")
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

  def addAvailablePlotToCountry(name: String, plot: Plot, visible: Boolean = false): Unit = {
    val index = game.availablePlots.indexOf(plot)
    assert(index >= 0, s"addAvailablePlotToCountry(): $plot is not available")
    val updatedPlots = game.plotData.copy(availablePlots = game.availablePlots.take(index) ::: game.availablePlots.drop(index + 1))
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

    if (name == Philippines && countryEventInPlay(Philippines, AbuSayyaf)) {
      val p = game getNonMuslim Philippines
      if (p.totalCells >= p.totalTroops) {
        game = game.adjustPrestige(-1)
        log(s"Decrease prestige by -1 to ${game.prestige} because Abu Sayyaf is in effect", Color.MapMarker)
      }
    }
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

  def saveAdjustment(desc: String): Unit = {
    game = game.copy(plays = AdjustmentMade(desc) :: game.plays)
    saveGameState()
  }
  def saveAdjustment(country: String, desc: String): Unit = {
    game = game.copy(plays = AdjustmentMade(s"$country -> $desc") :: game.plays)
    saveGameState()
  }

  def adjustSettings(param: Option[String]): Unit = {
    val options = List(
      "prestige", "funding", "difficulty", "lapsing cards",
      "removed cards", "first plot", "markers" , "reserves",
      "plots", "offmap troops", "posture", "auto roll",
      "bot logging", "color", "resolved plot countries", "exit after win"
    ).sorted :::countryNames(game.countries).sorted
    val choice = askOneOf("[Adjust] (? for list): ", options, param, allowNone = true,
                           abbr = CountryAbbreviations, allowAbort = false)
    choice foreach {
      case "prestige" =>
        adjustInt("Prestige", game.prestige, 1 to 12) foreach { value =>
          logAdjustment("Prestige", game.prestige, value)
          game = game.copy(prestige = value)
          saveAdjustment("Prestige")
        }
      case "posture" =>
        val newValue = oppositePosture(game.usPosture)
        logAdjustment("US posture", game.usPosture, newValue)
        game = game.copy(usPosture = newValue)
        saveAdjustment("US posture")

      case "funding" =>
        adjustInt("Funding", game.funding, 1 to 9) foreach { value =>
          logAdjustment("Funding", game.funding, value)
          game = game.copy(funding = value)
          saveAdjustment("Funding")
        }
      case "offmap troops" =>
        adjustInt("Offmap troops", game.offMapTroops, 0 to (game.offMapTroops + game.troopsAvailable)) foreach { value =>
          logAdjustment("Offmap troops", game.offMapTroops, value)
          game = game.copy(offMapTroops = value)
          saveAdjustment("Offmap troops")
        }
      case "auto roll" =>
        logAdjustment("Human auto roll", game.humanAutoRoll, !game.humanAutoRoll)
        game = game.copy(humanAutoRoll = !game.humanAutoRoll)
        saveAdjustment("Human auto roll")

      case "resolved plot countries" => adjustPlotTargets()
      case "difficulty"              => adjustDifficulty()
      case "bot logging"             => adjustBotLogging()
      case "color"                   => adjustShowColor()
      case "lapsing cards"           => adjustLapsingCards()
      case "removed cards"           => adjustRemovedCards()
      case "first plot"              => adjustFirstPlot()
      case "markers"                 => adjustMarkers()
      case "reserves"                => adjustReserves()
      case "plots"                   => adjustPlots()
      case "exit after win"          => adjustExitAfterWin()
      case name                      => adjustCountry(name)
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
      askOneOf(s"$label: ", choices, allowNone = true, allowAbort = false) match {
        case None =>
        case Some(INTEGER(x)) =>
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
      logAdjustment(s"$label", game.botDifficulties.map(_.name), updated.map(_.name))
      game = game.copy(botDifficulties = updated)
      saveAdjustment("Bot difficulty")
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

  def adjustExitAfterWin(): Unit = {
    val newValue = !game.exitAfterWin
    logAdjustment("Exit after win", game.exitAfterWin, newValue)
    game = game.copy(exitAfterWin = newValue)
    saveAdjustment("Exit after win")
  }

  def adjustLapsingCards(): Unit = {
    var lapsing = game.cardsLapsing
    @tailrec def getNextResponse(): Unit = {
      val notLapsing = LapsingCards filterNot lapsing.contains
      println()
      wrap("Lapsing    : ", lapsing.sorted map cardNumAndName) foreach println
      println()
      wrap("Not Lapsing: ", notLapsing.sorted map cardNumAndName) foreach println
      println()
      println("Enter a card number to move it between lapsing and not lapsing.")
      askOneOf("Card #: ", LapsingCards, allowNone = true, allowAbort = false).map(_.toInt) match {
        case None =>
        case Some(num) if lapsing contains num =>
          lapsing = lapsing filterNot(_ == num)
          getNextResponse()
        case Some(num) =>
          lapsing = num :: lapsing
          getNextResponse()
      }
    }
    getNextResponse()
    if (lapsing.toSet != game.cardsLapsing.toSet) {
      logAdjustment("Lapsing Events", cardNumsAndNames(game.cardsLapsing), cardNumsAndNames(lapsing))
      game = game.copy(cardsLapsing = lapsing)
      saveAdjustment("Lapsing Events")
    }
  }

  def adjustRemovedCards(): Unit = {
    var outOfPlay = game.cardsRemoved
    @tailrec def getNextResponse(): Unit = {
      println()
      wrap("Removed: ", outOfPlay.sorted map cardNumAndName) foreach println
      println()
      println("Enter a card number to move it between removed and not removed.")
      askCardNumber("Card #: ", removedLapsingOK = true) match {
        case None =>
        case Some(num) if outOfPlay contains num =>
          outOfPlay = outOfPlay filterNot(_ == num)
          getNextResponse()
        case Some(num) =>
          outOfPlay = num :: outOfPlay
          getNextResponse()
      }
    }
    getNextResponse()
    if (outOfPlay.toSet != game.cardsRemoved.toSet) {
      logAdjustment("Removed Cards", cardNumsAndNames(game.cardsRemoved), cardNumsAndNames(outOfPlay))
      game = game.copy(cardsRemoved = outOfPlay)
      saveAdjustment("Removed Cards")
    }
  }

  def adjustFirstPlot(): Unit = {
    var inPlay = game.firstPlotCard
    println()
    println(s"Current first plot card: ${inPlay map cardNumAndName getOrElse "none"}")
    println()
    println("Enter a card number to add or remove it as the first plot card.")
    askCardNumber("Card #: ", removedLapsingOK = true) foreach {
      case num if inPlay.exists(_ == num) => inPlay = None
      case num                            => inPlay = Some(num)
    }

    if (inPlay != game.firstPlotCard) {
      logAdjustment("First plot", game.firstPlotCard map cardNumAndName, inPlay map cardNumAndName)
      game = game.copy(firstPlotCard = inPlay)
      saveAdjustment("First plot")
    }
  }


  def adjustMarkers(): Unit = {
    val AllMarkers = GlobalMarkers.keys.toList.sorted
    var inPlay = game.markers
    val priorGameState = game
    def available = AllMarkers filterNot inPlay.contains
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
          inPlay = inPlay filterNot (_ == name)
          getNextResponse()
        case Some(name) =>
          inPlay = name :: inPlay
          if (name == TrumpTweetsON)
            inPlay = inPlay filterNot (_ == TrumpTweetsOFF)
          else if (name == TrumpTweetsOFF)
            inPlay = inPlay filterNot (_ == TrumpTweetsON)
          getNextResponse()
      }
    }
    getNextResponse()
    inPlay = inPlay.sorted
    if (inPlay != game.markers) {
      logAdjustment("Global Markers", game.markers, inPlay)
      game = game.copy(markers = inPlay)
      saveAdjustment("Global Markers")

      logExtraCellCapacityChange(priorGameState)
    }
  }

  def adjustPlotTargets(): Unit = {
    val origTargetNames = game.plotData.resolvedTargets.map(_.name)
    var targetNames = origTargetNames

    def nextAction(): Unit = {
      val nonTargets = countryNames(game.countries) filterNot targetNames.apply
      val choices = List(
        choice(nonTargets.nonEmpty, "add",  "Add a country to the resolved plot targets"),
        choice(targetNames.nonEmpty,    "del",  "Remove a country from the resolved plot targets"),
        choice(true,                "done", "Finished")
      ).flatten
      println("\nCountries where plots were resolved in the last plot resolution phase:")
      if (targetNames.isEmpty)
        println("none")
      else
        println(targetNames.toList.sorted.mkString(", "))
      askMenu("\nChoose one: ", choices, allowAbort = false).head  match {
        case "done" =>
        case "add"  =>
          val name = askCountry("Select country to add: ", nonTargets.sorted, allowAbort = false)
          targetNames = targetNames + name
          nextAction()
        case _ =>
          val name = askCountry("Select country to remove: ", targetNames.toList.sorted, allowAbort = false)
          targetNames = targetNames - name
          nextAction()
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
    var available = game.availablePlots.toVector
    var resolved  = game.resolvedPlots.toVector
    var outOfPlay = game.removedPlots.toVector

    def showAll(): Unit = {
      println()
      println("Available  : " + plotsDisplay(available.toList))
      println("Resolved   : " + plotsDisplay(resolved.toList))
      println("Out of play: " + plotsDisplay(outOfPlay.toList))
    }

    def doMove(spec: String): Unit = {
      val src = spec take 1
      val dest = spec drop 2
      val list = (if (src == "a") available else if (src == "r") resolved else outOfPlay).toList
      val plot = askPlots(list, 1, allowAbort = false).head
      val index = list.indexOf(plot)
      src match {
        case "a" => available = available.patch(index, Vector.empty, 1)
        case "r" => resolved  = resolved.patch(index, Vector.empty, 1)
        case _   => outOfPlay = outOfPlay.patch(index, Vector.empty, 1)
      }
      dest match {
        case "a" => available = available :+ plot
        case "r" => resolved  = resolved :+ plot
        case _   => outOfPlay = outOfPlay :+ plot
      }
    }

    def addNewPlot(): Unit = {
      val choices = List(
        "1"    -> "Add new Plot 1 to the available box",
        "2"    -> "Add new Plot 2 to the available box",
        "3"    -> "Add new Plot 3 to the available box",
        "wmd"  -> "Add new WMD Plot to the available box",
        "none" -> "Do not add a new plot to the available box"
      )
      askMenu("\nChoose one: ", choices, allowAbort = false).head  match {
        case "1"   => available = available :+ Plot1
        case "2"   => available = available :+ Plot2
        case "3"   => available = available :+ Plot3
        case "wmd" => available = available :+ PlotWMD
        case _     =>
      }
    }

    def nextAction(): Unit = {
      val choices = List(
        choice(available.nonEmpty, "a-r",  "Move available plot to the resolved box"),
        choice(available.nonEmpty, "a-o",  "Move available plot to out of play"),
        choice(resolved.nonEmpty,  "r-a",  "Move resolved plot to the available box"),
        choice(resolved.nonEmpty,  "r-o",  "Move resolved plot to out of play"),
        choice(outOfPlay.nonEmpty, "o-a",  "Move out of play plot to the available box"),
        choice(outOfPlay.nonEmpty, "o-a",  "Move out of play plot to the resolved box"),
        choice(true,               "new",  "Add a new plot to the available box"),
        choice(true,               "done", "Finished")
      ).flatten

      showAll()
      askMenu("\nChoose one: ", choices, allowAbort = false).head match {
        case "done" =>
        case "new"  => addNewPlot(); nextAction()
        case spec   => doMove(spec); nextAction()
      }
    }

    if (available.isEmpty && resolved.isEmpty && outOfPlay.isEmpty)
      println("Nothing to adjust, all plots are on the map")
    else {
      nextAction()
      val availableChanged = available.toList.sorted != game.plotData.availablePlots.sorted
      val resolvedChanged = resolved.toList.sorted != game.plotData.resolvedPlots.sorted
      val outOfPlayChanged = outOfPlay.toList.sorted != game.plotData.removedPlots.sorted
      if (availableChanged || resolvedChanged || outOfPlayChanged) {
        if (availableChanged)
          logAdjustment("Available plots", plotsDisplay(game.availablePlots), plotsDisplay(available.toList))
        if (resolvedChanged)
          logAdjustment("Resolved plots", plotsDisplay(game.resolvedPlots), plotsDisplay(resolved.toList))
        if (outOfPlayChanged)
          logAdjustment("Removed plots", plotsDisplay(game.removedPlots), plotsDisplay(outOfPlay.toList))

        val updatedPlots = game.plotData.copy(
         availablePlots = available.toList,
         resolvedPlots  = resolved.toList,
         removedPlots   = outOfPlay.toList)
        game = game.copy(plotData = updatedPlots)
        saveAdjustment("Available, resolved, out of play plots")
      }
    }
  }


  def adjustCountry(name: String): Unit = {
    @tailrec def getNextResponse(): Unit = {
      println()
      println(separator())
      game.countrySummary(name) foreach println
      println()

      if (game.isMuslim(name)) {
        val flip = if (name == Iran || name == Nigeria) List("flip to non-Muslim") else Nil
        val choices = (flip ::: List(
          "alignment", "governance", "active cells", "sleeper cells", "regime change",
          "cadre", "troops", "militia", "aid", "awakening", "reaction", "wmd cache",
          "besieged regime", "civil war", "caliphate capital", "plots", "markers", "advisors"
        )).sorted
        askOneOf(s"[$name attribute] (? for list): ", choices, allowNone = true, allowAbort = false) match {
          case None        =>
          case Some(attribute) =>
            attribute match {
              case "alignment"          => adjustAlignment(name)
              case "governance"         => adjustGovernance(name)
              case "active cells"       => adjustActiveCells(name)
              case "sleeper cells"      => adjustSleeperCells(name)
              case "cadre"              => adjustCadre(name)
              case "troops"             => adjustTroops(name)
              case "militia"            => adjustMilitia(name)
              case "aid"                => adjustAid(name)
              case "awakening"          => adjustAwakening(name)
              case "reaction"           => adjustReaction(name)
              case "besieged regime"    => adjustBesiegedRegime(name)
              case "regime change"      => adjustRegimeChange(name)
              case "civil war"          => adjustCivilWar(name)
              case "caliphate capital"  => adjustCaliphateCapital(name)
              case "plots"              => adJustCountryPlots(name)
              case "markers"            => adjustCountryMarkers(name)
              case "advisors"           => adjustCountryAdvisors(name)
              case "wmd cache"          => adjustCountryWMDCache(name)
              case "flip to non-Muslim" => adjustToNonMuslim(name)
            }
            getNextResponse()
        }
      }
      else { // Nonmuslim
        val flip = if (name == Iran || name == Nigeria) List("flip to muslim") else Nil
        val choices = {
          var xs = (flip :::List("posture", "active cells", "sleeper cells",
                              "cadre", "plots", "markers", "troops")).sorted
          if (name == UnitedStates || name == Israel || name == Iran)
            xs filterNot (_ == "posture")
          else
            xs
        }
        askOneOf(s"[$name attribute] (? for list): ", choices, allowNone = true, allowAbort = false) match {
          case None        =>
          case Some(attribute) =>
            attribute match {
              case "posture"        => adjustPosture(name)
              case "active cells"   => adjustActiveCells(name)
              case "sleeper cells"  => adjustSleeperCells(name)
              case "cadre"          => adjustCadre(name)
              case "plots"          => adJustCountryPlots(name)
              case "markers"        => adjustCountryMarkers(name)
              case "flip to muslim" => adjustToMuslim(name)
              case "troops"         => adjustTroops(name) // Valid in Philippines if Abu Sayyaf
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
        askOneOf(prompt, choices, allowNone = true, allowAbort = false) foreach { newPosture =>
          logAdjustment(name, "Posture", n.posture, newPosture)
          game = game.updateCountry(n.copy(postureValue = newPosture))
          saveAdjustment(name, "Posture")
        }
    }
  }

  def adjustAlignment(name: String): Unit = {
    game.getCountry(name) match {
      case _: NonMuslimCountry => throw new IllegalArgumentException(s"Cannot set alignment of non-Muslim country: $name")
      case m: MuslimCountry if m.isUntested =>
        println(s"$name is untested. Set the governance first.")
        pause()
      case m: MuslimCountry =>
        val choices = (Ally::Neutral::Adversary::Nil) filterNot (_ == m.alignment)
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
      case _: NonMuslimCountry => throw new IllegalArgumentException(s"Cannot set governance of non-Muslim country: $name")
      case m: MuslimCountry =>
        val choices = ((GovernanceUntested::Good::Fair::Poor::IslamistRule::Nil)
                      filterNot (_ == m.governance)
                      map govToString)
        val prompt = s"New governance (${orList(choices)}): "
        askOneOf(prompt, choices, allowNone = true, allowAbort = false) map govFromString foreach { newGov =>
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
            if (updated.alignment != Adversary) {
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

  def adjustCadre(name: String): Unit = {
    val country = game.getCountry(name)
    val newValue = !country.hasCadre
    logAdjustment(name, "Cadre", country.hasCadre, newValue)
    pause()
    country match {
      case m: MuslimCountry    => game = game.updateCountry(m.copy(hasCadre = newValue))
      case n: NonMuslimCountry => game = game.updateCountry(n.copy(hasCadre = newValue))
    }
    saveAdjustment(name, "Cadre")
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
      case _: NonMuslimCountry => throw new IllegalArgumentException(s"Cannot add militia to non-Muslim country: $name")
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
      case _: NonMuslimCountry => throw new IllegalArgumentException(s"Cannot add aid to non-Muslim country: $name")
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
      case _: NonMuslimCountry => throw new IllegalArgumentException(s"Cannot add awakening markers to non-Muslim country: $name")
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
      case _: NonMuslimCountry => throw new IllegalArgumentException(s"Cannot add reaction markers to non-Muslim country: $name")
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
      case _: NonMuslimCountry => throw new IllegalArgumentException(s"Cannot add besieged regime to non-Muslim country: $name")
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
      case _: NonMuslimCountry => throw new IllegalArgumentException(s"Cannot add Regime Change to non-Muslim country: $name")
      case m: MuslimCountry if m.isGood =>
        println("Cannot add Regime Change to a country with Good governance")
        pause()
      case m: MuslimCountry if m.isIslamistRule =>
        println("Cannot add Regime Change to a country under Islamist Rule")
        pause()
      case m: MuslimCountry =>
        val choices = (NoRegimeChange::GreenRegimeChange::TanRegimeChange::Nil) filterNot (_ == m.regimeChange)
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
      case _: NonMuslimCountry => throw new IllegalArgumentException(s"Cannot add Civil War to non-Muslim country: $name")
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
        saveAdjustment(name, "Caliphate Capital")
        logExtraCellCapacityChange(priorGameState)
    }
  }

  // Move plots between country and available/resolved
  def adJustCountryPlots(name: String): Unit = {
    val c = game getCountry name
    var country   = c.plots.toVector
    var available = game.availablePlots.toVector
    var resolved  = game.resolvedPlots.toVector
    var outOfPlay = game.removedPlots.toVector

    def showAll(): Unit = {
      val fmt = "%%-%ds: ".format(11 max name.length)
      def label(s: String) = fmt.format(s)
      println()
      println(label(name)          + mapPlotsDisplay(country.toList))
      println(label("Available")   + plotsDisplay(available.toList))
      println(label("Resolved")    + plotsDisplay(resolved.toList))
      println(label("Out of play") + plotsDisplay(outOfPlay.toList))
    }

    def doMove(spec: String): Unit = {
      val src = spec take 1
      val dest = spec drop 2
      if (src == "c") {
        val onMap = askMapPlots(country.toList, 1, allowAbort = false).head
        val index = country.indexOf(onMap)
        country = country.patch(index, Vector.empty, 1)
        dest match {
          case "a" => available = available :+ onMap.plot
          case "r" => resolved  = resolved :+ onMap.plot
          case _   => outOfPlay = outOfPlay :+ onMap.plot
        }
      }
      else {
        val list = (if (src == "a") available else if (src == "r") resolved else outOfPlay).toList
        val plot = askPlots(list, 1, allowAbort = false).head
        val index = list.indexOf(plot)
        src match {
          case "a" => available = available.patch(index, Vector.empty, 1)
          case "r" => resolved  = resolved.patch(index, Vector.empty, 1)
          case _   => outOfPlay = outOfPlay.patch(index, Vector.empty, 1)
        }
        country = country :+ PlotOnMap(plot)
      }
    }

    def nextAction(): Unit = {
      val choices = List(
        choice(country.nonEmpty,   "c-a",  s"Move plot from $name to the available box"),
        choice(country.nonEmpty,   "c-r",  s"Move plot from $name to the resolved box"),
        choice(country.nonEmpty,   "c-o",  s"Move plot from $name to out of play"),
        choice(available.nonEmpty, "a-c",  s"Move available plot to $name"),
        choice(resolved.nonEmpty,  "r-c",  s"Move resolved plot to $name"),
        choice(outOfPlay.nonEmpty, "o-c",  s"Move out of play plot to $name"),
        choice(true,               "done", "Finished")
      ).flatten

      showAll()
      askMenu("\nChoose one: ", choices, allowAbort = false).head  match {
        case "done" =>
        case spec   => doMove(spec); nextAction()
      }
    }

    if (country.isEmpty && available.isEmpty && resolved.isEmpty && outOfPlay.isEmpty)
      println("Nothing to adjust, there are no plots") // Should never happen!
    else {
      nextAction()
      val countryChanged = country.toList.sorted != c.plots.sorted
      val availableChanged = available.toList.sorted != game.plotData.availablePlots.sorted
      val resolvedChanged = resolved.toList.sorted != game.plotData.resolvedPlots.sorted
      val outOfPlayChanged = outOfPlay.toList.sorted != game.plotData.removedPlots.sorted
      if (countryChanged || availableChanged || resolvedChanged || outOfPlayChanged) {
        if (countryChanged)
          logAdjustment(name, "plots", mapPlotsDisplay(c.plots), mapPlotsDisplay(country.toList))
        if (availableChanged)
          logAdjustment("Available plots", plotsDisplay(game.availablePlots), plotsDisplay(available.toList))
        if (resolvedChanged)
          logAdjustment("Resolved plots", plotsDisplay(game.resolvedPlots), plotsDisplay(resolved.toList))
        if (outOfPlayChanged)
          logAdjustment("Removed plots", plotsDisplay(game.removedPlots), plotsDisplay(outOfPlay.toList))

        val updatedPlots = game.plotData.copy(
         availablePlots = available.toList,
         resolvedPlots  = resolved.toList,
         removedPlots   = outOfPlay.toList)
        game = game.copy(plotData = updatedPlots)
        c match {
          case m: MuslimCountry    => game = game.updateCountry(m.copy(plots = country.toList))
          case n: NonMuslimCountry => game = game.updateCountry(n.copy(plots = country.toList))
        }
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
    def available = AllMarkers filterNot inPlay.contains
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
          inPlay = inPlay filterNot(_ == name)
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
    var inPlace = m.markers filter (_ == Advisors)
    val origNumAdvisors = inPlace.size
    var available = List.fill(3 - numInOtherCountries - inPlace.size)(Advisors)

    if (inPlace.isEmpty && available.isEmpty)
      println("All 3 of the Advisors markers are in other countries")
    else {
      def getNextResponse(): Unit = {
        println()
        println(s"$name now contains ${amountOf(inPlace.size, "Advisors marker")}")
        println(s"There are ${amountOf(available.size, "available Advisors marker")}")
        val choices = List(
          choice(available.nonEmpty, "place",  "Add an Advisors marker"),
          choice(inPlace.nonEmpty,   "remove", "Remove an Advisors marker"),
          choice(true,               "done",   "Finished")
        ).flatten
        askMenu("\nChoose one:", choices, allowAbort = false).head match {
          case "place" =>
            inPlace = Advisors :: inPlace
            available = available.tail
            getNextResponse()

          case "remove" =>
            available = Advisors :: available
            inPlace = inPlace.tail
            getNextResponse()

          case _ =>
        }
      }
      getNextResponse()

      if (inPlace.size != origNumAdvisors) {
        val newMarkers = inPlace ::: (m.markers filterNot (_ == Advisors))
        logAdjustment(name, "Markers", m.markers, newMarkers)
        game = game.updateCountry(m.copy(markers = newMarkers))
        saveAdjustment(name, "Markers")
      }
    }
  }

  def adjustCountryWMDCache(name: String): Unit = {
    game.getCountry(name) match {
      case n: NonMuslimCountry =>
        adjustInt("WMD cache", n.wmdCache, 0 to 3) foreach { value =>
          logAdjustment(name, "WMD cache", n.wmdCache, value)
          game = game.updateCountry(n.copy(wmdCache = value))
          saveAdjustment(name, "WMD cache")
        }
      case m: MuslimCountry =>
        adjustInt("WMD cache", m.wmdCache, 0 to 3) foreach { value =>
          logAdjustment(name, "WMD cache", m.wmdCache, value)
          game = game.updateCountry(m.copy(wmdCache = value))
          saveAdjustment(name, "WMD cache")
        }
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

