
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
  def humanDieRoll =
    if (game.humanAutoRoll) dieRoll
    else  (getOneOf("Enter die roll: ", 1 to 6, None, false) map (_.toInt)).get
  
  val INTEGER = """(\d+)""".r
  
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
  def countryAbbr(candidates: List[String]) = CountryAbbreviations filter {
    case (_, name) => candidates contains name
  }
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
    case (1, US)       => "1 hidden plot"
    case (x, US)       => s"$x hidden plots"
  }
  
  val GlobalMarkers = List(
    "Bin Ladin", "Civil War", "Facebook", "Swedish Cartoons",
    "Iran Oil Crisis", "Arab Spring", "Oil Price Spike"
  ).sorted
  
  val CountryMarkers = List(
    "NATO", "Training Camps"
  )
  
  // Used to describe event markers that represent troops.
  // prestigeLoss: if true, the marker's presence during a plot will cause loss of prestige
  case class TroopsMarker(name: String, num: Int, prestigeLoss: Boolean)
  // When removing troop markers, the Bot will choose the first one
  // based on the following sort order.
  // - markers represent smaller numbers of troops come first
  // - markers that suffer prestige loss come first??
  implicit val TroopsMarkerOrdering = new Ordering[TroopsMarker] {
    def compare(x: TroopsMarker, y: TroopsMarker) = 
      if (x.num == y.num)
        y.prestigeLoss compare x.prestigeLoss // reversed
      else
        x.num compare y.num
  }
  
  type CardEvent       = Role => Unit
  type EventConditions = Role => Boolean
  val NoConditions: EventConditions =  _ => true
  
  sealed trait CardRemoval
  case object NoRemove       extends CardRemoval
  case object Remove         extends CardRemoval
  case object USRemove       extends CardRemoval
  case object JihadistRemove extends CardRemoval
  
  sealed trait CardMark
  case object NoMark       extends CardMark
  case object Mark         extends CardMark
  case object USMark       extends CardMark
  case object JihadistMark extends CardMark
  
  sealed trait CardLapsing
  case object NoLapsing       extends CardLapsing
  case object Lapsing         extends CardLapsing
  case object USLapsing       extends CardLapsing
  case object JihadistLapsing extends CardLapsing
  
  class Card(
    val number: Int,
    val name: String,
    val association: CardAssociation,
    val ops: Int,
    val remove: CardRemoval,
    val mark: CardMark,
    val lapsing: CardLapsing,
    val eventConditions: EventConditions,
    val executeEvent: CardEvent) {
      
    def numAndName = s"#$number $name"
    override def toString() = s"#$number $name (${opsString(ops)})"
    
    def eventIsPlayable(role: Role): Boolean =
      (association == Unassociated || association == role) && eventConditions(role)
      
    def eventWillTrigger(opponentRole: Role): Boolean =
      association == opponentRole && eventConditions(opponentRole)
  }
  
  def entry(card: Card) = (card.number -> card)
  
  val Cards = Map(
    entry(new Card(121, "Advisors", US, 1,
      NoRemove, Mark, NoLapsing, NoConditions,
      (role: Role) => ()
    )),
    entry(new Card(122, "Backlash", US, 1,
      NoRemove, NoMark, NoLapsing, NoConditions,
      (role: Role) => ()
    )),
    entry(new Card(123, "Humanitarian Aid", US, 1,
      NoRemove, NoMark, NoLapsing, NoConditions,
      (role: Role) => ()
    )),
    entry(new Card(124, "Pearl Roundabout", US, 1,
      NoRemove, NoMark, NoLapsing, NoConditions,
      (role: Role) => ()
    )),
    entry(new Card(125, "Peshmerga", US, 1,
      NoRemove, NoMark, NoLapsing, NoConditions,
      (role: Role) => ()
    )),
    entry(new Card(126, "Reaper", US, 1,
      NoRemove, NoMark, NoLapsing, NoConditions,
      (role: Role) => ()
    )),
    entry(new Card(127, "Reaper", US, 1,
      NoRemove, NoMark, NoLapsing, NoConditions,
      (role: Role) => ()
    )),
    entry(new Card(128, "Reaper", US, 1,
      NoRemove, NoMark, NoLapsing, NoConditions,
      (role: Role) => ()
    )),
    entry(new Card(129, "Special Forces", US, 1,
      NoRemove, NoMark, NoLapsing, NoConditions,
      (role: Role) => ()
    )),
    entry(new Card(130, "Special Forces", US, 1,
      NoRemove, NoMark, NoLapsing, NoConditions,
      (role: Role) => ()
    )),
    entry(new Card(131, "Arab Spring \"Fallout\"", US, 2,
      NoRemove, NoMark, NoLapsing, NoConditions,
      (role: Role) => ()
    )),
    entry(new Card(132, "Battle of Sirte", US, 2,
      NoRemove, NoMark, NoLapsing, NoConditions,
      (role: Role) => ()
    )),
    entry(new Card(133, "Benghazi Falls", US, 2,
      Remove, NoMark, NoLapsing, NoConditions,
      (role: Role) => ()
    )),
    entry(new Card(134, "Civil Resistance", US, 2,
      NoRemove, NoMark, NoLapsing, NoConditions,
      (role: Role) => ()
    )),
    entry(new Card(135, "Delta / SEALS", US, 2,
      NoRemove, NoMark, NoLapsing, NoConditions,
      (role: Role) => ()
    )),
    entry(new Card(136, "Factional Infighting", US, 2,
      NoRemove, NoMark, NoLapsing, NoConditions,
      (role: Role) => ()
    )),
    entry(new Card(137, "FMS", US, 2,
      NoRemove, NoMark, NoLapsing, NoConditions,
      (role: Role) => ()  // Note: No militia allowed in Good countries!UNSCR 1973
    )),
    entry(new Card(138, "Intel Community", US, 2,
      NoRemove, NoMark, NoLapsing, NoConditions,
      (role: Role) => ()
    )),
    entry(new Card(139, "Int'l Banking Regime", US, 2,
      NoRemove, NoMark, NoLapsing, NoConditions,
      (role: Role) => ()
    )),
    entry(new Card(140, "Maersk Alabama", US, 2,
      Remove, Mark, NoLapsing, NoConditions,
      (role: Role) => ()
    )),
    entry(new Card(141, "Malala Yousafzai", US, 2,
      Remove, NoMark, NoLapsing, NoConditions,
      (role: Role) => ()
    )),
    entry(new Card(142, "Militia", US, 2,
      NoRemove, NoMark, NoLapsing, NoConditions,
      (role: Role) => ()
    )),
    entry(new Card(143, "Obama Doctrine", US, 2,
      NoRemove, NoMark, NoLapsing, NoConditions,
      (role: Role) => ()
    )),
    entry(new Card(144, "Operation New Dawn", US, 2,
      NoRemove, NoMark, NoLapsing, NoConditions,
      (role: Role) => ()
    )),
    entry(new Card(145, "Russian Aid", US, 2,
      NoRemove, NoMark, NoLapsing, NoConditions,
      (role: Role) => ()
    )),
    entry(new Card(146, "Sharia", US, 2,
      NoRemove, NoMark, NoLapsing, NoConditions,
      (role: Role) => ()
    )),
    entry(new Card(147, "Strike Eagle", US, 2,
      NoRemove, NoMark, NoLapsing, NoConditions,
      (role: Role) => ()
    )),
    entry(new Card(148, "Tahrir Square", US, 2,
      NoRemove, NoMark, NoLapsing, NoConditions,
      (role: Role) => ()
    )),
    entry(new Card(149, "UN Nation Building", US, 2,
      NoRemove, NoMark, NoLapsing, NoConditions,
      (role: Role) => ()
    )),
    entry(new Card(150, "UNSCR 1973", US, 2,
      NoRemove, Mark, NoLapsing, NoConditions,
      (role: Role) => ()
    )),
    entry(new Card(151, "UNSCR 2118", US, 2,
      Remove, NoMark, NoLapsing, NoConditions,
      (role: Role) => ()
    )),
    entry(new Card(152, "Congress Acts", US, 3,
      NoRemove, NoMark, NoLapsing, NoConditions,
      (role: Role) => ()
    )),
    entry(new Card(153, "Facebook", US, 3,
      NoRemove, NoMark, NoLapsing, _ => { game.markerInPlay("Smartphones") },
      (role: Role) => ()
    )),
    entry(new Card(154, "Facebook", US, 3,
      NoRemove, NoMark, NoLapsing, _ => { game.markerInPlay("Smartphones") },
      (role: Role) => ()
    )),
    entry(new Card(155, "Fracking", US, 3,
      NoRemove, Mark, NoLapsing, NoConditions,
      (role: Role) => ()
    )),
    entry(new Card(156, "Gulf Union", US, 3,
      NoRemove, NoMark, NoLapsing, NoConditions,
      (role: Role) => ()
    )),
    entry(new Card(157, "Limited Deployment", US, 3,
      NoRemove, NoMark, NoLapsing, NoConditions,
      (role: Role) => ()
    )),
    entry(new Card(158, "Mass Turnout", US, 3,
      NoRemove, NoMark, NoLapsing, NoConditions,
      (role: Role) => ()
    )),
    entry(new Card(159, "NATO", US, 3,
      NoRemove, Mark, NoLapsing, NoConditions,
      (role: Role) => ()
    )),
    entry(new Card(160, "Op. Neptune Spear", US, 3,
      NoRemove, NoMark, NoLapsing, NoConditions,
      (role: Role) => ()
    )),
    entry(new Card(161, "PRISM", US, 3,
      NoRemove, NoMark, NoLapsing, NoConditions,
      (role: Role) => ()
    )),
    entry(new Card(162, "SCAF", US, 3,
      NoRemove, NoMark, NoLapsing, NoConditions,
      (role: Role) => ()
    )),
    entry(new Card(163, "Status Quo", US, 3,
      NoRemove, NoMark, NoLapsing, NoConditions,
      (role: Role) => ()
    )),
    entry(new Card(164, "Bloody Thursday", Jihadist, 1,
      NoRemove, Mark, NoLapsing, NoConditions,
      (role: Role) => ()
    )),
    entry(new Card(165, "Coup", Jihadist, 1,
      NoRemove, NoMark, NoLapsing, NoConditions,
      (role: Role) => ()
    )),
    entry(new Card(166, "Ferguson", Jihadist, 1,
      NoRemove, NoMark, Lapsing, NoConditions,
      (role: Role) => ()
    )),
    entry(new Card(167, "Houthi Rebels", Jihadist, 1,
      Remove, NoMark, NoLapsing, NoConditions,
      (role: Role) => ()
    )),
    entry(new Card(168, "IEDs", Jihadist, 1,
      NoRemove, NoMark, NoLapsing, NoConditions,
      (role: Role) => ()
    )),
    entry(new Card(169, "Islamic Maghreb", Jihadist, 1,
      NoRemove, NoMark, Lapsing, NoConditions,
      (role: Role) => ()
    )),
    entry(new Card(170, "Theft of State", Jihadist, 1,
      NoRemove, NoMark, NoLapsing, NoConditions,
      (role: Role) => ()
    )),
    entry(new Card(171, "Abu Ghraib Jail Break", Jihadist, 2,
      Remove, NoMark, NoLapsing, NoConditions,
      (role: Role) => ()
    )),
    entry(new Card(172, "Al-Shabaab", Jihadist, 2,
      NoRemove, NoMark, NoLapsing, NoConditions,
      (role: Role) => ()
    )),
    entry(new Card(173, "Arab Winter", Jihadist, 2,
      NoRemove, NoMark, Lapsing, NoConditions,
      (role: Role) => ()
    )),
    entry(new Card(174, "Boston Marathon", Jihadist, 2,
      Remove, NoMark, NoLapsing, NoConditions,
      (role: Role) => ()
    )),
    entry(new Card(175, "Censorship", Jihadist, 2,
      NoRemove, Mark, NoLapsing, NoConditions,
      (role: Role) => ()
    )),
    entry(new Card(176, "Change of State", Jihadist, 2,
      NoRemove, NoMark, NoLapsing, NoConditions,
      (role: Role) => ()
    )),
    entry(new Card(177, "Gaza Rockets", Jihadist, 2,
      NoRemove, NoMark, NoLapsing, NoConditions,
      (role: Role) => ()
    )),
    entry(new Card(178, "Ghost Soldiers", Jihadist, 2,
      NoRemove, NoMark, NoLapsing, NoConditions,
      (role: Role) => ()
    )),
    entry(new Card(179, "Korean Crisis", Jihadist, 2,
      NoRemove, NoMark, Lapsing, NoConditions,
      (role: Role) => ()
    )),
    entry(new Card(180, "Mosul Central Bank", Jihadist, 2,
      Remove, NoMark, NoLapsing, NoConditions,
      (role: Role) => ()
    )),
    entry(new Card(181, "NPT Safeguards Ignored", Jihadist, 2,
      Remove, NoMark, NoLapsing, NoConditions,  // Note: Only remove on die roll of 1-3
      (role: Role) => ()
    )),
    entry(new Card(182, "Paris Attacks", Jihadist, 2,
      NoRemove, NoMark, NoLapsing, NoConditions,
      (role: Role) => ()
    )),
    entry(new Card(183, "Pirates", Jihadist, 2,
      Remove, Mark, NoLapsing, NoConditions,
      (role: Role) => ()
    )),
    entry(new Card(184, "Sequestration", Jihadist, 2,
      Remove, Mark, NoLapsing, NoConditions,
      (role: Role) => ()
    )),
    entry(new Card(185, "al-Maliki", Jihadist, 3,
      Remove, NoMark, NoLapsing, NoConditions,
      (role: Role) => ()
    )),
    entry(new Card(186, "Boko Haram", Jihadist, 3,
      NoRemove, NoMark, NoLapsing, NoConditions,
      (role: Role) => ()
    )),
    entry(new Card(187, "Foreign Fighters", Jihadist, 3,
      NoRemove, NoMark, NoLapsing, NoConditions,
      (role: Role) => ()
    )),
    entry(new Card(188, "ISIL", Jihadist, 3,
      Remove, NoMark, NoLapsing, NoConditions,
      (role: Role) => ()
    )),
    entry(new Card(189, "Jihadist Videos", Jihadist, 3,
      NoRemove, NoMark, NoLapsing, NoConditions,
      (role: Role) => ()
    )),
    entry(new Card(190, "Martyrdom Operation", Jihadist, 3,
      NoRemove, NoMark, NoLapsing, NoConditions,
      (role: Role) => ()
    )),
    entry(new Card(191, "Muslim Brotherhood", Jihadist, 3,
      Remove, NoMark, NoLapsing, NoConditions,
      (role: Role) => ()
    )),
    entry(new Card(192, "Quagmire", Jihadist, 3,
      NoRemove, NoMark, NoLapsing, NoConditions,
      (role: Role) => ()
    )),
    entry(new Card(193, "Regional al-Qaeda", Jihadist, 3,
      NoRemove, NoMark, NoLapsing, NoConditions,
      (role: Role) => ()
    )),
    entry(new Card(194, "Snowden", Jihadist, 3,
      Remove, NoMark, NoLapsing, NoConditions,
      (role: Role) => ()
    )),
    entry(new Card(195, "Taliban Resurgent", Jihadist, 3,
      NoRemove, NoMark, NoLapsing, NoConditions,
      (role: Role) => ()
    )),
    entry(new Card(196, "Training Camps", Jihadist, 3,
      NoRemove, Mark, NoLapsing, NoConditions,
      (role: Role) => ()
    )),
    entry(new Card(197, "Unconfirmed", Jihadist, 3,
      Remove, NoMark, NoLapsing, NoConditions,
      (role: Role) => ()
    )),
    entry(new Card(198, "US Atrocities", Jihadist, 3,
      NoRemove, NoMark, NoLapsing, NoConditions,
      (role: Role) => ()
    )),
    entry(new Card(199, "US Consulate Attacked", Jihadist, 3,
      NoRemove, NoMark, Lapsing, NoConditions,
      (role: Role) => ()
    )),
    entry(new Card(200, "Critical Middle", Unassociated, 1,
      NoRemove, NoMark, NoLapsing, NoConditions,
      (role: Role) => ()
    )),
    entry(new Card(201, "Cross Border Support", Unassociated, 1,
      NoRemove, NoMark, NoLapsing, NoConditions,
      (role: Role) => ()
    )),
    entry(new Card(202, "Cyber Warfare", Unassociated, 1,
      NoRemove, NoMark, NoLapsing, NoConditions,
      (role: Role) => ()
    )),
    entry(new Card(203, "Day of Rage", Unassociated, 1,
      Remove, NoMark, NoLapsing, NoConditions,
      (role: Role) => ()
    )),
    entry(new Card(205, "Erdoğan Effect", Unassociated, 1,
      NoRemove, NoMark, NoLapsing, NoConditions,
      (role: Role) => ()
    )),
    entry(new Card(204, "Ebola Scare", Unassociated, 1,
      Remove, NoMark, USLapsing, NoConditions,
      (role: Role) => ()
    )),
    entry(new Card(206, "Friday of Anger", Unassociated, 1,
      NoRemove, NoMark, NoLapsing, NoConditions,
      (role: Role) => ()
    )),
    entry(new Card(207, "JV / Copycat", Unassociated, 1,
      NoRemove, NoMark, NoLapsing, NoConditions,
      (role: Role) => ()
    )),
    entry(new Card(208, "Kinder – Gentler", Unassociated, 1,
      NoRemove, NoMark, NoLapsing, NoConditions,
      (role: Role) => ()
    )),
    entry(new Card(209, "Quds Force", Unassociated, 1,
      NoRemove, NoMark, NoLapsing, NoConditions,
      (role: Role) => ()
    )),
    entry(new Card(210, "Sectarian Violence", Unassociated, 1,
      NoRemove, NoMark, NoLapsing, NoConditions,
      (role: Role) => ()
    )),
    entry(new Card(211, "Smartphones", Unassociated, 1,
      NoRemove, Mark, NoLapsing, NoConditions,
      (role: Role) => ()
    )),
    entry(new Card(212, "Smartphones", Unassociated, 1,
      NoRemove, Mark, NoLapsing, NoConditions,
      (role: Role) => ()
    )),
    entry(new Card(213, "Smartphones", Unassociated, 1,
      NoRemove, Mark, NoLapsing, NoConditions,
      (role: Role) => ()
    )),
    entry(new Card(214, "3 Cups of Tea", Unassociated, 2,
      NoRemove, Mark, NoLapsing, NoConditions,
      (role: Role) => ()
    )),
    entry(new Card(215, "Abu Bakr al-Baghdadi", Unassociated, 2,
      USRemove, NoMark, NoLapsing, NoConditions,
      (role: Role) => ()
    )),
    entry(new Card(216, "Abu Sayyaf (ISIL)", Unassociated, 2,
      USRemove, NoMark, NoLapsing, NoConditions,
      (role: Role) => ()
    )),
    entry(new Card(217, "Agitators", Unassociated, 2,
      NoRemove, NoMark, NoLapsing, NoConditions,
      (role: Role) => ()
    )),
    entry(new Card(218, "Al-Nusra Front", Unassociated, 2,
      NoRemove, NoMark, NoLapsing, NoConditions,
      (role: Role) => ()
    )),
    entry(new Card(219, "Ayman al-Zawahiri", Unassociated, 2,
      USRemove, NoMark, NoLapsing, NoConditions,
      (role: Role) => ()
    )),
    entry(new Card(220, "Daraa", Unassociated, 2,
      Remove, NoMark, NoLapsing, NoConditions,
      (role: Role) => ()
    )),
    entry(new Card(221, "FlyPaper", Unassociated, 2,
      NoRemove, NoMark, NoLapsing, NoConditions,
      (role: Role) => ()
    )),
    entry(new Card(222, "Hagel", Unassociated, 2,
      Remove, NoMark, NoLapsing, NoConditions,
      (role: Role) => ()
    )),
    entry(new Card(223, "Iranian Elections", Unassociated, 2,
      Remove, NoMark, NoLapsing, NoConditions,
      (role: Role) => ()
    )),
    entry(new Card(224, "Je Suis Charlie", Unassociated, 2,
      NoRemove, NoMark, NoLapsing, NoConditions,
      (role: Role) => ()
    )),
    entry(new Card(225, "Jihadi John", Unassociated, 2,
      USRemove, NoMark, NoLapsing, NoConditions,
      (role: Role) => ()
    )),
    entry(new Card(226, "Operation Serval", Unassociated, 2,
      NoRemove, USMark, NoLapsing, NoConditions,
      (role: Role) => ()
    )),
    entry(new Card(227, "Popular Support", Unassociated, 2,
      NoRemove, NoMark, NoLapsing, NoConditions,
      (role: Role) => ()
    )),
    entry(new Card(228, "Popular Support", Unassociated, 2,
      NoRemove, NoMark, NoLapsing, NoConditions,
      (role: Role) => ()
    )),
    entry(new Card(229, "Prisoner Exchange", Unassociated, 2,
      NoRemove, NoMark, NoLapsing, NoConditions,
      (role: Role) => ()
    )),
    entry(new Card(230, "Sellout", Unassociated, 2,
      NoRemove, NoMark, NoLapsing, NoConditions,
      (role: Role) => ()
    )),
    entry(new Card(231, "Siege of \"Kobanigrad\"", Unassociated, 2,
      NoRemove, NoMark, NoLapsing, NoConditions,
      (role: Role) => ()
    )),
    entry(new Card(232, "Trade Embargo", Unassociated, 2,
      USRemove, Mark, NoLapsing, NoConditions,
      (role: Role) => ()
    )),
    entry(new Card(233, "UN Ceasefire", Unassociated, 2,
      NoRemove, NoMark, NoLapsing, NoConditions,
      (role: Role) => ()
    )),
    entry(new Card(234, "Free Syrian Army", Unassociated, 3,
      Remove, NoMark, NoLapsing, NoConditions,
      (role: Role) => ()
    )),
    entry(new Card(235, "Qadhafi", Unassociated, 3,
      NoRemove, NoMark, NoLapsing, NoConditions,
      (role: Role) => ()
    )),
    entry(new Card(236, "Oil Price Spike", Unassociated, 3,
      NoRemove, NoMark, Lapsing, NoConditions,
      (role: Role) => ()
    )),
    entry(new Card(237, "Osama bin Ladin", Unassociated, 3,
      USRemove, NoMark, NoLapsing, NoConditions,
      (role: Role) => ()
    )),
    entry(new Card(238, "Revolution", Unassociated, 3,
      NoRemove, NoMark, NoLapsing, NoConditions,
      (role: Role) => ()
    )),
    entry(new Card(239, "Truce", Unassociated, 3,
      NoRemove, Mark, Lapsing, NoConditions,
      (role: Role) => ()
    )),
    entry(new Card(240, "US Election", Unassociated, 3,
      NoRemove, NoMark, NoLapsing, NoConditions,
      (role: Role) => ()
    ))
  )
  
  def cardNumbers = Cards.keys.toList
  val lapsingCardNumbers = (Cards.valuesIterator filter (_.lapsing != NoLapsing) map (_.number)).toList.sorted
  val removableCardNumbers = (Cards.valuesIterator filter (_.remove != NoRemove) map (_.number)).toList.sorted
  def cardNumAndName(number: Int): String = Cards(number).numAndName
  def cardNumsAndNames(xs: List[Int]): String = xs.sorted map cardNumAndName mkString ", "
  
  sealed trait Country {
    val name: String
    val governance: Int
    val sleeperCells: Int
    val activeCells: Int
    val hasCadre: Boolean
    val plots: List[Int]     // 1, 2, 3, 4 == WMD
    val markers: List[String]
    val wmdCache: Int        // Number of WMD plots cached
    
    def isUntested: Boolean
    def isGood         = governance == Good
    def isFair         = governance == Fair
    def isPoor         = governance == Poor
    def isIslamistRule = governance == IslamistRule
    
    def totalCells = sleeperCells + activeCells
    def hasMarker(name: String) = markers contains name
    
    def canAutoRecruit: Boolean
    def hasPlots = plots.nonEmpty
    def warOfIdeasOK(ops: Int): Boolean
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
    override def isUntested = posture == PostureUntested
    override def canAutoRecruit = false
    def isSchengen = Schengen contains name
    def isHard = posture == Hard
    def isSoft = posture == Soft
    override def warOfIdeasOK(ops: Int) = !(iranSpecialCase || name == UnitedStates || name == Israel)
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
    override def isUntested = governance == GovernanceUntested
    def isAlly      = alignment == Ally
    def isNeutral   = alignment == Neutral
    def isAdversary = alignment == Adversary
    
    def inRegimeChange = regimeChange != NoRegimeChange
    
    def isUntestedWithData: Boolean = isUntested && (
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
    
    // If a muslim country is untest, then it is valid a WoI target.
    override def warOfIdeasOK(ops: Int) = 
      (isUntested || ops >= governance) &&
      !(isAdversary || isGood || (inRegimeChange && (troops + militia - totalCells) < 5))
          
    override def canAutoRecruit = isIslamistRule || civilWar || inRegimeChange
    
    // TODO: Add other markers!!
    // The list is sorted so that markers repesent in 
    def troopsMarkers: List[TroopsMarker] = markers collect {
      case "NATO"       => TroopsMarker("NATO", 2, true)
      case "UNSCR 1973" => TroopsMarker("UNSCR 1973", 1, false)
    }
    
    def markerTroops: Int = troopsMarkers.foldLeft(0) { (total, tm) => total + tm.num }
    def totalTroops = troops + markerTroops
    def totalTroopsAndMilitia = totalTroops + militia // Used to calculate hit for attrition
    
    def canTakeAwakeningOrReactionMarker = !(isGood || isIslamistRule || civilWar)
    def caliphateCandidate = civilWar || isIslamistRule || inRegimeChange

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
      assert(!isIslamistRule, s"worsenGovernance() called on Islamist Rule country - ${name}")
      if (governance == Good)
        copy(governance = Fair, aidMarkers = (aidMarkers - 1) max 0)
      else if (governance == Fair)
        copy(governance = Poor, aidMarkers = (aidMarkers - 1) max 0, reaction = (reaction + 1) min 0)
      else  // governance == Poor
        copy(governance = IslamistRule, alignment = Adversary, awakening = 0, reaction = 0, 
             aidMarkers = 0, militia = 0, regimeChange = NoRegimeChange, besiegedRegime = false, 
             civilWar = false)
    }
    
    def canDeployTo(ops: Int) = alignment == Ally && ops >= governance
    def maxDeployFrom = if (inRegimeChange)
      if (troops - totalCells >= 5) troops - totalCells else 0
    else
      troops
    def canDeployFrom = maxDeployFrom > 0
      
    
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
  
  case class PlayedCard(role:Role, card: Card, event: Boolean, card2: Option[Card] = None) {
    override def toString() = {
      card2 match {
        case Some(c2) => 
          s"$role plays ${cardNumAndName(card.number)} and ${cardNumAndName(c2.number)} for reassessment"
        case None =>
          val action = if (event) "the event" else s"${opsString(card.ops)}"
          s"$role plays ${cardNumAndName(card.number)} for $action"
      }
    }
  }
  
  // There is a limit of 22 construction arguments for case classes
  // To work around this in the GameState, we will combine a couple of parameters
  case class CampCells(inCamp: Int, onMap: Int)
  case class Reserves(us: Int, jihadist: Int)
  
  case class GameState(
    scenarioName: String,
    humanRole: Role,
    humanAutoRoll: Boolean,
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
    reserves: Reserves = Reserves(0, 0),
    trainingCampCells: CampCells = CampCells(0, 0),
    oilPriceSpikes: Int = 0,
    resolvedPlots: List[Int] = Nil,
    cardsPlayed: List[PlayedCard] = Nil,   // Cards played during current turn (most recent first).
    firstPlotCard: Option[Int] = None,  // Card number
    cardsLapsing: List[Int] = Nil,       // Card numbers
    cardsRemoved: List[Int] = Nil   // Cards removed from the game.
  ) {
    
    def botRole = if (humanRole == US) Jihadist else US
      
    def markerInPlay(name: String) = markers contains name
    
    def cardPlaySummary: Seq[String] = {
      val b = new ListBuffer[String]
      b += s"Cards played this turn"
      b += separator()
      if (cardsPlayed.isEmpty)
        b += "none"
      else
        b ++= cardsPlayed.reverse map (_.toString)
      b.toList
    }
    
    def scenarioSummary: Seq[String] = {
      val b = new ListBuffer[String]
      b += s"Scenario: $scenarioName"
      b += separator()
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
      b += f"US posture     : $usPosture | World posture     : ${worldPostureDisplay}"
      b += f"US prestige    : $prestige%2d   | Jihadist funding  : $funding%2d"
      b += f"US reserves    : ${reserves.us}%2d   | Jihadist reserves : ${reserves.jihadist}%2d"
      b += f"Troops on track: $troopsAvailable%2d   | Troops off map    : $offMapTroops%2d"
      b += f"Cells on track : $cellsOnTrack%2d   | Militia on track  : $militiaAvailable%2d"
      b += f"Cells in camp  : ${trainingCampCells.inCamp}%2d   | Camp cells on map: ${trainingCampCells.onMap}%2d" 
      if (trainingCampsInPlay && trainingCampCapacity == 5)
      b += s"Training camp  : In Caliphate country (capacity $trainingCampCapacity)"
      if (trainingCampsInPlay && trainingCampCapacity == 5)
      b += s"Training camp  : In non-Caliphate country (capacity $trainingCampCapacity)"
      else
      b += s"Training camp  : Not in play"
      b += s"Markers        : ${if (markers.isEmpty) "none" else markers mkString ", "}"
      b += s"Lapsing        : ${if (cardsLapsing.isEmpty) "none" else cardNumsAndNames(cardsLapsing)}"
      b += s"1st plot       : ${firstPlotCard map cardNumAndName getOrElse "none"}"
      b += s"Resloved plots : ${plotsDisplay(resolvedPlots, Jihadist)}"
      b += s"Available plots: ${plotsDisplay(availablePlots, humanRole)}"
      if (activePlots.isEmpty)
        b += s"Active plots   : none"
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
      def item(num: Int, label: String, pluralLabel: Option[String] = None): Unit = {
        if (showAll || num > 0)
          items += amountOf(num, label, pluralLabel)
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
          val gov = if (m.isUntested) "Untested" else s"${govToString(m.governance)} ${m.alignment}"
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
          item(m.militia, "Militia", Some("Militia"))
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
      val civilWars = (muslims filter (_.civilWar)).toList
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
    def muslims    = countries filter (_.isInstanceOf[MuslimCountry]) map (_.asInstanceOf[MuslimCountry])
    def nonMuslims = countries filter (_.isInstanceOf[NonMuslimCountry]) map (_.asInstanceOf[NonMuslimCountry])
    
    def isMuslim(name: String)    = muslims exists (_.name == name)
    def isNonMuslim(name: String) = nonMuslims exists (_.name == name)
    
    // The methods assume a valid name and will throw an exception if an invalid name is used!
    def getCountry(name: String)   = (countries find (_.name == name)).get
    def getMuslim(name: String)    = (muslims find (_.name == name)).get
    def getNonMuslim(name: String) = (nonMuslims find (_.name == name)).get
    
    def getCountries(names: List[String]):  List[Country]          = names map getCountry
    def getMuslims(names: List[String]):    List[MuslimCountry]    = names map getMuslim
    def getNonMuslims(names: List[String]): List[NonMuslimCountry] = names map getNonMuslim
    
    def adjacentCountries(name: String)   = getCountries(getAdjacent(name))
    def adjacentMuslims(name: String)     = getMuslims(getAdjacent(name) filter isMuslim)
    def adjacentNonMuslims(name: String)  = getNonMuslims(getAdjacent(name) filter isNonMuslim)
  
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
    
    def setCaliphateCapital(name: String): GameState = {
      assert(isMuslim(name), s"setCaliphateCapital() called on non-muslim country: $name")
      val capital = getMuslim(name);
      assert(capital.caliphateCandidate, s"setCaliphateCapital() called on invalid country: $name")
      
      // First make sure there is no country marked as the capital
      val clearedState = updateCountries(muslims.map(_.copy(caliphateCapital = false)))
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
      case m: MuslimCountry    => m.isUntested || m.isNeutral || (m.isAlly && !m.isGood)
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
      val value = (nonMuslims.filterNot(_.name == UnitedStates).foldLeft(0) { 
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
    
    def troopsAvailable  = 15 - offMapTroops - muslims.foldLeft(0) { (a, c) => a + c.troops }
    def militiaAvailable = 15 - muslims.foldLeft(0) { (a, c) => a + c.militia }
    
    // If the "Training Camps" marker is in a country we add 3 extra cells available cells.
    // (Only useful when funding is as 9 or by event or civil war attrition)
    // If some of those cells are on the map and the training camp is removed, those
    // extra cells remain on the map until eliminated.
    // We use the trainingCampCells field to keep track of the training camp status:
    // inCamp -- the number of training camp cells currently in camp.
    //           when the mark is placed this will be set to 3 (or 5)
    // onMap  -- the number of training camp cell on the map.
    //           It is adjusted as cells are added to or removed from the map.
    // Training camp cells are always the last to be placed on the map and the first
    // to be removed.
    
    def trainingCamp        = muslims find (_.hasMarker("Training Camps")) map (_.name)
    def trainingCampsInPlay = trainingCamp.nonEmpty
    def trainingCampCapacity= trainingCamp map (tc => if (isCaliphateMember(tc)) 5 else 3) getOrElse 0 
    def totalCellsInPlay    = 15 + trainingCampCells.inCamp + trainingCampCells.onMap
    def cellsOnMap          = countries.foldLeft(0) { (a, c) => a + c.totalCells }
    def cellsOnTrack        = (15 - cellsOnMap) max 0  // Don't allow it to go negative due to camp cells
    // Number of cells available for operations
    def cellsAvailable(ignoreFunding: Boolean = false) = {
      if (ignoreFunding || (trainingCampsInPlay && funding == 9))
        trainingCampCells.inCamp + cellsOnTrack
      else
        fundingLevel match {
          case Tight    => (cellsOnTrack - 10) max 0
          case Moderate => (cellsOnTrack -  5) max 0
          case _        => cellsOnTrack
        }
    }
    def numGoodOrFair    = muslims.filter(c => c.isGood || c.isFair).size
    def numPoorOrIslamic = muslims.filter(c => c.isPoor || c.isIslamistRule).size
    def numIslamic       = muslims.filter(c => c.isIslamistRule).size
    def oilBump(c: MuslimCountry) = if (c.oilProducer) oilPriceSpikes else 0
    def goodResources =
      muslims.filter(_.isGood).foldLeft(0) { (a, c) => a + c.resources + oilBump(c) }
    def islamistResources = 
      muslims.filter(_.isIslamistRule).foldLeft(0) { (a, c) => a + c.resources + oilBump(c)} +
      (if (caliphateDeclared) 1 else 0)
    // Return true if any two Islamist Rule countries are adjacent.
    def islamistAdjacency: Boolean =
      muslims.filter(_.isIslamistRule).combinations(2).exists (xs => areAdjacent(xs.head.name, xs.last.name))
    
    // Remember, troops can always deploy to the track with a 1 op card.
    def deployPossible(ops: Int): Boolean =
      (troopsAvailable > 0 && muslims.exists(_.canDeployTo(ops))) || muslims.exists(_.canDeployFrom)
  
    def deployFromTargets: List[String] = {
      val ms = (muslims filter (_.canDeployFrom) map (_.name)).sorted
      if (troopsAvailable > 0) "track" :: ms else ms
    }
    
    def deployToTargets(ops: Int): List[String] = "track" :: (muslims filter (_.canDeployTo(ops)) map (_.name)).sorted
    
    def regimeChangeTargets(ops: Int): List[String] = 
      if (ops == 3 && usPosture == Hard && (troopsAvailable > 5 || muslims.exists(_.maxDeployFrom > 5)))
        (muslims filter (_.isIslamistRule) map (_.name)).sorted
      else 
        Nil
  
    def withdrawTargets(ops: Int): List[String] = 
      if (ops == 3 && usPosture == Soft)
        (muslims.filter(m => m.inRegimeChange && m.troops > 0) map (_.name)).sorted
      else
        Nil

    def disruptAffectsPrestige(name: String): Boolean = getCountry(name) match {
      case m: MuslimCountry    => (m.troops + m.militia) > 1 && m.troops > 0
      case _: NonMuslimCountry => false
    }
    // Returns the number of cells that may be disrupted in the given country (1 or 2)
    // Returns the losses if this country is the target of a disrupt operation
    def disruptLosses(name: String): Either[Int, Boolean] = {
      val c = getCountry(name) 
      val numLosses = c match {
        case m: MuslimCountry =>
          if ((m.troops + m.militia) > 1 && (m.troops > 0 || m.hasMarker("Advisors"))) 2
          else 1
        case n: NonMuslimCountry => if (n.isHard) 2 else 1
      }
      
      if (c.totalCells > 0)
        Left(numLosses min c.totalCells)
      else if (c.hasCadre)
        Right(true)
      else
        throw new IllegalStateException(s"$name has no cells or cadre to disrupt!")
    } 
    
    def disruptTargets(ops: Int): List[String] = {
      val muslimTargets = muslims.filter { m =>
        ops >= m.governance &&
        (m.hasCadre || m.totalCells > 0) && 
        (m.isAlly || (m.troops + m.militia) > 1)
      }
      val nonMuslimTargets = nonMuslims.filter { n =>
        !n.iranSpecialCase &&
        ops >= n.governance &&
        (n.hasCadre || n.totalCells > 0) 
      }
      ((muslimTargets ::: nonMuslimTargets) map (_.name)).sorted 
    }

    def alertTargets(ops: Int): List[String] = 
      if (ops == 3)
        (countries filter (_.plots.nonEmpty) map (_.name)).sorted
      else
        Nil
    
    def warOfIdeasTargets(ops: Int): List[String] =
      (countries filter (_.warOfIdeasOK(ops)) map (_.name)).sorted
    
  }
  
  def initialGameState(
    scenario: Scenario,
    humanRole: Role,
    humanAutoRoll: Boolean,
    botDifficulties: List[BotDifficulty]) = GameState(
      scenario.name,
      humanRole,
      humanAutoRoll,
      botDifficulties,
      0, // Turn number, zero indicates start of game.
      scenario.prestige,
      scenario.usPosture,
      scenario.funding,
      scenario.countries,
      scenario.markers.sorted)
  
  
  // Global variables
  var game = initialGameState(new Awakening2010, US, true, Muddled :: Nil)
  var previousState: Option[GameState] = None // Used to undo the last command
  
  // This history of game turns, most recent first (ie in reverse order.)
  // The current game state is NOT in the list.  It is added at the
  // end of the turn.
  // Allows user to roll back to a previous turn.
  var gameTurns = List.empty[GameState]
  
  
  // If num is 1 use the name as is
  // otherwise either use the plural if given or add an 's' to the name.
  def amountOf(num: Int, name: String, plural: Option[String] = None) = 
    (num, plural) match {
      case (1, _)            => s"$num $name"
      case (_, Some(plural)) => s"$num $plural"
      case _                 => s"$num ${name}s"
    }
    
  def opsString(num: Int) = amountOf(num, "Op")
  
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
      println(s"Enter one of:\n${orList(displayList)}")
      None
    }
    else
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

  def getCardNumber(prompt: String, 
                    initial: Option[String] = None,
                    allowNone: Boolean = true,
                    only3Ops: Boolean = false): Option[Int] = {
    def checkNumber(input: String): Boolean = input match {
      case INTEGER(num) if cardNumbers contains num.toInt =>
        if (only3Ops && Cards(num.toInt).ops != 3) {
          println("You must enter a 3 Ops cards")
          false
        }
        else
          true
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
    testResponse(initial)
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
    if (country.isUntested) {
      country match {
        case m: MuslimCountry    =>
          val newGov = if (dieRoll < 5) Poor else Fair
          game = game.updateCountry(m.copy(governance = newGov))
          log()
          log(s"${m.name} tested: ${govToString(newGov)} Neutral")
          
        case n: NonMuslimCountry =>
          val newPosture = if (dieRoll < 5) Soft else Hard
          game = game.updateCountry(n.copy(posture = newPosture))
          log()
          log(s"${n.name} tested: $newPosture")
      }
    }
  }
  
  
  
  def modifyWoiRoll(die: Int, m: MuslimCountry, ignoreGwotPenalty: Boolean = false, silent: Boolean = false): Int = {
    def logNotZero(value: Int, msg: String): Unit =
      if (!silent && value != 0) log(f"$value%+2d: $msg")
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
  
  
  def warOfIdeas(name: String, die: Int): Unit = {
    game.getCountry(name) match {
      case m: MuslimCountry =>
        assert(!m.isAdversary, "Cannot do War of Ideas in Adversary country")
        assert(!m.isGood, "Cannot do War of Ideas in muslim country with Good governance")
        assert(!(m.inRegimeChange && (m.troops + m.militia - m.totalCells) < 5),
                 "Cannot do War of Ideas in regime change country, not enought troops + militia")
        log()
        log(s"US performs War of Ideas in ${m.name}")
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
        if (newPosture == n.posture)
          log(s"Posture of ${n.name} stays $newPosture")
        else
          log(s"Posture of ${n.name} changes to $newPosture")
        if (newPosture == game.usPosture && game.prestige < 12) {
          game = game.adjustPrestige(1)
          log(s"Matches US posture, so increase US prestige by one to ${game.prestige}")
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
    val candidates = game.muslims filter (m => (m.awakening + m.reaction).abs > 1)
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
              if (worsened.isIslamistRule)
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
      var troopMarkersLost = for (TroopsMarker(name, num, _) <- m.troopsMarkers; if hitsRemaining > 0) 
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
      
      removeActiveCellsFromCountry(m.name, activeLost, true, s"${m.name}: Jihadist attrition - ")
      removeSleeperCellsFromCountry(m.name, sleepersLost, true, s"${m.name}: Jihadist attrition - ")
      hitsRemaining    
    }
  }
  
  def civilWarAttrition(): Unit = {
    val civilWars = game.muslims filter (m => (m.civilWar))
    log()
    log("Civil War Attrition")
    log(separator())
    if (civilWars.isEmpty)
      log("No countries in civil war")
    else {
      val caliphateCapital = civilWars find (_.caliphateCapital) map (_.name)
      // First if an "Advisors" marker is present in any of the countries, then 
      // add one militia to that country.
      for (m <- civilWars filter (_.hasMarker("Advisors")) if game.militiaAvailable > 0) {
        log("Add one militia to ${m.name} due to presence of Advisors")
        game = game.updateCountry(m.copy(militia = m.militia + 1))
      } 
      
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
                if (worsened.isIslamistRule)
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
  
  def addActiveCellsToCountry(name: String, num: Int, ignoreFunding: Boolean, logPrefix: String = "") =
    addCellsToCountry(name, true, num, ignoreFunding, logPrefix)
  
  def addSleeperCellsToCountry(name: String, num: Int, ignoreFunding: Boolean, logPrefix: String = "") =
    addCellsToCountry(name, false, num, ignoreFunding, logPrefix)
  
  // Move cells from the track (or training camp) to a country on the map.
  // Caller should ensure there are enough available cells to satisfy the move.
  // otherwise the function will throw an exception!
  def addCellsToCountry(name: String, active: Boolean, num: Int, ignoreFunding: Boolean, logPrefix: String = ""): Unit = {
    if (num > 0) {
      val cellType = if (active) "active cell" else "sleeper cell"
      val available = game.cellsAvailable(ignoreFunding)
      assert(available >= num, s"not enough available cells have: $available, need $num")
      val fromCamp  = game.trainingCampCells.inCamp min num
      val fromTrack = num - fromCamp
      (fromCamp, fromTrack) match {
        case (0, trk)   => log("%sAdd %s from the track to %s".format(logPrefix, amountOf(trk, cellType), name))
        case (cmp, 0)   => log("%sAdd %s from the training camp to %s.".format(logPrefix, amountOf(cmp, cellType), name))
        case (cmp, trk) => log("%sAdd %s to %s. %d from the track and %d from the training camp.".format(
                                logPrefix, amountOf(num, cellType), name, trk, cmp))
      }
      if (game.getCountry(name).hasCadre)
        log("%sRemove cadre marker from %s.".format(logPrefix, name))
      // The number on the track is calculated, so it does not need to be set here.
      game = game.copy(trainingCampCells = CampCells(game.trainingCampCells.inCamp - fromCamp,
                                                     game.trainingCampCells.onMap  + fromCamp))
      game.getCountry(name) match {
        case m: MuslimCountry if active    => game = game.updateCountry(m.copy(activeCells  = m.activeCells  + num, hasCadre = false))
        case m: MuslimCountry              => game = game.updateCountry(m.copy(sleeperCells = m.sleeperCells + num, hasCadre = false))
        case n: NonMuslimCountry if active => game = game.updateCountry(n.copy(activeCells  = n.activeCells  + num, hasCadre = false))
        case n: NonMuslimCountry           => game = game.updateCountry(n.copy(sleeperCells = n.sleeperCells + num, hasCadre = false))
      }
    }
  }
  
  def removeActiveCellsFromCountry(name: String, num: Int, adjustCadre: Boolean = true, logPrefix: String = "") =
    removeCellsFromCountry(name, true, num, adjustCadre, logPrefix)
    
  def removeSleeperCellsFromCountry(name: String, num: Int, adjustCadre: Boolean = true, logPrefix: String = "") =
    removeCellsFromCountry(name, false, num, adjustCadre, logPrefix)
    
  // Move cells from the a country on the map to the track (or training camp).
  // Caller should ensure there are enough cells of the requested type to satisfy
  // the removal, otherwise the function will throw an exception!
  def removeCellsFromCountry(name: String, active: Boolean, num: Int, adjustCadre: Boolean = true, logPrefix: String = ""): Unit = {
    if (num > 0) {
      val cellType = if (active) "active cell" else "sleeper cell"
      val c = game.getCountry(name)
      val available = if (active) c.activeCells else c.sleeperCells
      assert(available >= num, s"not enough ${cellType}s have: $available, need $num")
      val addCadre = adjustCadre && c.totalCells == num
      val toCamp = (game.trainingCampCapacity - game.trainingCampCells.inCamp) max 0 min num
      val toTrack = num - toCamp
      (toCamp, toTrack) match {
        case (0, trk)   => log("%sRemove %s from %s to the track".format(logPrefix, amountOf(trk, cellType), name))
        case (cmp, 0)   => log("%sRemove %s from %s to the training camp.".format(logPrefix, amountOf(cmp, cellType), name))
        case (cmp, trk) => log("%sRemove %s from %s. %d to the training camp and %d to the track.".format(
                                logPrefix, amountOf(num, cellType), name, cmp, trk))
      }
      if (addCadre)
        log("%sAdd cadre marker to %s.".format(logPrefix, name))
    
      // The number on the track is calculated, so it does not need to be set here.
      game = game.copy(trainingCampCells = CampCells(game.trainingCampCells.inCamp + toCamp,
                                                     game.trainingCampCells.onMap  - toCamp))
      game.getCountry(name) match {
        case m: MuslimCountry if active    => game = game.updateCountry(m.copy(activeCells  = m.activeCells  - num, hasCadre = addCadre))
        case m: MuslimCountry              => game = game.updateCountry(m.copy(sleeperCells = m.sleeperCells - num, hasCadre = addCadre))
        case n: NonMuslimCountry if active => game = game.updateCountry(n.copy(activeCells  = n.activeCells  - num, hasCadre = addCadre))
        case n: NonMuslimCountry           => game = game.updateCountry(n.copy(sleeperCells = n.sleeperCells - num, hasCadre = addCadre))
      }
    }
  }
  
  def flipSleeperCells(name: String, num: Int, logPrefix: String = ""): Unit = {
    if (num > 0) {
      val c = game.getCountry(name)
      assert(c.sleeperCells >= num, s"Cannot flip $num sleepers cells in $name, only ${c.sleeperCells} present")
      log("%sFlip %s to active in %s.".format(logPrefix, amountOf(num, "sleeper cell"), name))
      c match {
        case m: MuslimCountry    => game = game.updateCountry(m.copy(sleeperCells = m.sleeperCells - num, 
                                                                     activeCells  = m.activeCells  + num))
        case n: NonMuslimCountry => game = game.updateCountry(n.copy(sleeperCells = n.sleeperCells - num,
                                                                     activeCells  = n.activeCells  + num))
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
      log(s"World posture is $worldPosture $level and US posture is $worldPosture , prestige increases to ${game.prestige}")
    }
    if (game.humanRole == US) {
      game = game.copy(reserves = game.reserves.copy(us = 0))
      log(s"US reserves set to zero")
    }
    else {
      game = game.copy(reserves = game.reserves.copy(jihadist = 0))
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
    
    if (game.cardsLapsing.nonEmpty) {
      log(s"Discard the lapsing events: ${cardNumsAndNames(game.cardsLapsing)}")
      game = game.copy(cardsLapsing = Nil)
    }
    game.firstPlotCard foreach { num => 
      log(s"Discard the firstplot card: ${cardNumAndName(num)}")
      game = game.copy(firstPlotCard = None)
    }
    
    for (rc <- game.muslims filter (_.regimeChange == GreenRegimeChange)) {
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
    val (humanRole, botDifficulties) =
      getOneOf("Which side do you wish play? (US or Jihadist) ", "US"::"Jihadist"::Nil, None, false) match {
        case Some("US") => (US, Muddled :: Coherent :: Attractive :: Nil)
        case _          => (Jihadist, OffGuard :: Competent :: Adept :: Nil)
      }
    
    val humanAutoRoll = true
    game = initialGameState(scenario, humanRole, humanAutoRoll, botDifficulties)
    
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
    def prompt = {
      val numCards = game.cardsPlayed.size
      s"\n>>> Turn ${game.turn}, ${amountOf(numCards, "card")} played <<<\n${separator()}\nCommand : "
    }
    readLine(prompt) match {
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
      new Command("us",       """Enter a card number for a US card play"""),
      new Command("jihadist", """Enter a card number for a Jihadist card play"""),
      new Command("show",     """Display the current game state
                                |  show all        - entire game state
                                |  show played     - cards played during the current turn
                                |  show summary    - game summary including score
                                |  show scenario   - scenario and difficulty level
                                |  show caliphate  - countries making up the Caliphate
                                |  show civil wars - countries in civil war
                                |  show <country>  - state of a single country""".stripMargin),
      new Command("adjust",   """Adjust game settings <Minimal rule checking is applied>
                                |  adjust prestige      - US prestige level
                                |  adjust funding       - Jihadist funding level
                                |  adjust difficulty    - Jihadist ideology/US resolve
                                |  adjust lapsing cards - Current lapsing cards
                                |  adjust removed cards - Cards removed from the game
                                |  adjust first plot    - Current first plot card
                                |  adjust markers       - Current global event markers
                                |  adjust reserves      - US and/or Jihadist reserves
                                |  adjust plots         - Available/resolved plots
                                |  adjust offmap troops - Number of troops in off map box
                                |  adjust <country>     - Country specific settings""".stripMargin),
      new Command("history",  """Display game history"""),
      new Command("rollback", """Roll back card plays in the current turn or
                                |roll back to the start of any previous turn""".stripMargin),
      new Command("help",     """List available commands"""),
      new Command("quit",     """Save the current turn and quit the game""")
    )
    val CmdNames = (Commands map (_.name))
    
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
        case "show"     => showCommand(param)
        case "adjust"   => adjustSettings(param)
        case "history"  => game.history foreach println  // TODO: Allow > file.txt
        case "rollback" => println("Not implemented.")
        case cmd        => println(s"Internal error: Command '$cmd' is not valid")
      }
    }
  }
  
  
  
  def showCommand(param: Option[String]): Unit = {
    val options = "all" :: "cards" :: "summary" :: "scenario" :: "caliphate" ::
                  "civil wars" :: (game.countries map (_.name)).sorted
    getOneOf("Show: ", options, param, true, CountryAbbreviations) foreach {
      case "cards"      => printSummary(game.cardPlaySummary)
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
    printCountries("Muslim Countries with Good Governance", (game.muslims filter(_.isGood) map (_.name)).sorted)
    printCountries("Muslim Countries with Fair Governance", (game.muslims filter(_.isFair) map (_.name)).sorted)
    printCountries("Muslim Countries with Poor Governance", (game.muslims filter(_.isPoor) map (_.name)).sorted)
    printCountries("Muslim Countries under Islamic Rule",   (game.muslims filter(_.isIslamistRule) map (_.name)).sorted)
    printCountries("Untested Muslim Countries with Data",   (game.muslims filter(_.isUntestedWithData) map (_.name)).sorted)
    printCountries("Non-Muslim Countries with Hard Posture",(game.nonMuslims filter (_.isHard) map (_.name)).sorted)
    printCountries("Non-Muslim Countries with Soft Posture",(game.nonMuslims filter (_.isSoft) map (_.name)).sorted)
    val iranSpecial = game.nonMuslims find (_.iranSpecialCase) map (_.name)
    if (iranSpecial.nonEmpty)
      printCountries("Iran Special Case", iranSpecial.toList)
    printSummary(game.scoringSummary)
    printSummary(game.statusSummary)
    printSummary(game.civilWarSummary)
    printSummary(game.caliphateSummary)
  }
  
  sealed trait CardAction
  case class  Event(card: Card) extends CardAction
  case object Ops               extends CardAction
  case object CancelCard        extends CardAction
  
  // Returns "Event", "Ops", or "Cancel"
  def getCardAction(card: Card, role: Role) =
    if (card.eventIsPlayable(role))
      getOneOf("Event or Ops? ", "Event"::"Ops"::Nil) match {
        case Some("Event") => Event(card)
        case Some("Ops")   => Ops
        case _             => CancelCard
      }
  else
    Ops

  // Return a list of actions.
  // card2 is only used for US reassessment
  def getActionOrder(opponent: Role, card: Card, card2: Option[Card] = None): List[CardAction] =
    ((card :: card2.toList) filter (_.eventWillTrigger(opponent))) match {
      case c :: Nil =>
        println("\nThe %s event \"%s\" will trigger, which should happen first?".format(opponent, c.name))
        val choices = List(c.name, "Operations")
        getOneOf(s"${orList(choices)}? ", choices) match {
          case None               => Nil
          case Some("Operations") => List(Ops, Event(c))
          case _                  => List(Event(c), Ops)
        }
        
      case c1 :: c2 :: Nil =>
        println("\nThe %s events \"%s\" and \"%s\" will trigger, which should happen first?".
                format(opponent, c1.name, c2.name))
        val choices1 = List(c1.name, c2.name, "Operations")
        getOneOf(s"${orList(choices1)}? ", choices1) match {
          case None => Nil
          case Some(first) =>
            println("Which should happen second?")
            val choices2 = choices1 filterNot (_ == first)
            getOneOf(s"${orList(choices2)}? ", choices2) match {
              case None => Nil
              case Some(second) =>
                val third = (choices2 filterNot (_ == second)).head
                List(first, second, third) map {
                  case name if name == c1.name => Event(c1)
                  case name if name == c2.name => Event(c2)
                  case _ => Ops
                }
            }
        }
      case _ => // No events triggered
        List(Ops)
    }
    
  
  def usCardPlay(param: Option[String]): Unit = {
    getCardNumber("Card # ", param) foreach { cardNumber =>
      val card = Cards(cardNumber)
      println(s"$card")
      game.humanRole match {
        case US =>
          getCardAction(card, US) match {
            case Event(_) =>
              val playedCard = PlayedCard(US, card, event = true)
              log(playedCard.toString)
              game = game.copy(cardsPlayed = playedCard :: game.cardsPlayed)
              card.executeEvent(US)
                
            case Ops =>
              humanUsOpsCommand(card)

            case CancelCard =>
          }
          
        case Jihadist =>
          // The bot plays the card
      }
    }
  }
  
  def humanUsOpsCommand(card: Card): Unit = {
    val WarOfIdeas  = "War of Ideas"
    val Deploy      = "Deploy Troops"
    val RegimeChg   = "Regime Change"
    val Withdraw    = "Withdraw Troops"
    val Disrupt     = "Disrupt Cells"
    val Alert       = "Alert Plots"
    val Reassess    = "Reassessment"
    val AddReserves = "Add to Reserves"
    val UseReserves = "Expend Reserves"
    var reservesUsed = 0
    var inReserve = game.reserves.us
    var secondCard: Option[Card] = None   // For reassessment only
    def opsAvailable = card.ops + reservesUsed
    
    @tailrec def getNextResponse(): Option[String] = {
      val operations = List(
        Some(WarOfIdeas),
        if (game.deployPossible(opsAvailable))                  Some(Deploy)      else None,
        if (game.regimeChangeTargets(opsAvailable).nonEmpty)    Some(RegimeChg)   else None,
        if (game.withdrawTargets(opsAvailable).nonEmpty)        Some(Withdraw)    else None,
        if (game.disruptTargets(opsAvailable).nonEmpty)         Some(Disrupt)     else None,
        if (game.alertTargets(opsAvailable).nonEmpty)           Some(Alert)       else None,
        if (card.ops == 3 && reservesUsed == 0)                 Some(Reassess)    else None,
        if (card.ops < 3 && reservesUsed == 0 && inReserve < 2) Some(AddReserves) else None,
        if (card.ops < 3 && reservesUsed == 0 && inReserve > 0) Some(UseReserves) else None
      ).flatten
    
      println(s"\nYou have ${opsString(opsAvailable)} available and ${opsString(inReserve)} in reserve")
      getOneOf("US operation: ", operations) match {
        case None =>  None
        case Some(UseReserves) =>
          reservesUsed = inReserve
          inReserve = 0
          getNextResponse()
        case Some(Reassess) =>
          println("You must play a second 3 Ops card")
          getCardNumber("Card # ", None, true, only3Ops = true) match {
            case None => None // Cancel the operation
            case Some(cardNum) =>
              val card2 = Cards(cardNum)
              println(s"$card2")
              secondCard = Some(card2)
              Some(Reassess)
          }
        case operation => operation
      }
    }
    
    getNextResponse() foreach { operation =>
      if (operation == AddReserves) {
        // Don't prompt for event/ops order when playing to reserves.
        inReserve += (card.ops min 2)
        val playedCard = PlayedCard(US, card, event = false, secondCard)
        game = game.copy(
          cardsPlayed = playedCard :: game.cardsPlayed,
          reserves    = game.reserves.copy(us = inReserve))
        log(playedCard.toString)
        log(s"US adds ${opsString(card.ops)} to reserves.  Reserves are now ${opsString(inReserve)}.")
        if (card.eventWillTrigger(Jihadist))
          log("Jihadist event \"%s\" triggers".format(card.name))
          card.executeEvent(Jihadist)
      }
      else
        getActionOrder(Jihadist, card, secondCard) match {
          case Nil => // Cancel the operation
          case actions =>
            val playedCard = PlayedCard(US, card, event = false, secondCard)
            log(playedCard.toString)
            game = game.copy(cardsPlayed = playedCard :: game.cardsPlayed)
          
            if (reservesUsed > 0) {
              log(s"US player expends their reserves of ${opsString(reservesUsed)}")
              game = game.copy(reserves = game.reserves.copy(us = 0))
            }
          
            actions foreach {
              case Event(c) =>
                log("Jihadist event \"%s\" triggers".format(c.name))
                c.executeEvent(Jihadist)
              case Ops =>
                operation match {
                  case WarOfIdeas => humanWarOfIdeas(opsAvailable)
                  case Deploy     => humanDeploy(opsAvailable)
                  case Disrupt    => humanDisrupt(opsAvailable)
                  case RegimeChg  => humanRegimeChange()
                  case Withdraw   => humanWithdraw()
                  case Alert      => humanAlert()
                  case Reassess   => humanReasses()
                  case _ => // operation cancelled
                }
              case _ =>
            }
        }
    }
  }
  
  def humanWarOfIdeas(ops: Int): Unit = {
    val candidates = game.warOfIdeasTargets(ops).sorted
    log()
    log(s"US attempts War of Ideas operation with ${opsString(ops)}")
    val name = getOneOf("Select country: ", candidates, 
                        None, false, countryAbbr(candidates)).get
    if (game.isMuslim(name)) {
      testCountry(name)
      if (ops < 3 && game.getMuslim(name).isPoor)
        log(s"Not enough Ops to complete War of Ideas in $name")
      else
        warOfIdeas(name, humanDieRoll)
    }
    else
      warOfIdeas(name, humanDieRoll)
  }
  
  // Trops can alwasy deploy to the track.
  def humanDeploy(ops: Int): Unit = {
    log()
    log(s"US performs Deploy operation with ${opsString(ops)}")
    val sourceCandidates = game.deployFromTargets
    val source           = getOneOf("Deploy from: ", sourceCandidates, None, false, countryAbbr(sourceCandidates)).get
    val maxTroops        = if (source == "track") game.troopsAvailable else game.getMuslim(source).maxDeployFrom
    val numTroops        = getOneOf("How many troops: ", 1 to maxTroops, None, false).map(_.toInt).get
    val destCandidates   = game.deployToTargets(ops) filterNot (_ == source)
    val dest             = getOneOf("Deploy to: ", destCandidates, None, false, countryAbbr(destCandidates)).get
    def disp(name: String) = if (name == "track") "the troops track" else name
    log()
    log(s"Move ${amountOf(numTroops, "troop")} from ${disp(source)} to ${disp(dest)}")
    (source, dest) match {
      case ("track", dest) =>
        val d = game.getMuslim(dest)
        game  = game.updateCountry(d.copy(troops = d.troops + numTroops))
      case (source, "track") =>
        val s = game.getMuslim(source)
        game  = game.updateCountry(s.copy(troops = s.troops - numTroops))
      case (source, dest) =>
        val (s, d) = (game.getMuslim(source), game.getMuslim(dest))
        game       = game.updateCountries(List(s.copy(troops = s.troops - numTroops),
                                               d.copy(troops = d.troops + numTroops)))
    }
  }

  def humanDisrupt(ops: Int): Unit = {
    log()
    log(s"US performs Disrupt operation with ${opsString(ops)}")
    val candidates = game.disruptTargets(ops)
    val target = getOneOf("Disrupt in which country: ", candidates, None, false, countryAbbr(candidates)).get
    val c = game.getCountry(target)
    game.disruptLosses(target) match {
      case Left(cellsAffected) =>
        val flip   = cellsAffected min c.sleeperCells
        val remove = (cellsAffected - flip) min c.activeCells
        flipSleeperCells(target, flip)
        removeActiveCellsFromCountry(target, remove)
      case Right(cadreLost) =>
         log(s"Remove cadre marker in $target.")
         c match {
           case m: MuslimCountry    => game = game.updateCountry(m.copy(hasCadre = false))
           case n: NonMuslimCountry => game = game.updateCountry(n.copy(hasCadre = false))
         }
    }
    if (game.disruptAffectsPrestige(target)) {
      game = game.adjustPrestige(1)
      log(s"Increase prestige by +1 to ${game.prestige}")
    }
  }
    
  def humanRegimeChange(): Unit = {
  }
    
  def humanWithdraw(): Unit = {
  }
    
  def humanAlert(): Unit = {
  }
    
  def humanReasses(): Unit = {
  }
    


  def jihadistCardPlay(param: Option[String]): Unit = {
    getCardNumber("Jihadist card # ", param) foreach { cardNumber =>
      if (game.humanRole == US) {
        // Prompt for card  play
      }
      else {
        // ....
      }
    }
  }
  

  
  def adjustSettings(param: Option[String]): Unit = {
    val options = "prestige" ::"funding" :: "difficulty" :: "lapsing cards" :: "removed cards" :: 
                  "first plot" :: "markers" ::"reserves" :: "plots" :: "offmap troops" :: 
                  (game.countries map (_.name)).sorted
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
      case "lapsing cards" => adjustLapsingCards()
      case "removed cards" => adjustRemovedCards()
      case "first plot"    => adjustFirstPlot()
      case "markers"       => adjustMarkers()
      case "reserves"      => adjustReserves()
      case "plots"         => adjustPlots()
      case name            => adjustCountry(name)
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
      roleName <- getOneOf(s"Which side (${orList(choices)}): ", choices)
      role     = Role(roleName)
      current  = if (role == US) game.reserves.us else game.reserves.jihadist
      value    <- adjustInt(s"$role reserves", current, 0 to 2)
    } {
      logAdjustment(s"$role reserves", current, value)
      role match {
        case US       => game = game.copy(reserves = game.reserves.copy(us = value))
        case Jihadist => game = game.copy(reserves = game.reserves.copy(jihadist = value))
      }
    }
  }
  
  
  def adjustDifficulty(): Unit = {
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
    }  
  }
  
  def adjustLapsingCards(): Unit = {
    var inPlay = game.cardsLapsing
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
    if (inPlay.toSet != game.cardsLapsing.toSet) {
      logAdjustment("Lapsing Events", cardNumsAndNames(game.cardsLapsing), cardNumsAndNames(inPlay))
      game = game.copy(cardsLapsing = inPlay)
    }  
  }

  def adjustRemovedCards(): Unit = {
    var outOfPlay = game.cardsRemoved
    def available = removableCardNumbers filterNot outOfPlay.contains
    @tailrec def getNextResponse(): Unit = {
      println()
      println("Cards that are currently out of play:")
      println(if (outOfPlay.isEmpty) "none" else cardNumsAndNames(outOfPlay.sorted))
      println()
      println("Enter a card number to move it between in play and out of play.")
      getCardNumber("Card #: ") match {
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
    }  
  }
  
  def adjustFirstPlot(): Unit = {
    var inPlay = game.firstPlotCard
    println()
    println(s"Current first plot card: ${inPlay map cardNumAndName getOrElse "none"}")
    println()
    println("Enter a card number to add or remove it as the first plot card.")
    getCardNumber("Card #: ") foreach {
      case num if inPlay.exists(_ == num) => inPlay = None
      case num                            => inPlay = Some(num)
    }
    
    if (inPlay != game.firstPlotCard) {
      logAdjustment("First plot", game.firstPlotCard map cardNumAndName, inPlay map cardNumAndName)
      game = game.copy(firstPlotCard = inPlay)
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
      logAdjustment("Global Markers", game.markers, inPlay)
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
          println("Plots cannot be adjusted at this time.")
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
    else { // humanRole == Jihadist
      @tailrec def getNextResponse(): Unit = {
        println()
        println("Available plots:")
        showPlots(available, 1)
        println()
        println("Resolved plots:")
        showPlots(resolved, available.size + 1)
        println()
        if (available.isEmpty && resolved.isEmpty)
          println("Plots cannot be adjusted at this time.")
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
                    plotsDisplay(game.availablePlots, game.humanRole),
                    plotsDisplay(alist, game.humanRole))
      logAdjustment("Resolved plots", 
                    plotsDisplay(game.resolvedPlots, Jihadist),
                    plotsDisplay(rlist, Jihadist))
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
      case m: MuslimCountry if m.isUntested =>
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
            if (m.isUntested) {
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
    val maxCells = c.activeCells + game.cellsAvailable(ignoreFunding = true)
    if (maxCells == 0) {
      println("There a no cells available to add to this country.")
      pause()
    }
    else 
      adjustInt("Active cells", c.activeCells, 0 to maxCells) foreach { value =>
        if (value < c.activeCells)
          removeActiveCellsFromCountry(name, c.activeCells - value, false, s"$name adjusted: ")
        else if (value > c.activeCells)
          addActiveCellsToCountry(name, value - c.activeCells, true, s"$name adjusted: ")
      }
  }
  
  def adjustSleeperCells(name: String): Unit = {
    val c = game.getCountry(name)
    val maxCells = c.sleeperCells + game.cellsAvailable(ignoreFunding = true)
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
          removeSleeperCellsFromCountry(name, c.sleeperCells - value, false, s"$name adjusted: ")
        else if (value > c.sleeperCells)
          addSleeperCellsToCountry(name, value - c.sleeperCells, true, s"$name adjusted: ")
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
      case m: MuslimCountry if m.isIslamistRule =>
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
      case m: MuslimCountry if m.isIslamistRule =>
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
      case m: MuslimCountry if m.isIslamistRule =>
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
      case m: MuslimCountry if m.isIslamistRule =>
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
            val addedSleepers = (-updated.reaction min game.cellsAvailable(ignoreFunding = true))
            if (addedSleepers > 0)
              addSleeperCellsToCountry(name, addedSleepers, true)
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
    val country = game.getCountry(name)
    def numIndexesUsed(plots: Vector[Int], hidden: Boolean) = plots.size match {
      case 0           => 0
      case _ if hidden => 1
      case x           => x
    }
    def showPlots(plots: Vector[Int], startIndex: Int, hidden: Boolean): Unit = {
      @tailrec def showNext(list: List[Int], index: Int): Unit = list match {
        case Nil =>
        case plot :: rest =>
          println(s"$index) ${plotName(plot)}")
          showNext(rest, index + 1)
      }
    
      numIndexesUsed(plots, hidden) match {
        case 0           => println("none")
        case _ if hidden => println(s"$startIndex) ${plots.size} hidden plots")
        case _           => showNext(plots.toList, startIndex)
      }
    }

    // For hidden plot we select a random index.
    def selectedIndex(index: Int, plots: Vector[Int], hidden: Boolean): Int =
      if (hidden) nextInt(plots.size) else index

    var countryPlots = country.plots.toVector
    var resolved     = game.resolvedPlots.toVector
    var available    = game.availablePlots.toVector
    
    @tailrec def getNextResponse(): Unit = {
      val countryStart   = 1
      val resolvedStart  = countryStart  + countryPlots.size
      val availableStart = resolvedStart + resolved.size
      println()
      println()
      println(s"$name plots (can be removed)")
      println(separator(length = 25))
      showPlots(countryPlots, countryStart, game.humanRole == US)
      println()
      println(s"Resolved Plots (can be added to $name)")  
      println(separator(length = 25))
      showPlots(resolved, resolvedStart, false)
      println()
      println(s"Available Plots (can be added to $name)")  
      println(separator(length = 25))
      showPlots(available, availableStart, game.humanRole == US)
      println()
      if (countryPlots.isEmpty && resolved.isEmpty && available.isEmpty)
        println("Plots cannot be adjusted at this time.")
      else {
        val countryIndexes   = numIndexesUsed(countryPlots, game.humanRole == US)
        val resolvedIndexes  = numIndexesUsed(resolved, false)
        val availableIndexes = numIndexesUsed(available, game.humanRole == US)
        val totalIndexes     = countryIndexes + resolvedIndexes + availableIndexes
        println("Select the number corresponding to the plot to add/remove")
        getOneOf("Plot: ", 1 to totalIndexes) map (_.toInt) match {
          case None => // Cancel the adjustment
          case Some(num) =>
            val index = num - 1  // Actual indexes are zero based
            if (index < countryIndexes) {
              // Remove from country, prompt for destination (available or resolved)
              val choices = "resolved"::"available"::Nil
              getOneOf(s"Remove to (${orList(choices)}): ", choices) match {
                case None => // Cancel the adjustment
                case Some(target) =>
                  val i = selectedIndex(index, countryPlots, game.humanRole == US)
                  if (target == "resolved")
                    resolved = resolved :+ countryPlots(i)
                  else
                    available = available :+ countryPlots(i)
                  countryPlots = countryPlots.patch(i, Vector.empty, 1)
                  getNextResponse()
              }
            }
            else if (index < countryIndexes + resolvedIndexes) {
              // Add from resolved to country
              val i = selectedIndex(index - countryIndexes, resolved, false)
              countryPlots = countryPlots :+ resolved(i)
              resolved = resolved.patch(i, Vector.empty, 1)
              getNextResponse()
            }
            else {
              // Add from available to country
              val i = selectedIndex(index - countryIndexes - resolvedIndexes, available, game.humanRole == US)
              countryPlots   = countryPlots :+ available(i)
              available = available.patch(i, Vector.empty, 1)
              getNextResponse()
            }
        }
      }
    }
    getNextResponse()
    
    val clist = countryPlots.toList.sorted
    val rlist = resolved.toList.sorted
    val alist = available.toList.sorted
    if (clist != country.plots.sorted)
      logAdjustment(name, "plots", 
                    plotsDisplay(country.plots, game.humanRole),
                    plotsDisplay(clist, game.humanRole))
      
    if (rlist != game.resolvedPlots.sorted)
      logAdjustment("Resolved plots", 
                    plotsDisplay(game.resolvedPlots, Jihadist),
                    plotsDisplay(rlist, Jihadist))
      
    if (alist != game.availablePlots.sorted)
      logAdjustment("Available plots", 
                    plotsDisplay(game.availablePlots, game.humanRole),
                    plotsDisplay(alist, game.humanRole))

    game = game.copy(availablePlots = alist, resolvedPlots = rlist)
    country match {
      case m: MuslimCountry    => game = game.updateCountry(m.copy(plots = clist))
      case n: NonMuslimCountry => game = game.updateCountry(n.copy(plots = clist))
    }
  }
  
  def adjustCountryMarkers(name: String): Unit = {
    val country = game.getCountry(name)
    var inPlay = country.markers
    def available = CountryMarkers filterNot inPlay.contains
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
      getOneOf("Marker: ", CountryMarkers) match {
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
    if (inPlay != country.markers) {
      logAdjustment(name, "Markers", country.markers, inPlay)
      country match {
        case m: MuslimCountry    => game = game.updateCountry(m.copy(markers = inPlay))
        case n: NonMuslimCountry => game = game.updateCountry(n.copy(markers = inPlay))
      }
    }  
  }
}

