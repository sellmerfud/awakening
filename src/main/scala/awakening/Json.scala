
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

import scala.language.implicitConversions
import scala.collection.mutable
import scala.util.parsing.combinator._


object Json {
  sealed trait Value {
    def value: Any

    def str = this match {
      case Json.Str(value) => value
      case _ => throw InvalidData(this, "Expected Json.Str")
    }

    def strOpt = this match {
      case Json.Str(value) => Some(value)
      case _ => None
    }

    def obj = this match {
      case Json.Obj(value) => value
      case _ => throw InvalidData(this, "Expected Json.Obj")
    }

    def objOpt = this match {
      case Json.Obj(value) => Some(value)
      case _ => None
    }

    def arr = this match {
      case Json.Arr(value) => value
      case _ => throw InvalidData(this, "Expected Json.Arr")
    }

    def arrOpt = this match {
      case Json.Arr(value) => Some(value)
      case _ => None
    }

    def num = this match {
      case Json.Num(value) => value
      case _ => throw InvalidData(this, "Expected Json.Num")
    }

    def int = num.toInt
    def long = num.toLong
    def short = num.toShort
    def char = num.toChar
    def byte = num.toByte
    def float = num.toFloat
    def double = num

    def numOpt = this match {
      case Json.Num(value) => Some(value)
      case _ => None
    }

    def intOpt = numOpt.map(_.toInt)
    def longOpt = numOpt.map(_.toLong)
    def shortOpt = numOpt.map(_.toShort)
    def charOpt = numOpt.map(_.toChar)
    def byteOpt = numOpt.map(_.toByte)
    def floatOpt = numOpt.map(_.toFloat)
    def doubleOpt = numOpt

    def bool = this match {
      case Json.Bool(value) => value
      case _ => throw InvalidData(this, "Expected Json.Bool")
    }

    def boolOpt = this match {
      case Json.Bool(value) => Some(value)
      case _ => None
    }

    def isNull = this match {
      case Json.Null => true
      case _ => false
    }
  }

  case class Str(value: String) extends Value
  case class Num(value: Double) extends Value
  sealed abstract class Bool extends Value {
    def value: Boolean
  }
  object Bool {
    def apply(value: Boolean): Bool = if( value)  True else False
    def unapply(bool: Bool): Some[Boolean] = Some(bool.value)
  }
  case object False extends Bool {
    def value = false
  }
  case object True extends Bool {
    def value = true
  }
  case object Null extends Value {
    def value = null
  }

  case class Obj(value: mutable.LinkedHashMap[String, Value]) extends Value

  object Obj {
    implicit def from(items: IterableOnce[(String, Value)]): Obj =
      Obj(mutable.LinkedHashMap.from(items))

    def apply(): Obj = Obj(mutable.LinkedHashMap.empty[String, Value])

    def apply(item: (String, Value), items: (String, Value)*): Obj = {
      val map = mutable.LinkedHashMap[String, Value]()
      map.put(item._1, item._2)
      for ((key, value) <- items)
        map.put(key, value)
      Obj(map)
    }
  }

  case class Arr(value: mutable.ArrayBuffer[Value]) extends Value

  object Arr {
    implicit def from[T](items: IterableOnce[T])(implicit conv: T => Value): Arr = {
      val buf = mutable.ArrayBuffer[Value]()
      for (item <- items.iterator)
        buf += (conv(item): Value)
      Arr(buf)
    }

    def apply(items: Value*): Arr = {
      val buf = new mutable.ArrayBuffer[Value](items.length)
      for (item <- items)
          buf += item
      Arr(buf)
    }
  }

  // implicit conversions
  implicit def string2Str(x: String): Str = Str(x)
  implicit def boolean2bool(x: Boolean): Bool = if (x) Json.True else Json.False
  implicit def byte2num(x: Byte): Num = Num(x)
  implicit def short2num(x: Short): Num = Num(x)
  implicit def int2num(x: Int): Num = Num(x)
  implicit def long2num(x: Long): Num = Num(x.toDouble)
  implicit def float2Nnm(x: Float): Num = Num(x)
  implicit def double2num(x: Double): Num = Num(x)
  implicit def null2null(x: scala.Null): Json.Null.type = Json.Null

  // Quote a string according to "JSON rules".
  def quote(s: String) = {
    val charCount = s.codePointCount(0, s.length)
    val escaped =
      for (idx <- 0 until charCount)
      yield
        s.codePointAt(s.offsetByCodePoints(0, idx)) match {
          case 0x0d => "\\r"
          case 0x0a => "\\n"
          case 0x09 => "\\t"
          case 0x22 => "\\\""
          case 0x5c => "\\\\"
          case 0x2f => "\\/" // to avoid sending "</"
          case c if Character.isISOControl(c) && c > 0xffff =>
            val chars = Character.toChars(c)
            "\\u%04x\\u%04x".format(chars(0).toInt, chars(1).toInt)
          case c if Character.isISOControl(c) =>
            "\\u%04x".format(c.toInt)
          case c =>
            c.toChar.toString
        }
    "\"%s\"".format(escaped.mkString)
  }

  // An Exception thrown when parsing or building JSON.
  case class JsonException(reason: String) extends Exception(reason)
  case class InvalidData(data: Value, msg: String) extends Exception(s"$msg (data: $data)")

  private class EscapedStringParser extends JavaTokenParsers {
    override protected val whiteSpace = "".r

    def unicode: Parser[String] = rep1("\\u" ~> """[a-fA-F0-9]{4}""".r) ^^ { stringBytes =>
      new String(stringBytes.map(Integer.valueOf(_, 16).intValue.asInstanceOf[Char]).toArray)
    }

    def escaped: Parser[String] = "\\" ~> """[\\/bfnrt"]""".r ^^ { charStr =>
      val char = charStr match {
        case "r" => '\r'
        case "n" => '\n'
        case "t" => '\t'
        case "b" => '\b'
        case "f" => '\f'
        case x => x.charAt(0)
      }
      char.toString
    }

    def characters: Parser[String] = """[^\"[\x00-\x1F]\\]+""".r // comment to fix emac parsing "

    def string: Parser[String] = "\"" ~> rep(unicode | escaped | characters) <~ "\"" ^^ { list =>
      list.mkString("")
    }

    def parse(s: String) = {
      parseAll(string, s) match {
        case Success(result, _) => result
        case x @ Failure(msg, z) => throw new JsonException(x.toString)
        case x @ Error(msg, _) => throw new JsonException(x.toString)
      }
    }
  }

  private class JsonParser extends JavaTokenParsers {
    sealed trait KeyValueSeparators
    object Colon extends KeyValueSeparators

    def obj: Parser[Obj] = "{" ~> repsep(pair, ",") <~ "}" ^^ {
      pairs => Obj.from(pairs)
    }

    def arr: Parser[Arr] = "[" ~> repsep(value, ",") <~ "]" ^^ {
      items => Arr.from(items)
    }

    def pair: Parser[(String, Value)] = string ~ (":" ^^^ Colon) ~ value ^^ {
      case name ~ Colon ~ value => (name.str, value)
    }

    def number: Parser[Num] = floatingPointNumber ^^ {
      num => Num(num.toDouble)
    }

    lazy val stringParser = (new EscapedStringParser)

    def string: Parser[Str] = """"(\\\\|\\"|[^"])*+"""".r ^^ { escapedStr =>
        Str(stringParser.parse(escapedStr))
      }

    def `null`: Parser[Json.Null.type] = "null" ^^ { _ => Json.Null }

    def `true`: Parser[Json.Bool] = "true" ^^ { _ => Json.True }

    def `false`: Parser[Json.Bool] = "false" ^^ { _ => Json.False }

    def value: Parser[Value] = obj | arr | string | number | `null` | `true` | `false`

    def parse(s: String): Value = {
      parseAll(value, s) match {
        case Success(result, _) => result
        case x @ Failure(msg, z) => throw new JsonException(x.toString)
        case x @ Error(msg, _) => throw new JsonException(x.toString)
      }
    }
  }

  // Parses a JSON String representation into its native Scala representation.
  def parse(s: String) = new JsonParser().parse(s)

  // Returns a JSON representation of the given object.
  def output(topObject: Value, indent: Option[Int] = None): String = {
    import scala.util.Properties.lineSeparator
    val result = new StringBuilder
    val indentWidth = indent.getOrElse(-1)
    val (lineEnd, colonSep) = if (indentWidth > 0)
      (lineSeparator, " ")
    else
      ("", "")

    def indentPrefix(indentLevel: Int) = if (indentWidth > 0)
      " " * indentLevel * indentWidth
    else
      ""

    def outputArrayItems(arr: Arr, indentLevel: Int): Unit = {
      import arr.value
      if (value.isEmpty)
        result.append(lineEnd)
      else {
        val commas =  (false :: List.fill(value.size - 1)(true)).reverse
        for ((item, comma) <- value.zip(commas)) {
          result.append(indentPrefix(indentLevel))
          outputItem(item, indentLevel)
          if (comma)
            result.append(",")
          result.append(lineEnd)
        }
      }
    }

    def outputObjectPairs(obj: Obj, indentLevel: Int): Unit = {
      import obj.value
      if (value.isEmpty)
        result.append(lineEnd)
      else {
        val commas =  (false :: List.fill(value.size - 1)(true)).reverse
        val names = value.keys.toList
        for ((name, comma) <- names.zip(commas); item = value(name)) {
          result.append(indentPrefix(indentLevel)).append(quote(name)).append(":").append(colonSep)
          outputItem(item, indentLevel)
          if (comma)
            result.append(",")
          result.append(lineEnd)
        }
      }
    }

    def outputItem(obj: Value, indentLevel: Int): Unit =
      obj match {
        case Null =>
          result.append("null")
        case True =>
          result.append("true")
        case False =>
          result.append("false")
        case x: Num if x.value % 1 == 0 =>
          result.append(x.value.toLong.toString)
        case x: Num =>
          result.append(x.value.toString)
        case x: Str =>
          result.append(quote(x.value))
        case x: Arr =>
          result.append("[").append(lineEnd)
          outputArrayItems(x, indentLevel + 1)
          result.append(indentPrefix(indentLevel)).append("]")
        case x: Obj =>
          result.append("{").append(lineEnd)
          outputObjectPairs(x, indentLevel + 1)
          result.append(indentPrefix(indentLevel)).append("}")
      }

    outputItem(topObject, indentLevel = 0)
    result.append(lineEnd).toString
  }
}
