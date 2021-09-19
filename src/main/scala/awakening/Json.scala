
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

import scala.util.Sorting
import scala.util.parsing.combinator._


// An Exception thrown when parsing or building JSON.
class JsonException(reason: String) extends Exception(reason)


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

// Stolen from the scala book and fixed by making string quotation explicit.
private class JsonParser extends JavaTokenParsers {
  def obj: Parser[Map[String, Any]] = "{" ~> repsep(member, ",") <~ "}" ^^ (Map.empty ++ _)

  def arr: Parser[List[Any]] = "[" ~> repsep(value, ",") <~ "]"

  def member: Parser[(String, Any)] = string ~ ":" ~ value ^^ {
    case name ~ ":" ~ value => (name, value)
  }

  def number: Parser[Any] = floatingPointNumber ^^ {
    case num if num.matches(".*[.eE].*") => BigDecimal(num)
    case num => {
      val rv = num.toLong
      if (rv >= Int.MinValue && rv <= Int.MaxValue) rv.toInt else rv
    }
  }

  lazy val stringParser = (new EscapedStringParser)

  def string: Parser[String] = "\"(\\\\\\\\|\\\\\"|[^\"])*+\"".r ^^ { escapedStr =>
      stringParser.parse(escapedStr)
    }

  def value: Parser[Any] = obj | arr | string | number |
    "null" ^^ (x => null) | "true" ^^ (x => true) | "false" ^^ (x => false)

  def parse(s: String) = {
    parseAll(value, s) match {
      case Success(result, _) => result
      case x @ Failure(msg, z) => throw new JsonException(x.toString)
      case x @ Error(msg, _) => throw new JsonException(x.toString)
    }
  }
}



// Natively supported scalar types are: Boolean, Int, Long, String.
// Collections are Sequence[T], Map[String, T] where T includes the scalars defined above, or
// recursive Sequence or Map.
object Json {
  // Quote a string according to "JSON rules".
  def quote(s: String) = {
    def escapedChar(codePoint: Int): String = {
      codePoint match {
        case c if c > 0xffff =>
          val chars = Character.toChars(c)
          "\\u%04x\\u%04x".format(chars(0).toInt, chars(1).toInt)
        case c if c < 0x20 || c > 0x7e => "\\u%04x".format(c.toInt)
        case c => c.toChar.toString
      }
    }
    
    val charCount = s.codePointCount(0, s.length)
    val escaped = for (idx <- 0 until charCount)
      yield s.codePointAt(s.offsetByCodePoints(0, idx)) match {
        case 0x0d => "\\r"
        case 0x0a => "\\n"
        case 0x09 => "\\t"
        case 0x22 => "\\\""
        case 0x5c => "\\\\"
        case 0x2f => "\\/" // to avoid sending "</"
        case c => escapedChar(c)
      }

    "\"%s\"".format(escaped.mkString)
  }

  // Returns a JSON representation of the given object.
  def build(topObject: Any): String = {
    import scala.util.Properties.{ lineSeparator => nl }
    val result = new StringBuilder

    def buildItem(obj: Any, indent: Int): Unit = {
      val prefix = " " * indent

      def buildSeq(seq: Seq[Any]): Unit = {
        result.append("[").append(nl)
        if (seq.isEmpty)
          result.append(nl)
        else {
          val commas =  (false :: List.fill(seq.size - 1)(true)).reverse
          for ((value, comma) <- seq zip commas) {
            result.append(prefix).append("  ")
            buildItem(value, indent + 2)
            if (comma)
              result.append(",")
            result.append(nl)
          }
        }
        result.append(prefix).append("]")
      }
      
      def buildMap(m: Map[String, Any]): Unit = {
        result.append("{").append(nl)
        if (m.isEmpty)
          result.append(nl)
        else {
          val commas =  (false :: List.fill(m.size - 1)(true)).reverse
          val names = m.keys.toList.sorted
          for ((name, comma) <- names zip commas; value = m(name)) {
            result.append(prefix).append("  ").append(quote(name)).append(": ")
            buildItem(value, indent + 2)
            if (comma)
              result.append(",")
            result.append(nl)
          }
        }
        result.append(prefix).append("}")
      }
      
      obj match {
        case null            => result.append("null")
        case x: Boolean      => result.append(x.toString)
        case x: Number       => result.append(x.toString)
        case array: Array[_] => buildSeq(array.toSeq)
        case set: Set[_]     => buildSeq(set.toSeq)
        case list: Seq[_]    => buildSeq(list)
        case map: Map[_, _]  => buildMap(map.map { t => (t._1.toString, t._2) })
        case x               => result.append(quote(x.toString))
      }
      
    }
    
    buildItem(topObject, 0)
    result.append(nl).toString
  }


  // Parses a JSON String representation into its native Scala representation.
  def parse(s: String): Any = (new JsonParser).parse(s)
}

