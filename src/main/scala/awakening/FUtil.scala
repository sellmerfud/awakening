
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

import java.io.{File, FilenameFilter, BufferedReader, FileReader, FileWriter, ByteArrayOutputStream,
                InputStream, OutputStream, FileInputStream, FileOutputStream, Reader, Writer, IOException}
import java.util.Date
import java.nio.file.Files
import scala.collection.mutable.ListBuffer
import scala.collection.immutable.{ Vector, BitSet }
import scala.util.Properties.{lineSeparator, propOrEmpty, isWin}
import scala.sys.process.{Process, ProcessBuilder, ProcessLogger}
import scala.util.matching.Regex
import java.util.regex.Pattern.{quote => regexQuote}
import java.net.URI

object FUtil {

  private def cleanEnds(str: String, c: Char): String =
    str.dropWhile(_ == c).reverse.dropWhile(_ == c).reverse.mkString

  // Join pieces of a path together inserting the '/' separator between each piece
  def join(args: String*): String = {
    if (args.isEmpty)
      ""
    else {
      val prefix = if (args.head.startsWith("/")) "/" else ""
      val suffix = if (args.last.endsWith("/")) "/" else ""
      args.view.map(s => cleanEnds(s, '/')).filter(_ != "").mkString(prefix, "/", suffix) match {
        case "//" => "/"
        case path => path
      }
    }
  }

  // Convenience method for use with lists, vectors, etc.
  def joinl(args: Seq[String]): String = join(args:_*)


  // If windows, convert all backslashes to forward slashes.
  def normalizePath(path: String) = if (isWin) path.replace('\\', '/') else path
  def windowizePath(path: String) = if (isWin) path.replace('/', '\\') else path

  // If there is not parent directory, return "/" for "/", otherwise "."
  def dirname(path: String): String =
    new File(normalizePath(path)).getParent match {
      case null if path == "/" => "/"
      case null                => "."
      case parent              => parent
    }

  def basename(path: String): String = {
    if (path == "")
      ""
    else {
      val norm = normalizePath(path).reverse.dropWhile(_=='/').reverse
      (norm, norm.lastIndexOf('/')) match {
        case ("",  _) => "/"
        case (".", _) => "."
        case (p,  -1) => p
        case (p,   i) => p.substring(i + 1)
      }
    }
  }

  // Return the base name with extension stripped.
  // If the basename ends with the given extension the extension is removed
  // from the result.
  // If the given ext == ".*" then any extension is stripped.
  def basename(path: String, ext: String): String = {
    val base = basename(path)
    if (ext == ".*")
      subExt(base, "")
    else if (base endsWith ext)
      base.dropRight(ext.length)
    else
      base
  }

  def subExt(path: String, ext: String): String = {
    val base = basename(path)
    val pos  = path.lastIndexOf(".")
    if (pos < (path.length - base.length + 1) || pos == path.length - 1 )
      path + ext
    else
      path.substring(0, pos) + ext
  }

  /** Return the extension portion of the path's basename starting from the rightmost period.
  *
  *   If the base name does not contain a period, or if it ends with a period then
  *   an empty string is returned.
  *   If the basename starts with a period (dotfile) then the starting dot is not considered
  *   the start of an extension.
  *   {{{
  *      FUtil.extName("foo.txt")            #=> ".txt"
  *      FUtil.extName("/aaa/bbb/foo.txt")   #=> ".txt"
  *      FUtil.extName("foo")                #=> ""
  *      FUtil.extName("foo.")               #=> ""
  *      FUtil.extName(".foo")               #=> ""
  *      FUtil.extName(".foo.sh")            #=> ".sh"
  *   }}}
  */

  def extName(path: String): String = {
    val base = basename(path)
    val pos  = base.lastIndexOf('.')
    if (pos < 1 || pos == base.length - 1)
      ""
    else
      base.substring(pos)
  }

  def exists_?(path: String) = new File(path).exists

  def file_?(path: String) = new File(path).isFile

  def directory_?(path: String) = new File(path).isDirectory

  def read_?(path: String) = new File(path).canRead

  def write_?(path: String) = new File(path).canWrite

  def root_?(path: String) = new File(path).getCanonicalPath == "/"

  def length(path: String) = {
    val file = new File(path)
    if (file.isFile) file.length else -1
  }

  def expand_path(path: String) = new File(path).getCanonicalPath

  def touch(path: String, time: Long): Boolean = {
    val file = new File(path)
    (file.exists || file.createNewFile) && file.canWrite && file.setLastModified(time)
  }

  def touch(path: String): Boolean = touch(path, System.currentTimeMillis)

  def touch(path: String, date: Date): Boolean = touch(path, date.getTime)

  /** Touch the file with the modtime of the given other path */
  def touch(path: String, other: String): Boolean = {
    val f = new File(other)
    f.exists && touch(path, f.lastModified)
  }

  def rm(path: String): Boolean = {
    val file = new File(path)
    if (file.isFile)
      try {
        Files.delete(file.toPath)
        true
      }
      catch {
        case e: IOException =>
          val msg = Option(e.getMessage).getOrElse("")
          System.err.println(s"Error deleting file ($path): $msg")
        false
      }
    else
      false
  }

  def rmdir(path: String): Boolean = {
    val file = new File(path)
    if (file.isDirectory)
      try {
        Files.delete(file.toPath)
        true
      }
      catch {
        case e: IOException =>
          val msg = Option(e.getMessage).getOrElse("")
          System.err.println(s"Error deleting directory ($path): $msg")
        false
      }
    else
      false
  }

  def mv(oldName: String, newName: String): Boolean = {
    new File(oldName).renameTo(new File(newName))
  }

  def mkdir(path: String) = new File(path).mkdir

  def mkdir_p(path: String) = new File(path).mkdirs

  def chmod(arg: String, path: String): Boolean = if (isWin)
    false
  else {
    exec(Seq("chmod", arg, path)) match {
      case (0, _, _) => true
      case _ => false
    }
  }

  def chown(arg: String, path: String): Boolean = if (isWin)
    false
  else {
    exec(Seq("chown", arg, path)) match {
      case (0, _, _) => true
      case _ => false
    }
  }

  def temp_file: String = temp_file("bedrock-tmpfile")
  def temp_file(prefix: String): String = {
    val file = File.createTempFile(prefix, null)
    file.deleteOnExit
    file.getPath
  }

  def with_temp_file(block: String => Unit): Unit = {
    block(temp_file)
  }

  def tmpdir = propOrEmpty("java.io.tmpdir")

  def readFile(path: String): String = {
    val reader = new BufferedReader(new FileReader(path))
    val sb = new StringBuilder

    def read1Line: Unit = {
      reader.readLine match {
        case null =>
        case line => 
          if (sb.nonEmpty)
            sb.append(lineSeparator)
          sb.append(line)
          read1Line
      }
    }
    read1Line
    sb.toString
  }

  def readBytes(path: String, length: Long = Long.MaxValue, offset: Long = 0L): Array[Byte] = {
    import math.{BigInt, min, max}
    val fileLen     = BigInt(FUtil.length(path))
    val bytesToRead = if (fileLen <= 0 || length <= 0L) 0L
    else if (offset + BigInt(length) > fileLen)         max((fileLen - offset).toLong, 0)
    else                                                length

    if (bytesToRead > Int.MaxValue)
      throw new IllegalStateException(s"readBytes cannot read more than Int.MaxValue(${Int.MaxValue}) bytes.")
    else if (bytesToRead == 0L)
      Array.empty
    else {
      val in  = new FileInputStream(path)
      val out = new ByteArrayOutputStream(bytesToRead.toInt)
      try {
        in.skip(offset)
        val ba  = new Array[Byte](4096)
        def readOnce(numToRead: Int): Unit = {
          if (numToRead > 0) {
            val numRead = in.read(ba, 0, min(4096, numToRead.toInt))
            if (numRead > 0) out.write(ba, 0, numRead)
            if (numRead >= 0) readOnce(numToRead - numRead)
          }
        }
        readOnce(bytesToRead.toInt)
        out.toByteArray()
      }
      finally {
        try in.close() catch { case _: IOException => }
      }
    }
  }

  // Line endings are not included.
  def eachLine(path: String, limit: Option[Int] = None)(func: (String) => Any): Unit =
    eachLine(new BufferedReader(new FileReader(path)), limit)(func)
  

  // Line endings are not included.
  def eachLine(reader: BufferedReader, limit: Option[Int])(func: (String) => Any): Unit = {
    def read1Line(limit: Option[Int]): Unit =
      if ((limit fold true) (_ > 0))
        reader.readLine match {
          case null => // Reached eof
          case line =>
            func(line)
            read1Line(limit map (_ - 1))
        }

    read1Line(limit)
  }

  def readLines(path: String, limit: Option[Int]): Seq[String] =
    readLines(new BufferedReader(new FileReader(path)), limit)

  def readLines(path: String): Seq[String] = readLines(path, None)

  def readLines(reader: BufferedReader, limit: Option[Int]): Seq[String] = {
    var lines = Vector[String]()
    eachLine(reader, limit) (lines :+= _)
    lines
  }

  def readLines(reader: BufferedReader): Seq[String] = readLines(reader, None)

  def writeBytes(path: String, bytes: Array[Byte], length: Int = Int.MaxValue, offset: Int = 0): Unit = {
    import math.min
    val len = min(length, bytes.length)
    val stream = new FileOutputStream(path)
    stream.write(bytes, offset, len)
    stream.close
  }
  
  def appendBytes(path: String, bytes: Array[Byte], length: Int = Int.MaxValue, offset: Int = 0): Unit = {
    import math.min
    val len = min(length, bytes.length)
    val stream = new FileOutputStream(path, true)
    stream.write(bytes, offset, len)
    stream.close
  }

  // Create or overwrite the file designated by 'path' with the
  // given string of content.
  def writeFile(path: String, content: String): Unit = {
    val writer = new FileWriter(path)
    writer.write(content)
    writer.close
  }

  /** Copy an input stream to an output stream */
  def copy_stream(in: InputStream, out: OutputStream, bufsize: Int = 4096): Unit = {
    val ba  = new Array[Byte](bufsize)
    def readOnce: Unit = {
      val len = in.read(ba)
      if (len > 0) out.write(ba, 0, len)
      if (len >= 0) readOnce
    }
    readOnce
  }

  /** Copy an input stream to an output stream */
  def copy_reader(in: Reader, out: Writer, bufsize: Int = 4096): Unit = {
    val ca  = new Array[Char](bufsize)
    def readOnce: Unit = {
      val len = in.read(ca)
      if (len > 0) out.write(ca, 0, len)
      if (len >= 0) readOnce
    }
    readOnce
  }

  def copy_file(src: String, dest: String): Unit = {
    val (in, out) = (new FileInputStream(src), new FileOutputStream(dest))
    copy_stream(in, out)
    in.close
    out.close
  }

  // Used to capture process output.
  private class ExecLogger extends ProcessLogger {
    private val outbuf = new ListBuffer[String]
    private val errbuf = new ListBuffer[String]
    def stdout: Seq[String] = outbuf.toList
    def stderr: Seq[String] = errbuf.toList
    def buffer[T](f: => T): T = f
    def err(s: => String): Unit = errbuf += s
    def out(s: => String): Unit = outbuf += s
  }

  private def findRootCause(e: Throwable): Throwable =
    if (e.getCause == null || e.getCause == e) return e else return findRootCause(e.getCause)

  // Execute the given process capturing the exit code, stdout, and stdin.
  // This method blocks until the prcess has finished.
  def exec(proc: ProcessBuilder): (Int, Seq[String], Seq[String]) = {
    val logger = new ExecLogger
    try {
      val status = proc ! logger
      (status, logger.stdout, logger.stderr)
    }
    catch {
      case t: Throwable => findRootCause(t) match {
        case e: IOException if e.getMessage != null => """error=(\d+)""".r.findFirstMatchIn(e.getMessage) match {
          case Some(m) => (m.group(1).toInt, List(), List(e.getMessage))
          case _ => (-1, List(), List(e.getMessage))
        }
        case e: IOException => (-1, List(), List())
      }
    }
  }

  // Shortcuts for command use cases
  def exec(command: String, extraEnv: (String, String)*): (Int, Seq[String], Seq[String]) =
    exec(Process(command, None, extraEnv:_*))

  def exec(command: Seq[String], extraEnv: (String, String)*): (Int, Seq[String], Seq[String]) =
    exec(Process(command, None, extraEnv:_*))

  def exec(command: String, cwd: String, extraEnv: (String, String)*): (Int, Seq[String], Seq[String]) =
    exec(Process(command, Some(new File(cwd)), extraEnv:_*))

  def exec(command: Seq[String], cwd: String, extraEnv: (String, String)*): (Int, Seq[String], Seq[String]) =
    exec(Process(command, Some(new File(cwd)), extraEnv:_*))


  def diff_cmd = System.getenv("BEDROCK_DIFF") match {
    case null => "diff -u"
    case cmd  => cmd
  }

  def diff(path1: String, path2: String): (Int, Seq[String], Seq[String]) = {
    exec(diff_cmd.split("\\s+").toIndexedSeq ++ Seq(path1, path2))
  }

  val FNM_NOESCAPE = 1
  val FNM_PATHNAME = 2
  val FNM_DOTMATCH = 3
  val FNM_CASEFOLD = 4

  /**  Returns true if <i>path</i> matches against <i>pattern</i>.
  *
  *    The pattern is not a regular expression; instead it follows rules
  *    similar to shell filename globbing. It may contain the following
  *    metacharacters:
  *
  *    *         Matches any file. Can be restricted by other values in the
  *              glob. `*` will match all files; `c*` will match all files
  *              beginning with `c`; `*c` will match all files ending
  *              with `c`; and `*c*` will match all files that have `c` in them
  *              (including at the beginning or end). This is equivalent
  *              to `/ .* /x` in regexp.
  *    **        Matches directories recursively or files expansively.
  *    ?         Matches any one character. Equivalent to /./ in regexp.
  *    [set]     Matches any one character in set.
  *              Behaves exactly like character sets in Regexp, including set negation ([^a-z]).
  *    \         Escapes the next metacharacter.
  *
  *    <i>flags</i> is a BitSet of the `FNM_xxx` flags.
  *    The same glob pattern and flags are used by `FUtil.glob`.
  *
  *       FUtil.fnmatch('cat',       'cat')        #=> true  # match entire string
  *       FUtil.fnmatch('cat',       'category')   #=> false # only match partial string
  *       FUtil.fnmatch('c{at,ub}s', 'cats')       #=> false # { } isn't supported
  *
  *       FUtil.fnmatch('c?t',     'cat')          #=> true  # '?' match only 1 character
  *       FUtil.fnmatch('c??t',    'cat')          #=> false # ditto
  *       FUtil.fnmatch('c*',      'cats')         #=> true  # '*' match 0 or more characters
  *       FUtil.fnmatch('c*t',     'c/a/b/t')      #=> true  # ditto
  *       FUtil.fnmatch('ca[a-z]', 'cat')          #=> true  # inclusive bracket expression
  *       FUtil.fnmatch('ca[^t]',  'cat')          #=> false # exclusive bracket expression ('^' or '!')
  *
  *       FUtil.fnmatch('cat', 'CAT')               #=> false # case sensitive
  *       FUtil.fnmatch('cat', 'CAT', BitSet(FNM_CASEFOLD)) #=> true  # case insensitive
  *
  *       FUtil.fnmatch('?',   '/', BitSet(FNM_PATHNAME))  #=> false # wildcard doesn't match '/' on FNM_PATHNAME
  *       FUtil.fnmatch('*',   '/', BitSet(FNM_PATHNAME))  #=> false # ditto
  *       FUtil.fnmatch('[/]', '/', BitSet(FNM_PATHNAME))  #=> false # ditto
  *
  *       FUtil.fnmatch('\?',   '?')                 #=> true  # escaped wildcard becomes ordinary
  *       FUtil.fnmatch('\a',   'a')                 #=> true  # escaped ordinary remains ordinary
  *       FUtil.fnmatch('\a',   '\a', BitSet(FNM_NOESCAPE))  #=> true  # FNM_NOESACPE makes '\' ordinary
  *       FUtil.fnmatch('[\?]', '?')                 #=> true  # can escape inside bracket expression
  *
  *       FUtil.fnmatch('*',   '.profile')                #=> false # wildcard doesn't match leading
  *       FUtil.fnmatch('*',   '.profile', BitSet(FNM_DOTMATCH))  #=> true  # period by default.
  *       FUtil.fnmatch('.*',  '.profile')                #=> true
  *
  *       rbfiles = '**' '/' '*.rb' # you don't have to do like this. just write in single string.
  *       FUtil.fnmatch(rbfiles, 'main.rb')                    #=> false
  *       FUtil.fnmatch(rbfiles, './main.rb')                  #=> false
  *       FUtil.fnmatch(rbfiles, 'lib/song.rb')                #=> true
  *       FUtil.fnmatch('**.rb', 'main.rb')                    #=> true
  *       FUtil.fnmatch('**.rb', './main.rb')                  #=> false
  *       FUtil.fnmatch('**.rb', 'lib/song.rb')                #=> true
  *
  *       pattern = '*' '/' '*'
  *       FUtil.fnmatch(pattern, 'dave/.profile', BitSet(FNM_PATHNAME))  #=> false
  *       FUtil.fnmatch(pattern, 'dave/.profile', BitSet(FNM_PATHNAME + FNM_DOTMATCH)) #=> true
  *
  *       pattern = '*' '/' '*' '/' '*' '/' 'foo'
  *       FUtil.fnmatch(pattern, 'a/b/c/foo', BitSet(FNM_PATHNAME))     #=> true
  *       FUtil.fnmatch(pattern, '/a/b/c/foo', BitSet(FNM_PATHNAME))    #=> true
  *       FUtil.fnmatch(pattern, 'c:/a/b/c/foo', BitSet(FNM_PATHNAME))  #=> true
  *       FUtil.fnmatch(pattern, 'a/.b/c/foo', BitSet(FNM_PATHNAME))    #=> false
  *       FUtil.fnmatch(pattern, 'a/.b/c/foo', BitSet(FNM_PATHNAME + FNM_DOTMATCH)) #=> true
  */

  def fnmatch(pattern: String, string: String, flags: BitSet = BitSet.empty): Boolean = {
    import scala.annotation.switch
    val NUL: Char = 0
    val escape_ok = !flags(FNM_NOESCAPE)
    val pathname  = flags(FNM_PATHNAME)
    val period    = !flags(FNM_DOTMATCH)
    val nocase    = flags(FNM_CASEFOLD)

    var pattern_chars = pattern.toList
    var p = NUL
    def shift_p = {
      p = pattern_chars match {
        case Nil => NUL
        case c :: cs => pattern_chars = cs; c
      }
      p != NUL
    }

    def next_p = pattern_chars match {
      case Nil => NUL
      case c :: cs => c
    }

    var string_chars = string.toList
    val first_dot = string_chars  match {
      case '.' :: cs => true
      case _ => false
    }
    var last_s = NUL
    var s = NUL
    def shift_s = {
      last_s = s
      s = string_chars match {
        case Nil => NUL
        case c :: cs => string_chars = cs; c
      }
      s != NUL
    }

    def isdirsep(c: Char) = c == '/'
    def downcase(c: Char) = if (nocase) c.toLower else c

    def ISDIRSEP(c: Char) = pathname && isdirsep(c)
    def PERIOD(c: Char) = period && c == '.' && (first_dot || ISDIRSEP(last_s))

    // Advance to the next path separator or end of string.
    def path_next: Boolean = {
      while (shift_s && !isdirsep(s)) {}
      s != NUL
    }

    def range(ch: Char): Boolean = {
      var ok = false
      shift_p
      val not = p == '!' || p == '^'
      if (not)
        shift_p
      val test = downcase(ch)
      var cstart = NUL
      var cend = NUL

      while (p != ']') {
        if (escape_ok && p == '\\')
          shift_p
        cstart = p
        if (cstart == NUL)
          return false
        shift_p
        cend = cstart
        if (p == '-' && next_p != ']') {
          shift_p
          if (escape_ok && p == '\\')
            shift_p
          cend = p
          if (cend == NUL)
            return false
          shift_p
        }
        if (downcase(cstart) <= test && test <= downcase(cend))
          ok = true
      }
      return ok != not
    }

    // -- Begin
    shift_s
    while (shift_p) {
      (p: @switch) match {
        case '?' => if (s == NUL || ISDIRSEP(s) || PERIOD(s)) return false else shift_s

        case '*' =>
          while(shift_p && p == '*') {}  // Eat consequtive asterisks
          if (PERIOD(s))
            return false
          if (p == NUL)
            return !(pathname && path_next);
          else if (ISDIRSEP(p)) {
            if (path_next)
              shift_s
            else
              return false
          }
          else {
            val test = downcase(if (escape_ok && p == '\\') next_p else p)
            while (s != NUL) {
              if ((p == '?' || p == '[' || downcase(s) == test) &&
                    fnmatch((p :: pattern_chars).mkString, (s :: string_chars).mkString, flags + FNM_DOTMATCH))
                return true
              else if (ISDIRSEP(s))
                return false
              else
                shift_s
            }
            return false
          }

        case '[' =>
          if (s == NUL || ISDIRSEP(s) || PERIOD(s))
            return false;
          if (!range(s))
            return false
          shift_s

        case _ =>
          if (escape_ok && p == '\\' && next_p != NUL)
            shift_p
          if (downcase(p) != downcase(s))
            return false
          else
            shift_s
      }
    }
    return s == NUL
  }

  /** Returns the filenames found by expanding pattern which is a String or a Seq[String],
  *
  *   Note that this pattern is not a regexp (it‘s closer to a shell glob).
  *   Note that case sensitivity depends on your system.
  *
  *   *:	    Matches any file. Can be restricted by other values in the glob. * will match all files;
  *          c* will match all files beginning with c; *c will match all files ending with c; and *c*
  *          will match all files that have c in them (including at the beginning or end).
  *          Equivalent to / .* /x in regexp. Note, this will not match Unix-like hidden files (dotfiles).
  *          In order to include those in the match results, you must use something like "{*,.*}".
  *   **:	  Matches directories recursively.
  *   ?:	    Matches any one character. Equivalent to /.{1}/ in regexp.
  *   [set]:	Matches any one character in set. Behaves exactly like character sets in Regexp,
  *          including set negation ([^a-z]).
  *   {p,q}:	Matches either literal p or literal q. Matching literals may be more than one character
  *          in length. More than two literals may be specified. Equivalent to pattern alternation
  *          in regexp.
  *
  *
  *      glob("config.?")          #=> ["config.h"]
  *      glob("*.[a-z][a-z]")      #=> ["main.rb"]
  *      glob("*.[^r]*")           #=> ["config.h"]
  *      glob("*.{rb,h}")          #=> ["main.rb", "config.h"]
  *      glob("*")                 #=> ["config.h", "main.rb"]
  *      glob("*", BitSet(FNM_DOTMATCH))   #=> [".", "..", "config.h", "main.rb"]
  *
  *      rbfiles = FUtil.join("**", "*.rb")
  *      glob(rbfiles)                   #=> ["main.rb",
  *                                      #    "lib/song.rb",
  *                                      #    "lib/song/karaoke.rb"]
  *      libdirs = FUtil.join("**", "lib")
  *      glob(libdirs)                   #=> ["lib"]
  *
  *      librbfiles = FUtil.join("**", "lib", "**", "*.rb")
  *      glob(librbfiles)                #=> ["lib/song.rb",
  *                                      #    "lib/song/karaoke.rb"]
  *
  *      librbfiles = FUtil.join("**", "lib", "*.rb")
  *      glob(librbfiles)                #=> ["lib/song.rb"]
  */

  def glob(pattern: String): Seq[String] = glob(Seq(pattern), BitSet.empty)
  def glob(pattern: String, flags: BitSet): Seq[String] = glob(Seq(pattern), flags)

  def glob(patterns: Seq[String]): Seq[String] = glob(patterns, BitSet.empty)
  def glob(patterns: Seq[String], flags: BitSet): Seq[String] = {
    val escape_ok = !flags(FNM_NOESCAPE)
    val dotmatch  = flags(FNM_DOTMATCH)
    val nocase    = flags(FNM_CASEFOLD)

    def glob1(pattern: String): Seq[String] = {
      val rooted   = pattern.startsWith("/")
      val dir_only = pattern.endsWith("/")
      val fixed_pattern = if (rooted) pattern.drop(1) else pattern

      def downcase(s: String) = if (nocase) s.toLowerCase else s

      // Basic idea
      //  Split pattern based on dir separators
      //     For each sub pattern create a Glob object
      //        Three types:
      //             PLAIN     - a literal match
      //             RECURSIVE - corresponds to **/
      //             MAGICAL   - contains special glob characters
      //
      //    Starting with the first Glob gather the list of matches.
      //      For each match attempt to match with the next glob, and so on...

      // This filter is used by the glob_recursive method to get only the subdirectories in a
      // given directory. Hidden directories (that start with a period : .svn, .git) are not
      // returned unless the FNM_DOTMATCH flag has been specified.
      object SubdirsOnly extends FilenameFilter {
        def accept(dir: File, name: String) = {
          (dotmatch || !name.startsWith(".")) && new File(dir, name).isDirectory
        }
      }

      trait Glob {
        def list(dir: File, only_subdirs: Boolean = false): Seq[String]
      }

      case class Magical(pat: String) extends Glob {
        def list(dir: File, only_subdirs: Boolean): Seq[String] = {
          // The java File#list method does not include the "." and ".." entries so we must
          // add them here (but only if they match our pattern!)
          val specials = Seq(".", "..").filter(fnmatch(pat, _, flags + FNM_PATHNAME))
          val found = dir.list(new FilenameFilter {
            def accept(dir: File, name: String) = {
              (!(dir_only || only_subdirs) || new File(dir, name).isDirectory) && fnmatch(pat, name, flags + FNM_PATHNAME)
            }
          }).toSeq
          specials ++ found
        }
      }

      case class Plain(str: String) extends Glob {
        def list(dir: File, only_subdirs: Boolean): Seq[String] = {
          dir.list(new FilenameFilter {
            def accept(dir: File, name: String) = {
              (!(dir_only || only_subdirs) || new File(dir, name).isDirectory) && downcase(name) == downcase(str)
            }
          }).toSeq
        }
      }

      case class Recursive() extends Glob {
        def list(dir: File, only_subdirs: Boolean): Seq[String] =
          throw new UnsupportedOperationException("Recursive#list should not be called!")
      }

      // Represents "." and ".."
      // This is needed because File#list does not return special directories.
      case class Special(name: String) extends Glob {
        //  Special directories are always returned regardless of the dir_only an only_subdirs flags.
        def list(dir: File, only_subdirs: Boolean): Seq[String] = Seq(name)
      }

      // Return true if the pattern has special glob characters
      def has_magic(pat: String): Boolean = {
        var escaped = false
        for (c <- pat) c match {
          case '*' | '?' | '[' if !escaped => return true
          case '\\' if (escape_ok)         => escaped = !escaped
          case _                           => escaped = false
        }
        false
      }

      // Split a pattern according to the directory separators
      def split_by_dirs(pat: String, subs: Seq[String]): Seq[String] = {
        def find_dirsep(i: Int, chars: List[Char], open: Boolean, escaped: Boolean): Option[Int] = {
          chars match {
            case c :: cs => c match {
              case '['  if (!escaped)           => find_dirsep(i+1, cs, true, false)
              case ']'  if (!escaped)           => find_dirsep(i+1, cs, false, false)
              case '\\' if (escape_ok)          => find_dirsep(i+1, cs, open, !escaped)
              case '/'  if (!(open || escaped)) => Some(i);
              case _                            => find_dirsep(i+1, cs, open, false)
            }
            case _  => None
          }
        }
        find_dirsep(0, pat.toList, false, false) match {
          case Some(pos) => split_by_dirs(pat.drop(pos + 1), pat.take(pos) +: subs)
          case None      => pat +: subs
        }
      }

      // Remove adjacent recursive directory specifiers, since they are redundant.
      // "foo/**/**/bar" == "foo/**/bar"
      // If the last subpattern is a recursive and the pattern was not terminated with a '/'
      // then change it to a '*'
      def fix_recursives(pats: Seq[String]): Seq[String] = {
        pats.toList match {
          case Nil => Nil
          case "**" :: xs => xs.dropWhile(_ == "**") match {
            case Nil  => if (dir_only) Seq("**", "*") else Seq("*")
            case rest => "**" +: fix_recursives(rest)
          }
          case x :: xs => x +: fix_recursives(xs)
        }
      }

      //  Handle globbing of "**" type entries
      // Apply the list of glob this the given directories and all sub-directories recursively.
      def glob_recursive(dir: File, glob_list: Seq[Glob]): Seq[String] = {
        var results = Vector() ++ glob_dir(dir, glob_list)
        dir.list(SubdirsOnly).toSeq.foreach { subdir =>
          results = results ++ glob_recursive(new File(dir, subdir), glob_list)
        }
        results
      }

      def parent_name(dir: File) = if (rooted) dir.toString else dir.toString.drop(2)

      def glob_dir(dir: File, glob_list: Seq[Glob]): Seq[String] = {
        glob_list.toList match {
          case Nil               => Seq()
          case g :: Nil          => g.list(dir).map(join(parent_name(dir), _))
          case Recursive() :: gs => glob_recursive(dir, gs)
          case g :: gs           =>
            var results = Vector[String]()
            g.list(dir, true).foreach { subdir =>
              results = results ++ glob_dir(new File(dir, subdir), gs)
            }
            results
        }
      }

      // Filter out empty sub patterns. This would result when two or more adjacent directory separators
      // were encountered ("foo//bar" == "foo/bar")
      // Also remove redundant recursive patterns ("**/**/" == "**/")
      val globs = fix_recursives(split_by_dirs(fixed_pattern, Seq()).reverse.filter(_ != "")).map { pat =>
        pat match {
          case ".." | "."          => Special(pat)
          case "**"                => Recursive()
          case p if (has_magic(p)) => Magical(p)
          case p                   => Plain(p)
        }
      }

      glob_dir(new File(if (rooted) "/" else "."), globs)
    }

    patterns.flatMap(glob1).sortWith(_.toLowerCase < _.toLowerCase)
  }

  /**  Pathname represents the name of a file or directory on the filesystem, but not the file itself.
  *
  *    A Pathname can be relative or absolute. It’s not until you try to reference the file that
  *    it even matters whether the file exists or not.
  *
  *    Pathname is immutable.
  */
  case class Pathname(rawPath: String) {
    val path = FUtil.normalizePath(rawPath)
    val file = new File(path)
    lazy val absPrefix: Option[String] = if (isWin) "^([a-zA-Z]:)?/".r findFirstIn path
                                         else       "^/".r findFirstIn path
    lazy val withoutAbsPrefix: String = path.drop((absPrefix map (_.length)) getOrElse 0)
    override def toString() = path

    lazy val winPath = FUtil.windowizePath(path)
    
    /** Comparison is string based.
    *
    *   Two unequal Pathnames ("foo.txt" "./foo.txt") can refer to the same file.
    */
    override def equals(other: Any): Boolean =
        other match {
          case that: Pathname if that canEqual this => path == that.path
          case _ => false
        }
    def canEqual(other: Any): Boolean = other.isInstanceOf[Pathname]
    override def hashCode: Int = path.hashCode

    /** Return a new Pathname with the given pathname appended to this pathname */
    def join(args: Pathname*): Pathname = Pathname(FUtil.joinl( path +: (args map (_.path)) ))
    /** Return a new Pathname with the argument joined to this pathname */
    def + (pname: Pathname): Pathname = join(pname)
    def / (pname: Pathname): Pathname = join(pname)

    /** Return new Pathname that is an absolute path representation of this path */
    def absolute = Pathname(file.getAbsolutePath)

    /** Calls the supplied function with a new Pathname object for each element in the given path
    *   in ascending order.
    *   {{{
    *       Pathname("/path/to/some/file.txt") ascend println
    *         #=> /path/to/some/file.txt
    *         #=> /path/to/some
    *         #=> /path/to
    *         #=> /path
    *         #=> /
    *   }}}
    */
    def ascend(func: (Pathname) => Unit): Unit = {
      val prefix = absPrefix getOrElse ""
      val min    = if (isAbsolute) 0 else 1
      for (i <- filenames.size to min by -1)
        func(Pathname(FUtil.joinl(prefix :: (filenames take i))))
    }

    def basename = Pathname(FUtil.basename(path))
    def basename(ext: String) = Pathname(FUtil.basename(path, ext))

    /** Returns clean canonical pathname of this pathname with consecutive slashes and useless dots removed.
    *
    *   This method may access the file system.
    *   see: Pathname#cleanPath
    */
    def canonicalPath = Pathname(file.getCanonicalPath)

    /** Returns the immediate children of this pathname (files and directories).
    *
    *   It does not recurse.  If `withDirectory` is true (the default) it will
    *   include enough path information to access the files.  If `withDirectory`
    *   is false, only the file names are returned.
    *   The results never contain `.` or `..`.
    *
    *   see: eachChild()
    */
    def children(withDirectory: Boolean = true): Seq[Pathname] = {
      val entries = getEntries(withDotDirs = false)
      if (withDirectory) entries
      else               entries map (_.basename)
    }

    def chmod(modes: String) = FUtil.chmod(modes, path)
    def chown(user: String) = FUtil.chown(user, path)
    
    /** Returns clean version of this pathname with consecutive slashes and useless dots removed.
    *
    *   This method does not access the file system.
    *   see: Pathname#canonicalPath
    */
    def cleanPath: Pathname = {
      val EMPTY = List[String]()
      // Consecutive slashes are removed by the #filenames method.
      // `segs` is the cleaned path segments,
      // `count` is the number of leading `..` that must be accounted for.
      val (count, segs) = (filenames foldRight (0, EMPTY)) { case (seg, (count, segs)) =>
        seg match {
          case "."             => (count    , segs)
          case ".."            => (count + 1, segs)
          case n if count == 0 => (count    , n :: segs)
          case n               => (count - 1, segs)
        }
      }
      // Make absolute, or prepend count leading ".." if relative.
      val fixedSegs = if (isAbsolute)
        absPrefix.get :: segs
      else if (segs.size == 0 && count == 0)
        "." :: Nil
      else
        List.concat(List.fill(count)(".."), segs)

      Pathname(FUtil.joinl(fixedSegs))
    }


    /** Delete file or directory represented by this Pathname
    *
    *  see #rm, #rmdir, #rmtree
    */
    def delete(): Boolean =
      if      (isFile)      FUtil.rm(path)
      else if (isDirectory) FUtil.rmdir(path)
      else                  false


    /** Calls the supplied function with a new Pathname object for each element in the given path
    *   in descending order.
    *   {{{
    *     Pathname("/path/to/some/file.txt") descend println
    *       #=> /
    *       #=> /path
    *       #=> /path/to
    *       #=> /path/to/some
    *       #=> /path/to/some/file.txt
    *   }}}
    */
    def descend(func: (Pathname) => Unit): Unit = {
      val prefix = absPrefix getOrElse ""
      val min    = if (isAbsolute) 0 else 1
      for (i <- min to filenames.size)
        func(Pathname(FUtil.joinl(prefix :: (filenames take i))))
    }

    def dirname = Pathname(FUtil.dirname(path))

    /** Returns the sequence of path components.
    *
    *   "/usr/local/bin/file.txt"
    *     #=> Seq("usr", "local", "bin", "file.txt")
    *
    */
    lazy val filenames: List[String] = (withoutAbsPrefix split "/").toList filterNot (_ =="")


    // Line endings are not included.
    def eachLine(func: (String) => Any): Unit = FUtil.eachLine(path)(func)
    // Line endings are not included.
    def eachLine(limit: Int)(func: (String) => Any): Unit = FUtil.eachLine(path, Some(limit))(func)

    // Includes "." and "..".  Basenames only.
    def entries: Seq[Pathname] = getEntries(withDotDirs = true) map (_.basename)

    private val DotDirs = (p: Pathname) => {
      val base = p.basename.path
      base == "." || base == ".."
    }

    private def getEntries(withDotDirs: Boolean): Seq[Pathname] =
      if (isDirectory) {
        val entries = Pathname.glob(this/"*", BitSet(FNM_DOTMATCH))
        if (withDotDirs)
          entries
        else
          entries.filterNot(DotDirs)
      }
      else
        throw new IllegalStateException(s"${toString()} is not a directory!")

    /** Return the extension portion of the path's basename starting from the rightmost period.
    *
    *   If the base name does not contain a period, or if it ends with a period then
    *   an empty string is returned.
    *   If the basename starts with a period (dotfile) then the starting dot is not considered
    *   the start of an extension.
    *   {{{
    *      Pathname("foo.txt").extName            #=> ".txt"
    *      Pathname("/aaa/bbb/foo.txt").extName   #=> ".txt"
    *      Pathname("foo").extName                #=> ""
    *      Pathname("foo.").extName               #=> ""
    *      Pathname(".foo").extName               #=> ""
    *      Pathname(".foo.sh").extName            #=> ".sh"
    *   }}}
    */
    def extName: String = FUtil.extName(path)


    /** Performs a top down traversal of this pathname and, if it is a directory, all of
    *   its children, recursively.
    *   The test function will be called with each candidate Pathname. It must return
    *   one of:
    *      Pathname.INCLUDE -> The pathname will be included in the list
    *      Pathname.EXCLUDE -> The pathname will be excluded from the list but its children if any
    *                          will still be considered.
    *      Pathname.PRUNE   -> The pathname will be excluded from the list and none of its children
    *                          will be considered.
    */
    import Pathname.{FindVerb, INCLUDE, EXCLUDE, PRUNE}
    def find(test: (Pathname) => FindVerb): Seq[Pathname] = {
      def process(pname: Pathname, results: Vector[Pathname]): Vector[Pathname] = {
        def processChildren: Vector[Pathname] = if (pname.isDirectory) {
          val children = Pathname.glob(pname/"*", BitSet(FNM_DOTMATCH)).filterNot(DotDirs)
          (children foldLeft Vector[Pathname]()) ((r, c) => process(c, r))
        }
        else
          Vector.empty

        test(pname) match {
          case INCLUDE => (results :+ pname) ++ processChildren
          case EXCLUDE => results ++ processChildren
          case PRUNE   => results
        }
      }

      process(this, Vector.empty)
    }

    /** A shortcut for pname find (_ => INCLUDE) */
    def find(): Seq[Pathname] = find(_ => INCLUDE)

    def fnmatch(pattern: String, flags: BitSet = BitSet.empty) = FUtil.fnmatch(pattern, path, flags)

    /** Return true if the pathname is absolute */
    def isAbsolute: Boolean = file.isAbsolute
    def isRelative          = !isAbsolute
    def isDirectory         = file.isDirectory
    def isFile              = file.isFile
    def isHidden            = file.isHidden
    def exists              = file.exists
    def isReadable          = file.canRead
    def isWritable          = file.canWrite
    def isExecutable        = file.canExecute
    def isRoot              = {
      val cp = canonicalPath
      absPrefix.fold(false) { prefix => cp == Pathname(prefix) }
    }
    /** Returns true if base name begins with a period but is not `.` or `..` */
    def isDotFile = {
      val base = basename.path
      (base startsWith ".") && base != "." && base != ".."
    }

    def readable(value: Boolean)        = file.setReadable(value, false)
    def ownerReadable(value: Boolean)   = file.setReadable(value, true)
    def writable(value: Boolean)        = file.setWritable(value, false)
    def ownerWritable(value: Boolean)   = file.setWritable(value, true)
    def executable(value: Boolean)      = file.setExecutable(value, false)
    def ownerExecutable(value: Boolean) = file.setExecutable(value, true)

    def length = if (file.isFile) file.length else -1
    def size   = length

    /** Returns the dirname and basename of this path  */
    def split = (dirname, basename)
    
    /** Return a Pathname with the first string replaced by the second string */
    // When the first parameter is a String it is regex quoted so we match the literal string.
    def sub(sequence: java.lang.CharSequence, replacement: String): Pathname = 
      Pathname(new Regex(regexQuote(sequence.toString)).replaceFirstIn(path, replacement))
    /** Return a Pathname with the first regex match replaced by the second string */
    def sub(regex: Regex, replacement: String): Pathname = Pathname(regex.replaceFirstIn(path, replacement))
    
    def subExt(ext: String) = Pathname(FUtil.subExt(path, ext))
    
    /** Return the last modified time of the file/directory pointed to by this pathname.
    *
    *   If the file does not exist, return 0.
    */
    def lastModified: Long = file.lastModified

    /** Create the directory reference by this pathname.  Does not create intermediate directories */
    def mkdir(): Boolean = file.mkdir

    /** Create the directory reference by this pathname.  Creating intermediate directories as necessary*/
    def mkpath(): Boolean = file.mkdirs

    def inputStream(): FileInputStream = new FileInputStream(path)
    def inputStream[T](func: (FileInputStream) => T): T = {
      val stream = inputStream()
      try func(stream)
      finally { try stream.close() catch { case _: Throwable =>} }
    }

    def outputStream(): FileOutputStream = new FileOutputStream(path, false)
    def outputStream[T](func: (FileOutputStream) => T): T = {
      val stream = outputStream()
      try func(stream)
      finally { try stream.close() catch { case _: Throwable =>} }
    }
    
    def appendStream(): FileOutputStream = new FileOutputStream(path, true)
    def appendStream[T](func: (FileOutputStream) => T): T = {
      val stream = appendStream()
      try func(stream)
      finally { try stream.close() catch { case _: Throwable =>} }
    }

    def reader(): FileReader = new FileReader(path)
    def reader[T](func: (FileReader) => T): T = {
      val r = reader()
      try func(r)
      finally { try r.close() catch { case _: Throwable =>} }
    }

    def writer(): FileWriter = new FileWriter(path, false)
    def writer[T](func: (FileWriter) => T): T = {
      val w = writer()
      try func(w)
      finally { try w.close() catch { case _: Throwable =>} }
    }

    def appender(): FileWriter = new FileWriter(path, true)
    def appender[T](func: (FileWriter) => T): T = {
      val w = appender()
      try func(w)
      finally { try w.close() catch { case _: Throwable =>} }
    }

    /** Returns the parent of the file/dir represented by this Pathname */
    def parent: Pathname = if (path == ".") Pathname("..") else dirname

    /**  Read the contents of the file and return as an array of bytes.
    *
    *    If length is not specified then the entire file is read.
    */
    def read(length: Long = Long.MaxValue, offset: Long = 0L): Array[Byte] =
      FUtil.readBytes(path, length, offset)

    /**  Write the contents of a byte buffer to the file for this Pathname */
    def write(bytes: Array[Byte], length: Int = Int.MaxValue, offset: Int = 0): Unit =
      FUtil.writeBytes(path, bytes, length, offset)

    /**  Append the contents of a byte buffer to the file for this Pathname */
    def append(bytes: Array[Byte], length: Int = Int.MaxValue, offset: Int = 0): Unit =
      FUtil.appendBytes(path, bytes, length, offset)

    def readFile(): String = FUtil.readFile(path)

    def readLines(limit: Option[Int] = None): Seq[String] = FUtil.readLines(path, limit)

    /**  Returns a new Pathname represents the same path as the pathname except that
    *    it is relative to the given path name.
    *
    *    If `this.isAbsolute` must equal `other.isAbsolute`.  In other words both paths
    *    must be absolute OR both paths must be relative.
    *
    *    This method does not access the file system.
    *    {{{
    *       "/a/b/c/d".relativeTo("/a/b")        #=> c/d
    *       "/a/b".relativeTo("/a/b")            #=> .
    *       "/a/b/c/d".relativeTo("/a/b/x")      #=> ../c/d
    *       "/a/b/c/d".relativeTo("/a/b/c/d/e")  #=> ..
    *       "/a/b/c/d".relativeTo("/x/y")        #=> ../../a/b/c/d
    *       "a/b/c/d".relativeTo("a/b")          #=> c/d
    *       "a/b/c/d".relativeTo("a/b/x")        #=> ../c/d
    *       "a/b/c/d".relativeTo("a/b/c/d/e")    #=> ..
    *       "a/b/c/d".relativeTo("x/y")          #=> ../../a/b/c/d
    *       "a/b/c/d".relativeTo(".")            #=> a/b/c/d
    *       "a/b/c/d".relativeTo("")             #=> a/b/c/d (same as ".") cleanPath does this for us.
    *       "a/b/c/d".relativeTo("..")           #=> Exception - base has more leading ".." segments.
    *       ".".relativeTo("x")                  #=> ..
    *       "../a/b/c/d".relativeTo("..")        #=> a/b/c/d
    *       "../a/b/c".relativeTo("../a/b")      #=> c
    *       "../a/b/c".relativeTo("../x/y")      #=> ../../a/b/c
    *       "../a/b/c".relativeTo("x/y")         #=> ../../../a/b/c
    *       "".relativeTo("/a/b")                #=> Exception (mixed absolute/relative)
    *       "".relativeTo("a/b")                 #=> ../..
    *       "a/b/c".relativeTo("../a/b")         #=> Exception - base has more leading ".." segments.
    *    }}}
    */
    def relativeTo(base: Pathname): Pathname = {
      if (isAbsolute != base.isAbsolute)
        throw new IllegalStateException("both paths must be absolute or both must be relative")

      val segs     = cleanPath.filenames
      val baseSeqs = base.cleanPath.filenames

      // If the base starts with more ".." segments than path does, we cannot reliably
      // create a new relative path (without filesystem access).
      val dots     = (segs takeWhile (_ == ".."))
      val baseDots = (baseSeqs takeWhile (_ == ".."))
      if (isRelative && dots.size < baseDots.size)
        throw new IllegalStateException("base path has more leading \"..\" segments")

      val pathLen = segs.size
      val baseLen = baseSeqs.size
      val prefix  = ((segs zip baseSeqs) takeWhile { case (a, b) => a == b }).size
      val newSegs = if (baseLen == 0 || baseSeqs == List("."))
        segs
      else if (pathLen == prefix)
        List.fill(baseLen - pathLen)("..")
      else
        List.concat(List.fill(baseLen - prefix)(".."), segs.drop(prefix))
      Pathname(FUtil.joinl(newSegs)).cleanPath
    }


    /** This method is equivalent to other.relativeTo(this)
    *
    * A convenience method to use if your Pathname is the base path.
    */
    def relativize(other: Pathname): Pathname = other.relativeTo(this)
    
    def rename(newPath: Pathname): Boolean = FUtil.mv(path, newPath.path)
    
    def rm(): Boolean    = if (isFile) delete() else false
    def rmdir(): Boolean = if (isDirectory) delete() else false

    /** Deletes the directory and recursively any file and directories beneath it.
    *
    *   IMPORTANT: BE VERY CAREFUL using this method !!!!
    *
    *   return true if all directories/files were delete successfully.
    *   If any deletes fail, the rest are still tried, but the method will
    *   return false to indicate that there was at least one failure.
    */
    def rmtree(): Boolean = {
      // For safety, do not allow an empty path here!
      // If you want to remove the tree at the current working directory
      // then explicitly use "."
      if (path.trim == "")
        throw new IllegalStateException("rmtree called for empty pathname")
      
      // Fold Right so that leaves are deleted before their parents!
      find()
        .foldRight(true) { (p, result) => 
          p.delete()
          true
        }
    }
    
    
    def touch(): Boolean = FUtil.touch(path, System.currentTimeMillis)
    def touch(time: Long): Boolean = FUtil.touch(path, time)
    def touch(date: Date): Boolean = FUtil.touch(path, date.getTime)
    /** Touch the file with the modtime of the given other path */
    def touch(other: Pathname): Boolean = FUtil.touch(path, other.path)
      
    def toURI: URI = file.toURI
    
    /** Truncates the file represented by this Pathname to the given number of bytes */
    def truncate(newSize: Long): Unit = appendStream (_.getChannel.truncate(newSize))
    
    def writeFile(content: String): Unit = FUtil.writeFile(path, content)
    
    
  }

  object Pathname {
    import scala.language.implicitConversions
    import scala.util.Properties.{userDir => userDirProp, userHome => userHomeProp}
    import scala.math.Ordering

    /** Valid return values when using the Pathname#find() method */
    sealed trait FindVerb
    case object INCLUDE extends FindVerb
    case object EXCLUDE extends FindVerb
    case object PRUNE   extends FindVerb

    // Implicit ordering for pathnames when sorted.
    implicit val PathnameOrdering: Ordering[Pathname] = Ordering.by(_.path)
    // Auto conversion for Pathname instances.
    implicit final def stringToPathname(s: String): Pathname = Pathname(s)
    implicit final def pathnameToString(p: Pathname): String = p.toString

    def apply(uri: URI): Pathname = Pathname(new File(uri).getPath)
    
    def join(args: Pathname*) = Pathname(FUtil.joinl(args map (_.path)))

    /** Return scala.util.Properties.userDir as a Pathname */
    def userDir: Pathname = Pathname(userDirProp)

    /** Return scala.util.Properties.userHome as a Pathname */
    def userHome: Pathname = Pathname(userHomeProp)
    
    def temp = Pathname(FUtil.temp_file("tmpfile"))
    def temp(prefix: String) = Pathname(FUtil.temp_file(prefix))
  
    /** Calls FUtil.glob and maps results to Pathnames */
    def glob(spec: Pathname, flags: BitSet = BitSet.empty): Seq[Pathname] =
      FUtil.glob(spec.path, flags) map (Pathname(_))
    
  }
}
