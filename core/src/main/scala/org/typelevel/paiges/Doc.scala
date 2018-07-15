package org.typelevel.paiges

import java.io.PrintWriter
import java.lang.StringBuilder

import scala.annotation.tailrec
import scala.util.matching.Regex

/**
 * implementation of Wadler's classic "A Prettier Printer"
 *
 * http://homepages.inf.ed.ac.uk/wadler/papers/prettier/prettier.pdf
 */
sealed abstract class Doc extends Product with Serializable {

  import Doc.{ Align, Empty, Text, Line, Nest, Concat, Union }

  /**
   * Append the given Doc to this one.
   */
  def +(that: Doc): Doc =
    Concat(this, that)

  /**
   * Prepend the given String to this Doc.
   *
   * The expression `str +: d` is equivalent to `Doc.text(str) + d`.
   */
  def +:(str: String): Doc =
    Doc.text(str) + this

  /**
   * Append the given String to this Doc.
   *
   * The expression `d :+ str` is equivalent to `d + Doc.text(str)`.
   */
  def :+(str: String): Doc =
    this + Doc.text(str)

  /**
   * Synonym for .repeat. If n > 0 repeat the doc n times,
   * else return empty
   */
  def *(n: Int): Doc =
    repeat(n)

  /**
   * Append the given Doc to this one, separated by a newline.
   */
  def /(that: Doc): Doc =
    Concat(this, Concat(Doc.line, that))

  /**
   * Append the given Doc to this one, separated by a newline.
   */
  def line(that: Doc): Doc =
    this / that

  /**
   * Prepend the given String to this Doc, separated by a newline.
   *
   * The expression `str /: d` is equivalent to `Doc.text(str) / d`.
   */
  def /:(str: String): Doc =
    Concat(Doc.text(str), Concat(Doc.line, this))

  /**
   * Append the given String to this Doc, separated by a newline.
   *
   * The expression `d :/ str` is equivalent to `d / Doc.text(str)`.
   */
  def :/(str: String): Doc =
    this / Doc.text(str)

  /**
   * Append the given String to this one, separated by a newline.
   */
  def line(str: String): Doc =
    this :/ str

  /**
   * Append the given Doc to this one, separated by a space.
   */
  def space(that: Doc): Doc =
    Concat(this, Concat(Doc.space, that))

  /**
   * Append the given String to this Doc, separated by a space.
   */
  def space(that: String): Doc =
    this.space(Doc.text(that))

  /**
   * Append the given Doc to this one, separated by a space.
   */
  def &(that: Doc): Doc =
    this space that

  /**
   * Append the given String to this Doc, separated by a space.
   *
   * The expression `str &: d` is equivalent to `Doc.text(str) & d`.
   */
  def &:(that: String): Doc =
    Doc.text(that) space this

  /**
   * Append the given String to this Doc, separated by a space.
   *
   * The expression `d :& str` is equivalent to `d & Doc.text(str)`.
   */
  def :&(that: String): Doc =
    this space Doc.text(that)

  /**
   * Append the given Doc to this one, using a space (if there is
   * enough room), or a newline otherwise.
   */
  def lineOrSpace(that: Doc): Doc =
    Concat(this, Concat(Doc.lineOrSpace, that))

  /**
   * Append the given String to this Doc, using a space (if there is
   * enough room), or a newline otherwise.
   */
  def lineOrSpace(that: String): Doc =
    lineOrSpace(Doc.text(that))

  /**
   * Bookend this Doc between the given Docs.
   *
   * If the documents (when flattened) all fit on one line, then
   * newlines will be collapsed, spaces will be added,
   * and the document will render on one line. If you do not want
   * a space, see tightBracketBy
   *
   * Otherwise, newlines will be used on either side of this document,
   * and the requested level of indentation will be added as well.
   */
  def bracketBy(left: Doc, right: Doc, indent: Int = 2): Doc =
    Concat(left,
      Concat(Concat(Doc.line, this).nested(indent),
      Concat(Doc.line, right)).grouped)

  /**
   * Bookend this Doc between the given Docs.
   *
   * If the documents (when flattened) all fit on one line, then
   * newlines will be collapsed, without a space
   * and the document will render on one line. If you want
   * the newline to collapse to a space, see bracketBy.
   *
   * Otherwise, newlines will be used on either side of this document,
   * and the requested level of indentation will be added as well.
   */
  def tightBracketBy(left: Doc, right: Doc, indent: Int = 2): Doc =
    Concat(left,
      Concat(Concat(Doc.lineBreak, this).nested(indent),
      Concat(Doc.lineBreak, right)).grouped)

  /**
   * Treat this Doc as a group that can be compressed.
   *
   * The effect of this is to replace newlines with spaces, if there
   * is enough room. Otherwise, the Doc will be rendered as-is.
   */
  def grouped: Doc = {
    flattenBoolean.fold(this) {
      case (flattened, changed) =>
        if (changed) Union(flattened, () => this)
        else flattened
    }
  }

  /**
   * Returns true if all renders return the empty string
   */
  def isEmpty: Boolean = {
    @tailrec def loop(doc: Doc, stack: List[Doc]): Boolean =
      doc match {
        case Empty => stack match {
          case d1 :: tail => loop(d1, tail)
          case Nil => true
        }
        case Concat(_, Line(_)) =>
          false // minor optimization to short circuit sooner
        case Concat(a, Text(s)) =>
          // minor optimization to short circuit sooner
          s.isEmpty && loop(a, stack)
        case Concat(a, b) => loop(a, b :: stack)
        case Nest(i, d) => loop(d, stack)
        case Align(d) => loop(d, stack)
        case Text(s) =>
          // shouldn't be empty by construction, but defensive
          s.isEmpty && loop(Empty, stack)
        case Line(_) => false
        case Union(flattened, _) =>
          // flattening cannot change emptiness
          loop(flattened, stack)
      }
    loop(this, Nil)
  }

  /**
   * d.nonEmpty == !d.isEmpty
   */
  def nonEmpty: Boolean = !isEmpty

  private def renderGen(width: Int, trim: Boolean): String = {
    val bldr = new StringBuilder
    val it = Chunk.best(width, this, trim)
    while (it.hasNext) {
      bldr.append(it.next)
    }
    bldr.toString
  }

  /**
   * Render this Doc as a String, limiting line lengths to `width` or
   * shorter when possible.
   *
   * Note that this method does not guarantee there are no lines
   * longer than `width` -- it just attempts to keep lines within this
   * length when possible.
   */
  def render(width: Int): String = renderGen(width, false)

  /**
   * Render this Doc as a String, limiting line lengths to `width` or
   * shorter when possible.
   *
   * Note that this method does not guarantee there are no lines
   * longer than `width` -- it just attempts to keep lines within this
   * length when possible.
   *
   * Lines consisting of only indentation are represented by the empty string.
   */
  def renderTrim(width: Int): String = renderGen(width, true)

  /**
   * Render this Doc as a stream of strings, treating `width` in the
   * same way as `render` does.
   *
   * The expression `d.renderStream(w).mkString` is equivalent to
   * `d.render(w)`.
   */
  def renderStream(width: Int): Stream[String] =
    Chunk.best(width, this, false).toStream

  /**
   * Render this Doc as a stream of strings, treating `width` in the
   * same way as `render` does.
   *
   * The expression `d.renderStream(w).mkString` is equivalent to
   * `d.render(w)`.
   *
   * Lines consisting of only indentation are represented by the empty string.
   */
  def renderStreamTrim(width: Int): Stream[String] =
    Chunk.best(width, this, true).toStream

  /**
   * Render this Doc as a stream of strings, using
   * the widest possible variant. This is the same
   * as render(Int.MaxValue) except it is more efficient.
   */
  def renderWideStream: Stream[String] = {
    @tailrec
    def loop(pos: Int, lst: List[(Int, Doc)]): Stream[String] = lst match {
      case Nil => Stream.empty
      case (i, Empty) :: z => loop(pos, z)
      case (i, Concat(a, b)) :: z => loop(pos, (i, a) :: (i, b) :: z)
      case (i, Nest(j, d)) :: z => loop(pos, ((i + j), d) :: z)
      case (i, Align(d)) :: z => loop(pos, (pos, d) :: z)
      case (i, Text(s)) :: z => s #:: cheat(pos + s.length, z)
      case (i, Line(_)) :: z => Chunk.lineToStr(i) #:: cheat(i, z)
      case (i, Union(a, _)) :: z =>
        /*
         * if we are infinitely wide, a always fits
         */
        loop(pos, (i, a) :: z)
    }
    def cheat(pos: Int, lst: List[(Int, Doc)]) =
      loop(pos, lst)

    loop(0, (0, this) :: Nil)
  }

  /**
   * If n > 0, repeat the Doc that many times, else
   * return empty
   */
  def repeat(count: Int): Doc = {
    /*
     * only have log depth, so recursion is fine
     * d * (2n + c) = (dn + dn) + c
     */
    def loop(d: Doc, cnt: Int): Doc = {
      val n = cnt >> 1
      val dn2 =
        if (n > 0) {
          val dn = loop(d, n)
          Concat(dn, dn)
        }
        else {
          Empty
        }
      if ((cnt & 1) == 1) Concat(dn2, d) else dn2
    }
    if (count <= 0) Empty
    else loop(this, count)
  }

  /**
   * Nest appends spaces to any newlines occurring within this Doc.
   *
   * The effect of this is cumulative. For example, the expression
   * `x.nested(1).nested(2)` is equivalent to `x.nested(3)`.
   */
  def nested(amount: Int): Doc =
    this match {
      case Nest(i, d) => Nest(i + amount, d)
      case _ => Nest(amount, this)
    }

  /**
   * aligned sets the nesting to the column position before we
   * render the current doc. This is useful if you have:
   *
   * Doc.text("foo") + (Doc.text("bar").line(Doc.text("baz"))).align
   *
   * which will render as:
   *
   * foobar
   *    baz
   */
  def aligned: Doc = Align(this)

  private def writeToGen(width: Int, pw: PrintWriter, trim: Boolean): Unit = {
    val it = Chunk.best(width, this, trim)
    while(it.hasNext) {
      pw.append(it.next)
    }
  }

  /**
   * Render this Doc at the given `width`, and write it to the given
   * PrintWriter.
   *
   * The expression `x.writeTo(w, pw)` is equivalent to
   * `pw.print(x.render(w))`, but will usually be much more efficient.
   *
   * This method does not close `pw` or have any side-effects other
   * than the actual writing.
   */
  def writeTo(width: Int, pw: PrintWriter): Unit =
    writeToGen(width, pw, false)

  /**
   * Render this Doc at the given `width`, and write it to the given
   * PrintWriter.
   *
   * The expression `x.writeTo(w, pw)` is equivalent to
   * `pw.print(x.render(w))`, but will usually be much more efficient.
   *
   * This method does not close `pw` or have any side-effects other
   * than the actual writing.
   *
   * Lines consisting only of indentation are represented by the empty string.
   */
  def writeToTrim(width: Int, pw: PrintWriter): Unit =
    writeToGen(width, pw, true)

  /**
   * Compute a hash code for this Doc.
   */
  override lazy val hashCode: Int = {

    @inline def hash(curr: Int, c: Char): Int =
      curr * 1500450271 + c.toInt

    @tailrec def shash(n: Int, s: String, i: Int): Int =
      if (i < s.length) shash(hash(n, s.charAt(i)), s, i + 1) else n

    // Always go left to avoid triggering the lazy fill evaluation.
    renderWideStream.foldLeft(0xdead60d5) {
      case (n, s) => shash(n, s, 0)
    }
  }

  /**
   * Return a very terse string for this Doc.
   *
   * To get a full representation of the document's internal
   * structure, see `verboseString`.
   */
  override def toString: String =
    "Doc(...)"

  /**
   * Produce a verbose string representation of this Doc.
   *
   * Unlike `render`, this method will reveal the internal tree
   * structure of the Doc (i.e. how concatenation and union nodes are
   * constructed), as well as the contents of every text node.
   *
   * By default, only the left side of union nodes is displayed. If
   * `forceUnions = true` is passed, then both sides of the union are
   * rendered (making this potentially-expensive method even more
   * expensive).
   */
  def representation(forceUnions: Boolean = false): Doc = {
    @tailrec def loop(stack: List[Either[Doc, String]], suffix: Doc): Doc =
      stack match {
        case head :: tail =>
          head match {
            case Right(s) =>
              loop(tail, s +: suffix)
            case Left(d) =>
              d match {
                case Empty =>
                  loop(tail, "Empty" +: suffix)
                case Line(f) =>
                  loop(tail, s"Line($f)" +: suffix)
                case Text(s) =>
                  loop(tail, "Text(" +: s +: ")" +: suffix)
                case Nest(i, d) =>
                  loop(Left(d) :: Right(", ") :: Right(i.toString) :: Right("Nest(") :: tail, ")" +: suffix)
                case Align(d) =>
                  loop(Left(d) :: Right("Align(") :: tail, ")" +: suffix)
                case Concat(x, y) =>
                  loop(Left(y) :: Right(", ") :: Left(x) :: Right("Concat(") :: tail, ")" +: suffix)
                case Union(x, y) =>
                  if (forceUnions) {
                    loop(Left(y()) :: Right(", ") :: Left(x) :: Right("Union(") :: tail, ")" +: suffix)
                  } else {
                    loop(Left(x) :: Right("Union(") :: tail, ", ...)" +: suffix)
                  }
              }
          }
        case Nil =>
          suffix
      }
    loop(Left(this) :: Nil, Doc.empty)
  }

  /**
   * Convert this Doc to a single-line representation.
   *
   * All newlines are replaced with spaces (and optional indentation
   * is ignored). The resulting Doc will never render any newlines, no
   * matter what width is used.
   */
  def flatten: Option[Doc] = {

    /*
     * Note after this, docs are right associated.
     * Also note the left side of a grouped is always
     * flattened, this means the "fits" branch in rendering
     * always has a right associated Doc which means it is O(w)
     * to find if you can fit in width w.
     */
    def finish(d: Doc, front: List[Doc]): Doc =
      front.foldLeft(d) { (res, f) => Concat(f, res) }

    @tailrec
    def loop(h: Doc, stack: List[Doc], front: List[Doc]): Option[Doc] =
      h match {
        case Empty | Text(_) =>
          stack match {
            case Nil => Some(finish(h, front))
            case x :: xs => loop(x, xs, h :: front)
          }
        case l@Line(_) =>
          val next = l.asFlatDoc
          if (next eq Doc.line) None
          else loop(next, stack, front)
        case Nest(i, d) => loop(d, stack, front) // no Line, so Nest is irrelevant
        case Align(d) => loop(d, stack, front) // no Line, so Align is irrelevant
        case Union(a, _) => loop(a, stack, front) // invariant: flatten(union(a, b)) == flatten(a)
        case Concat(a, b) => loop(a, b :: stack, front)
      }
    loop(this, Nil, Nil)
  }

  /**
   * This method is similar to flatten, but returns None if no
   * flattening was needed (i.e. if no newlines or unions were present).
   *
   * As with flatten, the resulting Doc (if any) will never render any
   * newlines, no matter what width is used.
   */
  def flattenOption: Option[Doc] = {
    flattenBoolean.withFilter(_._2).map(_._1)
  }

  // return the flattened doc, and if it is different
  private def flattenBoolean: Option[(Doc, Boolean)] = {

    type DB = (Doc, Boolean)

    def finish(last: DB, front: List[DB]): DB =
      front.foldLeft(last) {
        case ((d1, c1), (d0, c2)) => (Concat(d0, d1), c1 || c2)
      }

    @tailrec
    def loop(h: DB, stack: List[DB], front: List[DB]): Option[DB] =
      h._1 match {
        case Empty | Text(_) =>
          stack match {
            case Nil => Some(finish(h, front))
            case x :: xs => loop(x, xs, h :: front)
          }
        case l@Line(_) =>
          val next = l.asFlatDoc
          if (next eq Doc.line) None
          else {
            val change = (next, true)
            stack match {
              case Nil => Some(finish(change, front))
              case x :: xs => loop(x, xs, change :: front)
            }
          }
        case Nest(i, d) =>
          /*
           * This is different from flatten which always strips
           * the Nest node. This will return None if there is
           * no embedded Line inside
           */
          loop((d, h._2), stack, front) // no Line, so Nest is irrelevant
        case Align(d) =>
          /*
           * This is different from flatten which always strips
           * the Align node. This will return None if there is
           * no embedded Line inside
           */
          loop((d, h._2), stack, front) // no Line, so Align is irrelevant
        case Union(a, _) => loop((a, true), stack, front) // invariant: flatten(union(a, b)) == flatten(a)
        case Concat(a, b) => loop((a, h._2), (b, h._2) :: stack, front)
      }
    loop((this, false), Nil, Nil)
  }

  /**
   * Returns the largest width which may affect how this Doc
   * renders. All widths larger than this amount are guaranteed to
   * render the same.
   *
   * Note that this does not guarantee that all widths below this
   * value are distinct, just that they may be distinct. This value is
   * an upper-bound on widths that produce distinct renderings, but
   * not a least upper-bound.
   */
  def maxWidth: Int = {
    @tailrec
    def loop(pos: Int, lst: List[(Int, Doc)], max: Int): Int = lst match {
      case Nil => math.max(max, pos)
      case (i, Empty) :: z => loop(pos, z, max)
      case (i, Concat(a, b)) :: z => loop(pos, (i, a) :: (i, b) :: z, max)
      case (i, Nest(j, d)) :: z => loop(pos, ((i + j), d) :: z, max)
      case (i, Align(d)) :: z => loop(pos, (pos, d) :: z, max)
      case (i, Text(s)) :: z => loop(pos + s.length, z, max)
      case (i, Line(_)) :: z => loop(i, z, math.max(max, pos))
      case (i, Union(a, _)) :: z =>
        // we always go left, take the widest branch
        loop(pos, (i, a) :: z, max)
    }

    loop(0, (0, this) :: Nil, 0)
  }
}

object Doc {

  /**
   * Represents an empty document (the empty string).
   */
  private[paiges] case object Empty extends Doc

  /**
   * Represents a single, literal newline.
   * if flattenToSpace == true, flatten returns space else empty
   */
  private[paiges] case class Line(options: Line.Options) extends Doc {
    def asFlatDoc: Doc = options.asFlatDoc
  }
  object Line {
    def apply(doc: Doc): Line = Line(Options(doc, indent = true))
    case class Options(asFlatDoc: Doc, indent: Boolean)
  }

  /**
   * The string must not be empty, and may not contain newlines.
   */
  private[paiges] case class Text(str: String) extends Doc

  /**
   * Represents a concatenation of two documents.
   */
  private[paiges] case class Concat(a: Doc, b: Doc) extends Doc

  /**
   * Represents a "remembered indentation level" for a
   * document. Newlines in this document will be followed by at least
   * this much indentation (nesting is cumulative).
   */
  private[paiges] case class Nest(indent: Int, doc: Doc) extends Doc

  /**
   * Align sets the nesting at the current position
   */
  private[paiges] case class Align(doc: Doc) extends Doc

  /**
   * Represents an optimistic rendering (on the left) as well as a
   * fallback rendering (on the right) if the first line of the left
   * is too long.
   *
   * There is an additional invariant on Union: `flatten(a) == flatten(b)`.
   *
   * By construction all `Union` nodes have this property; to preserve
   * this we don't expose the `Union` constructor directly, but only
   * the `.grouped` method on Doc.
   *
   * Additionally, the left side (a) MUST be right associated with
   * any Concat nodes to maintain efficiency in rendering. This
   * is currently done by flatten/flattenOption
   */
  private[paiges] case class Union(a: Doc, b: () => Doc) extends Doc {
    lazy val bDoc: Doc = b()
  }

  private[this] val maxSpaceTable = 20

  private[this] val spaceArray: Array[Text] =
    (1 to maxSpaceTable).map { i => Text(" " * i) }.toArray

  /**
   * Produce a document of exactly `n` spaces.
   *
   * If `n < 1`, and empty document is returned.
   */
  def spaces(n: Int): Doc =
    if (n < 1) Empty
    else if (n <= maxSpaceTable) spaceArray(n - 1)
    else {
      // n = max * d + r
      val d = n / maxSpaceTable
      val r = n % maxSpaceTable
      spaceArray(maxSpaceTable - 1) * d + spaces(r)
    }

  val space: Doc = spaceArray(0)
  val empty: Doc = Empty
  /**
   * when flattened a line becomes a space
   * You might also @see lineBreak if you want a line that
   * is flattened into empty
   */
  val line: Doc = Line(space)

  /** Line that does not flatten, but respects nesting. */
  val lineNoFlat: Doc = Line(Line.Options(line, indent = true))

  /** Line that does not flatten, and does not respect nesting. */
  val lineNoFlatNoIndent: Doc = Line(Line.Options(line, indent = false))

  /**
   * A lineBreak is a line that is flattened into
   * an empty Doc. This is generally useful in code
   * following tokens that parse without the need for
   * whitespace termination (consider ";" "," "=>" etc..)
   *
   * when we call `.grouped` on lineBreak we get lineOrEmpty
   */
  val lineBreak: Doc = Line(empty)

  /**
   * lineOrSpace renders a space if we can fit the rest
   * or inserts a newline. Identical to line.grouped
   */
  val lineOrSpace: Doc = line.grouped

  /**
   * lineOrEmpty renders as empty if we can fit the rest
   * or inserts a newline. Identical to lineBreak.grouped
   */
  val lineOrEmpty: Doc = lineBreak.grouped

  /**
   * Order documents by how they render at the given width.
   */
  def orderingAtWidth(w: Int): Ordering[Doc] =
    Ordering.by((d: Doc) => d.render(w))

  /**
   * Require documents to be equivalent at all the given widths, as
   * well as at their "wide" renderings.
   */
  def equivAtWidths(widths: List[Int]): Equiv[Doc] =
    new Equiv[Doc] {
      def equiv(x: Doc, y: Doc): Boolean = {
        widths.forall(w => x.render(w) == y.render(w)) &&
          x.renderWideStream.mkString == y.renderWideStream.mkString
      }
    }

  private[this] val charTable: Array[Doc] =
    (32 to 126).map { i => Text(i.toChar.toString) }.toArray

  /**
   * Build a document from a single character.
   */
  def char(c: Char): Doc =
    if ((' ' <= c) && (c <= '~')) charTable(c.toInt - 32)
    else if (c == '\n') line
    else Text(new String(Array(c)))

  /**
   * a literal comma, equivalent to char(',')
   */
  val comma: Doc = char(',')

  /**
   * Convert a string to text.
   *
   * This method translates newlines into an appropriate document
   * representation. The result may be much more complex than a single
   * `Text(_)` node.
   */
  def text(str: String): Doc = {
    def tx(i: Int, j: Int): Doc =
      if (i == j) Empty else Text(str.substring(i, j))

    // parse the string right-to-left, splitting at newlines.
    // this ensures that our concatenations are right-associated.
    @tailrec def parse(i: Int, limit: Int, doc: Doc): Doc =
      if (i < 0) tx(0, limit) + doc
      else str.charAt(i) match {
        case '\n' => parse(i - 1, i, line + tx(i + 1, limit) + doc)
        case _ => parse(i - 1, limit, doc)
      }

    if (str == "") Empty
    else if (str.length == 1) {
      val c = str.charAt(0)
      if ((' ' <= c) && (c <= '~')) charTable(c.toInt - 32)
      else if (c == '\n') line
      else Text(str)
    }
    else if (str.indexOf('\n') < 0) Text(str)
    else parse(str.length - 1, str.length, Empty)
  }

  /**
   * Convert an arbitrary value to a Doc, using `toString`.
   *
   * This method is equivalent to `Doc.text(t.toString)`.
   */
  def str[T](t: T): Doc =
    text(t.toString)

  private val splitWhitespace: Regex = """\s+""".r

  /**
   * Convert a string to text, replacing instances of the given
   * pattern with the corresponding separator.
   *
   * Like Doc.text, this method will also lift newlines into the Doc
   * abstraction.
   *
   * The default pattern to use is `"""\s+""".r` and the default
   * separator to use is `Doc.lineOrSpace`.
   */
  def split(str: String, pat: Regex = Doc.splitWhitespace, sep: Doc = Doc.lineOrSpace): Doc =
    foldDocs(pat.pattern.split(str, -1).map(Doc.text))((x, y) => x + (sep + y))

  /**
   * Collapse a collection of documents into one document, delimited
   * by a separator.
   *
   * For example:
   *
   *     import Doc.{ comma, line, text, fill }
   *     val ds = text("1") :: text("2") :: text("3") :: Nil
   *     val doc = fill(comma + line, ds)
   *
   *     doc.render(0)  // produces "1,\n2,\n3"
   *     doc.render(6)  // produces "1, 2,\n3"
   *     doc.render(10) // produces "1, 2, 3"
   */
  def fill(sep: Doc, ds: Iterable[Doc]): Option[Doc] = {
    val flatSepOpt = sep.flatten
    if (flatSepOpt.isEmpty) return None
    val flatSep = flatSepOpt.get
    val sepGroup = sep.grouped

    @tailrec
    def fillRec(x: Doc, lst: List[Doc], stack: List[Doc => Doc]): Option[Doc] = lst match {
      case Nil => Some(call(x, stack))
      case y :: tail =>

        /*
         * The cost of this algorithm c(n) for list of size n.
         * note that c(n) = 2 * c(n-1) + k
         * for some constant.
         * so, c(n) - c(n-1) = c(n-1) + k
         * which means that
         * c(n) = (0 until n).map(c(_)).sum + nk
         *
         * which is exponential in n (O(2^n))
         *
         * Making the second parameter in the union lazy fixes this.
         * This is exactly the motivation for keeping the second
         * parameter of Union lazy.
         */
        val (flaty, changedy) = {
          val res = y.flattenBoolean
          if (res.isEmpty) return None
          else res.get
        }
        val (flatx, changedx) = {
          val res = x.flattenBoolean
          if (res.isEmpty) return None
          res.get
        }

        (changedx, changedy) match {
          case (true, true) =>
            def cont(resty: Doc) = {
              val first = Concat(flatx, Concat(flatSep, resty))
              def second = Concat(x, Concat(sep, cheatRec(y, tail).get /* ??? */))
              // note that first != second
              Union(first, () => second)
            }
            fillRec(flaty, tail, (cont _) :: stack)
          case (true, false) =>
            // flaty == y but reassociated
            def cont(resty: Doc) = {
              val first = Concat(flatx, Concat(flatSep, resty))
              val second = Concat(x, Concat(sep, resty))
              // note that first != second
              Union(first, () => second)
            }
            fillRec(flaty, tail, (cont _) :: stack)
          case (false, true) =>
            // flatx == x but reassociated
            def cont(resty: Doc) = {
              val first = Concat(flatx, Concat(flatSep, resty))
              def second = Concat(flatx, Concat(sep, cheatRec(y, tail).get /* ??? */))
              // note that first != second
              Union(first, () => second)
            }
            fillRec(flaty, tail, (cont _) :: stack)
          case (false, false) =>
            // flaty == y but reassociated
            // flatx == x but reassociated
            fillRec(flaty, tail, { d: Doc => Concat(flatx, Concat(sepGroup, d)) } :: stack)
        }
    }

    def cheatRec(x: Doc, lst: List[Doc]): Option[Doc] =
      fillRec(x, lst, Nil)

    ds.toList match {
      case Nil => Some(Empty)
      case h :: tail => fillRec(h, tail, Nil)
    }
  }

  /**
   * Combine documents, using the given associative function.
   *
   * The function `fn` must be associative. That is, the expression
   * `fn(x, fn(y, z))` must be equivalent to `fn(fn(x, y), z)`.
   *
   * In practice this method builds documents from the right, so that
   * the resulting concatenations are all right-associated.
   */
  def foldDocs(ds: Iterable[Doc])(fn: (Doc, Doc) => Doc): Doc =
    if (ds.isEmpty) Doc.empty else {
      val xs = ds.toArray
      var d = xs(xs.length - 1)
      var i = xs.length - 2
      while (i >= 0) {
        d = fn(xs(i), d)
        i -= 1
      }
      d
    }

  /**
   * Split the given text into words (separated by whitespace), and
   * then join those words with a space or newline.
   *
   * This produces text which will wrap naturally at line boundaries,
   * producing a block of text.
   *
   * `paragraph` is an alias for Doc.split(s), which uses its default
   * arguments to split on whitespace and to rejoin the documents with
   * `Doc.lineOrSpace`.
   */
  def paragraph(s: String): Doc =
    split(s)

  /**
   * Concatenate the given documents together, delimited by the given
   * separator.
   *
   * For example, `intercalate(comma, List(a, b, c))` is equivalent to
   * `a + comma + b + comma + b`.
   */
  def intercalate(sep: Doc, ds: Iterable[Doc]): Doc =
    if (sep.isEmpty) foldDocs(ds)(Concat(_, _))
    else foldDocs(ds) { (a, b) => Concat(a, Concat(sep, b)) }

  /**
   * Concatenate the given documents together.
   *
   * `cat(ds)` is equivalent to `ds.foldLeft(empty)(_ + _)`
   */
  def cat(ds: Iterable[Doc]): Doc =
    intercalate(empty, ds)

  /**
   * Concatenate the given documents together, delimited by spaces.
   */
  def spread(ds: Iterable[Doc]): Doc =
    intercalate(space, ds)

  /**
   * Concatenate the given documents together, delimited by newlines.
   */
  def stack(ds: Iterable[Doc]): Doc =
    intercalate(line, ds)

  /**
   * A simple table which is the same as:
   * tabulate("", ' ', "", kv)
   *
   * or, no right separator and a space as the fill
   */
  def tabulate(kv: List[(String, Doc)]): Doc =
    tabulate(' ', "", kv)

  /**
   * build a table with the strings left aligned and
   * the Docs starting in the column after the longest string.
   * The Docs on the right are rendered aligned after the rightSep
   *
   * @param fill the character used to fill the columns to make the values aligned (i.e. ' ' or '.')
   * @param rightSep a string append left to the left of the value. Intended for use with bullets on values
   * @param rows a List of key, value pairs to put in a table.
   */
  def tabulate(fill: Char, rightSep: String, rows: Iterable[(String, Doc)]): Doc =
    if (rows.isEmpty) empty
    else {
      val fills = rows.iterator.map(_._1.length).max
      val rightD = Doc.text(rightSep)
      def keyToDoc(s: String): Doc = Doc.text(s) + Doc.char(fill).repeat(fills - s.length) + rightD
      intercalate(line, rows.map { case (k, v) => keyToDoc(k) + v.aligned })
    }
}
