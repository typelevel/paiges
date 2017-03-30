package com.github.johnynek.paiges

import java.io.PrintWriter
import java.lang.StringBuilder

import scala.annotation.tailrec

/**
 * implementation of Wadler's classic "A Prettier Printer"
 *
 * http://homepages.inf.ed.ac.uk/wadler/papers/prettier/prettier.pdf
 */
sealed abstract class Doc extends Serializable {
  /**
   * Concatenate with no space
   * We use `+:` for right associativity which is more efficient.
   */
  def +:(that: Doc): Doc = Doc.concat(that, this)

  /**
   * Convert the String to a Doc and concat
   */
  def :+(text: String): Doc =
    Doc.concat(this, Doc.text(text))

  /**
   * Convert the String to a Doc and concat
   */
  def +:(init: String): Doc =
    Doc.concat(Doc.text(init), this)

  /**
   * synonym for line. Concatenate with a newline between
   */
  def /(that: Doc): Doc = line(that)
  /**
   * synonym for line. Concatenate with a newline between
   */
  def /(str: String): Doc = line(Doc.text(str))

  /**
   * Try to make this left.space(this).space(right)
   * but grouped with an indentation of 2 on this if we
   * use newline
   */
  def bracketBy(left: Doc, right: Doc): Doc =
    (left +: ((Doc.line +: this).nest(2) +: (Doc.line +: right))).group

  /**
   * Concatenate with no space
   */
  def concat(that: Doc): Doc = Doc.concat(this, that)

  /**
   * Consider this item as a group where before rendering
   * we can replace newlines with a space if it can fit
   */
  def group: Doc = Doc.group(this)

  /**
   * a Doc is empty if all renderings will be the empty
   * string
   */
  def isEmpty: Boolean = Doc.isEmpty(this)

  /**
   * Concatenate with a space
   */
  def space(that: Doc): Doc = this +: Doc.space +: that

  /**
   * Concatenate with a space
   */
  def space(that: String): Doc = this +: Doc.space +: Doc.text(that)

  /**
   * Concatenate with a newline
   */
  def line(that: Doc): Doc = this +: Doc.line +: that

  /**
   * Concatenate with a newline
   */
  def line(str: String): Doc = line(Doc.text(str))

  /**
   * Use a space if we can fit, else use a newline
   */
  def spaceOrLine(that: Doc): Doc = this +: (Doc.spaceOrLine) +: that

  /**
   * Use a space if we can fit, else use a newline
   */
  def spaceOrLine(that: String): Doc = spaceOrLine(Doc.text(that))

  /**
   * Convert the Doc to a String with a desired maximum line
   */
  def render(maxLine: Int): String = Doc.render(this, maxLine)

  /**
   * Render into a stream of strings which should be
   * concatenated all together to form the final
   * document
   */
  def renderStream(maxLine: Int): Stream[String] =
    Doc.renderStream(this, maxLine)

  /**
   * nest replaces new lines with a newline plus this amount
   * of indentation. If there are no new lines, this is a no-op
   */
  def nest(amount: Int): Doc = Doc.Nest(amount, this)

  /**
   * using a given max-Line write to the print writer
   */
  def writeTo(maxLine: Int, pw: PrintWriter): Unit =
    Doc.write(this, maxLine, pw)

  override lazy val hashCode: Int = {
    import Doc.Tok
    @inline def hash(curr: Int, c: Char): Int =
      curr * 1500450271 + c.toInt
    @tailrec def shash(n: Int, s: String, i: Int): Int =
      if (i < s.length) shash(hash(n, s.charAt(i)), s, i + 1) else n
    Tok.fromDoc(this).foldLeft(0xdead60d5) {
      case (n, Tok.Line(i)) => hash(n, '\n') + (1500450271 * i)
      case (n, Tok.Text(s)) => shash(n, s, 0)
    }
  }

  override def toString: String = "Doc(...)"

  /**
   * Compare two Doc values; we expect that the comparison result
   * should be valid across all possible rendering widths.
   *
   * This method *may* be overly conservative -- it assumes that
   * documents need to have basically the same union structure when it
   * comes to newlines on the "right-hand side". This may end up being
   * a bit too cautious (i.e. some equal documents will be deemed
   * unequal).
   */
  def compare(that: Doc): Int = {
    import Doc.Tok

    def extract(s: String, part: String): Option[Tok.Text] =
      if (s.startsWith(part)) Some(Tok.Text(s.substring(part.length)))
      else None

    @tailrec def loop(xs: Stream[Tok], ys: Stream[Tok]): Int =
      (xs, ys) match {
        case (Stream.Empty, Stream.Empty) => 0
        case (Stream.Empty, _) => -1
        case (_, Stream.Empty) => 1
        case (Tok.Line(n1) #:: xs, Tok.Line(n2) #:: ys) =>
          val c = n1 compare n2
          if (c == 0) loop(xs, ys) else c
        case (Tok.Line(_) #:: _, _) => -1
        case (_, Tok.Line(_) #:: _) => 1
        case (Tok.Text(s1) #:: xs, Tok.Text(s2) #:: ys) =>
          if (s1.length == s2.length) {
            val c = s1 compare s2
            if (c == 0) loop(xs, ys) else c
          } else if (s1.length < s2.length) {
            extract(s2, s1) match {
              case Some(t) => loop(xs, t #:: ys)
              case None => s1 compare s2
            }
          } else {
            extract(s1, s2) match {
              case Some(t) => loop(t #:: xs, ys)
              case None => s1 compare s2
            }
          }
      }

    loop(Tok.fromDoc(this), Tok.fromDoc(that))
  }
}

object Doc {

  private case object Empty extends Doc

  /**
   * Represents a single, literal newline.
   */
  private case object Line extends Doc

  /**
   * The string must not be empty, and may not contain newlines.
   */
  private case class Text(str: String) extends Doc

  private case class Concat(a: Doc, b: Doc) extends Doc

  private case class Nest(indent: Int, doc: Doc) extends Doc

  /**
   * There is an additional invariant on Union that
   * a == flatten(b). By construction all have this
   * property, but this is why we don't expose Union
   * but only .group
   */
  private case class Union(a: Doc, b: Doc) extends Doc

  private[this] val maxSpaceTable = 20
  private[this] val spaceArray: Array[Text] =
    (1 to maxSpaceTable).map { i => Text(" " * i) }.toArray

  def spaces(n: Int): Doc =
    if (n < 1) Empty
    else if (n <= maxSpaceTable) spaceArray(n - 1)
    else Text(" " * n)

  val space: Doc = spaceArray(0)

  val comma: Doc = Doc.text(",")
  val line: Doc = Line
  val spaceOrLine: Doc = Union(space, line)
  val empty: Doc = Empty

  implicit val docOrdering: Ordering[Doc] =
    new Ordering[Doc] {
      def compare(x: Doc, y: Doc): Int = x compare y
    }

  /**
   * Convert a string to text. Note all `\n` are converted
   * to logical entities that the rendering is aware of.
   */
  def text(str: String): Doc =
    if (str == "") empty
    else if (str == " ") space
    else if (str == "\n") line
    else {
      str.split("\n", -1)
        .iterator
        .map(Text(_): Doc)
        .reduce { (d, str) =>
          Concat(d, Concat(Line, str))
        }
    }

  /**
   * Convert a T to a Doc using toString. Note that "\n" is
   * converted to a Line and is treated specially
   * by this code
   */
  def str[T](t: T): Doc =
    text(t.toString)

  def isEmpty(d: Doc): Boolean = {
    @annotation.tailrec
    def loop(doc: Doc, stack: List[Doc]): Boolean = doc match {
      case Empty => stack match {
        case d1 :: tail => loop(d1, tail)
        case Nil => true
      }
      case Concat(a, b) => loop(a, b :: stack)
      case Nest(i, d) => loop(d, stack)
      case Text(s) =>
        // shouldn't be empty by construction, but defensive
        s.isEmpty && loop(Empty, stack)
      case Line => false
      case Union(_, unflatten) => loop(unflatten, stack)
    }
    loop(d, Nil)
  }

  /*
   * A variant of fillwords is fill , which collapses a list of documents into a
   * document.  It puts a space between two documents when this leads to
   * reasonable layout, and a newline otherwise
   */
  def fill(sep: Doc, ds: Iterable[Doc]): Doc = {
    def fillRec(lst: List[Doc]): Doc = lst match {
      case Nil => Empty
      case x :: Nil => x
      case x :: y :: tail =>
        val xsep = x +: sep
        val first = flatten(xsep).space(fillRec(flatten(y) :: tail))
        val second = xsep.line(fillRec(y :: tail))
        Union(first, second)
    }
    fillRec(ds.toList)
  }

  /**
   * Convert assume we can replace space
   * with newline
   */
  def fillWords(s: String): Doc =
    foldDoc(s.split(" ", -1).map(text))(_.spaceOrLine(_))

  /**
   * split on `\s+` and foldDoc with spaceOrLine
   */
  def paragraph(s: String): Doc =
    foldDoc(s.split("\\s+", -1).map(text))(_.spaceOrLine(_))

  def concat(a: Doc, b: Doc): Doc = Concat(a, b)

  def foldDoc(ds: Iterable[Doc])(fn: (Doc, Doc) => Doc): Doc =
    ds.reduceOption(fn).getOrElse(Empty)

  def intercalate(d: Doc, ds: Iterable[Doc]): Doc =
    foldDoc(ds) { (a, b) => a +: (d +: b) }

  /**
   * intercalate with a space
   */
  def spread(ds: Iterable[Doc]): Doc = intercalate(space, ds)
  /**
   * intercalate with a newline
   */
  def stack(ds: Iterable[Doc]): Doc = intercalate(line, ds)

  /**
   * This returns a new doc where we can replace line with space
   * to fit into a line
   */
  def group(doc: Doc): Doc = Union(flatten(doc), doc)

  def renderStream(d: Doc, width: Int): Stream[String] =
    Doc2.best(width, d).map(_.str)

  def render(d: Doc, width: Int): String = {
    val bldr = new StringBuilder
    renderStream(d, width).foreach(bldr.append(_))
    bldr.toString
  }

  def write(d: Doc, width: Int, pw: PrintWriter): Unit = {
    renderStream(d, width).foreach(pw.append(_))
  }

  private def flatten(doc: Doc): Doc = doc match {
    case Empty => Empty
    case Concat(a, b) => Concat(flatten(a), flatten(b))
    case Nest(i, d) => Nest(i, flatten(d))
    case str@Text(_) => str
    case Line => space
    case Union(a, _) => a
  }


  /**
   * This is the second ADT introduced for efficiency reasons
   */
  sealed abstract class Doc2 {
    def str: String
  }

  private object Doc2 {
    @tailrec
    def fits(width: Int, d: Stream[Doc2]): Boolean =
      (width >= 0) && {
        if (d.isEmpty) true
        else d.head match {
          case Line2(_) => true
          case Text2(s) => fits(width - s.length, d.tail)
        }
      }

    def best(w: Int, d: Doc): Stream[Doc2] = {

      /**
       * This is not really tail recursive but many branches are, so
       * we cheat below in non-tail positions
       */
      @tailrec
      def loop(w: Int, k: Int, lst: List[(Int, Doc)]): Stream[Doc2] = lst match {
        case Nil => Stream.empty[Doc2]
        case (i, Empty) :: z => loop(w, k, z)
        case (i, Concat(a, b)) :: z => loop(w, k, (i, a) :: (i, b) :: z)
        case (i, Nest(j, d)) :: z => loop(w, k, ((i + j), d) :: z)
        case (i, Text(s)) :: z => Text2(s) #:: cheat(w, k + s.length, z)
        case (i, Line) :: z => Line2(i) #:: cheat(w, i, z)
        case (i, Union(x, y)) :: z =>
          val first = cheat(w, k, (i, x) :: z)
          if (fits(w - k, first)) first
          else loop(w, k, (i, y) :: z)
      }

      def cheat(w: Int, k: Int, lst: List[(Int, Doc)]): Stream[Doc2] =
        loop(w, k, lst)

      loop(w, 0, (0, d) :: Nil)
    }

    private[this] val indentMax = 100
    private[this] val indentTable: Array[String] =
      (0 to indentMax).iterator
        .map(makeIndentStr)
        .toArray

    def makeIndentStr(i: Int): String = "\n" + (" " * i)

    def lineToStr(indent: Int): String =
      if (indent <= indentMax) indentTable(indent)
      else makeIndentStr(indent)

    case class Text2(str: String) extends Doc2
    case class Line2(indent: Int) extends Doc2 {
      def str: String = lineToStr(indent)
    }
  }

  /**
   * Used internally by `Doc#compare`.
   */
  private sealed abstract class Tok

  private object Tok {

    /**
     * Create a stream of Tok values from a Doc.
     *
     * Tok resembles Doc2, but with differences. It is designed to
     * assist in comparisons and equality checks, not in rendering.
     *
     * We track how deeply nested unions are, and "save" that
     * information in all the newlines that we find in those delimited
     * regions.
     */
    def fromDoc(d: Doc): Stream[Tok] =
      d match {
        case Doc.Empty => Stream.empty
        case Doc.Line => Line(0) #:: Stream.empty
        case Doc.Text(s) => Tok.Text(s) #:: Stream.empty
        case Doc.Concat(x, y) => fromDoc(x) #::: fromDoc(y)
        case Doc.Nest(i, d) => fromDoc(d).flatMap {
          case ln @ Tok.Line(_) => ln :: Tok.Text(" " * i) :: Nil
          case x => x :: Nil
        }
        case Doc.Union(_, d) => fromDoc(d).map {
          case Tok.Line(n) => Tok.Line(n + 1)
          case x => x
        }
      }

    /**
     * Non-newline text, the normal case.
     */
    case class Text(s: String) extends Tok

    /**
     * Newline, together with how many levels of unions it contains.
     */
    case class Line(u: Int) extends Tok
  }
}
