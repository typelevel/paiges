package com.github.johnynek.paiges

import java.io.PrintWriter
import java.lang.StringBuilder
import scala.language.implicitConversions // implicit String to Doc

/**
 * implementation of Wadler's classic "A Prettier Printer"
 *
 * http://homepages.inf.ed.ac.uk/wadler/papers/prettier/prettier.pdf
 */
sealed abstract class Doc extends Serializable {
  /**
   * Concatenate with no space
   */
  def ++(that: Doc): Doc = Doc.concat(this, that)

  /**
   * synonym for line. Concatenate with a newline between
   */
  def /(that: Doc): Doc = line(that)

  /**
   * Try to make this left.space(this).space(right)
   * but grouped with an indentation of 2 on this if we
   * use newline
   */
  def bracketBy(left: Doc, right: Doc): Doc =
    (left ++ ((Doc.line ++ this).nest(2) ++ (Doc.line ++ right))).group

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
   * Concatenate with a space
   */
  def space(that: Doc): Doc = this ++ Doc.space ++ that

  /**
   * Concatenate with a newline
   */
  def line(that: Doc): Doc = this ++ Doc.line ++ that

  /**
   * Use a space if we can fit, else use a newline
   */
  def spaceOrLine(that: Doc): Doc = this ++ (Doc.spaceOrLine) ++ that

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

  override def toString: String = "Doc(...)"
}

object Doc {
  private[this] val maxSpaceTable = 20
  private[this] val spaceArray: Array[Text] =
    (1 to maxSpaceTable).map { i => Text(" " * i) }.toArray

  def spaces(n: Int): Doc =
    if (n < 1) Empty
    else if (n <= maxSpaceTable) spaceArray(n - 1)
    else Text(" " * n)

  val space: Doc = spaceArray(0)

  val comma: Doc = Doc(",")
  val line: Doc = Line
  val spaceOrLine: Doc = Union(space, line)
  val empty: Doc = Empty

  /**
   * An implicit to use strings as Docs in call-sites
   */
  implicit def fromString(str: String): Doc =
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
  def apply[T](t: T): Doc =
    fromString(t.toString)

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
        val xsep = x ++ sep
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
    foldDoc(s.split(" ", -1).map(apply))(_.spaceOrLine(_))

  /**
   * split on `\s+` and foldDoc with spaceOrLine
   */
  def paragraph(s: String): Doc =
    foldDoc(s.split("\\s+", -1).map(apply))(_.spaceOrLine(_))

  def concat(a: Doc, b: Doc): Doc = Concat(a, b)

  def foldDoc(ds: Iterable[Doc])(fn: (Doc, Doc) => Doc): Doc =
    ds.reduceOption(fn).getOrElse(Empty)

  def intercalate(d: Doc, ds: Iterable[Doc]): Doc =
    foldDoc(ds) { (a, b) => a ++ (d ++ b) }

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
    @annotation.tailrec
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
      @annotation.tailrec
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

  private case object Empty extends Doc
  private case class Concat(a: Doc, b: Doc) extends Doc
  private case class Nest(indent: Int, doc: Doc) extends Doc
  private case class Text(str: String) extends Doc
  private case object Line extends Doc
  private case class Union(a: Doc, b: Doc) extends Doc
}
