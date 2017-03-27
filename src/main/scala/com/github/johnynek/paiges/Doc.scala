package com.github.johnynek.paiges

import java.lang.StringBuilder
import java.io.PrintWriter

/**
 * implementation of Wadler's classic "A Prettier Printer"
 *
 * http://homepages.inf.ed.ac.uk/wadler/papers/prettier/prettier.pdf
 */
sealed abstract class Doc {
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
    (left ++ (Doc.line ++ this).nest(2) ++ (Doc.line ++ right)).group

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
  val space: Doc = Text(" ")
  val line: Doc = Line
  val spaceOrLine: Doc = Union(space, line)
  val empty: Doc = Empty

  def spaces(n: Int): Doc =
    if (n < 1) Empty
    else Text(" " * n)

  /**
   * An implicit to use strings as Docs in call-sites
   */
  implicit def fromString(s: String): Doc = apply(s)

  /**
   * Convert a String to a Doc. Note that "\n" is
   * converted to a Line and is treated specially
   * by this code
   */
  def apply(str: String): Doc = {
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
  }

  /*
   * A variant of fillwords is fill , which collapses a list of documents into a
   * document.  It puts a space between two documents when this leads to
   * reasonable layout, and a newline otherwise
   */
  def fill(ds: List[Doc]): Doc = ds match {
    case Nil => Empty
    case x :: Nil => x
    case x :: y :: tail =>
      val first = flatten(x).space(fill(flatten(y) :: tail))
      val second = x.line(fill(y :: tail))
      Union(first, second)
  }

  /**
   * Convert assume we can replace space
   * with newline
   */
  def fillWords(s: String): Doc =
    foldDoc(s.split(" ", -1).map(apply))(_.spaceOrLine(_))

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

  def render(d: Doc, width: Int): String =
    Doc2.layout(Doc2.best(width, d))

  def write(d: Doc, width: Int, pw: PrintWriter): Unit =
    Doc2.write(Doc2.best(width, d), pw)

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
  private sealed abstract class Doc2
  private object Doc2 {
    @annotation.tailrec
    def fits(width: Int, d: Doc2): Boolean = (width, d) match {
      case (w, _) if w < 0 => false
      case (w, Empty2) => true
      case (w, Line2(_, _)) => true
      case (w, Text2(s, d)) => fits(w - s.length, d)
    }

    def layout(doc: Doc2): String = {
      val bldr = new StringBuilder
      @annotation.tailrec
      def go(d: Doc2): Unit = d match {
        case Empty2 => ()
        case Line2(indent, next) =>
          bldr.append('\n')
          if (indent > 0) {
            bldr.append(" " * indent)
          }
          go(next)
        case Text2(s, next) =>
          bldr.append(s)
          go(next)
      }
      go(doc)
      bldr.toString
    }

    def write(doc: Doc2, pw: PrintWriter): Unit = {
      @annotation.tailrec
      def go(d: Doc2): Unit = d match {
        case Empty2 => ()
        case Line2(indent, next) =>
          pw.append('\n')
          if (indent > 0) {
            pw.append(" " * indent)
          }
          go(next)
        case Text2(s, next) =>
          pw.append(s)
          go(next)
      }
      go(doc)
    }

    def best(w: Int, d: Doc): Doc2 = {

      @annotation.tailrec
      def loop(w: Int, k: Int, lst: List[(Int, Doc)], stack: List[Doc2 => Doc2]): Doc2 = lst match {
        case Nil => Empty2
          @annotation.tailrec
          def unwind(d: Doc2, s: List[Doc2 => Doc2]): Doc2 = s match {
            case Nil => d
            case h :: tail => unwind(h(d), tail)
          }
          unwind(Empty2, stack)
        case (i, Empty) :: z => loop(w, k, z, stack)
        case (i, Concat(a, b)) :: z => loop(w, k, (i, a) :: (i, b) :: z, stack)
        case (i, Nest(j, d)) :: z => loop(w, k, ((i + j), d) :: z, stack)
        case (i, Text(s)) :: z => loop(w, k + s.length, z, { d: Doc2 => Text2(s, d) } :: stack)
        case (i, Line) :: z => loop(w, i, z, { d: Doc2 => Line2(i, d) } :: stack)
        case (i, Union(x, y)) :: z =>
          val first = cheat(w, k, (i, x) :: z, stack)
          if (fits(w - k, first)) first
          else loop(w, k, (i, y) :: z, stack)
      }
      // This is to cheat on tailrec
      def cheat(w: Int, k: Int, lst: List[(Int, Doc)], stack: List[Doc2 => Doc2]): Doc2 =
        loop(w, k, lst, stack)

      loop(w, 0, (0, d) :: Nil, Nil)
    }

    case object Empty2 extends Doc2
    case class Line2(indent: Int, next: Doc2) extends Doc2
    case class Text2(str: String, next: Doc2) extends Doc2
  }

  private case object Empty extends Doc
  private case class Concat(a: Doc, b: Doc) extends Doc
  private case class Nest(indent: Int, doc: Doc) extends Doc
  private case class Text(str: String) extends Doc
  private case object Line extends Doc
  private case class Union(a: Doc, b: Doc) extends Doc

}
