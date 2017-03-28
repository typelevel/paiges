package com.github.johnynek.paiges

import java.lang.StringBuilder
import java.io.PrintWriter

import scala.annotation.tailrec

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
    D2.layout(D2.best(width, d), new StringBuilder)(_ append _, _.toString)

  def write(d: Doc, width: Int, pw: PrintWriter): Unit =
    D2.layout(D2.best(width, d), pw)(_ append _, _ => ())

  private def flatten(doc: Doc): Doc =
    doc match {
      case Empty => Empty
      case Concat(a, b) => Concat(flatten(a), flatten(b))
      case Nest(i, d) => Nest(i, flatten(d))
      case str@Text(_) => str
      case Line => space
      case Union(a, _) => a
    }

  private[paiges] sealed abstract class D2

  private[paiges] object D2 {

    case class Line(indent: Int) extends D2
    case class Text(s: String) extends D2

    @tailrec def fits(width: Int, toks: Stream[D2]): Boolean =
      toks match {
        case _ if width < 0 => false
        case D2.Text(s) #:: rest => fits(width - s.length, rest)
        case _ => true
      }

    @tailrec def layout[C, R](toks: Stream[D2], context: C)(write: (C, String) => Unit, finish: C => R): R =
      toks match {
        case D2.Text(s) #:: tail =>
          write(context, s)
          layout(tail, context)(write, finish)
        case D2.Line(indent) #:: tail =>
          write(context, "\n")
          if (indent > 0) write(context, " " * indent)
          layout(tail, context)(write, finish)
        case _ =>
          finish(context)
      }

    def best(w: Int, d: Doc): Stream[D2] = {
      def recur(w: Int, k: Int, lst: List[(Int, Doc)]): Stream[D2] =
        lst match {
          case Nil =>
            Stream.empty
          case (i, doc) :: rest =>
            doc match {
              case Doc.Empty => recur(w, k, rest)
              case Doc.Concat(d1, d2) => recur(w, k, (i, d1) :: (i, d2) :: rest)
              case Doc.Nest(j, d) => recur(w, k, ((i + j), d) :: rest)
              case Doc.Text(s) => D2.Text(s) #:: recur(w, k + s.length, rest)
              case Doc.Line => D2.Line(i) #:: recur(w, i, rest)
              case Doc.Union(d1, d2) =>
                val first = recur(w, k, (i, d1) :: rest)
                if (fits(w - k, first)) first else recur(w, k, (i, d2) :: rest)
            }
        }

      recur(w, 0, (0, d) :: Nil)
    }
  }

  private case object Empty extends Doc
  private case class Concat(a: Doc, b: Doc) extends Doc
  private case class Nest(indent: Int, doc: Doc) extends Doc
  private case class Text(str: String) extends Doc
  private case object Line extends Doc
  private case class Union(a: Doc, b: Doc) extends Doc
}
