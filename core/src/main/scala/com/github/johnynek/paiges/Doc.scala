package com.github.johnynek.paiges

import java.io.PrintWriter
import java.lang.StringBuilder

import scala.annotation.tailrec

/**
 * implementation of Wadler's classic "A Prettier Printer"
 *
 * http://homepages.inf.ed.ac.uk/wadler/papers/prettier/prettier.pdf
 */
final class Doc private (private val inner: Node) extends Ordered[Doc] with Serializable {
  import Doc.doc

  /**
   * Concatenate with no space
   * We use `+:` for right associativity which is more efficient.
   */
  def +:(that: Doc): Doc = that.concat(this)

  /**
   * Convert the String to a Doc and concat
   */
  def :+(text: String): Doc =
    concat(Doc.text(text))

  /**
   * Convert the String to a Doc and concat
   */
  def +:(init: String): Doc =
    Doc.text(init).concat(this)

  /**
   * Repeat this item count times
   */
  def *(count: Int): Doc =
    repeat(count)

  /**
   * synonym for line. Concatenate with a newline between
   */
  def /(that: Doc): Doc = line(that)
  /**
   * synonym for line. Concatenate with a newline between
   */
  def /(str: String): Doc = line(Doc.text(str))

  /**
   * Concatenate with no space
   */
  def concat(that: Doc): Doc =
    doc(Node.Concat(inner, that.inner))

  /**
   * Return all the possible Docs WITHOUT any union
   * nodes
   */
  def deunioned: Stream[Doc] =
    Tree.deunioned(Tree.toRenderedTree(inner)).map(doc)

  /**
   * Convert all lines to spaces and
   * return a Node of only Empty, Text, and Concat
   * nodes
   */
  def flatten: Doc =
    doc(Node.flatten(inner))

  /**
   * If the doc has no Line nodes, return None, else
   * flatten the document.
   */
  def flattenOption: Option[Doc] =
    Node.flattenOption(inner).map(doc)

  /**
   * Consider this item as a group where before rendering
   * we can replace newlines with a space if it can fit
   */
  def group: Doc =
    Node.flattenOption(inner) match {
      case Some(flat) =>
        // todo, flat could already be in the doc
        // set. This complicates comparisons
        doc(Node.Union(flat, () => inner))
      case None => this
    }

  /**
   * a Doc is empty if all renderings will be the empty
   * string
   */
  def isEmpty: Boolean = Node.isEmpty(inner)

  /**
   * Is there a width such that this and that doc
   * would render the same?
   */
  def isSubDocOf(that: Doc): Boolean =
    Tree.isSubNode(Tree.toRenderedTree(inner), Tree.toRenderedTree(that.inner))

  /**
   * Repeat the given document a number of times
   *
   * if (count <= 0) we return empty
   */
  def repeat(count: Int): Doc =
    doc(Node.repeat(inner, count))

  /**
   * Concatenate with a space
   */
  def space(that: Doc): Doc =
    this +: Doc.space +: that

  /**
   * Concatenate with a space
   */
  def space(that: String): Doc =
    this +: Doc.space +: Doc.text(that)

  /**
   * Concatenate with a newline
   */
  def line(that: Doc): Doc =
    this +: Doc.line +: that

  /**
   * Concatenate with a newline
   */
  def line(str: String): Doc =
    line(Doc.text(str))

  /**
   * Use a space if we can fit, else use a newline
   */
  def spaceOrLine(that: Doc): Doc =
    this +: (Doc.spaceOrLine) +: that

  /**
   * Use a space if we can fit, else use a newline
   */
  def spaceOrLine(that: String): Doc =
    spaceOrLine(Doc.text(that))

  /**
   * What is the largest width that is relevant
   * for this Doc (all internal branches are
   * the same at this width and greater)
   *
   * val m = maxWidth(d)
   * render(d, m) == render(d, n)
   * for all n >= m
   *
   */
  def maxWidth: Int = Rendered.maxWidth(inner)

  /**
   * Convert the Doc to a String with a desired maximum line
   */
  def render(maxLine: Int): String = {
    val bldr = new StringBuilder
    renderStream(maxLine).foreach(bldr.append(_))
    bldr.toString
  }

  /**
   * Render into a stream of strings which should be
   * concatenated all together to form the final
   * document
   */
  def renderStream(width: Int): Stream[String] =
    Rendered.best(width, inner).map(_.str)

  /**
   * nest replaces new lines with a newline plus this amount
   * of indentation. If there are no new lines, this is a no-op
   */
  def nest(amount: Int): Doc =
    doc(Node.Nest(amount, inner))

  /**
   * using a given max-Line write to the print writer
   */
  def writeTo(maxLine: Int, pw: PrintWriter): Unit =
    renderStream(maxLine).foreach(pw.append(_))

  override lazy val hashCode: Int = {
    /**
     * Always go left to avoid triggering
     * the lazy fill evaluation
     */
    @inline def hash(curr: Int, c: Char): Int =
      curr * 1500450271 + c.toInt

    @tailrec def shash(n: Int, s: String, i: Int): Int =
      if (i < s.length) shash(hash(n, s.charAt(i)), s, i + 1) else n

    renderStream(Int.MaxValue).foldLeft(0xdead60d5) {
      case (n, s) => shash(n, s, 0)
    }
  }

  override def toString: String = {
    var remaining = 40
    val bldr = new java.lang.StringBuilder
    val r = renderStream(80)
      .filter(_.nonEmpty)
      .takeWhile { str =>
        if (remaining == 0) {
          // signal that we have more to write
          bldr.append("...")
          remaining = -1
          false
        }
        else {
          val tooMuch = remaining < str.length
          val substring = if (tooMuch) str.substring(0, remaining) else str
          remaining -= substring.length
          bldr.append(substring)
          if (tooMuch) { bldr.append("...") }
          true
        }
      }
      .foreach(_ => ()) // side effect already happened

    s"Doc(${bldr.toString})##$hashCode"
  }

  override def equals(that: Any): Boolean = that match {
    case d: Doc => (this eq d) || (compare(d) == 0)
    case _ => false
  }

  /**
   * Compare two Doc values by finding the first
   */
  def compare(that: Doc): Int =
    Tree.compareTree(toRenderedTree, that.toRenderedTree)

  private[paiges] def toRenderedTree: Tree.RenderedTree =
    Tree.toRenderedTree(inner)
}

object Doc {
  import Node._

  private def doc(d: Node): Doc = new Doc(d)

  private[this] val maxSpaceTable = 100
  private[this] val spaceArray: Array[Doc] =
    (1 to maxSpaceTable)
      .map { s => doc(Node.spaces(s)) }
      .toArray

  def spaces(n: Int): Doc =
    if (n == 0) empty
    else if (n <= maxSpaceTable) spaceArray(n - 1)
    else doc(Node.spaces(n))

  val space: Doc = spaceArray(0)

  val comma: Doc = doc(Text(","))
  val line: Doc = doc(Line)
  val spaceOrLine: Doc = doc(Union(space.inner, () => Line))
  val empty: Doc = doc(Empty)

  implicit val docOrdering: Ordering[Doc] =
    new Ordering[Doc] {
      def compare(x: Doc, y: Doc): Int = x compare y
    }

  /**
   * Convert a string to text. Note all `\n` are converted
   * to logical entities that the rendering is aware of.
   */
  def text(str: String): Doc = {
    if (str == "\n") line
    else {
      def node(s: String): Node =
        if (str == "") Empty
        else if (str == " ") space.inner
        else if (str == "  ") spaces(2).inner
        else Text(s)

      doc(str.split("\n", -1)
        .iterator
        .map(node)
        .reduce { (d, str) =>
          Concat(d, Concat(Line, str))
        })
      }
    }

  /**
   * Convert a T to a Doc using toString. Note that "\n" is
   * converted to a Line and is treated specially
   * by this code
   */
  def str[T](t: T): Doc =
    text(t.toString)

  /**
   * Try to make this left.space(middle).space(right)
   * but grouped with an indentation of 2 on this if we
   * use newline
   */
  def bracket(left: Doc, middle: Doc, right: Doc): Doc =
    (left +: ((Doc.line +: middle).nest(2) +: (Doc.line +: right))).group

  /*
   * A variant of fillwords is fill , which collapses a list of documents into a
   * document.  It puts a space between two documents when this leads to
   * reasonable layout, and a newline otherwise
   */
  def fill(sep: Doc, ds: Iterable[Doc]): Doc = {
    val sepNode = sep.inner

    def space(n1: Node, n2: Node): Node =
      Concat(n1, Concat(Node.space, n2))

    def line(n1: Node, n2: Node): Node =
      Concat(n1, Concat(Node.Line, n2))

    def spaceOrLine(n1: Node, n2: Node): Node =
      Concat(n1, Concat(Doc.spaceOrLine.inner, n2))

    def fillRec(lst: List[Node]): Node = lst match {
      case Nil => Empty
      case x :: Nil => x
      case x :: y :: tail =>
        /**
         * The cost of this algorithm c(n) for list of size n.
         * note that c(n) = 2 * c(n-1) + k
         * for some constant.
         * so, c(n) - c(n-1) = c(n-1) + k
         * which means that
         * c(n) = (0 until n).map(c(_)).sum + nk
         *
         * which is exponential in n (O(2^n))
         *
         * making the second parameter in the union lazy would fix this.
         * that seems an expensive fix for a single combinator. Maybe
         * there is an alternative way to express this that is not
         * exponential.
         *
         * On top of this difficulty, this formulation creates
         * Union nodes that violate the invariant that Union(a, b)
         * means a == flatten(b). It still has flatten(a) == flatten(b),
         * however. This fact seems to complicate comparison of Doc
         * which is valuable.
         */
        val xsep = if (sepNode != Empty) Node.Concat(x, sepNode) else x
        (Node.flattenOption(xsep), Node.flattenOption(y)) match {
          case (Some(flatx), Some(flaty)) =>
            val resty = fillRec(flaty :: tail)
            val first = space(flatx, resty)
            def second = line(xsep, fillRec(y :: tail))
            // note that first != second
            Union(first, () => second)
          case (Some(flatx), None) =>
            val resty = fillRec(y :: tail)
            val first = space(flatx, resty)
            def second = line(xsep, resty)
            // note that first != second
            Union(first, () => second)
          case (None, Some(flaty)) =>
            val resty = fillRec(flaty :: tail)
            val first = space(xsep, resty)
            def second = line(xsep, fillRec(y :: tail))
            // note that first != second
            Union(first, () => second)
          case (None, None) =>
            val resty = fillRec(y :: tail)
            spaceOrLine(xsep, resty)
        }
    }
    doc(fillRec(ds.iterator.map(_.inner).toList))
  }

  /**
   * Convert assume we can replace space
   * with newline, but newlines are preserved
   */
  def fillWords(s: String): Doc =
    foldDoc(s.split(" ", -1).map(text))(_.spaceOrLine(_))

  /**
   * split on `\s+` and foldDoc with spaceOrLine
   */
  def paragraph(s: String): Doc =
    foldDoc(s.split("\\s+", -1).map(text))(_.spaceOrLine(_))


  def foldDoc(ds: Iterable[Doc])(fn: (Doc, Doc) => Doc): Doc =
    ds.reduceOption(fn).getOrElse(empty)

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

}
