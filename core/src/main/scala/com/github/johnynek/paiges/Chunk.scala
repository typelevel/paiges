package com.github.johnynek.paiges

import scala.annotation.tailrec

/**
 * This is the second ADT introduced for efficiency reasons
 */
sealed abstract class Chunk {
  def str: String
}

private object Chunk {

  /**
   * Str represents a string (`s`) to be displayed.
   *
   * The string must be non-empty and not contain newlines.
   */
  case class Str(str: String) extends Chunk

  /**
   * Break represents a newline followed by zero-or-more spaces of
   * indentation.
   *
   * The indentation must be non-negative.
   */
  case class Break(indent: Int) extends Chunk {
    def str: String = lineToStr(indent)
  }

  /**
   * Given a width and 
   */
  def best(w: Int, d: Doc): Stream[Chunk] = {

    /**
     * Return the length of this line if it fits
     */
    @tailrec
    def fits(pos: Int, d: Stream[Chunk]): Boolean =
      (w >= pos) && {
        if (d.isEmpty) true
        else d.head match {
          case Break(_) => true
          case Str(s) => fits(pos + s.length, d.tail)
        }
      }
    /**
     * This is not really tail recursive but many branches are, so
     * we cheat below in non-tail positions
     */
    @tailrec
    def loop(pos: Int, lst: List[(Int, Doc)]): Stream[Chunk] = lst match {
      case Nil => Stream.empty
      case (i, Doc.Empty) :: z => loop(pos, z)
      case (i, Doc.Concat(a, b)) :: z => loop(pos, (i, a) :: (i, b) :: z)
      case (i, Doc.Nest(j, d)) :: z => loop(pos, ((i + j), d) :: z)
      case (i, Doc.Text(s)) :: z => Str(s) #:: cheat(pos + s.length, z)
      case (i, Doc.Line) :: z => Break(i) #:: cheat(i, z)
      case (i, u@Doc.Union(x, _)) :: z =>
        /**
         * If we can fit the next line from x, we take it.
         */
        val first = cheat(pos, (i, x) :: z)
        if (fits(pos, first)) first
        else loop(pos, (i, u.bDoc) :: z)
    }

    def cheat(pos: Int, lst: List[(Int, Doc)]): Stream[Chunk] =
      loop(pos, lst)

    loop(0, (0, d) :: Nil)
  }

  /**
   * We follow the same algorithm as best, but only
   * track what the largest width is that triggers
   * a branch to the left
   */
  def maxWidth(d: Doc): Int = {

    /**
     * Return the length of this line
     */
    @tailrec
    def lineSize(pos: Int, d: Stream[Chunk]): Int =
      if (d.isEmpty) pos
      else d.head match {
        case Break(_) => pos
        case Str(s) => lineSize(pos + s.length, d.tail)
      }

    /**
     * This is not really tail recursive but many branches are, so
     * we cheat below in non-tail positions
     */
    @tailrec
    def loop(pos: Int, lst: List[(Int, Doc)], max: Int): Stream[(Int, Chunk)] = lst match {
      case Nil => Stream.empty
      case (i, Doc.Empty) :: z => loop(pos, z, max)
      case (i, Doc.Concat(a, b)) :: z => loop(pos, (i, a) :: (i, b) :: z, max)
      case (i, Doc.Nest(j, d)) :: z => loop(pos, ((i + j), d) :: z, max)
      case (i, Doc.Text(s)) :: z => (max, Str(s)) #:: cheat(pos + s.length, z, max)
      case (i, Doc.Line) :: z => (max, Break(i)) #:: cheat(i, z, max)
      case (i, Doc.Union(x, _)) :: z =>
        val first = cheat(pos, (i, x) :: z, max)
        val neededWidth = lineSize(pos, first.map(_._2))
        /**
         * if width >= neededWidth, we would branch left here (to x)
         * else we go right
         */
        if (neededWidth <= max) first
        else loop(pos, (i, x) :: z, neededWidth)
    }

    def cheat(pos: Int, lst: List[(Int, Doc)], max: Int): Stream[(Int, Chunk)] =
      loop(pos, lst, max)

    loop(0, (0, d) :: Nil, 0)
      .map(_._1)
      .reduceOption(_ max _)
      .getOrElse(0)
  }

  private[this] final val indentMax = 100

  private[this] def makeIndentStr(i: Int): String = "\n" + (" " * i)

  private[this] val indentTable: Array[String] =
    (0 to indentMax).iterator
      .map(makeIndentStr)
      .toArray

  def lineToStr(indent: Int): String =
    if (indent <= indentMax) indentTable(indent)
    else makeIndentStr(indent)
}
