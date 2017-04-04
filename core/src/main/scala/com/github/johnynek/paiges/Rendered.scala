package com.github.johnynek.paiges

import scala.annotation.tailrec

/**
 * This is the second ADT introduced at rendering
 */
private[paiges] sealed abstract class Rendered {
  def str: String
}

private[paiges] object Rendered {
  import Node._

  case class RText(str: String) extends Rendered
  case class RLine(indent: Int) extends Rendered {
    def str: String = Strings.indentString(indent)
  }

  def best(w: Int, d: Node): Stream[Rendered] = {
    /**
     * Return the length of this line if it fits
     */
    @tailrec
    def fits(pos: Int, d: Stream[Rendered]): Boolean =
      (w >= pos) && {
        if (d.isEmpty) true
        else d.head match {
          case RLine(_) => true
          case RText(s) => fits(pos + s.length, d.tail)
        }
      }
    /**
     * This is not really tail recursive but many branches are, so
     * we cheat below in non-tail positions
     */
    @tailrec
    def loop(pos: Int, lst: List[(Int, Node)]): Stream[Rendered] = lst match {
      case Nil => Stream.empty
      case (i, Empty) :: z => loop(pos, z)
      case (i, Concat(a, b)) :: z => loop(pos, (i, a) :: (i, b) :: z)
      case (i, Nest(j, d)) :: z => loop(pos, ((i + j), d) :: z)
      case (i, Text(s)) :: z => RText(s) #:: cheat(pos + s.length, z)
      case (i, Line) :: z => RLine(i) #:: cheat(i, z)
      case (i, u@Union(x, _)) :: z =>
        /**
         * If we can fit the next line from x, we take it.
         */
        val first = cheat(pos, (i, x) :: z)
        if (fits(pos, first)) first
        else loop(pos, (i, u.bNode) :: z)
    }

    def cheat(pos: Int, lst: List[(Int, Node)]): Stream[Rendered] =
      loop(pos, lst)

    loop(0, (0, d) :: Nil)
  }

  /**
   * We follow the same algorithm as best, but only
   * track what the largest width is that triggers
   * a branch to the left
   */
  def maxWidth(d: Node): Int = {
    /**
     * Return the length of this line
     */
    @tailrec
    def lineSize(pos: Int, d: Stream[Rendered]): Int =
      if (d.isEmpty) pos
      else d.head match {
        case RLine(_) => pos
        case RText(s) => lineSize(pos + s.length, d.tail)
      }

    /**
     * This is not really tail recursive but many branches are, so
     * we cheat below in non-tail positions
     */
    @tailrec
    def loop(pos: Int, lst: List[(Int, Node)], max: Int): Stream[(Int, Rendered)] = lst match {
      case Nil => Stream.empty
      case (i, Empty) :: z => loop(pos, z, max)
      case (i, Concat(a, b)) :: z => loop(pos, (i, a) :: (i, b) :: z, max)
      case (i, Nest(j, d)) :: z => loop(pos, ((i + j), d) :: z, max)
      case (i, Text(s)) :: z => (max, RText(s)) #:: cheat(pos + s.length, z, max)
      case (i, Line) :: z => (max, RLine(i)) #:: cheat(i, z, max)
      case (i, Union(x, _)) :: z =>
        val first = cheat(pos, (i, x) :: z, max)
        val neededWidth = lineSize(pos, first.map(_._2))
        /**
         * if width >= neededWidth, we would branch left here (to x)
         * else we go right
         */
        if (neededWidth <= max) first
        else loop(pos, (i, x) :: z, neededWidth)
    }

    def cheat(pos: Int, lst: List[(Int, Node)], max: Int): Stream[(Int, Rendered)] =
      loop(pos, lst, max)

    loop(0, (0, d) :: Nil, 0)
      .map(_._1)
      .reduceOption(_ max _)
      .getOrElse(0)
  }
}
