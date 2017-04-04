package com.github.johnynek.paiges

import scala.annotation.tailrec

private[paiges] sealed abstract class Node
private[paiges] object Node {
  case object Empty extends Node

  /**
   * Represents a single, literal newline.
   */
  case object Line extends Node

  /**
   * The string must not be empty, and may not contain newlines.
   */
  case class Text(str: String) extends Node

  case class Concat(a: Node, b: Node) extends Node

  case class Nest(indent: Int, doc: Node) extends Node

  /**
   * There is an additional invariant on Union that
   * a == flatten(b). By construction all have this
   * property, but this is why we don't expose Union
   * but only .group
   */
  case class Union(a: Node, b: () => Node) extends Node {
    lazy val bNode: Node = b()
    override def toString: String = s"Union($a, $bNode)"
  }

  private[this] val maxSpaceTable = 100
  private[this] val spaceArray: Array[Text] =
    (1 to maxSpaceTable)
      .map { s => Text(Strings.spaceString(s)) }
      .toArray

  def empty: Node = Empty
  def spaces(n: Int): Node =
    if (n == 0) Empty
    else if (n <= maxSpaceTable) spaceArray(n - 1)
    else Text(Strings.spaceString(n))

  val space: Node = spaces(1)

  /**
   * Convert all lines to spaces and
   * return a Node of only Empty, Text, and Concat
   * nodes
   */
  def flatten(doc: Node): Node = doc match {
    case Empty => Empty
    case Line => space
    case str@Text(_) => str
    case Nest(i, d) => flatten(d) // no Line, so Nest is irrelevant
    case Concat(a, b) => Concat(flatten(a), flatten(b))
    case Union(a, _) => flatten(a)
  }

  /**
   * If the doc has no Line nodes, return None, else
   * flatten the document.
   */
  def flattenOption(doc: Node): Option[Node] = doc match {
    case Empty | Text(_)=> None
    case Line => Some(space)
    case Nest(i, d) =>
      /*
       * This is different from flatten which always strips
       * the Nest node. This will return None if there is
       * no embedded Line inside
       */
      flattenOption(d)
    case Concat(a, b) =>
      // stack safety may be an issue here
      (flattenOption(a), flattenOption(b)) match {
        case (Some(fa), Some(fb)) => Some(Concat(fa, fb))
        case (Some(fa), None) => Some(Concat(fa, b))
        case (None, Some(fb)) => Some(Concat(a, fb))
        case (None, None) => None
      }
    case Union(a, _) => flattenOption(a).orElse(Some(a))
  }

  /**
   * A Doc is empty if and only if all renderings are empty
   */
  def isEmpty(d: Node): Boolean = {
    @tailrec
    def loop(doc: Node, stack: List[Node]): Boolean = doc match {
      case Empty => stack match {
        case d1 :: tail => loop(d1, tail)
        case Nil => true
      }
      case Concat(_, Line) => false // minor optimization to short circuit sooner
      case Concat(a, Text(s)) =>
        // minor optimization to short circuit sooner
        s.isEmpty && loop(a, stack)
      case Concat(a, b) => loop(a, b :: stack)
      case Nest(i, d) => loop(d, stack)
      case Text(s) =>
        // shouldn't be empty by construction, but defensive
        s.isEmpty && loop(Empty, stack)
      case Line => false
      case Union(flattened, _) =>
        // flattening cannot change emptiness
        loop(flattened, stack)
    }
    loop(d, Nil)
  }

  def repeat(n: Node, count: Int): Node = {
    /**
     * only have log depth, so recursion is fine
     * d * (2n + c) = (dn + dn) + c
     */
    def loop(d: Node, cnt: Int): Node = {
      val n = cnt / 2
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
    if (count <= 0) empty
    else loop(n, count)
  }
}
