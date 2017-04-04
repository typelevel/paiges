package com.github.johnynek.paiges

import scala.annotation.tailrec

private[paiges] object Tree {
  import Rendered._
  import Node._

  private[paiges] case class Fix[F[_]](unfix: F[Fix[F]])
  private[paiges] type StreamTree[T] = Stream[Either[(T, () => T), Rendered]]
  private[paiges] type RenderedTree = Fix[StreamTree]

  private def docTree(s: StreamTree[RenderedTree]): RenderedTree = Fix[StreamTree](s)

  private[paiges] def toRenderedTree(d: Node): RenderedTree = {
    case class Bounds(min: Int, max: Int) {
      def contains(x: Int): Boolean = (min <= x) && (x < max)

      def split(x: Int): Option[(Option[Bounds], Bounds)] =
        if (contains(x)) Some((if (x > min) Some(Bounds(min, x)) else None, Bounds(x, max)))
        else None
    }

    /**
     * Return the minimum width needed to go down
     * the left branch.
     *
     * Note we carry the current minimum (minV) in
     * order to stop early.
     */
    @tailrec
    def fits(pos: Int, d: RenderedTree, minV: Int): Int = d.unfix match {
      case Stream.Empty => pos min minV// we always can fit by going left
      case Right(RLine(_)) #:: _ => pos min minV
      case Right(RText(s)) #:: tail =>
        val nextPos = pos + s.length
        if (nextPos >= minV) minV
        else fits(nextPos, docTree(tail), minV)
      case Left((a, b)) #:: _ =>
        val amin = cheatFits(pos, a, minV)
        fits(pos, b(), amin)
    }
    def cheatFits(pos: Int, d: RenderedTree, minV: Int): Int = fits(pos, d, minV)

    @tailrec
    def loop(pos: Int, lst: List[(Int, Node)], bounds: Bounds): RenderedTree = lst match {
      case Nil => docTree(Stream.empty)
      case (i, Empty) :: z => loop(pos, z, bounds)
      case (i, Concat(a, b)) :: z => loop(pos, (i, a) :: (i, b) :: z, bounds)
      case (i, Nest(j, d)) :: z => loop(pos, ((i + j), d) :: z, bounds)
      case (i, Text(s)) :: z => docTree(Right(RText(s)) #:: cheat(pos + s.length, z, bounds).unfix)
      case (i, Line) :: z => docTree(Right(RLine(i)) #:: cheat(i, z, bounds).unfix)
      case (i, u@Union(a, _)) :: z =>
        /**
         * if we can go left, we do, otherwise we go right. So, in the current
         * bounds, there is a threshold for the current node:
         * if (w < wmin) go right
         * else go left
         */
        val as = cheat(pos, (i, a) :: z, bounds)
        val minLeftWidth = fits(pos, as, Int.MaxValue)
        bounds.split(minLeftWidth) match {
          case None =>
            // cannot go left
            loop(pos, (i, u.bNode) :: z, bounds)
          case Some((None, _)) =>
            // always go left, because bounds.min == minLeftWidth
            as
          case Some((Some(rb), lb)) => // note when the width is smaller we go right
            val left = cheat(pos, (i, a) :: z, lb)
            def right = cheat(pos, (i, u.bNode) :: z, rb)
            docTree(Stream(Left((left, () => right))))
        }
    }

    def cheat(pos: Int, lst: List[(Int, Node)], bounds: Bounds) =
      loop(pos, lst, bounds)

    loop(0, (0, d) :: Nil, Bounds(0, Int.MaxValue))
  }

  private[paiges] def deunioned(d: RenderedTree): Stream[Node] = {

    def cat(a: Node, b: Node) = a match {
      case Empty => b
      case other => b match {
        case Empty => other
        case oo => Concat(other, oo)
      }
    }
    @tailrec
    def loop(tree: RenderedTree, prefix: Node): Stream[Node] = tree.unfix match {
      case Stream.Empty => Stream(prefix)
      case (Right(RText(t)) #:: tail) =>
        loop(docTree(tail), cat(prefix, Text(t)))
      case (Right(RLine(n)) #:: tail) =>
        loop(docTree(tail), cat(prefix, cat(Line, spaces(n))))
      case (Left((a, b)) #:: _) =>
        cheat(a, prefix) #::: cheat(b(), prefix)
    }
    def cheat(tree: RenderedTree, prefix: Node): Stream[Node] = loop(tree, prefix)

    loop(d, Empty)
  }

  private def push(d: Rendered, t: RenderedTree): RenderedTree =
    docTree(Right(d) #:: t.unfix)

  private def liftUnion(f: RenderedTree): Either[(RenderedTree, () => RenderedTree), RenderedTree] = f.unfix match {
    case Stream.Empty => Right(docTree(Stream.empty))
    case (Right(r) #:: tail) =>
      liftUnion(docTree(tail)) match {
        case Right(t) => Right(push(r, t))
        case Left((a, b)) => Left((push(r, a), () => push(r, b())))
      }
    case (Left(left) #:: _) => Left(left)
  }

  private def space2(n: Int) = Right(RText(Strings.spaceString(n))) // can memoize this

  private def extract(s: String, part: String): Option[RText] =
    if (s.startsWith(part)) Some(RText(s.substring(part.length)))
    else None

  /**
   * Remove b from a
   *
   * consider None to be the empty set, which can't otherwise
   * be represented
   */
  private[paiges] def setDiff(a: RenderedTree, b: RenderedTree): Option[RenderedTree] =
    (a.unfix, b.unfix) match {
      case (Left((as, bs)) #:: _, _) =>
        val adiff = setDiff(as, b)
        def bdiff = setDiff(bs(), b)
        adiff match {
          case Some(ad) =>
            bdiff match {
              case Some(bd) => Some(docTree(Left((ad, () => bd)) #:: Stream.empty))
              case None => adiff
            }
          case None => bdiff
        }
      case (_, Left((as, bs)) #:: _) =>
        liftUnion(a) match {
          case Left(left) =>
            setDiff(docTree(Left(left) #:: Stream.empty), b)
          case Right(nounion) =>
            setDiff(nounion, as) match {
              case None => // we have already emptied, so we are done:
                None
              case Some(remainder) =>
                setDiff(remainder, bs())
            }
        }
      case (Stream.Empty, Stream.Empty) => None // this is now the empty set
      case (Stream.Empty, _) => Some(a)
      case (Right(RLine(nx)) #:: tailx, (right@Right(RLine(ny))) #:: taily) if (nx == ny) =>
        setDiff(docTree(tailx), docTree(taily)).map { diff =>
          docTree(right #:: diff.unfix)
        }
      case ((left@Right(RText(s1))) #:: xtail, (right@Right(RText(s2))) #:: ytail) =>
        if (s1.length == s2.length) {
         if (s1 == s2) {
           setDiff(docTree(xtail), docTree(ytail)).map { diff =>
             docTree(right #:: diff.unfix)
           }
         }
         else Some(a)
        }
        else if (s1.length < s2.length) {
          extract(s2, s1) match {
            case Some(t) =>
              setDiff(docTree(xtail), docTree(Right(t) #:: ytail)).map { diff =>
                docTree(left #:: diff.unfix)
              }
            case None => Some(a)
          }
        } else {
          extract(s1, s2) match {
            case Some(t) =>
              setDiff(docTree(Right(t) #:: xtail), docTree(ytail)).map { diff =>
                docTree(right #:: diff.unfix)
              }
            case None => Some(a)
          }
        }
      case (_, _) => Some(a) // they don't match at this point
    }

  /**
   * does the docTree on the left contain the one on right
   */
  def isSubNode(a: RenderedTree, b: RenderedTree): Boolean =
    (a.unfix, b.unfix) match {
      case (Left((as, bs)) #:: _, _) =>
        isSubNode(as, b) && isSubNode(bs(), b)
      case (_, Left((as, bs)) #:: _) =>
        liftUnion(a) match {
          case Left((aa, ab)) =>
            isSubNode(docTree(Left((aa, ab)) #:: Stream.empty), b)
          case Right(nounion) =>
            isSubNode(nounion, as) || isSubNode(nounion, bs())
        }
      case (Stream.Empty, Stream.Empty) => true
      case (Stream.Empty, _) => false
      case (_, Stream.Empty) => false
      case (Right(RLine(nx)) #:: tailx, Right(RLine(ny)) #:: taily) =>
        if (nx == ny) isSubNode(docTree(tailx), docTree(taily))
        else {
          val m = nx min ny
          // pull the space out
          isSubNode(docTree(space2(nx - m) #:: tailx), docTree(space2(ny - m) #:: taily))
        }
      case (Right(RLine(_)) #:: _, _) => false // line comes after text (different from ascii!)
      case (_, Right(RLine(_)) #:: _) => false
      case (Right(RText(s1)) #:: xtail, Right(RText(s2)) #:: ytail) =>
        if (s1.length == s2.length) {
          (s1 == s2) && isSubNode(docTree(xtail), docTree(ytail))
        }
        else if (s1.length < s2.length) {
          extract(s2, s1) match {
            case Some(t) => isSubNode(docTree(xtail), docTree(Right(t) #:: ytail))
            case None => false
          }
        } else {
          extract(s1, s2) match {
            case Some(t) => isSubNode(docTree(Right(t) #:: xtail), docTree(ytail))
            case None => false
          }
        }
    }

  /**
   * The main trick is that Union(a, b) has the property that a is less than or equal to b
   * in our sort by construction. So, we can first compare on the left,
   * and if that is equal, go to right.
   */
  def compareTree(xs: RenderedTree, ys: RenderedTree): Int =
    (xs.unfix, ys.unfix) match {
      case (Left((xa, xb)) #:: _, Left((ya, yb)) #:: _) =>
        val c1 = compareTree(xa, ya)
        if (c1 == 0) {
          /**
           * now we should compare(xb - xa, yb - ya)
           */
          setDiff(xb(), xa) match {
            case None =>
              // here xa == xb()
              setDiff(yb(), ya) match {
                case None =>
                  // yb == ya, but xa == yb
                  0
                case Some(ydiff) =>
                  compareTree(xa, ydiff)
              }
            case Some(xdiff) =>
              setDiff(yb(), ya) match {
                case None =>
                  // yb == ya
                  // we know that xa == ya, and yb == ya
                  // so xdiff != ya, but can we say > ya?
                  compareTree(xdiff, ya)
                case Some(ydiff) =>
                  compareTree(xdiff, ydiff)
              }
          }
        }
        else c1
      case (_, Left((as, bs)) #:: _) =>
        liftUnion(xs) match {
          case Right(nonunion) =>
            val c1 = compareTree(nonunion, as)
            if (c1 == 0) compareTree(nonunion, bs())
            else c1
          case Left(un) => compareTree(docTree(Left(un) #:: Stream.empty), ys)
        }
      case (Left((as, bs)) #:: _, _) =>
        liftUnion(ys) match {
          case Right(nonunion) =>
            val c1 = compareTree(as, nonunion)
            if (c1 == 0) compareTree(bs(), nonunion)
            else c1
          case Left(un) => compareTree(xs, docTree(Left(un) #:: Stream.empty))
        }
      case (Stream.Empty, Stream.Empty) => 0
      case (Stream.Empty, _) => -1
      case (_, Stream.Empty) => 1
      case (Right(RLine(nx)) #:: tailx, Right(RLine(ny)) #:: taily) =>
        if (nx == ny) compareTree(docTree(tailx), docTree(taily))
        else {
          val m = nx min ny
          // pull the space out
          compareTree(docTree(space2(nx - m) #:: tailx), docTree(space2(ny - m) #:: taily))
        }
      case (Right(RLine(_)) #:: _, _) => 1 // line comes after text (different from ascii!)
      case (_, Right(RLine(_)) #:: _) => -1
      case (Right(RText(s1)) #:: xtail, Right(RText(s2)) #:: ytail) =>
        if (s1.length == s2.length) {
          val c = s1 compare s2
          if (c == 0) compareTree(docTree(xtail), docTree(ytail))
          else c
        }
        else if (s1.length < s2.length) {
          extract(s2, s1) match {
            case Some(t) => compareTree(docTree(xtail), docTree(Right(t) #:: ytail))
            case None => s1 compare s2
          }
        } else {
          extract(s1, s2) match {
            case Some(t) => compareTree(docTree(Right(t) #:: xtail), docTree(ytail))
            case None => s1 compare s2
          }
        }
    }
}
