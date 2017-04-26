package org.typelevel.paiges

import scala.annotation.tailrec

object DocTree {

  import Doc._
  import Chunk.{ Break, Str }
  import Step.{ Emit, Split }

  private def docTree(s: StreamTree[DocTree]): DocTree =
    Fix(s)

  private case class Bounds(min: Int, max: Int) {
    def contains(x: Int): Boolean = (min <= x) && (x < max)

    def split(x: Int): Option[(Option[Bounds], Bounds)] =
      if (contains(x)) {
        val right = if (x > min) Some(Bounds(min, x)) else None
        Some((right, Bounds(x, max)))
      }
      else None

    def split2(x: Int): Option[Bounds] =
      if (contains(x)) {
        Some(Bounds(x, max))
      }
      else None
  }

  private[paiges] def toDocTree(d: Doc): DocTree = {

    /**
     * Return the minimum width needed to go down
     * the left branch.
     *
     * Note we carry the current minimum (minV) in
     * order to stop early.
     */
    @tailrec
    def fits(pos: Int, d: DocTree, minV: Int): Int =
      d.unfix match {
        case Stream.Empty => pos min minV// we always can fit by going left
        case Emit(Break(_, _, _)) #:: _ => pos min minV
        case Emit(Str(s)) #:: tail =>
          val nextPos = pos + s.length
          if (nextPos >= minV) minV
          else fits(nextPos, docTree(tail), minV)
        case Split(a, b) #:: _ =>
          val amin = cheatFits(pos, a, minV)
          fits(pos, b(), amin)
      }

    def cheatFits(pos: Int, d: DocTree, minV: Int): Int =
      fits(pos, d, minV)

    @tailrec
    def loop(pos: Int, lst: List[((Int, Boolean),  Doc)], bounds: Bounds): DocTree = lst match {
      case Nil => docTree(Stream.empty)
      case (_, Empty) :: z => loop(pos, z, bounds)
      case (indent, Concat(a, b)) :: z => loop(pos, (indent, a) :: (indent, b) :: z, bounds)
      case ((i, abs), Nest(j, d)) :: z => loop(pos, (((i + j), abs), d) :: z, bounds)
      case (_, Align(d)) :: z => loop(pos, ((pos, true), d) :: z, bounds)
      case (_, Text(s)) :: z => docTree(Emit(Str(s)) #:: cheat(pos + s.length, z, bounds).unfix)
      case ((i, abs), Line(fts)) :: z => docTree(Emit(Break(i, fts, abs)) #:: cheat(i, z, bounds).unfix)
      case (indent, u@Union(a, _)) :: z =>
        /**
         * if we can go left, we do, otherwise we go right. So, in the current
         * bounds, there is a threshold for the current node:
         *
         * The problem is, pos is only a lower bound. We could be shifted more
         * to the right due to nesting or concatenation later. So to preserve
         * the invariant that a == b implies f(a) == f(b) we need to be prepared
         * for such shifting.
         *
         * if (w < wmin) go right
         * else go left
         */
        val as = cheat(pos, (indent, a) :: z, bounds)
        val minLeftWidth = fits(pos, as, bounds.max)
        bounds.split2(minLeftWidth) match {
          case None =>
            // cannot go left
            loop(pos, (indent, u.bDoc) :: z, bounds)
          case Some(b) =>
            val left = cheat(pos, (indent, a) :: z, b)
            def right = cheat(pos, (indent, u.bDoc) :: z, bounds)
            docTree(Stream(Split(left, () => right)))
          // case Some((None, _)) =>
          //   // always go left, because bounds.min == minLeftWidth
          //   as
          // case Some((Some(rb), lb)) => // note when the width is smaller we go right
          //   val left = cheat(pos, (indent, a) :: z, lb)
          //   def right = cheat(pos, (indent, u.bDoc) :: z, rb)
          //   docTree(Stream(Split(left, () => right)))
        }
    }

    def cheat(pos: Int, lst: List[((Int, Boolean), Doc)], bounds: Bounds): DocTree =
      loop(pos, lst, bounds)

    loop(0, ((0, false), d) :: Nil, Bounds(0, Int.MaxValue))
  }

  /**
   * This is a method for testing. It emits only Text and Line
   * nodes, so it loses information, namely the Nesting/Alignment
   * nodes are lost. Alignment creates absolute positioning,
   * as compared to the relative positioning of nesting. Thus
   * the output of this method, while useful for law checking,
   * should not be exposed to users
   */
  private[paiges] def deunioned(d: DocTree): Stream[Doc] = {

    def cat(a: Doc, b: Doc): Doc =
      a match {
        case Empty => b
        case other => b match {
          case Empty => other
          case oo => Concat(other, oo)
        }
      }

    @tailrec
    def loop(tree: DocTree, prefix: Doc): Stream[Doc] =
      tree.unfix match {
        case Stream.Empty => Stream(prefix)
        case (Emit(Str(t)) #:: tail) =>
          loop(docTree(tail), cat(prefix, Text(t)))
        case (Emit(Break(n, fts, abs)) #:: tail) =>
          // We are ignoring if this is an absolute (Align) or not (Nest)
          // to add this we would need to find the previous line
          // where we could do the Align-ment to.
          loop(docTree(tail), cat(prefix, cat(Line(fts), spaces(n))))
        case (Split(a, b) #:: _) =>
          cheat(a, prefix) #::: cheat(b(), prefix)
      }
    def cheat(tree: DocTree, prefix: Doc): Stream[Doc] =
      loop(tree, prefix)

    loop(d, Empty)
  }

  private def push(d: Chunk, t: DocTree): DocTree =
    docTree(Emit(d) #:: t.unfix)

  private def liftUnion(f: DocTree): Step[DocTree, DocTree] =
    f.unfix match {
      case Stream.Empty =>
        Emit(docTree(Stream.empty))
      case (Emit(r) #:: tail) =>
        liftUnion(docTree(tail)) match {
          case Emit(t) => Emit(push(r, t))
          case Split(a, b) => Split(push(r, a), () => push(r, b()))
        }
      case (Split(a, b) #:: _) => Split(a, b)
    }

  private def space2(n: Int): Step[Nothing, Chunk] =
    Emit(Str(" " * n)) // can memoize this

  private def extract(s: String, part: String): Option[Str] =
    if (s.startsWith(part)) Some(Str(s.substring(part.length)))
    else None

  /**
   * Remove b from a
   *
   * consider None to be the empty set, which can't otherwise
   * be represented
   */
  private[paiges] def setDiff(a: DocTree, b: DocTree): Option[DocTree] =
    (a.unfix, b.unfix) match {
      case (Split(as, bs) #:: _, _) =>
        val adiff = setDiff(as, b)
        def bdiff = setDiff(bs(), b)
        adiff match {
          case Some(ad) =>
            bdiff match {
              case Some(bd) => Some(docTree(Split(ad, () => bd) #:: Stream.empty))
              case None => adiff
            }
          case None => bdiff
        }
      case (_, Split(as, bs) #:: _) =>
        liftUnion(a) match {
          case Split(x, y) =>
            setDiff(docTree(Split(x, y) #:: Stream.empty), b)
          case Emit(nounion) =>
            setDiff(nounion, as) match {
              case None => // we have already emptied, so we are done:
                None
              case Some(remainder) =>
                setDiff(remainder, bs())
            }
        }
      case (Stream.Empty, Stream.Empty) => None // this is now the empty set
      case (Stream.Empty, _) => Some(a)
      case (Emit(bx@Break(nx, fx, absx)) #:: tailx, (right@Emit(by@Break(ny, fy, absy))) #:: taily) if (fx == fy) && (absx == absy) =>
        if (nx == ny) {
          setDiff(docTree(tailx), docTree(taily)).map { diff =>
            docTree(right #:: diff.unfix)
          }
        }
        else {
          val m = nx min ny
          // pull the space out
          val newLeft = docTree(Emit(Break(m, fx, absx)) #:: space2(nx - m) #:: tailx)
          val newRight = docTree(Emit(Break(m, fy, absy)) #:: space2(ny - m) #:: tailx)
          setDiff(newLeft, newRight)
        }
      case ((left@Emit(Str(s1))) #:: xtail, (right@Emit(Str(s2))) #:: ytail) =>
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
              setDiff(docTree(xtail), docTree(Emit(t) #:: ytail)).map { diff =>
                docTree(left #:: diff.unfix)
              }
            case None => Some(a)
          }
        } else {
          extract(s1, s2) match {
            case Some(t) =>
              setDiff(docTree(Emit(t) #:: xtail), docTree(ytail)).map { diff =>
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
  def isSubDoc(a: DocTree, b: DocTree): Boolean =
    (a.unfix, b.unfix) match {
      case (Split(as, bs) #:: _, _) =>
        isSubDoc(as, b) && isSubDoc(bs(), b)
      case (_, Split(as, bs) #:: _) =>
        liftUnion(a) match {
          case Split(aa, ab) =>
            isSubDoc(docTree(Split(aa, ab) #:: Stream.empty), b)
          case Emit(nounion) =>
            isSubDoc(nounion, as) || isSubDoc(nounion, bs())
        }
      case (Stream.Empty, Stream.Empty) => true
      case (Stream.Empty, _) => false
      case (_, Stream.Empty) => false
      case (Emit(Break(nx, fx, absx)) #:: tailx, Emit(Break(ny, fy, absy)) #:: taily) =>
        (fx == fy) && (absx == absy) && {
          if (nx == ny) isSubDoc(docTree(tailx), docTree(taily))
          else {
            val m = nx min ny
            // pull the space out
            isSubDoc(docTree(space2(nx - m) #:: tailx), docTree(space2(ny - m) #:: taily))
          }
        }
      case (Emit(Break(_, _, _)) #:: _, _) => false // line comes after text (different from ascii!)
      case (_, Emit(Break(_, _, _)) #:: _) => false
      case (Emit(Str(s1)) #:: xtail, Emit(Str(s2)) #:: ytail) =>
        if (s1.length == s2.length) {
          (s1 == s2) && isSubDoc(docTree(xtail), docTree(ytail))
        }
        else if (s1.length < s2.length) {
          extract(s2, s1) match {
            case Some(t) => isSubDoc(docTree(xtail), docTree(Emit(t) #:: ytail))
            case None => false
          }
        } else {
          extract(s1, s2) match {
            case Some(t) => isSubDoc(docTree(Emit(t) #:: xtail), docTree(ytail))
            case None => false
          }
        }
    }

  /**
   * The main trick is that Union(a, b) has the property that a is less than or equal to b
   * in our sort by construction. So, we can first compare on the left,
   * and if that is equal, go to right.
   */
  def compareTree(xs: DocTree, ys: DocTree): Int = {
    (xs.unfix, ys.unfix) match {
      case (Split(xa, xb) #:: _, Split(ya, yb) #:: _) =>
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
      case (_, Split(as, bs) #:: _) =>
        liftUnion(xs) match {
          case Emit(nonunion) =>
            val c1 = compareTree(nonunion, as)
            if (c1 == 0) compareTree(nonunion, bs())
            else c1
          case Split(a, b) => compareTree(docTree(Split(a, b) #:: Stream.empty), ys)
        }
      case (Split(as, bs) #:: _, _) =>
        liftUnion(ys) match {
          case Emit(nonunion) =>
            val c1 = compareTree(as, nonunion)
            if (c1 == 0) compareTree(bs(), nonunion)
            else c1
          case Split(a, b) => compareTree(xs, docTree(Split(a, b) #:: Stream.empty))
        }
      case (Stream.Empty, Stream.Empty) => 0
      case (Stream.Empty, _) => -1
      case (_, Stream.Empty) => 1
      case (Emit(Break(nx, fx, absx)) #:: tailx, Emit(Break(ny, fy, absy)) #:: taily) =>
        if (fx == fy) {
          if (absx == absy) {
            if (nx == ny) compareTree(docTree(tailx), docTree(taily))
            else {
              val m = nx min ny
              // pull the space out
              compareTree(docTree(space2(nx - m) #:: tailx), docTree(space2(ny - m) #:: taily))
            }
          }
          else {
            // put absolute newlines before relative newlines
            if (absx) -1 else 1
          }
        }
        else {
          if (fx) {
            // we flatten the left to space, but the right to newline and
            // line comes after text
            -1
          } else 1
        }
      case (Emit(Break(_, _, _)) #:: _, _) => 1 // line comes after text (different from ascii!)
      case (_, Emit(Break(_, _, _)) #:: _) => -1
      case (Emit(Str(s1)) #:: xtail, Emit(Str(s2)) #:: ytail) =>
        if (s1.length == s2.length) {
          val c = s1 compare s2
          if (c == 0) compareTree(docTree(xtail), docTree(ytail))
          else c
        }
        else if (s1.length < s2.length) {
          extract(s2, s1) match {
            case Some(t) => compareTree(docTree(xtail), docTree(Emit(t) #:: ytail))
            case None => s1 compare s2
          }
        } else {
          extract(s1, s2) match {
            case Some(t) => compareTree(docTree(Emit(t) #:: xtail), docTree(ytail))
            case None => s1 compare s2
          }
        }
    }
  }
}
