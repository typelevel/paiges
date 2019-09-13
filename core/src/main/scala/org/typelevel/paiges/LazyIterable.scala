package org.typelevel.paiges

sealed abstract class LazyIterable[+A] extends Iterable[A] {
  def iterator: Iterator[A] =
    this match {
      case LazyIterable.Empty =>
        Iterator.empty
      case cons @ LazyIterable.Cons(_, _) =>
        new Iterator[A] {
          var xs: LazyIterable.Cons[A] = cons
          def hasNext: Boolean = xs != null
          def next: A = {
            val res = xs.head
            xs = xs.continue() match {
              case next @ LazyIterable.Cons(_, _) => next
              case LazyIterable.Empty => null
            }
            res
          }
        }
    }
}

object LazyIterable {

  case object Empty extends LazyIterable[Nothing]

  case class Cons[+A](override val head: A, continue: () => LazyIterable[A]) extends LazyIterable[A]

  def empty[A]: LazyIterable[A] = Empty

  def cons[A](head: A, tail: => LazyIterable[A]): LazyIterable[A] =
    Cons(head, tail _)

  def fromIterator[A](it: Iterator[A]): LazyIterable[A] =
    if (it.hasNext) Cons(it.next, () => fromIterator(it)) else Empty
}
