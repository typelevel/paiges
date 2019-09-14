package org.typelevel.paiges

object ScalaVersionCompat {
  type LazyList[+A] = scala.collection.immutable.Stream[A]
  val LazyList = scala.collection.immutable.Stream

  def lazyListFromIterator[A](it: Iterator[A]): LazyList[A] =
    it.toStream
}
