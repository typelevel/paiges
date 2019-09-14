package org.typelevel.paiges

object ScalaVersionCompat {
  def lazyListFromIterator[A](it: Iterator[A]): LazyList[A] =
    LazyList.from(it)
}
