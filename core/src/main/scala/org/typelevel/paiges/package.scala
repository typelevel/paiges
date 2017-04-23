package org.typelevel

package object paiges {

  type StreamTree[T] = Stream[Step[T, Chunk]]

  type DocTree = Fix[StreamTree]

  @annotation.tailrec
  private[paiges] def call[A](a: A, stack: List[A => A]): A = stack match {
    case Nil => a
    case h :: tail => call(h(a), tail)
  }
}
