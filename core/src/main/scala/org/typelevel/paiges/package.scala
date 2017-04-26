package org.typelevel

package object paiges {

  type StreamTree[T] = Stream[Step[T, Chunk]]

  type DocTree = Fix[StreamTree]

  @annotation.tailrec
  private[paiges] def call[A](a: A, stack: List[A => A]): A = stack match {
    case Nil => a
    case h :: tail => call(h(a), tail)
  }

  private[paiges] def extract(s: String, part: String): Option[String] =
    if (s.startsWith(part)) Some(s.substring(part.length))
    else None
}
