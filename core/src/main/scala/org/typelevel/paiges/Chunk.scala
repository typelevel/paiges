package org.typelevel.paiges

import scala.annotation.tailrec

private[paiges] object Chunk {

  /**
   * Given a width and Doc find the Iterator
   * of Chunks.
   */
  def best(w: Int, d: Doc): Iterator[String] = {

    val nonNegW = w max 0

    sealed abstract class ChunkStream
    object ChunkStream {
      case object Empty extends ChunkStream
      case class Item(str: String, position: Int, stack: List[(Int, Doc)], isBreak: Boolean) extends ChunkStream {
        def stringChunk: String = if (isBreak) lineToStr(position) else str
        private[this] var next: ChunkStream = null
        def step: ChunkStream = {
          // do a cheap local computation.
          // lazy val is thread-safe, but more expensive
          // since everything is immutable here, this is
          // safe
          val res = next
          if (res != null) res
          else {
            val c = loop(position, stack)
            next = c
            c
          }
        }
      }
    }

    class ChunkIterator(var current: ChunkStream) extends Iterator[String] {
      def hasNext: Boolean = (current != ChunkStream.Empty)
      def next: String = {
        val item = current.asInstanceOf[ChunkStream.Item]
        val res = item.stringChunk
        current = item.step
        res
      }
    }
    /*
     * Return the length of this line if it fits
     */
    @tailrec
    def fits(pos: Int, d: ChunkStream): Boolean =
      (nonNegW >= pos) && {
        d match {
          case ChunkStream.Empty => true
          case item: ChunkStream.Item =>
            item.isBreak || fits(item.position, item.step)
        }
      }
    /*
     * This is not really tail recursive but many branches are, so
     * we cheat below in non-tail positions
     */
    @tailrec
    def loop(pos: Int, lst: List[(Int, Doc)]): ChunkStream = lst match {
      case Nil => ChunkStream.Empty
      case (i, Doc.Empty) :: z => loop(pos, z)
      case (i, Doc.Concat(a, b)) :: z => loop(pos, (i, a) :: (i, b) :: z)
      case (i, Doc.Nest(j, d)) :: z => loop(pos, ((i + j), d) :: z)
      case (_, Doc.Align(d)) :: z => loop(pos, (pos, d) :: z)
      case (i, Doc.Text(s)) :: z => ChunkStream.Item(s, pos + s.length, z, false)
      case (i, Doc.Line(_)) :: z => ChunkStream.Item(null, i, z, true)
      case (i, u@Doc.Union(x, _)) :: z =>
        /*
         * If we can fit the next line from x, we take it.
         */
        val first = cheat(pos, (i, x) :: z)
        /*
         * Note that in Union the left side is always right associated.
         * This means the "fits" branch in rendering
         * always has a right associated Doc which means it is O(w)
         * to find if you can fit in width w.
         */
        if (fits(pos, first)) first
        else loop(pos, (i, u.bDoc) :: z)
    }

    def cheat(pos: Int, lst: List[(Int, Doc)]) =
      loop(pos, lst)

    new ChunkIterator(loop(0, (0, d) :: Nil))
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
