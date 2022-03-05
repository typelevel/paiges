package org.typelevel.paiges

import scala.annotation.tailrec

private[paiges] object Chunk {

  /**
   * Given a width and Doc find the Iterator
   * of Chunks.
   */
  def best(w: Int, d: Doc, trim: Boolean): Iterator[String] = {

    val nonNegW = w.max(0)

    sealed abstract class ChunkStream
    object ChunkStream {
      case object Empty extends ChunkStream
      case class Item(str: String, position: Int, stack: List[(Int, Doc)]) extends ChunkStream {
        def isLine: Boolean = str == null
        def stringChunk: String = if (isLine) lineToStr(position) else str
        private[this] var next: ChunkStream = _
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
      def hasNext: Boolean = current != ChunkStream.Empty
      def next(): String = {
        val item = current.asInstanceOf[ChunkStream.Item]
        val res = item.stringChunk
        current = item.step
        res
      }
    }

    class TrimChunkIterator(var current: ChunkStream) extends Iterator[String] {
      private val lineCombiner = new TrimChunkIterator.LineCombiner
      def hasNext: Boolean = current != ChunkStream.Empty || lineCombiner.nonEmpty
      def next(): String =
        current match {
          case ChunkStream.Empty => lineCombiner.finalLine()
          case item: ChunkStream.Item =>
            current = item.step
            lineCombiner.addItem(item).getOrElse(next())
        }
    }

    object TrimChunkIterator {
      class LineCombiner {
        private var line: StringBuilder = new StringBuilder
        def nonEmpty: Boolean = line.nonEmpty
        def finalLine(): String = {
          val res = line.toString
          line = new StringBuilder
          LineCombiner.trim(res)
        }
        def addItem(item: ChunkStream.Item): Option[String] =
          if (item.isLine) {
            val v = LineCombiner.trim(line.toString)
            line = new StringBuilder(lineToStr(item.position))
            Some(v)
          } else {
            line.append(item.str)
            None
          }
      }
      object LineCombiner {
        private def trim(s: String) = {
          var ind = s.length
          while (ind >= 1 && s.charAt(ind - 1) == ' ') ind = ind - 1
          s.substring(0, ind)
        }
      }
    }

    @tailrec
    def fits(pos: Int, d: ChunkStream): Boolean =
      (nonNegW >= pos) && {
        d match {
          case ChunkStream.Empty => true
          case item: ChunkStream.Item =>
            item.isLine || fits(item.position, item.step)
        }
      }
    /*
     * This is not really tail recursive but many branches are, so
     * we cheat below in non-tail positions
     */
    @tailrec
    def loop(pos: Int, lst: List[(Int, Doc)]): ChunkStream =
      lst match {
        case Nil                          => ChunkStream.Empty
        case (_, Doc.Empty) :: z          => loop(pos, z)
        case (i, Doc.FlatAlt(a, _)) :: z  => loop(pos, (i, a) :: z)
        case (i, Doc.Concat(a, b)) :: z   => loop(pos, (i, a) :: (i, b) :: z)
        case (i, Doc.Nest(j, d)) :: z     => loop(pos, ((i + j), d) :: z)
        case (_, Doc.Align(d)) :: z       => loop(pos, (pos, d) :: z)
        case (_, Doc.Text(s)) :: z        => ChunkStream.Item(s, pos + s.length, z)
        case (_, Doc.ZeroWidth(s)) :: z   => ChunkStream.Item(s, pos, z)
        case (i, Doc.Line) :: z           => ChunkStream.Item(null, i, z)
        case (i, d @ Doc.LazyDoc(_)) :: z => loop(pos, (i, d.evaluated) :: z)
        case (i, Doc.Union(x, y)) :: z    =>
          /*
           * If we can fit the next line from x, we take it.
           */
          val first = cheat(pos, (i, x) :: z)
          /*
           * Note that in Union the left side is always 2-right-associated.
           * This means the "fits" branch in rendering
           * always has a 2-right-associated Doc which means it is O(w)
           * to find if you can fit in width w.
           */
          if (fits(pos, first)) first
          else loop(pos, (i, y) :: z)
      }

    def cheat(pos: Int, lst: List[(Int, Doc)]) =
      loop(pos, lst)

    val stream = loop(0, (0, d) :: Nil)
    if (trim) new TrimChunkIterator(stream) else new ChunkIterator(stream)
  }

  // $COVERAGE-OFF$
  // code of the form `final val x = ...` is inlined. we never
  // access this field at runtime, but it is still used.
  final private[this] val indentMax = 100
  // $COVERAGE-ON$

  private[this] def makeIndentStr(i: Int): String = "\n" + (" " * i)

  private[this] val indentTable: Array[String] =
    (0 to indentMax).iterator
      .map(makeIndentStr)
      .toArray

  def lineToStr(indent: Int): String =
    if (indent <= indentMax) indentTable(indent)
    else makeIndentStr(indent)
}
