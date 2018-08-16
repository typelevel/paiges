package org.typelevel.paiges

import scala.annotation.tailrec

private[paiges] object Chunk {
  final class Thunk[A](x: () => A) {
    lazy val force: A = x()
  }

  sealed trait ChunkStream extends Product with Serializable

  object ChunkStream {
    case object Empty extends ChunkStream
    case class Text(s: String, tail: Thunk[ChunkStream]) extends ChunkStream
    case class Line(pos: Int, tail: Thunk[ChunkStream]) extends ChunkStream
  }

  class ChunkIterator(var current: ChunkStream) extends Iterator[String] {
    def hasNext: Boolean = current != ChunkStream.Empty
    def next: String = current match {
      case ChunkStream.Text(s, tail) =>
        current = tail.force
        s
      case ChunkStream.Line(pos, tail) =>
        current = tail.force
        lineToStr(pos)
      case ChunkStream.Empty => throw new RuntimeException("next called on empty iterator")
    }
  }

  class TrimChunkIterator(var current: ChunkStream) extends Iterator[String] {
    private val lineCombiner = new TrimChunkIterator.LineCombiner
    def hasNext: Boolean = current != ChunkStream.Empty || lineCombiner.nonEmpty
    def next: String = current match {
      case ChunkStream.Empty => lineCombiner.finalLine()
      case ChunkStream.Text(s, tail) =>
        current = tail.force
        lineCombiner.addText(s)
        next
      case ChunkStream.Line(pos, tail) =>
        current = tail.force
        lineCombiner.addLine(pos)
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
      def addText(s: String): Unit = {
        val _ = line.append(s)
        ()
      }
      def addLine(pos: Int): String = {
        val v = LineCombiner.trim(line.toString)
        line = new StringBuilder(lineToStr(pos))
        v
      }
    }
    object LineCombiner {
      private def trim(s: String): String = {
        var ind = s.length
        while (ind >= 1 && s.charAt(ind - 1) == ' ') ind = ind - 1
        s.substring(0, ind)
      }
    }
  }

  @tailrec
  def fits(width: Int, d: ChunkStream): Boolean =
    (width >= 0) && {
      d match {
        case ChunkStream.Empty | ChunkStream.Line(_, _) => true
        case ChunkStream.Text(s, tail) =>
          val n = s.length
          fits(width - n, tail.force)
      }
    }

  /**
   * Given a width and Doc find the Iterator
   * of Chunks.
   */
  def best(w: Int, d: Doc, trim: Boolean): Iterator[String] = {
    val nonNegW = w max 0

    @tailrec
    def loop(pos: Int, lst: List[(Int, Doc)]): ChunkStream = lst match {
      case Nil => ChunkStream.Empty
      case (i, Doc.Empty) :: z => loop(pos, z)
      case (i, Doc.Concat(a, b)) :: z => loop(pos, (i, a) :: (i, b) :: z)
      case (i, Doc.Nest(j, d)) :: z => loop(pos, (i + j, d) :: z)
      case (_, Doc.Align(d)) :: z => loop(pos, (pos, d) :: z)
      case (i, Doc.Text(s)) :: z => ChunkStream.Text(s, new Thunk(() => loop(pos + s.length, z)))
      case (i, Doc.Line(_)) :: z => ChunkStream.Line(i, new Thunk(() => loop(i, z)))
      case (i, Doc.LazyDoc(d)) :: z => loop(pos, (i, d.evaluated) :: z)
      case (i, Doc.Union(x, y)) :: z =>
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
        if (fits(nonNegW - pos, first)) first
        else loop(pos, (i, y) :: z)
    }
    def cheat(pos: Int, lst: List[(Int, Doc)]): ChunkStream = loop(pos, lst)

    val stream = loop(0, (0, d) :: Nil)
    if (trim) new TrimChunkIterator(stream) else new ChunkIterator(stream)
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
