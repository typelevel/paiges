package com.github.johnynek.paiges

import org.scalatest.FunSuite

object LineWrap {
  /**
   * Return chunks, not lines (new line chars are in the string)
   */
  def apply(lines: Iterable[String], width: Int): Stream[String] = {
    val doc = lines.foldLeft((Doc.empty, Doc.empty)) {
      case ((doc, paragraph), "") => ((doc +: paragraph +: Doc.line), Doc.empty)
      case ((doc, paragraph), line) => (doc, paragraph +: Doc.paragraph(line))
    }

    (doc._1 +: doc._2).renderStream(width)
  }
}
