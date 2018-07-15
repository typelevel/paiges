package org.typelevel.paiges

import org.scalatest.FunSuite
import Doc._

class LineTests extends FunSuite {
  test("line2 cancels grouping") {
    assert("\n\n" == (line + lineNoFlat).grouped.render(10))
  }
  test("line2 preserves nesting") {
    assert("\n  \n  " == (line + lineNoFlat).grouped.nested(2).render(10))
  }
  test("line3 cancels grouping") {
    assert("\n\n" == (line + lineNoFlatNoIndent).grouped.render(10))

    val quote = text("\"\"\"")
    val string = quote + lineNoFlatNoIndent + quote
    val doc = text("foo") + string.tightBracketBy(text("("), text(")"))
    assert(
      doc.grouped.render(100) ==
        """foo(
        |  '''
        |'''
        |)""".stripMargin.replace('\'', '"')
    )
  }
  test("line3 ignores nesting") {
    assert("\n  \n" == (line + lineNoFlatNoIndent).grouped.nested(2).render(10))
  }

}
