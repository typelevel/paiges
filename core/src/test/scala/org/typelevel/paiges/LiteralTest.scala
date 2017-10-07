package org.typelevel.paiges

import org.scalatest.FunSuite
import org.typelevel.paiges.Doc.{text, literal, line}

class LiteralTest extends FunSuite {

  test("ignore .nested") {
    assert("\n" == literal("\n").nested(2).render(1))
    assert("\n  " == text("\n").nested(2).render(1))
  }

  test("ignore .grouped") {
    assert("\n" == literal("\n").grouped.render(1))
    assert(" " == text("\n").grouped.render(1))
  }

  test("respect .render(width)") {
    assert("\n " == (literal("\n") + line).grouped.render(1))
    assert("\n \n" == (literal("\n ") + line).grouped.render(1))
    assert("\n\n " == (literal("\n\n") + line).grouped.render(1))
  }

  test("respect .maxWidth") {
    assert(literal("\n").grouped.maxWidth == 0)
    assert(text("\n").grouped.maxWidth == 1)
  }

  test("respect .renderWideStream") {
    assert("\n" == literal("\n").grouped.renderWideStream.mkString)
    assert(" " == text("\n").grouped.renderWideStream.mkString)
  }

  test("respect .isEmpty") {
    assert(literal("").isEmpty)
  }

  test("respect .representation") {
    assert("Literal(a¶b)" == literal("a\nb").representation().render(10))
  }

}
