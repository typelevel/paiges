package org.typelevel.paiges

import org.scalatest.funsuite.AnyFunSuite

class ColorTest extends AnyFunSuite {

  val Quote = """Three Rings for the Elven-kings under the sky,
Seven for the Dwarf-lords in their halls of stone,
Nine for Mortal Men doomed to die,
One for the Dark Lord on his dark throne
In the Land of Mordor where the Shadows lie.
One Ring to rule them all, One Ring to find them,
One Ring to bring them all and in the darkness bind them
In the Land of Mordor where the Shadows lie.
"""

  val TwoPi = Math.PI * 2.0
  val TwoThirdsPi = TwoPi / 3.0

  // x cycles in [0, 2π).
  def fromAngle(x: Double): Style = {
    val r = ((0.5 + Math.cos(x) / 2) * 6).toInt
    val g = ((0.5 + Math.cos(x - TwoThirdsPi) / 2) * 6).toInt
    val b = ((0.5 + Math.cos(x + TwoThirdsPi) / 2) * 6).toInt
    Style.XTerm.Fg.laxColor(r, g, b)
  }

  def rainbow(s: String, steps: Int = -1): Doc = {
    val n = if (steps <= 0) s.length else steps
    def loop(acc: Doc, i: Int, j: Int): Doc =
      if (i >= s.length) acc
      else s.charAt(i) match {
        case ' ' =>
          loop(acc + Doc.lineOrSpace, i + 1, j)
        case c =>
          val x = (j * TwoPi) / n
          val d0 = Doc.char(c).style(fromAngle(x))
          loop(acc + d0, i + 1, j + 1)
      }
    loop(Doc.empty, 0, 0)
  }

  test("rainbow demo") {
    val demo = rainbow(Quote).render(80) + "\n\n" + rainbow(Quote, 7).render(28)
    println(demo)
  }
}
