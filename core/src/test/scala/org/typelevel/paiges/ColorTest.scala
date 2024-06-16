/*
 * Copyright 2017 Typelevel
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

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
  def fromAngle(x: Double): (Int, Int, Int) = {
    val r = ((0.5 + Math.cos(x) / 2) * 6).toInt
    val g = ((0.5 + Math.cos(x - TwoThirdsPi) / 2) * 6).toInt
    val b = ((0.5 + Math.cos(x + TwoThirdsPi) / 2) * 6).toInt
    (r, g, b)
  }

  def fg(x: Double): Style = {
    val (r, g, b) = fromAngle(x)
    Style.XTerm.Fg.laxColor(r, g, b)
  }

  def bg(x: Double): Style = {
    val (r, g, b) = fromAngle(x)
    Style.XTerm.Bg.laxColor(r, g, b) ++ Style.Ansi.Fg.Black
  }

  def rainbow(s: String, steps: Int = -1, styler: Double => Style): Doc = {
    val n = if (steps <= 0) s.length else steps
    def loop(acc: Doc, i: Int, j: Int): Doc =
      if (i >= s.length) acc
      else
        s.charAt(i) match {
          case ' ' =>
            loop(acc + Doc.lineOrSpace, i + 1, j)
          case c =>
            val x = (j * TwoPi) / n
            val d0 = Doc.char(c).style(styler(x))
            loop(acc + d0, i + 1, j + 1)
        }
    loop(Doc.empty, 0, 0)
  }

  test("rainbow demo") {
    val demo = rainbow(Quote, styler = fg).render(80) + "\n\n" + rainbow(Quote, 7, styler = bg).render(28)
    println(demo)
  }
}
