/*
 * Copyright 2023 Typelevel
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

import scala.annotation.tailrec
import scala.util.Random
import org.scalatest.funsuite.AnyFunSuite

object PaigesTest {

  def repr(d: Doc): String =
    d.representation().render(100)

  def esc(s: String): String =
    "\"" + s.replace("\t", "\\t").replace("\n", "\\n") + "\""

  def debugEq(x: Doc, y: Doc): String = {
    val maxW = Integer.max(x.maxWidth, y.maxWidth)
    (0 until maxW).find(w => !Doc.orderingAtWidth(w).equiv(x, y)) match {
      case Some(w) => s"$w: ${repr(x)} != ${repr(y)} (${esc(x.render(w))} != ${esc(y.render(w))})"
      case None    => sys.error("should not happen")
    }
  }

  def debugNeq(x: Doc, y: Doc): String = {
    val maxW = Integer.max(x.maxWidth, y.maxWidth)
    (0 until maxW).find(w => Doc.orderingAtWidth(w).equiv(x, y)) match {
      case Some(w) => s"$w: ${repr(x)} == ${repr(y)} (${esc(x.render(w))} == ${esc(y.render(w))})"
      case None    => sys.error("should not happen")
    }
  }

  /**
   * Returns true of the given doc contains a hardLine that
   * cannot be grouped or flattened away
   */
  def containsHardLine(doc: Doc): Boolean = {
    import Doc._
    @tailrec
    def loop(stack: List[Doc]): Boolean =
      stack match {
        case Nil => false
        case h :: tail =>
          h match {
            case Line                           => true
            case Empty | Text(_) | ZeroWidth(_) => loop(tail)
            case FlatAlt(_, b)                  => loop(b :: tail)
            case Concat(a, b)                   => loop(a :: b :: tail)
            case Nest(_, d)                     => loop(d :: tail)
            case Align(d)                       => loop(d :: tail)
            case d @ LazyDoc(_)                 => loop(d.evaluated :: tail)
            case Union(a, b)                    => loop(a :: b :: tail)
          }
      }
    loop(doc :: Nil)
  }

  implicit val docEquiv: Equiv[Doc] =
    new Equiv[Doc] {
      def equiv(x: Doc, y: Doc): Boolean = {
        val maxWidth = Integer.max(x.maxWidth, y.maxWidth)
        def randomWidth(): Int = Random.nextInt(maxWidth)
        val widths =
          if (maxWidth == 0) 0 :: Nil
          else 0 :: randomWidth() :: randomWidth() :: Nil
        Doc.equivAtWidths(widths).equiv(x, y)
      }
    }

  implicit class EquivSyntax(lhs: Doc) {
    override def toString: String = lhs.toString
    def ===(rhs: Doc): Boolean = docEquiv.equiv(lhs, rhs)
  }

  def slowRenderTrim(d: Doc, width: Int): String = {
    val parts = d.render(width).split("\n", -1).toList
    parts match {
      case Nil => sys.error("unreachable")
      case other =>
        other
          .map(str => str.reverse.dropWhile(_ == ' ').reverse)
          .mkString("\n")
    }
  }

  def twoRightAssociated(d: Doc): Boolean = {
    import Doc._
    d match {
      case Empty | Text(_) | ZeroWidth(_) | Line => true
      case FlatAlt(a, _)                         => twoRightAssociated(a)
      case Concat(Concat(Concat(_, _), _), _)    => false
      case Concat(a, b) =>
        twoRightAssociated(a) && twoRightAssociated(b)
      case Union(a, _)    => twoRightAssociated(a)
      case f @ LazyDoc(_) => twoRightAssociated(f.evaluated)
      case Align(d)       => twoRightAssociated(d)
      case Nest(_, d)     => twoRightAssociated(d)
    }
  }

  // Definition of `fill` from the paper
  def fillSpec(sep: Doc, ds: List[Doc]): Doc = {
    import Doc._
    ds match {
      case Nil      => empty
      case x :: Nil => x.grouped
      case x :: y :: zs =>
        Union(x.flatten + (sep.flatten + defer(fillSpec(sep, y.flatten :: zs))),
              x + (sep + defer(fillSpec(sep, y :: zs)))
        )
    }
  }
}

class PaigesTest extends AnyFunSuite {
  import Doc.text
  import PaigesTest._

  test("basic test") {
    assert((text("hello") + text("world")).render(100) == "helloworld")
  }

  test("nested test") {
    assert(
      (text("yo") + (text("yo\nho\nho").nested(2))).render(100) ==
        """yoyo
  ho
  ho"""
    )
  }

  test("paper example") {
    val g = (((text("hello") :/ "a").grouped :/ "b").grouped :/ "c").grouped
    assert(
      g.render(5) ==
        """hello
a
b
c"""
    )
    assert(g.render(11) == "hello a b c")
  }

  test("nesteding with paragraph") {
    val words = List("this", "is", "a", "test", "of", "a", "block", "of", "text")
    val d1 = Doc.paragraph(words.mkString(" "))
    val d2 = d1 + (Doc.line :+ "love, Oscar").nested(2)
    assert(d2.render(0) == words.mkString("", "\n", "\n  love, Oscar"))
    assert(d2.render(100) == words.mkString("", " ", "\n  love, Oscar"))
  }

  test("test paragraph") {
    val p = Doc.paragraph(
      "This  is      some crazy\n text that should       loook super normal\n\n after we get rid of      the spaces"
    )
    assert(p.render(10) == """This is
some crazy
text that
should
loook
super
normal
after we
get rid of
the spaces""")
  }

  test("dangling space 1") {
    val d = Doc
      .stack(
        List(
          Doc.text("a"),
          Doc.text("b"),
          Doc.empty
        )
      )
      .nested(2)
    val expected = "a\n  b\n"
    assert(d.renderTrim(100) == expected)
    assert(d.renderTrim(100) == slowRenderTrim(d, 100))
    assert(d.renderStreamTrim(100).mkString == expected)
  }

  test("dangling space 2") {
    val d = Doc
      .stack(
        List(
          Doc.empty,
          Doc.text("a"),
          Doc.empty,
          Doc.empty,
          Doc.text("b")
        )
      )
      .nested(2)
    val expected = "\n  a\n\n\n  b"
    assert(d.renderTrim(100) == expected)
    assert(d.renderTrim(100) == slowRenderTrim(d, 100))
    assert(d.renderStreamTrim(100).mkString == expected)
  }

  test("renderTrim trims a single line") {
    import Doc._
    val d = Text("a   ")
    val expected = "a"
    assert(d.renderTrim(100) == expected)
    assert(d.renderTrim(100) == slowRenderTrim(d, 100))
    assert(d.renderStreamTrim(100).mkString == expected)
  }

  test("hard union cases") {

    /**
     * if s == space, and n == line
     * we know that:
     * a * (s|n) * b * (s|n) * c =
     *
     * (a * s * ((b * s * c) | (b * n * c)) |
     *   (a * n * (b * s * c) | (b * n * c))
     */
    val first = Doc.paragraph("a b c")
    val second = Doc.fill(Doc.lineOrSpace, List("a", "b", "c").map(Doc.text))
    /*
     * I think this fails perhaps because of the way fill constructs
     * Unions. It violates a stronger invariant that Union(a, b)
     * means a == flatten(b). It has the property that flatten(a) == flatten(b)
     * but that is weaker. Our current comparison algorithm seems
     * to leverage this fact
     */
    assert(first === second)

    /**
     * lineOrSpace == (s | n)
     * flatten(lineOrSpace) = s
     * group(lineOrSpace) = (s | (s|n)) == (s | n)
     */
    assert(Doc.lineOrSpace.grouped === Doc.lineOrSpace)
  }

  test("test json array example") {
    val items = (0 to 20).map(Doc.str(_))
    val parts = Doc.fill(Doc.comma + Doc.line, items)
    val ary = "[" +: ((parts :+ "]").aligned)
    assert(ary.renderWideStream.mkString == (0 to 20).mkString("[", ", ", "]"))
    val expect = """[0, 1, 2, 3, 4, 5,
                   | 6, 7, 8, 9, 10, 11,
                   | 12, 13, 14, 15, 16,
                   | 17, 18, 19, 20]""".stripMargin
    assert(ary.render(20) == expect)
  }

  test("test json map example") {
    val kvs = (0 to 20).map(i => text("\"%s\": %s".format(s"key$i", i)))
    val parts = Doc.fill(Doc.comma + Doc.lineOrSpace, kvs)

    val map = parts.bracketBy(Doc.text("{"), Doc.text("}"))
    assert(
      map.render(1000) == (0 to 20)
        .map(i => "\"%s\": %s".format(s"key$i", i))
        .mkString("{ ", ", ", " }")
    )
    assert(
      map.render(20) == (0 to 20)
        .map(i => "\"%s\": %s".format(s"key$i", i))
        .map("  " + _)
        .mkString("{\n", ",\n", "\n}")
    )

    val map2 = parts.tightBracketBy(Doc.text("{"), Doc.text("}"))
    assert(
      map2.render(1000) == (0 to 20)
        .map(i => "\"%s\": %s".format(s"key$i", i))
        .mkString("{", ", ", "}")
    )
    assert(
      map2.render(20) == (0 to 20)
        .map(i => "\"%s\": %s".format(s"key$i", i))
        .map("  " + _)
        .mkString("{\n", ",\n", "\n}")
    )
  }

  test("maxWidth is stack safe") {
    assert(Doc.intercalate(Doc.lineOrSpace, (1 to 100000).map(Doc.str)).maxWidth >= 0)
  }

  test("renderWide is stack safe") {
    val nums = 1 to 100000
    assert(Doc.intercalate(Doc.lineOrSpace, nums.map(Doc.str)).renderWideStream.mkString == nums.mkString(" "))
  }

  test("lineBreak works as expected") {
    import Doc._
    // render a tight list:
    val res = text("(") + Doc.intercalate((Doc.comma + Doc.lineBreak).grouped, (1 to 20).map(Doc.str)) + text(")")
    assert(res.render(10) == """(1,2,3,4,
                               |5,6,7,8,9,
                               |10,11,12,
                               |13,14,15,
                               |16,17,18,
                               |19,20)""".stripMargin)
    assert(res.renderWideStream.mkString == (1 to 20).mkString("(", ",", ")"))
  }
  test("align works as expected") {
    import Doc._
    // render with alignment
    val d1 = text("fooooo ") + text("bar").line(text("baz")).aligned

    assert(d1.render(0) == """fooooo bar
                             |       baz""".stripMargin)
  }

  test("fill example") {
    import Doc.{comma, fill, text}
    val ds = text("1") :: text("2") :: text("3") :: Nil
    val doc = fill(comma + Doc.line, ds)

    assert(doc.render(0) == "1,\n2,\n3")
    assert(doc.render(6) == "1, 2,\n3")
    assert(doc.render(10) == "1, 2, 3")
  }

  test("Doc.tabulate works in some example cases") {
    val caseMatch = List(
      ("Item1(x)", Doc.text("callItem(x)")),
      ("ItemXandItemY(x, y)", Doc.text("callItem(x)") / Doc.text("callItem(y)")),
      ("ItemXandItemYandZ(x, y, z)", Doc.text("callItem(x)") / Doc.text("callItem(y)") / Doc.text("callItem(z)"))
    )

    val expected = """case Item1(x)                   => callItem(x)
                     |case ItemXandItemY(x, y)        => callItem(x)
                     |                                   callItem(y)
                     |case ItemXandItemYandZ(x, y, z) => callItem(x)
                     |                                   callItem(y)
                     |                                   callItem(z)""".stripMargin

    assert(Doc.tabulate(' ', " => ", caseMatch.map { case (s, d) => ("case " + s, d) }).render(20) == expected)
  }

  test("abbreviated Doc.tabulate works in an example case") {

    val pairs = List(
      "alpha: " -> Doc.text("the first item in the list"),
      "beta: " -> Doc.text("another item;") / Doc.text("this one is longer"),
      "gamma: " -> Doc.text("a third, uninteresting case"),
      "delta: " -> Doc.text("a fourth,") / (Doc.text("multiline,") / Doc.text("indented")).nested(2) / Doc.text("case"),
      "epsilon: " -> Doc.text("the ultimate case")
    )

    val expected = """alpha:   the first item in the list
                     |beta:    another item;
                     |         this one is longer
                     |gamma:   a third, uninteresting case
                     |delta:   a fourth,
                     |         multiline,
                     |           indented
                     |         case
                     |epsilon: the ultimate case""".stripMargin
    assert(Doc.tabulate(pairs).render(40) == expected)
  }

  test("cat") {
    assert(Doc.cat(List("1", "2", "3").map(Doc.text)).render(80) == "123")
  }

  test("defer doesn't evaluate immediately") {
    var count = 0
    val doc = Doc.defer {
      count += 1
      Doc.text("done")
    }
    assert(count == 0)
    assert(doc.renderWideStream.mkString == "done")
    assert(count == 1)
  }

  test("defer short circuits") {
    var d1count = 0
    val d1 = Doc.defer {
      d1count += 1
      Doc.empty
    }

    var d2count = 0
    val d2 = Doc.defer {
      d2count += 1
      d1
    }

    d2.render(0)
    assert(d1count == 1)
    assert(d2count == 1)
    // rendering d1 does not increment d1
    d1.render(0)
    assert(d1count == 1)
    assert(d2count == 1)
  }
}
