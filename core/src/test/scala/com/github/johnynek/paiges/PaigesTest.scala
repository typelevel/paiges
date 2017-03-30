package com.github.johnynek.paiges

import org.scalatest.FunSuite
import org.scalatest.prop.PropertyChecks._
import org.scalacheck.{Arbitrary, Gen}

class PaigesTest extends FunSuite {
  import Generators._

  import Doc.text

  // implicit val generatorDrivenConfig =
  //   PropertyCheckConfiguration(minSuccessful = 100)

  test("basic test") {
     assert((text("hello") +: text("world")).render(100) == "helloworld")
  }

  test("nest test") {
    assert((text("yo") +: (text("yo\nho\nho").nest(2))).render(100) ==
"""yoyo
  ho
  ho""")
  }

  test("paper example") {
    val g = text("hello").line("a").group.line("b").group.line("c").group
    assert(g.render(5) ==
"""hello
a
b
c""")
    assert(g.render(11) == "hello a b c")
  }

  test("nesting with fillWords") {
    val words = List("this", "is", "a", "test", "of", "a", "block", "of", "text")
    val d1 = Doc.fillWords(words.mkString(" "))
    val d2 = d1 +: (Doc.line :+ "love, Oscar").nest(2)
    assert(d2.render(0) == words.mkString("", "\n", "\n  love, Oscar"))
    assert(d2.render(100) == words.mkString("", " ", "\n  love, Oscar"))
  }


  test("test paragraph") {
    val p = Doc.paragraph("This  is      some crazy\n text that should       loook super normal\n\n after we get rid of      the spaces")
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

  test("(x = y) -> (x.## = y.##)") {
    forAll { (a: Doc, b: Doc) =>
      if ((a compare b) == 0) assert(a.## == b.##) else succeed
    }
  }

  test("concat is associative") {
    forAll { (a: Doc, b: Doc, c: Doc, width: Int) =>
      assert(((a +: b) +: c).render(width) ==
        (a +: (b +: c)).render(width))
    }
  }
  test("empty does not change things") {
    forAll { (a: Doc, width: Int) =>
      assert((a +: Doc.empty).render(width) == a.render(width))
      assert((Doc.empty +: a).render(width) == a.render(width))
    }
  }

  test("fillWords can == .map('\\n' => ' ')") {
    forAll { (str: String) =>
      // this test fails if str is all newlines (e.g. "\n")
      if (str.exists(_ != '\n')) {
        val newLineToSpace = str.map {
          case '\n' => ' '
          case other => other
        }
        assert(Doc.fillWords(str).render(str.length) == newLineToSpace)
      } else succeed
    }
  }

  test("line works as expected") {
    forAll { (a: String, b: String) =>
      assert((text(a) line b).render(0) ==
        s"$a\n$b")
    }
  }

  test("space works as expected") {
    forAll { (a: String, b: String) =>
      assert((text(a) space b).render(0) ==
        s"$a $b")
    }
  }

  test("isEmpty == true means render is empty String") {
    forAll { (d: Doc, w0: Int, ws: List[Int]) =>
      if (d.isEmpty) (w0 :: ws).foreach { w =>
        val str = d.render(w)
        assert(str.isEmpty, s"width: $w gave str: $str, should be empty")
      }
      else succeed
    }
  }

  test("test json array example") {
    val items = (0 to 20).map(Doc.str(_))
    val parts = Doc.fill(Doc.comma, items)
    val ary = "[" +: ((parts :+ "]").nest(2))
    assert(ary.render(1000) == (0 to 20).mkString("[", ", ", "]"))
    val expect = """[0, 1, 2, 3, 4, 5,
                   |  6, 7, 8, 9, 10,
                   |  11, 12, 13, 14,
                   |  15, 16, 17, 18,
                   |  19, 20]""".stripMargin
    assert(ary.render(20) == expect)
  }

  test("test json map example") {
    val kvs = (0 to 20).map { i => text("\"%s\": %s".format(s"key$i", i)) }
    val parts = Doc.fill(Doc.comma, kvs)
    val map = parts.bracketBy(Doc.text("{"), Doc.text("}"))
    assert(map.render(1000) == (0 to 20).map { i => "\"%s\": %s".format(s"key$i", i) }.mkString("{ ", ", ", " }"))
    assert(map.render(20) == (0 to 20).map { i => "\"%s\": %s".format(s"key$i", i) }.map("  " + _).mkString("{\n", ",\n", "\n}"))
  }
}
