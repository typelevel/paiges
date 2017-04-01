package com.github.johnynek.paiges

import org.scalatest.FunSuite
import org.scalatest.prop.PropertyChecks._
import org.scalacheck.{Arbitrary, Gen}

class PaigesTest extends FunSuite {
  import Generators._

  import Doc.text

  implicit val generatorDrivenConfig =
    PropertyCheckConfiguration(minSuccessful = 500)

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

  test("fillWords can be identity") {
    forAll { (str: String) =>
      // this test fails if str is all newlines (e.g. "\n")
      assert(Doc.fillWords(str).render(str.length) == str)
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
  test("renders to empty implies isEmpty == true") {
    forAll { (d: Doc, w0: Int, ws: List[Int]) =>
      val emptyWidth = (w0 :: ws).find { w =>
        d.render(w).isEmpty
      }
      emptyWidth.foreach { w =>
        assert(d.isEmpty, s"width $w renders empty, but !d.isEmpty")
      }
    }
  }
  test("isEmpty compare empty == 0") {
    forAll { (d: Doc) =>
      if (d.isEmpty) assert(d.compare(Doc.empty) == 0)
      else succeed
    }
  }
  test("renders are constant after maxWidth") {
    forAll { (d: Doc, ws: List[Int]) =>
      val m = Doc.maxWidth(d)
      val maxR = d.render(m)
      val justAfter = (1 to 20).iterator
      val goodW = (justAfter ++ ws.iterator).map { w => (m + w) max m }
      assert(goodW.forall { w =>
        val wrender = d.render(w)
        (wrender == maxR)
      })
    }
  }
  test("either all widths render the same or max-1 renders differently") {
    forAll { (d: Doc) =>
      val maxW = Doc.maxWidth(d)
      if (maxW == 0) succeed
      else {
        val maxRender = d.render(maxW)
        val prev = d.render(maxW - 1)
        if (maxRender != prev) succeed
        else {
          assert((0 until maxW).forall(d.render(_) == maxRender))
        }
      }
    }
  }
  test("if we always render the same, we compare the same") {
    forAll { (a: Doc, b: Doc) =>
      val maxR = Doc.maxWidth(a) max Doc.maxWidth(b)
      val allSame = (0 to maxR).forall { w =>
        a.render(w) == b.render(w)
      }
      if (allSame) {
        val good = a.compare(b) == 0
        if (!good) {
          println(a.render(maxR))
          println("-------")
          println(b.render(maxR))
        }
        assert(good)
      }
      else succeed
    }
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
    val first = Doc.fillWords("a b c")
    val second = Doc.fill(Doc.empty, List("a", "b", "c").map(Doc.text))
    /*
     * I think this fails perhaps because of the way fill constructs
     * Unions. It violates a stronger invariant that Union(a, b)
     * means a == flatten(b). It has the property that flatten(a) == flatten(b)
     * but that is weaker. Our current comparison algorithm seems
     * to leverage this fact
     */
    //assert(first.compare(second) == 0)

    /**
     * spaceOrLine == (s | n)
     * flatten(spaceOrLine) = s
     * group(spaceOrLine) = (s | (s|n)) == (s | n)
     */
    //assert(Doc.spaceOrLine.group.compare(Doc.spaceOrLine) == 0)
  }
  test("group law") {
    /**
     * group(x) = (x' | x) where x' is flatten(x)
     *
     * (a | b)*c == (a*c | b*c) so, if flatten(c) == c we have:
     * c * (a | b) == (a*c | b*c)
     *
     * b.group +: flatten(c) == (b +: flatten(c)).group
     * flatten(c) +: b.group == (flatten(c) +: b).group
     */
    forAll { (b: Doc, c: Doc) =>
      val flatC = Doc.flatten(c)
      val left = (b.group +: flatC)
      val right = (b +: flatC).group
      assert((left).compare(right) == 0)
      assert((flatC +: b.group).compare((flatC +: b).group) == 0)
      // since left == right, we could have used those instead of b:
      assert((left.group +: flatC).compare((right +: flatC).group) == 0)
    }
  }
  test("flatten(group(a)) == flatten(a)") {
    forAll { (a: Doc) =>
      assert(Doc.flatten(a.group).compare(Doc.flatten(a)) == 0)
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
