package com.github.johnynek.paiges

import org.scalatest.FunSuite
import org.scalatest.prop.PropertyChecks._
import org.scalacheck.{Arbitrary, Gen}
import scala.collection.immutable.SortedSet

class PaigesTest extends FunSuite {
  import Generators._

  import Doc.text

  implicit val generatorDrivenConfig =
    PropertyCheckConfiguration(minSuccessful = 500)

  test("basic test") {
     assert((text("hello") + text("world")).render(100) == "helloworld")
  }

  test("nested test") {
    assert((text("yo") + (text("yo\nho\nho").nested(2))).render(100) ==
"""yoyo
  ho
  ho""")
  }

  test("paper example") {
    val g = (((text("hello") :/ "a").grouped :/ "b").grouped :/ "c").grouped
    assert(g.render(5) ==
"""hello
a
b
c""")
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
      assert(((a + b) + c).render(width) ==
        (a + (b + c)).render(width))
    }
  }
  test("empty does not change things") {
    forAll { (a: Doc, width: Int) =>
      assert((a + Doc.empty).render(width) == a.render(width))
      assert((Doc.empty + a).render(width) == a.render(width))
    }
  }

  test("Doc.split(s, \" \", space).render(w) = s") {
    forAll { (s: String, w: Int) =>
      assert(Doc.split(s, " ".r, Doc.space).render(w) == s)
    }
  }

  test("splitting x on x gives x") {
    forAll(Gen.frequency((10, Gen.identifier), (1, Gen.const(" "))), Gen.choose(0, 100)) { (s, w) =>
      assert(Doc.split(s, s.r, Doc.text(s)).render(w) == s)
    }
  }

  test("(a /: b)  works as expected") {
    forAll { (a: String, b: String) =>
      assert((a /: Doc.text(b)).render(0) == s"$a\n$b")
    }
  }

  test("space works as expected") {
    forAll { (a: String, b: String) =>
      assert((text(a) space b).render(0) ==
        s"$a $b")
    }
  }

  test("isEmpty == render(w).isEmpty for all w") {
    forAll { (d: Doc) =>
      if (d.isEmpty) {
        assert((0 to d.maxWidth).forall(d.render(_).isEmpty), s"${d.representation(true).render(50)} has nonEmpty renderings")
      }
      else succeed
    }
  }
  test("nonEmpty == !isEmpty") {
    forAll { (d: Doc) =>
      assert(d.nonEmpty == !d.isEmpty)
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
      val m = d.maxWidth
      val maxR = d.render(m)
      val justAfter = (1 to 20).iterator
      val goodW = (justAfter ++ ws.iterator).map { w => (m + w) max m }
      assert(goodW.forall { w => d.render(w) == maxR })
    }
  }
  test("if we always render the same, we compare the same") {
    forAll { (a: Doc, b: Doc) =>
      val maxR = a.maxWidth max b.maxWidth
      val allSame = (0 to maxR).forall { w =>
        a.render(w) == b.render(w)
      }
      if (allSame) assert(a.compare(b) == 0)
      else succeed
    }
  }
  test("render(w) == render(0) for w <= 0") {
    forAll { (a: Doc, w: Int) =>
      val wNeg = if (w > 0) -w else w
      assert(a.render(wNeg) == a.render(0), s"${a.representation(true).render(40)}.render($wNeg) fails")
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
    val first = Doc.paragraph("a b c")
    val second = Doc.fill(Doc.lineOrSpace, List("a", "b", "c").map(Doc.text))
    /*
     * I think this fails perhaps because of the way fill constructs
     * Unions. It violates a stronger invariant that Union(a, b)
     * means a == flatten(b). It has the property that flatten(a) == flatten(b)
     * but that is weaker. Our current comparison algorithm seems
     * to leverage this fact
     */
    assert(first.compare(second) == 0)

    /**
     * lineOrSpace == (s | n)
     * flatten(lineOrSpace) = s
     * group(lineOrSpace) = (s | (s|n)) == (s | n)
     */
    assert(Doc.lineOrSpace.grouped.compare(Doc.lineOrSpace) == 0)
  }
  test("group law") {
    /**
     * group(x) = (x' | x) where x' is flatten(x)
     *
     * (a | b)*c == (a*c | b*c) so, if flatten(c) == c we have:
     * c * (a | b) == (a*c | b*c)
     *
     * b.grouped + flatten(c) == (b + flatten(c)).grouped
     * flatten(c) + b.grouped == (flatten(c) + b).grouped
     */
    forAll { (b: Doc, c: Doc) =>
      val flatC = c.flatten
      val left = (b.grouped + flatC)
      val right = (b + flatC).grouped
      assert((left).compare(right) == 0)
      assert((flatC + b.grouped).compare((flatC + b).grouped) == 0)
      // since left == right, we could have used those instead of b:
      assert((left.grouped + flatC).compare((right + flatC).grouped) == 0)
    }
  }
  test("flatten(group(a)) == flatten(a)") {
    forAll { (a: Doc) =>
      assert(a.grouped.flatten.compare(a.flatten) == 0)
    }
  }
  test("a.flatten == a.flatten.flatten") {
    forAll { (a: Doc) =>
      val aflat = a.flatten
      assert(aflat.compare(aflat.flatten) == 0)
    }
  }
  test("a.flatten == a.flattenOption.getOrElse(a)") {
    forAll { (a: Doc) =>
      assert(a.flatten.compare(a.flattenOption.getOrElse(a)) == 0)
    }
  }

  test("the left and right side of a union are the same after flattening") {
    import Doc._
    def okay(d: Doc): Boolean = d match {
      case Empty | Text(_) => true
      case Line(d) => okay(d)
      case Concat(a, b) => okay(a) && okay(b)
      case Nest(j, d) => okay(d)
      case Align(d) => okay(d)
      case u@Union(a, _) =>
        (a.flatten.compare(u.bDoc.flatten) == 0) && okay(a) && okay(u.bDoc)
    }

    forAll { (a: Doc) => assert(okay(a)) }
  }

  test("the left side of a union has a next line as long or longer than the right") {
    import Doc._

    def nextLineLength(d: Doc): (Boolean, Int) = d match {
      case Line(_) => (true, 0)
      case Empty => (false, 0)
      case Text(s) => (false, s.length)
      case Nest(j, d) => nextLineLength(d) // nesteding only matters AFTER the next line
      case Align(d) => nextLineLength(d) // aligning only matters AFTER the next line
      case Concat(a, b) =>
        val r1@(done, l) = nextLineLength(a)
        if (!done) {
          val (d2, l2) = nextLineLength(b)
          (d2, l2 + l)
        } else r1
      case Union(a, _) => nextLineLength(a) // assume the property is true
    }

    def okay(d: Doc): Boolean = d match {
      case Empty | Text(_) | Line(_) => true
      case Nest(j, d) => okay(d)
      case Align(d) => okay(d)
      case Concat(a, b) => okay(a) && okay(b)
      case u@Union(a, _) =>
        nextLineLength(a)._2 >= nextLineLength(u.bDoc)._2
    }

    forAll { (a: Doc) => assert(okay(a)) }
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

  test("test Doc.text") {
    forAll { (s: String) =>
      assert(Doc.text(s).render(0) == s)
    }
  }

  test("test json map example") {
    val kvs = (0 to 20).map { i => text("\"%s\": %s".format(s"key$i", i)) }
    val parts = Doc.fill(Doc.comma + Doc.lineOrSpace, kvs)
    val map = parts.bracketBy(Doc.text("{"), Doc.text("}"))
    assert(map.render(1000) == (0 to 20).map { i => "\"%s\": %s".format(s"key$i", i) }.mkString("{ ", ", ", " }"))
    assert(map.render(20) == (0 to 20).map { i => "\"%s\": %s".format(s"key$i", i) }.map("  " + _).mkString("{\n", ",\n", "\n}"))
  }

  test("isSubDoc works correctly: group") {
    forAll { (d: Doc) =>
      val f = d.flatten
      val g = d.grouped
      assert(f.isSubDocOf(g))
    }
  }

  test("isSubDoc works correctly: fill") {
    forAll { (d0: Doc, d1: Doc, dsLong: List[Doc]) =>
      // we need at least 2 docs for this law
      val ds = (d0 :: d1 :: dsLong.take(4))
      val f = Doc.fill(Doc.lineOrSpace, ds)
      val g = Doc.intercalate(Doc.space, ds.map(_.flatten))
      assert(g.isSubDocOf(f))
    }
  }

  test("if isSubDoc is true, there is some width that renders the same") {
    forAll { (d1: Doc, d2: Doc) =>
      if (d1.isSubDocOf(d2)) {
        val mx = d1.maxWidth max d2.maxWidth
        assert((0 to mx).exists { w => d1.render(w) == d2.render(w) })
      }
      else succeed
    }
  }
  test("a isSubDocOf b and b isSubDocOf a iff a == b") {
    forAll { (a: Doc, b: Doc) =>
      assert(a.isSubDocOf(a))
      assert(b.isSubDocOf(b))
      val cmp = a compare b
      val eq = a.isSubDocOf(b) && b.isSubDocOf(a)
      if (cmp == 0) assert(eq)
      else assert(!eq)
    }
  }
  test("setDiff(a, a) == None") {
    forAll { (a: Doc) =>
      val atree = DocTree.toDocTree(a)
      // we should totally empty a tree
      assert(DocTree.setDiff(atree, atree).isEmpty)
    }
  }
  test("after setDiff isSubDoc is false") {
    forAll { (a: Doc, b: Doc) =>
      val atree = DocTree.toDocTree(a)
      val btree = DocTree.toDocTree(b)
      if (DocTree.isSubDoc(atree, btree)) {
        DocTree.setDiff(btree, atree) match {
          case None =>
            // If a is a subset of b, and b - a == empty, then a == b
            assert(a.compare(b) == 0)
          case Some(diff) =>
            assert(!DocTree.isSubDoc(atree, diff))
        }
      }
      else {
        /*
         * We either have disjoint, overlapping, or btree is a strict subset of atree
         */
        DocTree.setDiff(btree, atree) match {
          case None =>
            // if we btree is a strict subset of of atree
            assert(DocTree.isSubDoc(btree, atree))
          case Some(bMinusA) =>
            // disjoint or overlapping, so atree and bMinusA are disjoint
            assert(!DocTree.isSubDoc(atree, bMinusA))
            assert(((DocTree.deunioned(atree).toSet) & (DocTree.deunioned(bMinusA).toSet)).isEmpty)
        }
      }
    }
  }

  test("deunioning removes all unions") {
    forAll { (d: Doc) =>
      assert(d.deunioned.forall(_.deunioned.length == 1))
    }
  }

  test("if deunioned is a subset, then isSubDocOf") {
    forAll { (a: Doc, b: Doc) =>
      val da = a.deunioned
      val db = b.deunioned
      assert(a.isSubDocOf(b) == SortedSet(da: _*).subsetOf(SortedSet(db: _*)))
    }
  }
  test("Doc.repeat matches naive implementation") {
    /**
     * comparing large equal documents can be very slow
     * :(
     */
    implicit val generatorDrivenConfig =
      PropertyCheckConfiguration(minSuccessful = 30)
    val smallTree = Gen.choose(0, 3).flatMap(genTree)
    val smallInt = Gen.choose(0, 10)

    def simple(n: Int, d: Doc, acc: Doc): Doc = if(n <= 0) acc else simple(n - 1, d, acc + d)

    forAll(smallTree, smallInt) { (d: Doc, small: Int) =>
      assert(simple(small, d, Doc.empty).compare(d * small) == 0)
    }
  }
  test("(d * a) * b == d * (a * b)") {
    /**
     * comparing large equal documents can be very slow
     * :(
     */
    implicit val generatorDrivenConfig =
      PropertyCheckConfiguration(minSuccessful = 30)
    val smallTree = Gen.choose(0, 3).flatMap(genTree)
    val smallInt = Gen.choose(0, 3)

    forAll(smallTree, smallInt, smallInt) { (d: Doc, a: Int, b: Int) =>
      assert(((d * a) * b).compare(d * (a * b)) == 0)
    }
  }
  test("text(s) * n == s * n for identifiers") {
    forAll(Gen.identifier, Gen.choose(0, 20)) { (s, n) =>
      assert((Doc.text(s) * n).render(0) == (s * n))
    }
  }

  test("maxWidth is stack safe") {
    assert(Doc.intercalate(Doc.lineOrSpace, (1 to 100000).map(Doc.str)).maxWidth >= 0)
  }

  test("renderWide is stack safe") {
    val nums = (1 to 100000)
    assert(Doc.intercalate(Doc.lineOrSpace, nums.map(Doc.str)).renderWideStream.mkString == nums.mkString(" "))
  }

  test("render(w) == renderStream(w).mkString") {
    forAll { (d: Doc, w: Int) =>
      assert(d.render(w) == d.renderStream(w).mkString)
    }
  }
  test("renderWide == render(maxWidth)") {
    forAll { (d: Doc) =>
      val max = d.maxWidth
      assert(d.renderWideStream.mkString == d.render(max))
    }
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
    val d1 = text("fooooo ") + (text("bar") line text("baz")).aligned

    assert(d1.render(0) == """fooooo bar
                             |       baz""".stripMargin)
  }

  test("fill example") {
    import Doc.{ comma, text, fill }
    val ds = text("1") :: text("2") :: text("3") :: Nil
    val doc = fill(comma + Doc.line, ds)

    assert(doc.render(0) == "1,\n2,\n3")
    assert(doc.render(6) == "1, 2,\n3")
    assert(doc.render(10) == "1, 2, 3")
  }

  test("a == b implies f(a) == f(b)") {
    import Doc.docOrdering

    def law(a: Doc, b: Doc, f: Doc => Doc) =
      if (docOrdering.equiv(a, b)) {
        assert(docOrdering.equiv(f(a), f(b)), s"${a.representation(true).render(40)}\n\n${b.representation(true).render(40)}")
      }
      else ()

    // Here are some hard cases
    val examples = List((Doc.line, Doc.lineBreak, { (d: Doc) => d.grouped }))
    examples.foreach { case (a, b, f) => law(a, b, f) }

    implicit val generatorDrivenConfig =
      PropertyCheckConfiguration(minSuccessful = 5000)

    forAll(genDoc, genDoc, unary) { (a, b, f) => law(a, b, f) }
  }
}
