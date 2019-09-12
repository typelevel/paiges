package org.typelevel.paiges

import org.scalacheck.Gen
import org.scalatest.Assertion
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks._

class PaigesScalacheckTest extends AnyFunSuite {
  import Doc.text
  import Generators._
  import PaigesTest._

  implicit val generatorDrivenConfig =
    PropertyCheckConfiguration(minSuccessful = 500)

  test("(x = y) -> (x.## = y.##)") {
    forAll { (a: Doc, b: Doc) =>
      assert(a.## == a.##)
      assert(b.## == b.##)
      if (a == b) assert(a.## == b.##) else succeed
    }
  }

  test("concat is associative") {
    forAll { (a: Doc, b: Doc, c: Doc) =>
      assert(((a + b) + c) === (a + (b + c)))
    }
  }

  test("line is associative") {
    forAll { (a: Doc, b: Doc, c: Doc) =>
      assert(a.line(b).line(c) === a.line(b.line(c)))
    }
  }

  test("lineOrSpace is associative") {
    forAll { (a: Doc, b: Doc, c: Doc) =>
      assert(a.lineOrSpace(b).lineOrSpace(c) === a.lineOrSpace(b.lineOrSpace(c)))
    }
  }

  test("LazyDoc.evaluate never returns a LazyDoc") {
    forAll { (a: Doc) =>
      val ld = Doc.LazyDoc(() => a)
      assert(!ld.evaluated.isInstanceOf[Doc.LazyDoc])
    }
  }

  test("writeTo works") {
    import java.io._
    forAll { (doc: Doc, w: Int) =>
      val baos = new ByteArrayOutputStream()
      val pw = new PrintWriter(baos)
      doc.writeTo(w, pw)
      pw.close()
      val s1 = baos.toString("UTF-8")
      val s2 = doc.render(w)
      assert(s1 == s2)
    }
  }

  test("writeToTrim works") {
    import java.io._
    forAll { (doc: Doc, w: Int) =>
      val baos = new ByteArrayOutputStream()
      val pw = new PrintWriter(baos)
      doc.writeToTrim(w, pw)
      pw.close()
      val s1 = baos.toString("UTF-8")
      val s2 = doc.renderTrim(w)
      assert(s1 == s2)
    }
  }

  test("empty does not change things") {
    forAll { (a: Doc) =>
      assert((a + Doc.empty) === a)
      assert((Doc.empty + a) === a)
    }
  }

  test("spaces(n) == text(\" \") * n == text(\" \" * n)") {
    forAll(Gen.choose(-10, 1000)) { n =>
      val sn = Doc.spaces(n)
      val tn = if (n <= 0) Doc.text("") else Doc.text(" ") * n
      val un = if (n <= 0) Doc.text("") else Doc.text(" " * n)

      assert(sn === tn)
      assert(tn === un)
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

  test("renderStreamTrim and renderTrim are consistent") {
    forAll { (d: Doc, width0: Int) =>
      val width = width0 & 0xFFF
      assert(d.renderStreamTrim(width).mkString == d.renderTrim(width),
        s"input=${repr(d)}")
    }
  }

  test("trim-law: renderTrim is what we expect") {
    forAll { (d: Doc) =>
      val trim = d.renderTrim(100)
      val slowTrim = slowRenderTrim(d, 100)
      assert(trim == slowTrim, s"input=${repr(d)}")
    }
  }

  test("(a /: b)  works as expected") {
    forAll { (a: String, b: String) =>
      assert((a /: Doc.text(b)).render(0) == s"$a\n$b")
    }
  }

  test("space works as expected") {
    forAll { (a: String, b: String) =>
      val res = s"$a $b"
      assert((text(a) space b).render(0) == res)
      assert((text(a) & text(b)).render(0) == res)
      assert((text(a) :& b).render(0) == res)
      assert((a &: text(b)).render(0) == res)
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
      if (d.isEmpty) assert(d === Doc.empty)
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

  test("render(w) == render(0) for w <= 0") {
    forAll { (a: Doc, w: Int) =>
      val wNeg = if (w > 0) -w else w
      assert(a.render(wNeg) == a.render(0), s"${a.representation(true).render(40)}.render($wNeg) fails")
    }
  }

  test("c.flatten + d.flatten == (c + d).flatten") {
    forAll { (c: Doc, d: Doc) =>
      c.flatten + d.flatten === (c + d).flatten
    }
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
    def law(b: Doc, c: Doc): Assertion = {
      val flatC = c.flatten

      val left = b.grouped + flatC
      val right = (b + flatC).grouped
      assert(left === right, debugEq(left, right))

      if (!flatC.containsHardNewline) {
        val lhs0 = flatC + b.grouped
        val rhs0 = (flatC + b).grouped
        assert(lhs0 === rhs0, debugEq(lhs0, rhs0))
      }

      // since left == right, we could have used those instead of b:
      val lhs1 = left.grouped + flatC
      val rhs1 = (right + flatC).grouped
      assert(lhs1 === rhs1, debugEq(lhs1, rhs1))
    }

    import Doc._
    val b0 = Concat(Text("b"), Union(Text(" "), FlatAlt(Line, Text(" "))))
    val c0 = Union(Concat(Text("b"), Text(" ")), Concat(Text("b"), FlatAlt(Line, Text(" "))))
    law(b0, c0)
    forAll { (b: Doc, c: Doc) =>
      law(b, c)
    }
  }

  test("flatten(group(a)) == flatten(a)") {
    forAll { (a: Doc) =>
      assert(a.grouped.flatten === a.flatten)
    }
  }
  test("a.flatten == a.flatten.flatten") {
    forAll { (a: Doc) =>
      val aflat = a.flatten
      assert(aflat === aflat.flatten)
    }
  }
  test("a.flatten == a.flattenOption.getOrElse(a)") {
    forAll { (a: Doc) =>
      val lhs = a.flatten
      val rhs = a.flattenOption.getOrElse(a)
      assert(lhs === rhs, debugEq(lhs, rhs))
    }
  }

  test("a.aligned.aligned == a.aligned") {
    forAll { a: Doc =>
      assert(a.aligned.aligned === a.aligned)
    }
  }

  test("a is flat ==> Concat(a, Union(b, c)) === Union(Concat(a, b), Concat(a, c))") {
    import Doc._
    forAll { (aGen: Doc, bc: Doc) =>
      val a = aGen.flatten
      if (a.containsHardNewline) {
        ()
      } else {
        bc.grouped match {
          case d@Union(b, c) =>
            val lhs = Concat(a, d)
            val rhs = Union(Concat(a, b), Concat(a, c))
            assert(lhs === rhs, debugEq(lhs, rhs))
          case _ =>
        }
      }
    }
  }

  test("c is flat ==> Concat(Union(a, b), c) === Union(Concat(a, c), Concat(b, c))") {
    import Doc._
    forAll { (ab: Doc, cGen: Doc) =>
      val c = cGen.flatten
      ab.grouped match {
        case d@Union(a, b) =>
          assert(Concat(d, c) === Union(Concat(a, c), Concat(b, c)))
        case _ =>
      }
    }
  }

  test("Union invariant: `a.flatten == b.flatten`") {
    forAll { d: Doc.Union => assert(d.a.flatten === d.b.flatten) }
  }

  test("Union invariant: `a != b`") {
    forAll { d: Doc.Union => assert(d.a !== d.b) }
  }

  test("Union invariant: `a` has 2-right-associated `Concat` nodes") {
    forAll { d: Doc.Union =>
      assert(PaigesTest.twoRightAssociated(d.a), repr(d))
    }
  }

  test("Union invariant: the first line of `a` is at least as long as the first line of `b`") {
    forAll(Gen.choose(1, 200), genUnion) { (n, u) =>
      def firstLine(d: Doc) = {
        def loop(s: Stream[String], acc: List[String]): String =
          s match {
            case Stream.Empty => acc.reverse.mkString
            case head #:: _ if head.contains('\n') => (head.takeWhile(_ != '\n') :: acc).reverse.mkString
            case head #:: tail => loop(tail, head :: acc)
          }
        loop(d.renderStream(n), Nil)
      }
      assert(firstLine(u.a).length >= firstLine(u.b).length)
    }
  }

  test("test Doc.text") {
    forAll { (s: String) =>
      assert(Doc.text(s).render(0) == s)
    }
  }

  test("Doc.repeat matches naive implementation") {
    /**
     * comparing large equal documents can be very slow
     * :(
     */
    implicit val generatorDrivenConfig =
      PropertyCheckConfiguration(minSuccessful = 30)
    val smallTree = Gen.choose(0, 3).flatMap(genTree(_, withFill = true))
    val smallInt = Gen.choose(0, 10)

    def simple(n: Int, d: Doc, acc: Doc): Doc =
      if(n <= 0) acc else simple(n - 1, d, acc + d)

    forAll(smallTree, smallInt) { (d: Doc, small: Int) =>
      assert(simple(small, d, Doc.empty) === (d * small))
    }
  }
  test("(d * a) * b == d * (a * b)") {
    /**
     * comparing large equal documents can be very slow
     * :(
     */
    implicit val generatorDrivenConfig =
      PropertyCheckConfiguration(minSuccessful = 30)
    val smallTree = Gen.choose(0, 3).flatMap(genTree(_, withFill = true))
    val smallInt = Gen.choose(0, 3)

    forAll(smallTree, smallInt, smallInt) { (d: Doc, a: Int, b: Int) =>
      assert(((d * a) * b) === (d * (a * b)))
    }
  }
  test("text(s) * n == s * n for identifiers") {
    forAll(Gen.identifier, Gen.choose(0, 20)) { (s, n) =>
      assert((Doc.text(s) * n).render(0) == (s * n))
    }
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

  test("character concat works") {
    forAll { (c1: Char, c2: Char) =>
      val got = Doc.char(c1) + Doc.char(c2)
      val expected = Doc.text(new String(Array(c1, c2)))
      assert(got === expected)
    }

    // here is a hard case:
    assert((Doc.char('a') + Doc.char('\n')) === Doc.text("a\n"))
  }

  test("fill matches spec") {
    val docsGen = for {
      n <- Gen.choose(0, 12)
      ds <- Gen.listOfN(n, genDoc)
    } yield ds
    forAll(genDoc, docsGen) { (sep: Doc, ds: List[Doc]) =>
      val lhs = Doc.fill(sep, ds)
      val rhs = fillSpec(sep, ds)
      assert(lhs === rhs, debugEq(lhs, rhs))
    }
  }

  test("FlatAlt invariant 0: FlatAlt renders as default") {
    forAll { (d1: Doc, d2: Doc, w: Int) =>
      Doc.FlatAlt(d1, d2).render(w) == d1.render(w)
    }
  }

  test("FlatAlt invariant 1: All constructed FlatAlt have width(default) <= width(flattened)") {
    import Doc._
    def law(d: Doc): Boolean =
      d match {
        case Empty | Text(_) | Line => true
        case FlatAlt(a, b) =>
          a.maxWidth <= b.maxWidth
        case Concat(a, b) =>
          law(a) && law(b)
        case Union(a, b) =>
          law(a) && law(b)
        case f@LazyDoc(_) => law(f.evaluated)
        case Align(d) => law(d)
        case Nest(_, d) => law(d)
      }

    forAll { d: Doc => assert(law(d)) }
  }

  test("FlatAlt invariant 2: default != whenFlat (otherwise the FlatAlt is redundant)") {
    import Doc._
    def law(d: Doc): Boolean =
      d match {
        case Empty | Text(_) | Line => true
        case FlatAlt(a, b) =>
          a !== b
        case Concat(a, b) =>
          law(a) && law(b)
        case Union(a, b) =>
          law(a) && law(b)
        case f@LazyDoc(_) => law(f.evaluated)
        case Align(d) => law(d)
        case Nest(_, d) => law(d)
      }

    forAll { d: Doc => assert(law(d)) }
  }

  test("FlatAlt invariant 3: FlatAlt does not occur on the left side of a union") {
    import Doc._
    def law(d: Doc, isLeft: Boolean): Boolean =
      d match {
        case Empty | Text(_) | Line => true
        case FlatAlt(a, b) => !isLeft && law(a, isLeft) && law(b, isLeft)
        case Concat(a, b) =>
          law(a, isLeft) && law(b, isLeft)
        case Union(a, b) =>
          // we only care about the first parent of a FlatAlt node;
          // once we see a union its right side could have a FlatAlt
          // but its left side must not.
          law(a, true) && law(b, false)
        case f@LazyDoc(_) => law(f.evaluated, isLeft)
        case Align(d) => law(d, isLeft)
        case Nest(_, d) => law(d, isLeft)
      }

    forAll { d: Doc =>
      assert(law(d, false), s"input=${repr(d)}")
    }
  }

  test("flattened docs never have FlatAlt") {
    import Doc._
    def law(d: Doc): Boolean =
      d match {
        case FlatAlt(_, _) => false
        case Empty | Text(_) | Line => true
        case Concat(a, b) => law(a) && law(b)
        case Union(a, b) => law(a) && law(b)
        case f@LazyDoc(_) => law(f.evaluated)
        case Align(d) => law(d)
        case Nest(_, d) => law(d)
      }

    forAll { (d: Doc) =>
      assert(law(d.flatten), s"input=${repr(d)}")
    }
  }
}
