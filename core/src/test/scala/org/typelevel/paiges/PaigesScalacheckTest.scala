package org.typelevel.paiges

import org.scalacheck.Gen
import org.scalatest.prop.PropertyChecks._
import org.scalatest.{ Assertion, FunSuite }

class PaigesScalacheckTest extends FunSuite {
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
        s"input=${d.representation().render(100)}")
    }
  }

  test("trim-law: renderTrim is what we expect") {
    forAll { (d: Doc) =>
      val trim = d.renderTrim(100)
      val slowTrim = slowRenderTrim(d, 100)
      assert(trim == slowTrim, s"input=${d.representation().render(100)}")
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
    forAll { (b: Doc, c: Doc) =>
      val flatC = c.flatten
      val left = (b.grouped + flatC)
      val right = (b + flatC).grouped
      assert(left === right)
      assert((flatC + b.grouped) === (flatC + b).grouped)
      // since left == right, we could have used those instead of b:
      assert((left.grouped + flatC) === (right + flatC).grouped)
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
      assert(a.flatten === a.flattenOption.getOrElse(a))
    }
  }

  test("a.aligned.aligned == a.aligned") {
    forAll { a: Doc =>
      assert(a.aligned.aligned === a.aligned)
    }
  }

  test("the left and right side of a union are the same after flattening") {
    import Doc._
    def okay(d: Doc): Boolean = d match {
      case Empty | Text(_) | Line(_) => true
      case Concat(a, b) => okay(a) && okay(b)
      case Nest(j, d) => okay(d)
      case Align(d) => okay(d)
      case LazyDoc(d) => okay(d.evaluated)
      case Union(a, b) =>
        (a.flatten === b.flatten) && okay(a) && okay(b)
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
      case LazyDoc(d) => nextLineLength(d.evaluated)
      case Union(a, _) => nextLineLength(a) // assume the property is true
    }

    def okay(d: Doc): Boolean = d match {
      case Empty | Text(_) | Line(_) => true
      case Nest(j, d) => okay(d)
      case Align(d) => okay(d)
      case Concat(a, b) => okay(a) && okay(b)
      case LazyDoc(d) => okay(d.evaluated)
      case Union(a, b) =>
        nextLineLength(a)._2 >= nextLineLength(b)._2
    }

    forAll { (a: Doc) => assert(okay(a)) }
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
    val smallTree = Gen.choose(0, 3).flatMap(genTree)
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
    val smallTree = Gen.choose(0, 3).flatMap(genTree)
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

  test("Union invariants") {
    import Doc._
    def invariant(d: Doc): Assertion = d match {
      case Empty | Text(_) => assert(true)
      case Concat(Concat(_, _), _) => assert(false, "Left-associative Concat")
      case Concat(a, b) => invariant(a); invariant(b)
      case _ => assert(false, s"Illegal doc: ${d}")
    }
    forAll { d: Doc =>
      d.grouped match {
        case Union(a, _) => invariant(a)
        case _ => ()
      }
    }
  }
}
