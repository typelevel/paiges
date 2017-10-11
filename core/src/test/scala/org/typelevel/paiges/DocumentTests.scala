package org.typelevel.paiges

import org.scalatest.FunSuite
import org.scalatest.prop.PropertyChecks._

class DocumentTest extends FunSuite {
  import Doc.text
  import PaigesTest._

  implicit val generatorDrivenConfig =
    PropertyCheckConfiguration(minSuccessful = 500)

  def document[A](a: A)(implicit d: Document[A]): Doc =
    d.document(a)

  test("Document[Unit]") {
    document(()) === text("()")
  }

  test("Document[Boolean]") {
    document(true) === text("true")
    document(false) === text("false")
  }

  test("Document[Byte]") {
    forAll((x: Byte) => document(x) === text(x.toString))
  }

  test("Document[Short]") {
    forAll((x: Short) => document(x) === text(x.toString))
  }

  test("Document[Int]") {
    forAll((x: Int) => document(x) === text(x.toString))
  }

  test("Document[Long]") {
    forAll((x: Long) => document(x) === text(x.toString))
  }

  test("Document[Float]") {
    forAll((x: Float) => document(x) === text(x.toString))
  }

  test("Document[Double]") {
    forAll((x: Double) => document(x) === text(x.toString))
  }

  test("Document[String]") {
    forAll((s: String) => document(s) === text(s))
  }

  test("Document[List[Int]]") {
    val inst = Document.documentIterable[Int]("list")
    val d = inst.document(1 to 12)
    assert(d.render(80) == "list(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)")

    val expected = """list(
                     |  1, 2, 3,
                     |  4, 5, 6,
                     |  7, 8, 9,
                     |  10, 11,
                     |  12
                     |)""".stripMargin
    assert(d.render(10) == expected)
  }

  test("Ops") {
    import Document.ops._
    assert(("a".doc + "b".doc).render(80) == "ab")
    assert((1.doc space "is" space true.doc).render(80) == "1 is true")
  }
}
