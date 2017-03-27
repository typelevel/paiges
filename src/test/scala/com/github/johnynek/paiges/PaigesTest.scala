package com.github.johnynek.paiges

import org.scalatest.FunSuite
import org.scalatest.prop.PropertyChecks._
import org.scalacheck.{Arbitrary, Gen}

class PaigesTest extends FunSuite {
  import Generators._

  test("basic test") {
     assert((Doc("hello") ++ Doc("world")).render(100) == "helloworld")
  }

  test("nest test") {
    assert((Doc("yo") ++ (Doc("yo\nho\nho").nest(2))).render(100) ==
"""yoyo
  ho
  ho""")
  }

  test("paper example") {
    val g = Doc("hello").line("a").group.line("b").group.line("c").group
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
    val d2 = d1 ++ (Doc.line ++ "love, Oscar").nest(2)
    assert(d2.render(0) == words.mkString("", "\n", "\n") ++ ("  love, Oscar"))
    assert(d2.render(100) == words.mkString("", " ", "\n") ++ ("  love, Oscar"))
  }

  test("concat is associative") {
    forAll { (a: Doc, b: Doc, c: Doc, width: Int) =>
      assert(((a ++ b) ++ c).render(width) ==
        (a ++ (b ++ c)).render(width))
    }
  }
  test("empty does not change things") {
    forAll { (a: Doc, width: Int) =>
      assert((a ++ Doc.empty).render(width) == a.render(width))
      assert((Doc.empty ++ a).render(width) == a.render(width))
    }
  }

  test("fillWords can == .map('\\n' => ' ')") {
    forAll { (str: String) =>
      val newLineToSpace = str.map {
        case '\n' => ' '
        case other => other
      }
      assert(Doc.fillWords(str).render(str.length) == newLineToSpace)
    }
  }

  test("line works as expected") {
    forAll { (a: String, b: String) =>
      assert((Doc(a) line Doc(b)).render(0) ==
        s"$a\n$b")
    }
  }

  test("space works as expected") {
    forAll { (a: String, b: String) =>
      assert((Doc(a) space Doc(b)).render(0) ==
        s"$a $b")
    }
  }
}

object Generators {
  val asciiString: Gen[String] =
    Gen.listOf(Gen.choose(32.toChar, 126.toChar)).map(_.mkString)

  val generalString: Gen[String] =
    implicitly[Arbitrary[String]].arbitrary

  val doc0Gen: Gen[Doc] = Gen.frequency(
    (1, Doc.empty),
    (1, Doc.space),
    (1, Doc.line),
    (1, Doc.spaceOrLine),
    (10, asciiString.map(Doc(_))),
    (10, generalString.map(Doc(_))),
    (3, asciiString.map(Doc.fillWords(_))),
    (3, generalString.map(Doc.fillWords(_)))
    )

  val combinators: Gen[(Doc, Doc) => Doc] =
    Gen.oneOf(
    { (a: Doc, b: Doc) => a concat b },
    { (a: Doc, b: Doc) => a space b },
    { (a: Doc, b: Doc) => a line b },
    { (a: Doc, b: Doc) => a spaceOrLine b })

  val unary: Gen[Doc => Doc] =
    Gen.oneOf( Gen.const({ d: Doc => d.group }),
      Gen.choose(0, 40).map { i => { d: Doc => d.nest(i) } })

  val folds: Gen[(List[Doc] => Doc)] =
    Gen.oneOf(
    { ds: List[Doc] => Doc.fill(ds) },
    { ds: List[Doc] => Doc.spread(ds) },
    { ds: List[Doc] => Doc.stack(ds) })


  def genTree(depth: Int): Gen[Doc] =
    if (depth <= 0) doc0Gen
    else Gen.frequency(
      // bias to simple stuff
      (10, doc0Gen),
      (1,
        for {
          u <- unary
          d <- genTree(depth - 1)
        } yield u(d)),
      (2,
        for {
          c <- combinators
          d0 <- genTree(depth - 1)
          d1 <- genTree(depth - 1)
        } yield c(d0, d1)),
      (1,
        for {
        fold <- folds
        num <- Gen.choose(0, 10)
        ds <- Gen.listOfN(num, Gen.lzy(genTree(depth - 1)))
      } yield fold(ds)))

  val genDoc: Gen[Doc] =
    Gen.choose(0, 10).flatMap(genTree)

  implicit val arbDoc: Arbitrary[Doc] = Arbitrary(genDoc)
}
