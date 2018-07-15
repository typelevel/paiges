package org.typelevel.paiges

import org.scalacheck.{Arbitrary, Cogen, Gen}

object Generators {
  import Doc.text

  val asciiString: Gen[String] =
    for {
      n <- Gen.choose(1, 10)
      cs <- Gen.listOfN(n, Gen.choose(32.toChar, 126.toChar))
    } yield cs.mkString

  val generalString: Gen[String] =
    implicitly[Arbitrary[String]].arbitrary

  val doc0Gen: Gen[Doc] = Gen.frequency(
    (1, Doc.empty),
    (1, Doc.space),
    (1, Doc.line),
//    (1, Doc.line2),
//    (1, Doc.line3),
    (1, Doc.lineBreak),
    (1, Doc.lineOrSpace),
    (10, asciiString.map(text(_))),
    (10, generalString.map(text(_))),
    (3, asciiString.map(Doc.split(_))),
    (3, generalString.map(Doc.split(_))),
    (3, generalString.map(Doc.paragraph(_)))
    )

  val combinators: Gen[(Doc, Doc) => Doc] =
    Gen.oneOf(
    { (a: Doc, b: Doc) => a + b },
    { (a: Doc, b: Doc) => a space b },
    { (a: Doc, b: Doc) => a / b },
    { (a: Doc, b: Doc) => a lineOrSpace b })

  val unary: Gen[Doc => Doc] =
    Gen.oneOf(
      Gen.const({ d: Doc => d.grouped }),
      Gen.const({ d: Doc => d.aligned }),
      Gen.choose(0, 40).map { i => { d: Doc => d.nested(i) } })

  val folds: Gen[(List[Doc] => Doc)] =
    Gen.oneOf(
    doc0Gen.map { sep =>
      { ds: List[Doc] => Doc.fill(sep, ds.take(8)).get }
    },
    Gen.const({ ds: List[Doc] => Doc.spread(ds) }),
    Gen.const({ ds: List[Doc] => Doc.stack(ds) }))

  val maxDepth = 7

  def genTree(depth: Int): Gen[Doc] = {
    val ugen = for {
      u <- unary
      d <- genTree(depth - 1)
    } yield u(d)

    val cgen = for {
      c <- combinators
      d0 <- genTree(depth - 1)
      d1 <- genTree(depth - 1)
    } yield c(d0, d1)

    val fgen = for {
      fold <- folds
      num <- Gen.choose(0, 20)
      ds <- Gen.listOfN(num, Gen.lzy(genTree(depth - 1)))
    } yield fold(ds)

    if (depth <= 0) doc0Gen
    else if (depth >= maxDepth - 1) {
      Gen.frequency(
        // bias to simple stuff
        (6, doc0Gen),
        (1, ugen),
        (2, cgen),
        (1, fgen))
    } else {
      // bias to simple stuff
      Gen.frequency(
        (6, doc0Gen),
        (1, ugen),
        (2, cgen))
    }
  }

  val genDoc: Gen[Doc] =
    Gen.choose(0, 7).flatMap(genTree)

  implicit val arbDoc: Arbitrary[Doc] =
    Arbitrary(genDoc)

  implicit val cogenDoc: Cogen[Doc] =
    Cogen[Int].contramap((d: Doc) => d.hashCode)
}
