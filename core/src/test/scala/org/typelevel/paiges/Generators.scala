package org.typelevel.paiges

import org.scalacheck.Shrink.shrink
import org.scalacheck.{ Arbitrary, Cogen, Gen, Shrink }

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
      { ds: List[Doc] => Doc.fill(sep, ds.take(8)) }
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

  implicit val nonEmptyUnionGen: Gen[Doc.Union] =
    genDoc.map(_.grouped).filter(d => d.isUnion && !d.isEmpty).map(_.asInstanceOf[Doc.Union])

  implicit val arbNonEmptyUnion: Arbitrary[Doc.Union] = Arbitrary(nonEmptyUnionGen)

  implicit val cogenDoc: Cogen[Doc] =
    Cogen[Int].contramap((d: Doc) => d.hashCode)

  implicit val shrinkDoc: Shrink[Doc] = {
    import Doc._
    def interleave[A](xs: Stream[A], ys: Stream[A]): Stream[A] =
      if (xs.isEmpty) ys
      else if (ys.isEmpty) xs
      else xs.head #:: ys.head #:: interleave(xs.tail, ys.tail)
    def combine[A](a: A)(f: A => A)(implicit F: Shrink[A]): Stream[A] = {
      val sa = shrink(a)
      a #:: interleave(sa, sa.map(f))
    }
    def combine2[A](a: A, b: A)(f: (A, A) => A)(implicit F: Shrink[A]): Stream[A] = {
      val (sa, sb) = (shrink(a), shrink(b))
      a #:: b #:: interleave(interleave(sa, sb), sa.flatMap(x => sb.map(y => f(x, y))))
    }
    Shrink {
      case Union(a, b) => combine2(a, b)(Union)
      case Concat(a, b) => combine2(a, b)(_ + _)
      case Text(s) => shrink(s).map(text)
      case Nest(i, d) => combine(d)(_.nested(i))
      case Align(d) => combine(d)(_.aligned)
      case Line(_) | Empty | LazyDoc(_) => Stream.empty
    }
  }

  final case class ReasonableWidth(n: Int) extends AnyVal

  implicit val reasonableWidthGen: Gen[ReasonableWidth] =
    Gen.oneOf(Gen.const(Int.MaxValue), Gen.choose(1, 200)).map(ReasonableWidth)

  implicit val arbReasonableWidth: Arbitrary[ReasonableWidth] =
    Arbitrary(reasonableWidthGen)
}
