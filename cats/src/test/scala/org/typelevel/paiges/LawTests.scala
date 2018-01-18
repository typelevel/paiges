package org.typelevel.paiges

import catalysts.Platform
import catalysts.macros.TypeTagM // need this import for implicit macros

import cats.Semigroupal
import cats.Contravariant
import cats.kernel.{Eq, Monoid}
import cats.laws.discipline.{SemigroupalTests, ContravariantTests, SerializableTests}
import cats.kernel.laws.discipline.MonoidTests
import cats.laws.discipline.eq.catsLawsEqForFn1

import org.typelevel.discipline.Laws
import org.typelevel.discipline.scalatest.Discipline
import org.scalacheck.Arbitrary
import org.scalactic.anyvals.{PosZDouble, PosInt, PosZInt}
import org.scalatest.FunSuite
import org.scalatest.prop.Configuration

class LawTests extends LawChecking with CatsDocument {
  import org.typelevel.paiges.Generators._
  import org.typelevel.paiges.instances._

  implicit val docEq: Eq[Doc] =
    Eq.instance { (x: Doc, y: Doc) =>
      PaigesTest.docEquiv.equiv(x, y)
    }

  implicit def monoidTests[A: Eq: Arbitrary: Monoid] = MonoidTests[A]

  implicit def arbitraryForDocument[A]: Arbitrary[Document[A]] =
    Arbitrary(Document.useToString[A])

  implicit def eqForDocument[A: Arbitrary]: Eq[Document[A]] =
    Eq.by[Document[A], A => Doc](inst => (a: A) => inst.document(a))

  laws[MonoidTests, Doc].check(_.monoid)

  checkAll("Contravariant[Document]", ContravariantTests[Document].contravariant[Int, Int, Int])
  checkAll("Contravariant[Document]", SerializableTests.serializable(Contravariant[Document]))

  {
    implicit val semigroupalDocument: Semigroupal[Document] =
      CatsDocument.semigroupalDocument(Doc.char(','))
    checkAll("Semigroupal[Document]", SemigroupalTests[Document].semigroupal[Int, Int, Int])
    checkAll("Semigroupal[Document]", SerializableTests.serializable(Semigroupal[Document]))
  }
}

abstract class LawChecking extends FunSuite with Configuration with Discipline {

  lazy val checkConfiguration: PropertyCheckConfiguration =
    PropertyCheckConfiguration(
      minSuccessful = if (Platform.isJvm) PosInt(50) else PosInt(5),
      maxDiscardedFactor = if (Platform.isJvm) PosZDouble(5.0) else PosZDouble(50.0),
      minSize = PosZInt(0),
      sizeRange = if (Platform.isJvm) PosZInt(10) else PosZInt(5),
      workers = PosInt(1))

  // The scalacheck defaults 'sizeRange' (100) is too high for Scala-js, so we reduce to 10.
  // We also set `minSuccessful` to 100 unconditionally.
  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    if (Platform.isJvm) PropertyCheckConfiguration(sizeRange = 100, minSuccessful = 100)
    else PropertyCheckConfiguration(sizeRange = 10, minSuccessful = 100)

  case class LawChecker[L <: Laws](name: String, laws: L) {
    def check(f: L => L#RuleSet): Unit = checkAll(name, f(laws))
  }

  def laws[L[_] <: Laws, A](implicit lws: L[A], tag: TypeTagM[A]): LawChecker[L[A]] =
    laws[L, A]("")

  def laws[L[_] <: Laws, A](extraTag: String)(implicit laws: L[A], tag: TypeTagM[A]): LawChecker[L[A]] =
    LawChecker("[" + tag.name.toString + (if(extraTag != "") "@@" + extraTag else "") + "]", laws)
}
