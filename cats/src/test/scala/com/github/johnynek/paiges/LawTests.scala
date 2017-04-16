package org.typelevel.paiges

import catalysts.Platform
import catalysts.macros.TypeTagM // need this import for implicit macros

import cats.kernel.Eq
import cats.kernel.laws._

import org.typelevel.discipline.Laws
import org.typelevel.discipline.scalatest.Discipline
import org.scalacheck.{Arbitrary, Cogen}
import org.scalactic.anyvals.{PosZDouble, PosInt, PosZInt}
import org.scalatest.FunSuite
import org.scalatest.prop.Configuration

class LawTests extends LawChecking {
  import org.typelevel.paiges.Generators._
  import org.typelevel.paiges.instances._

  implicit def orderLaws[A: Eq: Cogen: Arbitrary] = OrderLaws[A]
  implicit def groupLaws[A: Eq: Arbitrary] = GroupLaws[A]

  laws[OrderLaws, Doc].check(_.order)
  laws[GroupLaws, Doc].check(_.monoid)
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
