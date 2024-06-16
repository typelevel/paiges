/*
 * Copyright 2017 Typelevel
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.typelevel.paiges

import cats.Semigroupal
import cats.Contravariant
import cats.kernel.{Eq, Monoid}
import cats.laws.discipline.{ContravariantTests, DeferTests, ExhaustiveCheck, SemigroupalTests, SerializableTests}
import cats.kernel.laws.discipline.MonoidTests
import cats.laws.discipline.eq.catsLawsEqForFn1Exhaustive

import org.typelevel.discipline.scalatest.FunSuiteDiscipline
import org.scalacheck.Arbitrary
import org.scalactic.anyvals.{PosInt, PosZDouble, PosZInt}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.prop.Configuration

class LawTests extends LawChecking with CatsDocument {
  import org.typelevel.paiges.Generators._
  import org.typelevel.paiges.instances._

  implicit val docEq: Eq[Doc] =
    Eq.instance((x: Doc, y: Doc) => PaigesTest.docEquiv.equiv(x, y))

  implicit def monoidTests[A: Monoid]: MonoidTests[A] = MonoidTests[A]

  implicit def arbitraryForDocument[A]: Arbitrary[Document[A]] =
    Arbitrary(Document.useToString[A])

  implicit def eqForDocument[A: ExhaustiveCheck]: Eq[Document[A]] =
    Eq.by[Document[A], A => Doc](inst => (a: A) => inst.document(a))

  implicit val eqBool: Eq[Boolean] =
    Eq.instance[Boolean](_ == _)

  checkAll("Monoid[Doc]", MonoidTests[Doc].monoid)
  checkAll("Monoid[Style]", MonoidTests[Style].monoid)

  checkAll("Contravariant[Document]", ContravariantTests[Document].contravariant[Boolean, Boolean, Boolean])
  checkAll("Contravariant[Document]", SerializableTests.serializable(Contravariant[Document]))
  checkAll("Defer[Document]", DeferTests[Document].defer[Boolean])

  {
    implicit val semigroupalDocument: Semigroupal[Document] =
      CatsDocument.semigroupalDocument(Doc.char(','))
    checkAll("Semigroupal[Document]", SemigroupalTests[Document].semigroupal[Boolean, Boolean, Boolean])
    checkAll("Semigroupal[Document]", SerializableTests.serializable(Semigroupal[Document]))
  }
}

abstract class LawChecking extends AnyFunSuite with Configuration with FunSuiteDiscipline {

  lazy val checkConfiguration: PropertyCheckConfiguration =
    PropertyCheckConfiguration(
      minSuccessful = if (Platform.isJvm) PosInt(50) else PosInt(5),
      maxDiscardedFactor = if (Platform.isJvm) PosZDouble(5.0) else PosZDouble(50.0),
      minSize = PosZInt(0),
      sizeRange = if (Platform.isJvm) PosZInt(10) else PosZInt(5),
      workers = PosInt(1)
    )

  // The scalacheck defaults 'sizeRange' (100) is too high for Scala-js, so we reduce to 10.
  // We also set `minSuccessful` to 100 unconditionally.
  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    if (Platform.isJvm) PropertyCheckConfiguration(sizeRange = 100, minSuccessful = 100)
    else PropertyCheckConfiguration(sizeRange = 10, minSuccessful = 100)
}
