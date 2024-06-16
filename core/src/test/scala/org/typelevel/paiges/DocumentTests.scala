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

import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks._

class DocumentTest extends AnyFunSuite {
  import Doc.text
  import PaigesTest._

  implicit val generatorDrivenConfig: PropertyCheckConfiguration =
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
    assert(1.doc.space("is").space(true.doc).render(80) == "1 is true")
  }
}
