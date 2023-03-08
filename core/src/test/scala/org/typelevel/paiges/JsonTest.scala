/*
 * Copyright 2023 Typelevel
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

/**
 * A simple JSON ast
 */
sealed abstract class Json {
  def toDoc: Doc
}

object Json {
  import Doc.{str, text}

  def escape(str: String): String =
    str.flatMap {
      case '\\'  => "\\\\"
      case '\n'  => "\\n"
      case '"'   => "\""
      case other => other.toString
    }

  case class JString(str: String) extends Json {
    def toDoc = text("\"%s\"".format(escape(str)))
  }
  case class JDouble(toDouble: Double) extends Json {
    def toDoc = str(toDouble)
  }
  case class JInt(toInt: Int) extends Json {
    def toDoc = str(toInt)
  }
  case class JBool(toBoolean: Boolean) extends Json {
    def toDoc = str(toBoolean)
  }
  case object JNull extends Json {
    def toDoc = text("null")
  }
  case class JArray(toVector: Vector[Json]) extends Json {
    def toDoc = {
      val parts = Doc.intercalate(Doc.comma, toVector.map(j => (Doc.line + j.toDoc).grouped))
      "[" +: ((parts :+ " ]").nested(2))
    }
  }
  case class JObject(toMap: Map[String, Json]) extends Json {
    def toDoc = {
      val kvs = toMap.map { case (s, j) =>
        JString(s).toDoc + text(":") + ((Doc.lineOrSpace + j.toDoc).nested(2))
      }
      val parts = Doc.fill(Doc.comma, kvs)
      parts.bracketBy(text("{"), text("}"))
    }
  }
}

class JsonTest extends AnyFunSuite {
  import Json._

  test("test nesteded array json example") {
    val inner = JArray((1 to 20).map(i => JInt(i)).toVector)
    val outer = JArray(Vector(inner, inner, inner))

    assert(outer.toDoc.render(20) == """[
  [ 1, 2, 3, 4, 5,
    6, 7, 8, 9, 10,
    11, 12, 13, 14,
    15, 16, 17, 18,
    19, 20 ],
  [ 1, 2, 3, 4, 5,
    6, 7, 8, 9, 10,
    11, 12, 13, 14,
    15, 16, 17, 18,
    19, 20 ],
  [ 1, 2, 3, 4, 5,
    6, 7, 8, 9, 10,
    11, 12, 13, 14,
    15, 16, 17, 18,
    19, 20 ] ]""")
  }
}
