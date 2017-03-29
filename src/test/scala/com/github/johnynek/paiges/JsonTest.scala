package com.github.johnynek.paiges

import org.scalatest.FunSuite

/**
 * A simple JSON ast
 */
sealed abstract class Json {
  def toDoc: Doc
}

object Json {
  def escape(str: String): String =
    str.flatMap {
      case '\\' => "\\\\"
      case '\n' => "\\n"
      case '"' => "\""
      case other => other.toString
    }

  case class JString(str: String) extends Json {
    def toDoc = Doc("\"%s\"".format(escape(str)))
  }
  case class JNumber(toDouble: Double) extends Json {
    def toDoc = Doc(toDouble)
  }
  case class JBool(toBoolean: Boolean) extends Json {
    def toDoc = Doc(toBoolean)
  }
  case object JNull extends Json {
    def toDoc = Doc("null")
  }
  case class JArray(toVector: Vector[Json]) extends Json {
    def toDoc = {
      val parts = Doc.intercalate(Doc.comma, toVector.map { j => Doc.spaceGroup(j.toDoc) })
      Doc("[") ++ ((parts ++ Doc(" ]")).nest(2))
    }
  }
  case class JObject(toMap: Map[String, Json]) extends Json {
    def toDoc = {
      val kvs = toMap.map { case (s, j) =>
        JString(s).toDoc ++ Doc(":") ++ ((Doc.spaceOrLine ++ j.toDoc).nest(2))
      }
      val parts = Doc.fill(Doc.comma, kvs)
      parts.bracketBy("{", "}")
    }
  }
}

class JsonTest extends FunSuite {
  import Json._

  test("test nested array json example") {
    val inner = JArray((1 to 20).map { i => JNumber(i.toDouble) }.toVector)
    val outer = JArray(Vector(inner, inner, inner))

    assert(outer.toDoc.render(20) ==
"""[
  [ 1.0, 2.0, 3.0,
    4.0, 5.0, 6.0,
    7.0, 8.0, 9.0,
    10.0, 11.0,
    12.0, 13.0,
    14.0, 15.0,
    16.0, 17.0,
    18.0, 19.0,
    20.0 ],
  [ 1.0, 2.0, 3.0,
    4.0, 5.0, 6.0,
    7.0, 8.0, 9.0,
    10.0, 11.0,
    12.0, 13.0,
    14.0, 15.0,
    16.0, 17.0,
    18.0, 19.0,
    20.0 ],
  [ 1.0, 2.0, 3.0,
    4.0, 5.0, 6.0,
    7.0, 8.0, 9.0,
    10.0, 11.0,
    12.0, 13.0,
    14.0, 15.0,
    16.0, 17.0,
    18.0, 19.0,
    20.0 ] ]""")
  }
}
