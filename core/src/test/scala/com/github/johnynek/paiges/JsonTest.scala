package com.github.johnynek.paiges

import org.scalatest.FunSuite

/**
 * A simple JSON ast
 */
sealed abstract class Json {
  def toDoc: Doc
}

object Json {
  import Doc.{text, str}

  def escape(str: String): String =
    str.flatMap {
      case '\\' => "\\\\"
      case '\n' => "\\n"
      case '"' => "\""
      case other => other.toString
    }

  case class JString(str: String) extends Json {
    def toDoc = text("\"%s\"".format(escape(str)))
  }
  case class JNumber(toDouble: Double) extends Json {
    def toDoc = str(toDouble)
  }
  case class JBool(toBoolean: Boolean) extends Json {
    def toDoc = str(toBoolean)
  }
  case object JNull extends Json {
    def toDoc = text("null")
  }
  case class JArray(toVector: Vector[Json]) extends Json {
    def toDoc = {
      val parts = Doc.intercalate(Doc.comma, toVector.map { j => (Doc.line +: j.toDoc).group })
      "[" +: ((parts :+ " ]").nest(2))
    }
  }
  case class JObject(toMap: Map[String, Json]) extends Json {
    def toDoc = {
      val kvs = toMap.map { case (s, j) =>
        JString(s).toDoc +: text(":") +: ((Doc.spaceOrLine +: j.toDoc).nest(2))
      }
      val parts = Doc.fill(Doc.comma, kvs)
      Doc.bracket(text("{"), parts, text("}"))
    }
  }
}

class JsonTest extends FunSuite {
  import Json._

  test("test nested array json example") {
    val inner = JArray((1 to 20).map { i => JNumber(i.toDouble) }.toVector)
    val outer = JArray(Vector(inner, inner, inner))

    if (1.0.toString == "1") {
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


    } else {
      assert(outer.toDoc.render(20) == """[
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
}
