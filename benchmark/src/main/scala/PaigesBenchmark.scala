package bench

import java.util.concurrent.TimeUnit
import org.openjdk.jmh.annotations._

import org.typelevel.paiges.Doc

@State(Scope.Benchmark)
@OutputTimeUnit(TimeUnit.MILLISECONDS)
class PaigesBenchmark {

  @Param(Array("1", "10", "100", "1000", "10000"))
  var size: Int = 0

  var strs: Vector[String] = Vector.empty

  @Setup
  def setup(): Unit = {
    strs = (1 to size).map(_.toString).toVector
  }

  @Benchmark
  def mkstring(): String =
    strs.mkString

  @Benchmark
  def mkstringComma(): String =
    strs.mkString(", ")

  @Benchmark
  def docConcat(): String =
    strs.foldLeft(Doc.empty) { (d1, s) => d1 :+ s }.render(0)

  @Benchmark
  def docConcatComma(): String =
    strs.foldLeft(Doc.empty) { (d1, s) => d1 :+ ", " :+ s }.render(0)

  @Benchmark
  def intercalate(): String =
    Doc.intercalate(Doc.text(", "), strs.map(Doc.text)).render(0)

  @Benchmark
  def fill0(): String =
    Doc.fill(Doc.text(","), strs.map(Doc.text)).get.render(0)

  @Benchmark
  def fill100(): String =
    Doc.fill(Doc.text(","), strs.map(Doc.text)).get.render(100)

}
