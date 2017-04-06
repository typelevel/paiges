package bench

import java.util.concurrent.TimeUnit
import org.openjdk.jmh.annotations._

import com.github.johnynek.paiges.Doc

//@BenchmarkMode(Array(Mode.AverageTime))
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
    strs.mkString(", ")
  //
  // @Benchmark
  // def concat(): String =
  //   strs.reduceLeft(_ + ", " + _)

  @Benchmark
  def intercalate(): String =
    Doc.intercalate(Doc.text(", "), strs.map(Doc.text)).render(0)

  @Benchmark
  def fill(): Doc =
    Doc.fill(Doc.text(","), strs.map(Doc.text))

  @Benchmark
  def fill0(): String =
    Doc.fill(Doc.text(","), strs.map(Doc.text)).render(0)

  @Benchmark
  def fill100(): String =
    Doc.fill(Doc.text(","), strs.map(Doc.text)).render(100)
}
