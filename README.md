# Paiges

## Overview

Paiges is an implementation of
[Wadler's "A Prettier Printer"](http://homepages.inf.ed.ac.uk/wadler/papers/prettier/prettier.pdf).

The library is useful any time you find yourself generating text or
source code where you'd like to control the length of lines (e.g.
paragraph wrapping).

The name *Paiges* is a reference to the [Paige compositor](https://en.wikipedia.org/wiki/Paige_Compositor)
and the fact that it helps you layout pages.

[![Build Status](https://api.travis-ci.org/typelevel/paiges.svg)](https://travis-ci.org/typelevel/paiges)
[![codecov.io](http://codecov.io/github/typelevel/paiges/coverage.svg?branch=master)](http://codecov.io/github/typelevel/paiges?branch=master)
[![Latest version](https://index.scala-lang.org/typelevel/paiges/paiges-core/latest.svg?color=orange)](https://index.scala-lang.org/typelevel/paiges/paiges-core)

## Quick Start

Paiges supports Scala 2.10, 2.11, and 2.12. It supports both the JVM
and JS platforms.

To use Paiges in your own project, you can include this snippet in
your `build.sbt` file:

```scala
// use this snippet for the JVM
libraryDependencies += "org.typelevel" %% "paiges-core" % "0.2.0"

// use this snippet for JS, or cross-building
libraryDependencies += "org.typelevel" %%% "paiges-core" % "0.2.0"
```

Paiges also provides types to work with Cats via the *paiges-cats*
module:

```scala
// use this snippet for the JVM
libraryDependencies += "org.typelevel" %% "paiges-cats" % "0.2.0"

// use this snippet for JS, or cross-building
libraryDependencies += "org.typelevel" %%% "paiges-cats" % "0.2.0"
```

## Description

This code is a direct port of the code in section 7 of this paper,
with an attempt to be idiomatic in scala while preserving the original
code's properties, including laziness.

This algorithm is optimal and bounded. From the paper:

> Say that a pretty printing algorithm is optimal if it chooses line
> breaks so as to avoid overflow whenever possible; say that it is
> bounded if it can make this choice after looking at no more than the
> next w characters, where w is the line width. Hughes notes that
> there is no algorithm to choose line breaks for his combinators that
> is optimal and bounded, while the layout algorithm presented here
> has both properties.

Some selling points of this code:

 1. Lazy, O(1) concatenation
 2. Competitive performance (e.g. 3-5x slower than `mkString`)
 3. Elegantly handle indentation
 4. Flexible line-wrapping strategies
 5. Functional cred ;)

## Examples

Here's an example of using Paiges to generate the source code for a
case class:

```scala
import org.typelevel.paiges._

/**
 * Produces a case class given a name and zero-or-more
 * field/type pairs.
 */
def mkCaseClass(name: String, fields: (String, String)*): Doc = {
  val prefix = Doc.text("case class ") + Doc.text(name) + Doc.char('(')
  val suffix = Doc.char(')')
  val types = fields.map { case (k, v) =>
    Doc.text(k) + Doc.char(':') + Doc.space + Doc.text(v)
  }
  val body = Doc.intercalate(Doc.char(',') + Doc.line, types)
  body.tightBracketBy(prefix, suffix)
}

val c = mkCaseClass(
  "Dog", "name" -> "String", "breed" -> "String",
  "height" -> "Int", "weight" -> "Int")

c.render(80)
// case class Dog(name: String, breed: String, height: Int, weight: Int)

c.render(60)
// case class Dog(
//   name: String,
//   breed: String,
//   height: Int,
//   weight: Int
// )
```

For more examples, see the [tutorial](docs/src/main/tut/intro.md).

## Benchmarks

The Paiges benchmarks are written against JMH. To run them, you'll
want to use a command like this from SBT:

```
benchmark/jmh:run -wi 5 -i 5 -f1 -t1 bench.PaigesBenchmark
```

By default the values reported are *ops/ms* (operations per
millisecond), so higher numbers are better.

The parameters used here are:

 * `-wi`: the number of times to run during warmup
 * `-i`: the number of times to benchmark
 * `-f`: the number of processes to use during benchmarking
 * `-t`: the number of threads to use during benchmarking

In other words, the example command-line runs one thread in one
process, with a relatively small number of warmups + runs (so that it
will finish relatively quickly).

## Organization

The current Paiges maintainers are:

 * [Oscar Boykin](https://github.com/johnynek)
 * [Colt Frederickson](https://github.com/coltfred)
 * [Erik Osheim](https://github.com/non)

People are expected to follow the [Typelevel Code of Conduct](http://typelevel.org/conduct.html)
when discussing Paiges on the Github page or other official venues.

Concerns or issues can be sent to any of Paiges' maintainers, or to
the Typelevel organization.

## License

Paiges is licensed under the [Apache License, Version 2.0](LICENSE)
(the "License"); you may not use this software except in compliance
with the License.

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
implied. See the License for the specific language governing
permissions and limitations under the License.
