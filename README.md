# Paiges

## Summary

An implementation of [Wadler's "A Prettier Printer"](http://homepages.inf.ed.ac.uk/wadler/papers/prettier/prettier.pdf).
This code is a direct port of the code in section 7 of this paper, with an attempt to be idiomatic
in scala.

This algorithm is optimal and bounded. From the paper:

> Say that a pretty printing algorithm is optimal if it chooses line
> breaks so as to avoid overflow whenever possible; say that it is
> bounded if it can make this choice after looking at no more than the
> next w characters, where w is the line width. Hughes notes that
> there is no algorithm to choose line breaks for his combinators that
> is optimal and bounded, while the layout algorithm presented here
> has both properties.

The name is a reference to the [Paige compositor](https://en.wikipedia.org/wiki/Paige_Compositor)
and the fact that it helps you layout pages.

Some selling points of this code:

 1. Lazy, O(1) concatenation
 2. Competitive performance (e.g. 3-5x slower than `mkString`)
 3. Elegantly handle indentation
 4. Flexible line-wrapping strategies
 5. Lawful equality and ordering
 6. Functional cred ;)

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
 * [Erik Osheim](https://github.com/non)
 * [Colt Frederickson](https://github.com/coltfred)

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
