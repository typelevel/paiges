# Paiges
An implementation of [Wadler's "A Prettier Printer"](http://homepages.inf.ed.ac.uk/wadler/papers/prettier/prettier.pdf).
This code is a direct port of the code in section 7 of this paper, with an attempt to be idiomatic
in scala.

The name is a reference to the [Paige compositor](https://en.wikipedia.org/wiki/Paige_Compositor)
and the fact that it helps you layout pages.

The main utilities or this code:

1. not have to go crazy dealing with indentation when producing text documents, and to do so a clean way.
2. support cases where whitespace and newlines might be interchangable.


