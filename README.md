# Paiges
An implementation of [Wadler's "A Prettier Printer"](http://homepages.inf.ed.ac.uk/wadler/papers/prettier/prettier.pdf).
This code is a direct port of the code in section 7 of this paper, with an attempt to be idiomatic
in scala.

This algorithm is optimal and bounded. From the paper:
> Say that a pretty printing algorithm is optimal if it chooses line breaks
> so as to avoid overflow whenever possible; say that it is bounded if it can make
> this choice after looking at no more than the next w characters, where w is the
> line width. Hughes notes that there is no algorithm to choose line breaks for his
> combinators that is optimal and bounded, while the layout algorithm presented
> here has both properties.

The name is a reference to the [Paige compositor](https://en.wikipedia.org/wiki/Paige_Compositor)
and the fact that it helps you layout pages.

The main utilities or this code:

1. not have to go crazy dealing with indentation when producing text documents, and to do so a clean way.
2. support cases where whitespace and newlines might be interchangable.


