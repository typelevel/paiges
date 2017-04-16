# Paiges

Paiges is an implementation of Philip Wadler's
[*Prettier Printer*](http://homepages.inf.ed.ac.uk/wadler/papers/prettier/prettier.pdf).

### Getting started

To get started, you'll want to import `Doc`. This is the basis for
Paiges, and is likely the only type you'll need to work with:

```tut:book
import org.typelevel.paiges.Doc
```

## Creating documents

Many documents are defined in terms of other documents, but to get
started you'll need to create documents to represent chunks of text.
Here are three good ways to do it.

```tut:book
// unbroken text from a String
val cat = Doc.text("cat")

// same as text, but using .toString first
val pair = Doc.str((1, 2))

// allow breaking on word-boundaries
val doc = Doc.paragraph(
  """The basic tool for the manipulation of reality is the
  manipulation of words. If you can control the meaning of
  words, you can control the people who must use them.""")
```

We can use a single line of text to contrast these methods:

```tut:silent
val howl = "I saw the best minds of my generation destroyed by madness..."
```

The first two, `Doc.text` and `Doc.str`, create a fragment of text that
will always render exactly as shown. This means that if you create a
very long piece of text, it won't be wrapped to fit smaller widths.

```tut:book
Doc.text(howl).render(40)
```

By contrast, `Doc.paragraph` will add line breaks to make the document
fit:

```tut:book
Doc.paragraph(howl).render(40)
```

There are also some useful methods defined in the `Doc` companion:

 * `Doc.empty`: an empty document, equivalent to `Doc.text("")`
 * `Doc.space`: a single space, equivalent to `Doc.text(" ")`
 * `Doc.comma`: a single comma, equivalent to `Doc.text(",")`
 * `Doc.line`: a single newline, equivalent to  `Doc.text("\n")`. When flattened, this becomes space.
 * `Doc.lineBreak`: a single newline that flattens to empty.
 * `Doc.spaces(n)`: *n* spaces, equivalent to `Doc.text(" " * n)`
 * `Doc.lineOrSpace`: a space or newline, depending upon rendering width
 * `Doc.lineOrEmpty`: empty or newline, depending upon rendering width

## Combining documents

Paiges' ability to vary rendering based on width comes from the
different combinators it provides on `Doc`. In general, you'll want to
build documents to represent discrete, atomic bits of text, and then
use other combinators to build up larger documents.

This section gives a brief overview to some of the most useful
combinators.

### Concatenation

The most basic operations on documents involve concatenation: stitching
documents together to create new documents. Since `Doc` is immutable,
none of these methods modify documents directly. Instead, they return a
new document which represents the concatenation. This immutability
allows Paiges to render at different widths so efficiently.

Documents are concatenation with `+`. As a convenience, Paiges also
provides `+:` and `:+` to prepend or append a string.

```tut:book
val doc1 = Doc.text("my pet is a ") + Doc.text("cat")
val doc2 = Doc.text("my pet is a ") :+ "cat"
val doc3 = "my pet is a " +: Doc.text("cat")

val doc4 = Doc.text("my pet is a " + "cat")
```

The first three documents are equivalent. However, the last is
different: it performs a string concatenation before creating a single
document, which will be less efficient. When using `Doc`, it's
preferable to turn literal strings into documents before concatenating
them.

There are also several other types of concatenation (which can all be
written in terms of `+`). For example, `/` concatenates documents with
a newline in between:

```tut:book
// equivalent to Doc.text("first line") + Doc.line + Doc.text("second line")
val doc5 = Doc.text("first line")  / Doc.text("second line")
val doc6 = Doc.text("first line")  :/ "second line"
val doc7 = "first line" /: Doc.text("second line")
```
