package org.typelevel.paiges

trait Document[A] { self =>
  def document(a: A): Doc

  def contramap[Z](f: Z => A): Document[Z] =
    Document.instance(z => self.document(f(z)))
}

object Document {

  def apply[A](implicit ev: Document[A]): Document[A] = ev

  def instance[A](f: A => Doc): Document[A] =
    new Document[A] {
      def document(a: A): Doc = f(a)
    }

  implicit val documentString: Document[String] =
    Document.instance(Doc.text)

  implicit val documentChar: Document[Char] =
    Document.instance(Doc.char)

  implicit val documentUnit: Document[Unit] = useToString[Unit]
  implicit val documentBoolean: Document[Boolean] = useToString[Boolean]
  implicit val documentByte: Document[Byte] = useToString[Byte]
  implicit val documentShort: Document[Short] = useToString[Short]
  implicit val documentInt: Document[Int] = useToString[Int]
  implicit val documentLong: Document[Long] = useToString[Long]
  implicit val documentFloat: Document[Float] = useToString[Float]
  implicit val documentDouble: Document[Double] = useToString[Double]

  def documentIterable[A](name: String)(implicit ev: Document[A]): Document[Iterable[A]] =
    Document.instance { xs =>
      val body = Doc.fill(Doc.comma + Doc.line, xs.map(ev.document)).get /* ??? */
      body.tightBracketBy(Doc.text(name) :+ "(", Doc.char(')'))
    }

  /**
   * In general you will get better rendering and performance by
   * writing your own instance of Document[A] using the document
   * combinators. However, this method is provided as a convenience
   * when these issues are not applicable.
   */
  def useToString[A]: Document[A] =
    FromToString.asInstanceOf[Document[A]]

  object FromToString extends Document[Any] {
    def document(any: Any): Doc = Doc.text(any.toString)
  }

  trait Ops[A] {
    def instance: Document[A]
    def self: A
    def doc: Doc = instance.document(self)
  }

  trait ToDocumentOps {
    implicit def toDocumentOps[A](target: A)(implicit tc: Document[A]): Ops[A] = new Ops[A] {
      val instance: Document[A] = tc
      val self: A = target
    }
  }

  object ops extends ToDocumentOps
}
