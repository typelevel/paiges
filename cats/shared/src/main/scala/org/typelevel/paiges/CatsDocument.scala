/*
 * Copyright 2024 Typelevel
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.typelevel.paiges

import cats.{Contravariant, Defer, Semigroupal}

trait CatsDocument {
  implicit val contravariantDocument: Contravariant[Document] =
    new Contravariant[Document] {
      def contramap[A, Z](d: Document[A])(f: Z => A): Document[Z] = d.contramap(f)
    }

  def semigroupalDocument(sep: Doc): Semigroupal[Document] =
    new Semigroupal[Document] {
      def product[A, B](fa: Document[A], fb: Document[B]): Document[(A, B)] =
        Document.instance { case (a, b) =>
          fa.document(a) + sep + fb.document(b)
        }
    }

  implicit val deferDocument: Defer[Document] =
    new Defer[Document] {
      def defer[A](d: => Document[A]): Document[A] =
        Document.defer(d)
    }
}

object CatsDocument extends CatsDocument
