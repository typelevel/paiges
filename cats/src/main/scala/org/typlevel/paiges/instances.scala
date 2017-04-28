package org.typelevel.paiges

import cats.kernel.{Order, Monoid}

package object instances {
  implicit val paigesDocMonoid: Monoid[Doc] =
    new Monoid[Doc] {
      def empty: Doc = Doc.empty
      def combine(x: Doc, y: Doc): Doc = x + y
    }
}
