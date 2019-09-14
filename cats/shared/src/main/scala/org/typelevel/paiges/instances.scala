package org.typelevel.paiges

import cats.kernel.{Eq, Monoid}

package object instances {
  implicit val paigesDocMonoid: Monoid[Doc] =
    new Monoid[Doc] {
      def empty: Doc = Doc.empty
      def combine(x: Doc, y: Doc): Doc = x + y
    }

  implicit val paigesStyleMonoid: Monoid[Style] =
    new Monoid[Style] {
      def empty: Style = Style.Empty
      def combine(x: Style, y: Style): Style = x ++ y
    }

  implicit val paigesStyleEq: Eq[Style] =
    Eq.fromUniversalEquals
}
