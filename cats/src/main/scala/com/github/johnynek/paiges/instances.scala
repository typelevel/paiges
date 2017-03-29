package com.github.johnynek.paiges

import cats.kernel.Monoid

package object instances {
  implicit val paigesDocMonoid: Monoid[Doc] =
    new Monoid[Doc] {
      def empty: Doc = Doc.empty
      def combine(x: Doc, y: Doc): Doc = x +: y
    }

  // TODO: implement Eq (and eventually Order)
}
