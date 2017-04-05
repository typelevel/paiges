package com.github.johnynek.paiges

import cats.kernel.{Order, Monoid}

package object instances {
  implicit val paigesDocMonoid: Monoid[Doc] =
    new Monoid[Doc] {
      def empty: Doc = Doc.empty
      def combine(x: Doc, y: Doc): Doc = x + y
    }

  implicit val paigesDocOrder: Order[Doc] =
    new Order[Doc] {
      def compare(x: Doc, y: Doc): Int = x compare y
    }
}
