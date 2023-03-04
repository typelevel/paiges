/*
 * Copyright 2023 Typelevel
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
