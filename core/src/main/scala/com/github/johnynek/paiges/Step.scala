package com.github.johnynek.paiges

sealed abstract class Step[+A, +B]

object Step {
  case class Emit[B](value: B) extends Step[Nothing, B]
  case class Split[A](left: A, right: () => A) extends Step[A, Nothing]
}
