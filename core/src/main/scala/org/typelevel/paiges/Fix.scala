package org.typelevel.paiges

private[paiges] case class Fix[F[_]](unfix: F[Fix[F]])
