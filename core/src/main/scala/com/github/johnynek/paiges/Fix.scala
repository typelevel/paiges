package com.github.johnynek.paiges

private[paiges] case class Fix[F[_]](unfix: F[Fix[F]])
