package tofu.logging

import tofu.compat.unused
import tofu.WithContext

object LoggableContextSuite {

  def summonInstance[R: Loggable, F[_]: ({ type L[x[_]] = WithContext[x, R] })#L](): Unit = {
    LoggableContext.of[F].instance
    ()
  }

  @unused
  def summonInstanceUnambiguously[
      R1: Loggable,
      R2: Loggable,
      F[_]: ({ type L[x[_]] = WithContext[x, R1] })#L: ({ type L[x[_]] = WithContext[x, R2] })#L
  ](): Unit = {
    LoggableContext.of[F].instance[R1]
    ()
  }

}
