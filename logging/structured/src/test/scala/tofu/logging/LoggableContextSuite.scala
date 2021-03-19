package tofu.logging

import tofu.HasContext
import tofu.compat.unused

object LoggableContextSuite {

  def summonInstance[R: Loggable, F[_]: *[_] HasContext R](): Unit = {
    LoggableContext.of[F].instance
    ()
  }

  @unused
  def summonInstanceUnambiguously[R1: Loggable, R2: Loggable, F[_]: *[_] HasContext R1: *[_] HasContext R2](): Unit = {
    LoggableContext.of[F].instance[R1]
    ()
  }

}
