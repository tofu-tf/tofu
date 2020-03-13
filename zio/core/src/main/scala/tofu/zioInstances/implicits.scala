package tofu.zioInstances
import java.io.IOException

import tofu.optics.{Contains, Extract}
import zio.clock.Clock
import zio.console.Console
import zio.random.Random

object implicits extends ZioTofuImplicits1

class ZioTofuImplicits1 extends ZioTofuImplicits2 {
  @inline final implicit def rioTofuImplicit[R]: RioTofuInstance[R] = rioTofuInstance

  @inline final implicit def zioTofuErrorsToImplicit[R, E]: ZioTofuErrorsToInstance[R, E, Nothing] =
    zioTofuErrorsToInstance

  @inline final implicit def zioTofuErrorsExtractToImplicit[R, E, E1: * Extract E]: ZioTofuErrorsToInstance[R, E, E1] =
    zioTofuExtractErrorsInstance

  @inline final implicit def zioTofuTimeoutImplicit[R <: Clock, E]: ZIOTofuTimeoutInstance[R, E] =
    zioTofuTimeoutInstance

  @inline final implicit def zioTofuConcurrentImplicit[R1, E1, R, E]: ZioTofuConcurrentInstance[R1, E1, R, E] =
    zioTofuConcurrentInstance

  @inline final implicit def zioTofuConsoleImplicit[R <: Console, E >: IOException]: ZIOTofuConsoleInstance[R, E] =
    zioTofuConsoleInstance

  @inline final implicit def zioTofuRandomImplicit[R <: Random, E]: ZIOTofuRandomInstance[R, E] = zioTofuRandomInstance

  @inline final implicit def zioTofuUnliftImplicit[R, E]: ZioTofuUnliftInstance[Any, R, E] = zioTofuUnliftInstance

  @inline final implicit def zioTofuContainsUnliftImplicit[R1, R2: * Contains R1, E]: ZioTofuUnliftInstance[R1, R2, E] =
    zioTofuContainsUnliftInstance[R1, R2, E]
}
trait ZioTofuImplicits2 {
  @inline final implicit def zioTofuImplicit[R, E]: ZioTofuInstance[R, E] = zioTofuInstance
}
