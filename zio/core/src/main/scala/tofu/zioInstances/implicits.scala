package tofu.zioInstances
import tofu.optics.Extract
import zio.clock.Clock
import zio.console.Console
import zio.random.Random

import java.io.IOException

object implicits {
  @inline final implicit def rioTofuImplicit[R]: RioTofuInstance[R] = rioTofuInstance

  @inline final implicit def zioTofuErrorsToImplicit[R, E, E1]: ZioTofuErrorsToInstance[R, E, Nothing] =
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
}
