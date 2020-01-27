package tofu.zioInstances
import java.io.IOException

import zio.clock.Clock
import zio.console.Console
import zio.random.Random

object implicits {
  @inline final implicit def rioTofuImplicit[R]: RioTofuInstance[R] = rioTofuInstance

  @inline final implicit def zioTofuErrorsToImplicit[R, E, E1]: ZioTofuErrorsToInstance[R, E, E1] =
    zioTofuErrorsToInstance

  @inline final implicit def zioTofuTimeoutImplicit[R <: Clock, E]: ZIOTofuTimeoutInstance[R, E] =
    zioTofuTimeoutInstance

  @inline final implicit def zioTofuConcurrentImplicit[R1, E1, R, E]: ZioTofuConcurrentInstance[R1, E1, R, E] =
    zioTofuConcurrentInstance

  @inline final implicit def zioTofuConsoleImplicit[R <: Console, E >: IOException]: ZIOTofuConsoleInstance[R, E] =
    zioTofuConsoleInstance

  @inline final implicit def zioTofuRandomImplicit[R <: Random, E]: ZIOTofuRandomInstance[R, E] = zioTofuRandomInstance
}
