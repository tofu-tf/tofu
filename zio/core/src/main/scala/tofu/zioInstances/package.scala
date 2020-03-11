package tofu
import java.io.IOException

import tofu.optics.{Contains, Extract}
import tofu.optics.functions.{containsUnit, extractSubtype}
import tofu.zioInstances.{
  RioTofuInstance,
  ZIOTofuConsoleInstance,
  ZIOTofuRandomInstance,
  ZIOTofuTimeoutInstance,
  ZioTofuUnliftInstance,
  ZioTofuConcurrentInstance,
  ZioTofuConcurrentInstanceUIO,
  ZioTofuErrorsToInstance,
  ZioTofuInstance
}
import zio.clock.Clock
import zio.console.Console
import zio.random.Random

package object zioInstances extends ZioInstances1

class ZioInstances1 extends ZioInstances2 {
  private[this] val rioTofuInstanceAny: RioTofuInstance[Any] = new RioTofuInstance
  final def rioTofuInstance[R]: RioTofuInstance[R]           = rioTofuInstanceAny.asInstanceOf[RioTofuInstance[R]]

  private[this] val zioErrorsToInstanceAny: ZioTofuErrorsToInstance[Any, Any, Nothing] =
    new ZioTofuErrorsToInstance[Any, Any, Nothing]()(extractSubtype[Nothing, Any])
  final def zioTofuErrorsToInstance[R, E]: ZioTofuErrorsToInstance[R, E, Nothing] =
    zioErrorsToInstanceAny.asInstanceOf[ZioTofuErrorsToInstance[R, E, Nothing]]
  final def zioTofuExtractErrorsInstance[R, E, E1: * Extract E]: ZioTofuErrorsToInstance[R, E, E1] =
    new ZioTofuErrorsToInstance

  private[this] val zioTofuTimeoutInstanceAny: ZIOTofuTimeoutInstance[Clock, Any] = new ZIOTofuTimeoutInstance
  final def zioTofuTimeoutInstance[R <: Clock, E]: ZIOTofuTimeoutInstance[R, E] =
    zioTofuTimeoutInstanceAny.asInstanceOf[ZIOTofuTimeoutInstance[R, E]]

  private[this] val zioTofuConcurrentInstanceAny: ZioTofuConcurrentInstance[Any, Nothing, Any, Nothing] =
    new ZioTofuConcurrentInstanceUIO

  final def zioTofuConcurrentInstance[R1, E1, R, E]: ZioTofuConcurrentInstance[R1, E1, R, E] =
    zioTofuConcurrentInstanceAny.asInstanceOf[ZioTofuConcurrentInstance[R1, E1, R, E]]

  private[this] val zioTofuConsoleInstanceAny: ZIOTofuConsoleInstance[Console, IOException] = new ZIOTofuConsoleInstance

  final def zioTofuConsoleInstance[R <: Console, E >: IOException]: ZIOTofuConsoleInstance[R, E] =
    zioTofuConsoleInstanceAny.asInstanceOf[ZIOTofuConsoleInstance[R, E]]

  private[this] val zioTofuRandomInstanceAny: ZIOTofuRandomInstance[Random, Nothing] = new ZIOTofuRandomInstance

  final def zioTofuRandomInstance[R <: Random, E]: ZIOTofuRandomInstance[R, E] =
    zioTofuRandomInstanceAny.asInstanceOf[ZIOTofuRandomInstance[R, E]]

  private[this] val zioTofuUnliftInstanceAny = new ZioTofuUnliftInstance[Any, Any, Any]()(containsUnit[Any, Any])
  final def zioTofuUnliftInstance[R, E]: ZioTofuUnliftInstance[Any, R, E] =
    zioTofuUnliftInstanceAny.asInstanceOf[ZioTofuUnliftInstance[Any, R, E]]
  final def zioTofuContainsUnliftInstance[R1, R2: * Contains R1, E]: ZioTofuUnliftInstance[R1, R2, E] =
    new ZioTofuUnliftInstance[R1, R2, E]
}
trait ZioInstances2 {

  private[this] val zioTofuInstanceAny: ZioTofuInstance[Any, Any] = new ZioTofuInstance
  final def zioTofuInstance[R, E]: ZioTofuInstance[R, E]          = zioTofuInstanceAny.asInstanceOf[ZioTofuInstance[R, E]]
}
