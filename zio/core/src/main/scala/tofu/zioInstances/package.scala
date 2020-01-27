package tofu
import tofu.optics.{Contains, Extract}
import tofu.optics.functions.extractSubtype
import tofu.zioInstances.{RioTofuInstance, ZIOTofuTimeoutInstance, ZIOUnliftInstance, ZioTofuConcurrentInstance, ZioTofuConcurrentInstanceUIO, ZioTofuErrorsToInstance, ZioTofuInstance}
import zio.clock.Clock

package object zioInstances extends ZioInstances1

class ZioInstances1 extends ZioInstances2 {
  private[this] val rioTofuInstanceAny: RioTofuInstance[Any] = new RioTofuInstance
  final def rioTofuInstance[R]: RioTofuInstance[R]           = rioTofuInstanceAny.asInstanceOf[RioTofuInstance[R]]

  private[this] def zioErrorsToInstanceAny: ZioTofuErrorsToInstance[Any, Any, Nothing] =
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

  final implicit def zioUnliftInstance[R1, R2, E](implicit _r12: R2 Contains R1): ZIOUnliftInstance[R1, R2, E] =
    new ZIOUnliftInstance[R1, R2, E]
}
trait ZioInstances2 {

  private[this] val zioTofuInstanceAny: ZioTofuInstance[Any, Any] = new ZioTofuInstance
  final def zioTofuInstance[R, E]: ZioTofuInstance[R, E]          = zioTofuInstanceAny.asInstanceOf[ZioTofuInstance[R, E]]
}
