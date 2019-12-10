package tofu
import tofu.zioInstances.{
  ZIOTofuTimeoutInstance,
  ZioTofuConcurrentInstance,
  ZioTofuConcurrentInstanceUIO,
  ZioTofuErrorsToInstance,
  ZioTofuInstance
}
import zio.clock.Clock

package object zioInstances extends ZioInstances1

class ZioInstances1 {
  private[this] val zioTofuInstanceAny: ZioTofuInstance[Any, Any] = new ZioTofuInstance
  final def zioTofuInstance[R, E]: ZioTofuInstance[R, E]          = zioTofuInstanceAny.asInstanceOf[ZioTofuInstance[R, E]]

  private[this] val zioErrorsToInstanceAny: ZioTofuErrorsToInstance[Any, Any, Nothing] = new ZioTofuErrorsToInstance
  final def zioTofuErrorsToInstance[R, E, E1]: ZioTofuErrorsToInstance[R, E, E1] =
    zioErrorsToInstanceAny.asInstanceOf[ZioTofuErrorsToInstance[R, E, E1]]

  private[this] val zioTofuTimeoutInstanceAny: ZIOTofuTimeoutInstance[Clock, Any] = new ZIOTofuTimeoutInstance
  final def zioTofuTimeoutInstance[R <: Clock, E]: ZIOTofuTimeoutInstance[R, E] =
    zioTofuTimeoutInstanceAny.asInstanceOf[ZIOTofuTimeoutInstance[R, E]]

  private[this] val zioTofuConcurrentInstanceAny: ZioTofuConcurrentInstance[Any, Nothing, Any, Nothing] =
    new ZioTofuConcurrentInstanceUIO

  final def zioTofuConcurrentInstance[R1, E1, R, E]: ZioTofuConcurrentInstance[R1, E1, R, E] =
    zioTofuConcurrentInstanceAny.asInstanceOf[ZioTofuConcurrentInstance[R1, E1, R, E]]
}
