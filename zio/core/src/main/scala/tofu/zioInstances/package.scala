package tofu
import tofu.zioInstances.{ZIOTofuTimeoutInstance, ZioTofuInstance}
import zio.clock.Clock

package object zioInstances extends ZioInstances1

class ZioInstances1 {
  final def zioTofuInstance[R, E]: ZioTofuInstance[R, E] = new ZioTofuInstance

  final def zioTofuTimeoutInstance[R <: Clock, E]: ZIOTofuTimeoutInstance[R, E] = new ZIOTofuTimeoutInstance
}
