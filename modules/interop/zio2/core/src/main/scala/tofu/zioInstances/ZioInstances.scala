package tofu
package zioInstances

import glass.functions.extractSubtype
import glass.{Contains, Extract}
import zio.Tag

import java.io.IOException

private[zioInstances] class ZioInstances {
  private[this] val rioTofuInstanceAny: RioTofuInstance[Any] = new RioTofuInstance
  final def rioTofuInstance[R]: RioTofuInstance[R]           = rioTofuInstanceAny.asInstanceOf[RioTofuInstance[R]]

  private[this] val zioErrorsToInstanceAny: ZioTofuErrorsToInstance[Any, Any, Nothing]             =
    new ZioTofuErrorsToInstance[Any, Any, Nothing]()(extractSubtype[Nothing, Any])
  final def zioTofuErrorsToInstance[R, E]: ZioTofuErrorsToInstance[R, E, Nothing]                  =
    zioErrorsToInstanceAny.asInstanceOf[ZioTofuErrorsToInstance[R, E, Nothing]]
  final def zioTofuExtractErrorsInstance[R, E, E1: Extract[_, E]]: ZioTofuErrorsToInstance[R, E, E1] =
    new ZioTofuErrorsToInstance

  private[this] val zioTofuTimeoutInstanceAny: ZioTofuTimeoutInstance[Any, Any] = new ZioTofuTimeoutInstance
  final def zioTofuTimeoutInstance[R, E]: ZioTofuTimeoutInstance[R, E]          =
    zioTofuTimeoutInstanceAny.asInstanceOf[ZioTofuTimeoutInstance[R, E]]

  private[this] val zioTofuConsoleInstanceAny: ZioTofuConsoleInstance[Any, IOException] = new ZioTofuConsoleInstance
  final def zioTofuConsoleInstance[R, E >: IOException]: ZioTofuConsoleInstance[R, E]   =
    zioTofuConsoleInstanceAny.asInstanceOf[ZioTofuConsoleInstance[R, E]]

  private[this] val zioTofuRandomInstanceAny: ZioTofuRandomInstance[Any, Nothing] = new ZioTofuRandomInstance
  final def zioTofuRandomInstance[R, E]: ZioTofuRandomInstance[R, E]              =
    zioTofuRandomInstanceAny.asInstanceOf[ZioTofuRandomInstance[R, E]]

  final def zioTofuContainsUnliftInstance[R1: Tag, R2: Tag: Contains[_, R1], E]
      : ZioTofuContainsUnliftInstance[R1, R2, E] =
    new ZioTofuContainsUnliftInstance

  private[this] val rioTofuUnliftIOInstanceAny: RioTofuUnliftIOInstance[Any] = new RioTofuUnliftIOInstance
  final def rioTofuUnliftIOInstance[R]: RioTofuUnliftIOInstance[R]           =
    rioTofuUnliftIOInstanceAny.asInstanceOf[RioTofuUnliftIOInstance[R]]

  private[this] val rioTofuUnsafeExecFutureInstanceAny: RioTofuUnsafeExecFutureInstance[Any] =
    new RioTofuUnsafeExecFutureInstance
  final def rioTofuUnsafeExecFutureInstance[R]: RioTofuUnsafeExecFutureInstance[R]           =
    rioTofuUnsafeExecFutureInstanceAny.asInstanceOf[RioTofuUnsafeExecFutureInstance[R]]

  private[this] val zioTofuInstanceAny: ZioTofuInstance[Any, Any] = new ZioTofuInstance
  final def zioTofuInstance[R, E]: ZioTofuInstance[R, E]          = zioTofuInstanceAny.asInstanceOf[ZioTofuInstance[R, E]]

  private[this] val zioTofuWithRunInstanceAny                          = new ZioTofuWithRunInstance[Any, Any]
  final def zioTofuWithRunInstance[R, E]: ZioTofuWithRunInstance[R, E] =
    zioTofuWithRunInstanceAny.asInstanceOf[ZioTofuWithRunInstance[R, E]]

  final def zioTofuUnliftManyInstance[R, E, R1: Tag]: ZioTofuUnliftManyInstance[R, E, R1] =
    new ZioTofuUnliftManyInstance

  private[this] val zioTofuBlockingInstanceAny                           =
    new ZioTofuBlockingInstance[Any, Any]
  final def zioTofuBlockingInstance[R, E]: ZioTofuBlockingInstance[R, E] =
    zioTofuBlockingInstanceAny.asInstanceOf[ZioTofuBlockingInstance[R, E]]

  private[this] val rioTofuBlockingInstanceAny                     =
    new RioTofuBlockingInstance[Any]
  final def rioTofuBlockingInstance[R]: RioTofuBlockingInstance[R] =
    rioTofuBlockingInstanceAny.asInstanceOf[RioTofuBlockingInstance[R]]

  private[this] val zioTofuBiInstanceAny               = new ZioTofuBiInstance[Any]
  final def zioTofuBiInstance[R]: ZioTofuBiInstance[R] = zioTofuBiInstanceAny.asInstanceOf[ZioTofuBiInstance[R]]
}
