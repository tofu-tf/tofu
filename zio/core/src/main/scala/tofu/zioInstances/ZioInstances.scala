package tofu
package zioInstances

import java.io.IOException
import tofu.optics.functions.extractSubtype
import tofu.optics.{Contains, Extract}
import zio.clock.Clock
import zio.console.Console
import zio.random.Random
import zio.{Has, Tag}
import zio.blocking.Blocking
import zio.ZIO

private[zioInstances] class ZioInstances {
  private[this] val rioTofuInstanceAny: RioTofuInstance[Any] = new RioTofuInstance
  final def rioTofuInstance[R]: RioTofuInstance[R]           = rioTofuInstanceAny.asInstanceOf[RioTofuInstance[R]]

  private[this] val zioErrorsToInstanceAny: ZioTofuErrorsToInstance[Any, Any, Nothing]             =
    new ZioTofuErrorsToInstance[Any, Any, Nothing]()(extractSubtype[Nothing, Any])
  final def zioTofuErrorsToInstance[R, E]: ZioTofuErrorsToInstance[R, E, Nothing]                  =
    zioErrorsToInstanceAny.asInstanceOf[ZioTofuErrorsToInstance[R, E, Nothing]]
  final def zioTofuExtractErrorsInstance[R, E, E1: * Extract E]: ZioTofuErrorsToInstance[R, E, E1] =
    new ZioTofuErrorsToInstance

  private[this] val zioTofuTimeoutInstanceAny: ZioTofuTimeoutInstance[Clock, Any] = new ZioTofuTimeoutInstance
  final def zioTofuTimeoutInstance[R <: Clock, E]: ZioTofuTimeoutInstance[R, E]   =
    zioTofuTimeoutInstanceAny.asInstanceOf[ZioTofuTimeoutInstance[R, E]]

  private[this] val zioTofuConcurrentInstanceAny: ZioTofuConcurrentInstance[Any, Nothing, Any, Nothing] =
    new ZioTofuConcurrentInstanceUIO

  final def zioTofuConcurrentInstance[R1, E1, R, E]: ZioTofuConcurrentInstance[R1, E1, R, E] =
    zioTofuConcurrentInstanceAny.asInstanceOf[ZioTofuConcurrentInstance[R1, E1, R, E]]

  private[this] val zioTofuConsoleInstanceAny: ZioTofuConsoleInstance[Console, IOException] = new ZioTofuConsoleInstance

  final def zioTofuConsoleInstance[R <: Console, E >: IOException]: ZioTofuConsoleInstance[R, E] =
    zioTofuConsoleInstanceAny.asInstanceOf[ZioTofuConsoleInstance[R, E]]

  private[this] val zioTofuRandomInstanceAny: ZioTofuRandomInstance[Random, Nothing] = new ZioTofuRandomInstance

  final def zioTofuRandomInstance[R <: Random, E]: ZioTofuRandomInstance[R, E] =
    zioTofuRandomInstanceAny.asInstanceOf[ZioTofuRandomInstance[R, E]]

  final def zioTofuContainsUnliftInstance[R1, R2: * Contains R1, E]: ZioTofuContainsUnliftInstance[R1, R2, E] =
    new ZioTofuContainsUnliftInstance[R1, R2, E]

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

  final def zioTofuUnliftHasInstance[R <: Has[_], E, C: Tag]: ZioTofuUnliftHasInstance[R, R with Has[C], E, C] =
    new ZioTofuUnliftHasInstance

  /**  a shortcut for simplifying WithLocal instance definition,
    *  since zioTofuUnliftHasInstance has trouble to infer R
    * {{{
    *   case class MyContext()
    *   type HasMy = Has[MyContext]
    *   type Context = Blocking with Clock with HasMy
    *   type My[+A] = RIO[Context, A]
    *   implicit val myLocal: My WithLocal MyContext = zioLocal
    * }}}
    */
  final def zioLocal[R <: Has[C], E, C: Tag]: ZIO[R, E, *] WithLocal C =
    tofu.zioInstances.zioTofuUnliftHasInstance[R, E, C]

  private[this] val zioTofuBlockingInstanceAny                                       =
    new ZioTofuBlockingInstance[Blocking, Any]
  final def zioTofuBlockingInstance[R <: Blocking, E]: ZioTofuBlockingInstance[R, E] =
    zioTofuBlockingInstanceAny.asInstanceOf[ZioTofuBlockingInstance[R, E]]

  private[this] val rioTofuBlockingInstanceAny                                 =
    new RioTofuBlockingInstance[Blocking]
  final def rioTofuBlockingInstance[R <: Blocking]: RioTofuBlockingInstance[R] =
    rioTofuBlockingInstanceAny.asInstanceOf[RioTofuBlockingInstance[R]]
}
