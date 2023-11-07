package tofu.zioInstances
import glass.{Contains, Extract}
import zio.Tag

import java.io.IOException

object implicits extends ZioTofuImplicits1

private[zioInstances] class ZioTofuImplicits1 extends ZioTofuImplicits2 {
  @inline final implicit def rioTofuImplicit[R]: RioTofuInstance[R] = rioTofuInstance

  @inline final implicit def zioTofuErrorsExtractToImplicit[R, E, E1: Extract[_, E]]: ZioTofuErrorsToInstance[R, E, E1] =
    zioTofuExtractErrorsInstance

  @inline final implicit def zioTofuTimeoutImplicit[R, E]: ZioTofuTimeoutInstance[R, E] =
    zioTofuTimeoutInstance

  @inline final implicit def zioTofuConsoleImplicit[R, E >: IOException]: ZioTofuConsoleInstance[R, E] =
    zioTofuConsoleInstance

  @inline final implicit def zioTofuRandomImplicit[R, E]: ZioTofuRandomInstance[R, E] = zioTofuRandomInstance

  @inline final implicit def zioTofuContainsUnliftImplicit[R1: Tag, R2: Tag: Contains[_, R1], E]
      : ZioTofuContainsUnliftInstance[R1, R2, E] =
    zioTofuContainsUnliftInstance[R1, R2, E]

  @inline final implicit def rioTofuUnliftIOImplicit[R]: RioTofuUnliftIOInstance[R] = rioTofuUnliftIOInstance

  @inline final implicit def rioTofuUnsafeExecFutureImplicit[R]: RioTofuUnsafeExecFutureInstance[R] =
    rioTofuUnsafeExecFutureInstance

  @inline final implicit def rioTofuBlockingImplicit[R]: RioTofuBlockingInstance[R] =
    rioTofuBlockingInstance[R]

  @inline final implicit def zioTofuBiImplicit[R]: ZioTofuBiInstance[R] = zioTofuBiInstance[R]
}
private[zioInstances] trait ZioTofuImplicits2 extends ZioTofuImplicits3 {
  @inline final implicit def zioTofuErrorsToImplicit[R, E]: ZioTofuErrorsToInstance[R, E, Nothing] =
    zioTofuErrorsToInstance
  @inline final implicit def zioTofuImplicit[R, E]: ZioTofuInstance[R, E]                          = zioTofuInstance
  @inline final implicit def zioTofuWithRunImplicit[R, E]: ZioTofuWithRunInstance[R, E]            = zioTofuWithRunInstance
  @inline final implicit def zioTofuBlockingImplicit[R, E]: ZioTofuBlockingInstance[R, E]          =
    zioTofuBlockingInstance[R, E]
}

private[zioInstances] trait ZioTofuImplicits3 {
  @inline final implicit def zioTofuUnliftManyImplicit[R, E, R1: Tag]: ZioTofuUnliftManyInstance[R, E, R1] =
    zioTofuUnliftManyInstance
}
