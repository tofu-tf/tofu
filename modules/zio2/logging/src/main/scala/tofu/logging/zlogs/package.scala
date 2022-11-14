package tofu.logging

import zio.{FiberRef, UIO, URIO, Unsafe, ZIO, Runtime}

package object zlogs {
  type ZLogs[R]    = Logs[UIO, URIO[R, *]]
  type ULogs       = Logs[UIO, UIO]
  type ZLogging[R] = Logging[URIO[R, *]]
  type ULogging    = Logging[UIO]

  private[zlogs] lazy val TofuAnnotatedContextRef: FiberRef[Map[LogAnnotation[_], Any]] =
    Unsafe.unsafe(implicit unsafe =>
      Runtime.default.unsafe
        .run(ZIO.scoped(FiberRef.make[Map[LogAnnotation[_], Any]](Map.empty)))
        .getOrThrow()
    )
}
