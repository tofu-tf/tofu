package tofu.logging.zlogs

import tofu.logging.zlogs.ZLogContextLive.ContextDump
import tofu.logging.{LoggedValue, LogAnnotation}
import zio._

class ZLogContextLive(
    ref: => FiberRef[ContextDump]
) extends ZLogContext with ZLogContext.LoggedValuesExtractor {

  override def update[A](key: LogAnnotation[A], value: A): UIO[Unit] =
    ref.update(_ + (key -> value))

  override def get[A](key: LogAnnotation[A]): UIO[Option[A]] =
    ref.get.map(_.get(key).asInstanceOf[Option[A]])

  override def getOrUpdate[A](key: LogAnnotation[A], value: A): UIO[A] = {

    ref.modify[A] { dataMap =>
      if (dataMap.contains(key)) {
        (dataMap(key).asInstanceOf[A], dataMap)
      } else {
        (value, dataMap + (key -> value))
      }
    }
  }

  override def locally[R, E, B](updates: ZLogContext => UIO[Unit])(zio: ZIO[R, E, B]): ZIO[R, E, B] = {
    for {
      oldValue <- ref.get
      b        <- ZIO.acquireReleaseWith(updates(this))(_ => ref.set(oldValue))(_ => zio)
    } yield b
  }

  override def loggedValue: UIO[LoggedValue] = ref.get.map(
    _.map { case (annotation, value) =>
      annotation.name -> annotation.apply(value.asInstanceOf[annotation.Value])
    }
  )

}

object ZLogContextLive {

  type ContextDump = Map[LogAnnotation[_], Any]

  private[zlogs] lazy val TofuLogContextRef: FiberRef[ContextDump] =
    Unsafe.unsafe(implicit unsafe =>
      Runtime.default.unsafe
        .run(ZIO.scoped(FiberRef.make[ContextDump](Map.empty)))
        .getOrThrow()
    )
}
