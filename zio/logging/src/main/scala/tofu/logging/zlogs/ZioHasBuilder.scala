package tofu.logging.zlogs

import izumi.reflect.Tag
import tofu.logging.Loggable
import tofu.logging.zlogs.ZioHasBuilder.UnHas
import zio.Has

import scala.annotation.implicitNotFound

class ZioHasBuilder[R](val loggable: Loggable[R]) extends AnyVal { self =>
  def of[U <: Has[_]](implicit U: UnHas[U]) =
    new ZioHasBuilder[R with U](loggable.narrow[R with U] + U.loggable.narrow[R with U])

  def make[R1 <: R]: ZLogs[R1] = ZLogs.withContext[R1](loggable.narrow[R1])
}

object ZioHasBuilder {
  @implicitNotFound(
    "Could not understand ${U} as zio loggable module. Check it is `Has[SomeService]` and `SomeService` has `Loggable` instance"
  )
  class UnHas[U](val loggable: Loggable[U]) extends AnyVal

  object UnHas {
    implicit def unHas[S: Tag](implicit S: Loggable[S]) =
      new UnHas[Has[S]](S.contramap(_.get))
  }
}
