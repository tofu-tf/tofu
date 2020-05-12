package tofu.memo

import java.util.concurrent.TimeUnit

import cats.effect.Clock
import tofu.syntax.monadic._
import cats.{Functor, Monad}
import tofu.Guarantee
import tofu.concurrent.{MakeMVar, MakeRef}

import scala.concurrent.duration.FiniteDuration

object Cached {
  def apply[I[_]] = new CachedApply[I]

  private def usingState[F[_]: Monad, A](value: F[A], ttl: Long, control: F[CacheControl])(
      state: CacheState[F, A]
  )(implicit clock: Clock[F]): F[A] = {
    for {
      cacheControl <- control
      now          <- clock.realTime(TimeUnit.MILLISECONDS)
      validAfter    = (now - ttl).max(cacheControl.invalidated.millis)
      result       <- state.getOrElse(value, now, validAfter)
    } yield result
  }

  class CachedApply[I[_]] {
    def apply[F[_]: Monad: Clock, A](
        value: F[A]
    )(ttl: FiniteDuration, control: F[CacheControl])(implicit I: Functor[I]) =
      new CachedApply2[I, F, A](value)(ttl.toMillis, control)
  }

  class CachedApply2[I[_]: Functor, F[_]: Monad: Clock, A](value: F[A])(ttl: Long, control: F[CacheControl]) {
    def apply(method: CacheMethod)(implicit refs: MakeRef[I, F], mvars: MakeMVar[I, F], FG: Guarantee[F]): I[F[A]] =
      CacheState.in[I, F, A](method).map(usingState(value, ttl, control))

    def ref(implicit refs: MakeRef[I, F]) = CacheState.refIn[I, F, A]().map(usingState(value, ttl, control))

    def mvar(implicit mvars: MakeMVar[I, F], FG: Guarantee[F]) =
      CacheState.mvarIn[I, F, A]().map(usingState(value, ttl, control))
  }
}
