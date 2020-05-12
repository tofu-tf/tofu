package tofu.memo

import java.util.concurrent.TimeUnit

import cats.effect.Clock
import tofu.syntax.monadic._
import cats.{Functor, Monad}
import tofu.Guarantee
import tofu.concurrent.{MVars, MakeMVar, MakeRef, Refs}

import scala.concurrent.duration.FiniteDuration

object CachedFunc {
  def apply[I[_]] = new CachedApply[I]

  class CachedApply[I[_]] {
    def apply[F[_]: Monad: Clock, A, B](
        f: A => F[B]
    )(ttl: FiniteDuration, control: F[CacheControl])(implicit I: Functor[I]) =
      new CacheApply2[I, F, A, B](f)(ttl.toMillis, control)
  }

  class CacheApply2[I[_]: Functor, F[_]: Clock: Monad, A, B](f: A => F[B])(ttl: Long, control: F[CacheControl]) {
    def apply(method: CacheMethod)(implicit refs: Refs[F], mvars: MVars[F], FG: Guarantee[F]) =
      new CacheApply3[I, F, A, B](f)(ttl, control)(CacheState(method, _))

    def refVals(implicit refs: Refs[F]) = new CacheApply3[I, F, A, B](f)(ttl, control)(CacheState.ref)

    def mvarVals(implicit mvars: MVars[F], FG: Guarantee[F]) =
      new CacheApply3[I, F, A, B](f)(ttl, control)(CacheState.mvar)
  }

  class CacheApply3[I[_]: Functor, F[_]: Clock: Monad, A, B](f: A => F[B])(ttl: Long, control: F[CacheControl])(
      factory: CacheVal[B] => F[CacheState[F, B]]
  ) {
    def apply(method: CacheMethod)(implicit mr: MakeRef[I, F], mv: MakeMVar[I, F], FG: Guarantee[F]): I[A => F[B]] =
      CacheKeyState[I, F, A, B](method)(factory).map(usingState[F, A, B](f)(ttl, control))

    def ref(implicit refs: MakeRef[I, F]): I[A => F[B]] =
      CacheKeyState.ref[I, F, A, B](factory).map(usingState[F, A, B](f)(ttl, control))

    def mvar(implicit mvars: MakeMVar[I, F], FG: Guarantee[F]): I[A => F[B]] =
      CacheKeyState.mvar[I, F, A, B](factory).map(usingState[F, A, B](f)(ttl, control))
  }

  private def usingState[F[_]: Monad, A, B](
      f: A => F[B]
  )(ttl: Long, control: F[CacheControl])(state: CacheKeyState[F, A, B])(a: A)(implicit clock: Clock[F]) =
    for {
      cacheControl <- control
      now          <- clock.realTime(TimeUnit.MILLISECONDS)
      validAfter    = (now - ttl).max(cacheControl.invalidated.millis)
      result       <- state.getOrElse(f(a), a, now, validAfter)
    } yield result
}
