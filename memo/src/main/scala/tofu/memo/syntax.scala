package tofu.memo

import java.util.concurrent.TimeUnit

import cats.{Functor, Monad}
import cats.effect.{Clock, Concurrent, Timer}
import cats.syntax.flatMap._
import cats.syntax.functor._
import tofu.Guarantee
import tofu.concurrent.{MVars, MakeMVar, MakeRef, Refs}

import scala.concurrent.duration.FiniteDuration

@deprecated("use `Cached` and `CachedFunc`", since = "0.4.18")
object syntax extends Memoize.ToMemoizeOps {
  implicit class MemoizeCache0Ops[F[_], A](val value: F[A]) extends AnyVal {
    @deprecated("use `Cached`", since = "0.4.18")
    def cached[I[_]: Functor](
        ttl: FiniteDuration,
        control: F[CacheControl],
        method: CacheMethod = CacheMethod.MVar
    )(implicit clock: Clock[F], F: Monad[F], FG: Guarantee[F], refs: MakeRef[I, F], mvars: MakeMVar[I, F]): I[F[A]] =
      for (state <- CacheState.in[I, F, A](method)) yield {
        val ttlMilliseconds = ttl.toMillis
        for {
          cacheControl <- control
          now          <- clock.realTime(TimeUnit.MILLISECONDS)
          validAfter   = (now - ttlMilliseconds).max(cacheControl.invalidated.millis)
          result       <- state.getOrElse(value, now, validAfter)
        } yield result
      }

    @deprecated("unsafe", since = "0.4.18")
    def cachedUnsafe(
        ttl: FiniteDuration,
        control: F[CacheControl],
        method: CacheMethod = CacheMethod.MVar
    )(implicit timer: Timer[F], F: Concurrent[F], memo: Memoize[F]): F[A] =
      cached(ttl, control, method).memoize.flatten
  }

  implicit class MemoizeCache1Ops[F[_], A, B](val f: A => F[B]) extends AnyVal {
    @deprecated("use `CachedFunc`", since = "0.4.18")
    def cached(ttl: FiniteDuration,
               control: F[CacheControl],
               keyMethod: CacheMethod = CacheMethod.MVar,
               valueMethod: CacheMethod = CacheMethod.MVar)(
        implicit
        clock: Clock[F],
        FV: MVars[F],
        FR: Refs[F],
        F: Monad[F],
        FG: Guarantee[F]
    ): F[A => F[B]] =
      for (state <- CacheKeyState[F, F, A, B](keyMethod, valueMethod)) yield {
        val ttlMilliseconds = ttl.toMillis
        a: A =>
          for {
            cacheControl <- control
            now          <- clock.realTime(TimeUnit.MILLISECONDS)
            validAfter   = (now - ttlMilliseconds).max(cacheControl.invalidated.millis)
            result       <- state.getOrElse(f(a), a, now, validAfter)
          } yield result
      }

    @deprecated("unsafe", since = "0.4.18")
    def cachedUnsafe(
        ttl: FiniteDuration,
        control: F[CacheControl],
        keyMethod: CacheMethod = CacheMethod.MVar,
        valueMethod: CacheMethod = CacheMethod.MVar
    )(implicit timer: Timer[F], F: Concurrent[F], memo: Memoize[F]): A => F[B] = {
      val fs = cached(ttl, control, keyMethod, valueMethod).memoize
      a =>
        fs.flatMap(_(a))
    }
  }
}
