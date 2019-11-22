package tofu
package zioInstances
import cats.Functor
import cats.effect.{CancelToken, Fiber}
import tofu.internal.CachedMatcher
import tofu.zioInstances.ZioTofuInstance.convertFiber
import zio.clock.Clock
import zio.{Fiber => ZFiber, _}

import scala.concurrent.duration.FiniteDuration

class ZioTofuInstance[R, E]
    extends RunContext[ZIO[R, E, *]] with Errors[ZIO[R, E, *], E] with Start[ZIO[R, E, *]]
    with Finally[ZIO[R, E, *], Exit[E, *]] {
  type Ctx      = R
  type Lower[a] = ZIO[Any, E, a]
  val context: ZIO[R, E, R] = ZIO.access[R](r => r)
  val functor: Functor[ZIO[R, E, *]] = new Functor[ZIO[R, E, *]] {
    def map[A, B](fa: ZIO[R, E, A])(f: A => B): ZIO[R, E, B] = fa.map(f)
  }

  final def runContext[A](fa: ZIO[R, E, A])(ctx: R): ZIO[Any, E, A]   = fa.provide(ctx)
  final def local[A](fa: ZIO[R, E, A])(project: R => R): ZIO[R, E, A] = fa.provideSome(project)
  final override def ask[A](f: R => A): ZIO[R, E, A]                  = ZIO.fromFunction(f)

  final def restore[A](fa: ZIO[R, E, A]): ZIO[R, E, Option[A]] = fa.option
  final def raise[A](err: E): ZIO[R, E, A]                     = ZIO.fail(err)
  final def tryHandleWith[A](fa: ZIO[R, E, A])(f: E => Option[ZIO[R, E, A]]): ZIO[R, E, A] =
    fa.catchSome(CachedMatcher(f))
  final override def handleWith[A](fa: ZIO[R, E, A])(f: E => ZIO[R, E, A]): ZIO[R, E, A] = fa.catchAll(f)

  final def start[A](fa: ZIO[R, E, A]): ZIO[R, E, Fiber[ZIO[R, E, *], A]] = fa.fork.map(convertFiber)
  final def racePair[A, B](
      fa: ZIO[R, E, A],
      fb: ZIO[R, E, B]
  ): ZIO[R, E, Either[(A, Fiber[ZIO[R, E, *], B]), (Fiber[ZIO[R, E, *], A], B)]] =
    (fa raceWith fb)(
      { case (l, f) => l.fold(f.interrupt *> ZIO.halt(_), RIO.succeed).map(lv => Left((lv, convertFiber(f)))) },
      { case (r, f) => r.fold(f.interrupt *> ZIO.halt(_), RIO.succeed).map(rv => Right((convertFiber(f), rv))) }
    )

  final def race[A, B](fa: ZIO[R, E, A], fb: ZIO[R, E, B]): ZIO[R, E, Either[A, B]] = fa.raceEither(fb)
  final def never[A]: ZIO[R, E, A]                                                  = ZIO.never
  final def fireAndForget[A](fa: ZIO[R, E, A]): ZIO[R, E, Unit]                     = fa.fork.unit

  final def bracket[A, B, C](
      init: ZIO[R, E, A]
  )(action: A => ZIO[R, E, B])(release: (A, Boolean) => ZIO[R, E, C]): ZIO[R, E, B] =
    init.bracketExit[R, E, B]((a, e) => release(a, e.succeeded).ignore, action)


  final def finallyCase[A, B, C](init: ZIO[R, E, A])(action: A => ZIO[R, E, B])(release: (A, Exit[E, B]) => ZIO[R, E, C]): ZIO[R, E, B] =
    init.bracketExit((a, e) => release(a, e).ignore, action)
}

class ZIOTofuTimeoutInstance[R <: Clock, E] extends Timeout[ZIO[R, E, *]] {
  final def timeoutTo[A](fa: ZIO[R, E, A], after: FiniteDuration, fallback: ZIO[R, E, A]): ZIO[R, E, A] =
    fa.timeoutTo[R, E, A, ZIO[R, E, A]](fallback)(ZIO.succeed)(zio.duration.Duration.fromScala(after)).flatten
}

object ZioTofuInstance {
  def convertFiber[R, E, A](f: ZFiber[E, A]): Fiber[ZIO[R, E, *], A] = new Fiber[ZIO[R, E, *], A] {
    def cancel: CancelToken[ZIO[R, E, *]] = f.interrupt.unit
    def join: ZIO[R, E, A]                = f.join
  }
}


