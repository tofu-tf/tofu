package tofu
package zioInstances
import java.io.IOException

import cats.{Applicative, Functor, ~>}
import cats.effect.{CancelToken, Fiber}
import tofu.generate.GenRandom
import tofu.internal.CachedMatcher
import tofu.lift.Unlift
import tofu.optics.{Contains, Extract}
import tofu.syntax.funk.funK
import tofu.zioInstances.ZioTofuInstance.convertFiber
import zio.clock.Clock
import zio.console.Console
import zio.random.Random
import zio.{Fiber => ZFiber, _}

import scala.concurrent.{ExecutionContext, Future}
import scala.concurrent.duration.FiniteDuration

class ZioTofuInstance[R, E]
    extends WithRun[ZIO[R, E, *], ZIO[Any, E, *], R] with Errors[ZIO[R, E, *], E] with Start[ZIO[R, E, *]]
    with Finally[ZIO[R, E, *], Exit[E, *]] with Execute[ZIO[R, E, *]] {
  val context: ZIO[R, E, R] = ZIO.access[R](r => r)
  val functor: Functor[ZIO[R, E, *]] = new Functor[ZIO[R, E, *]] {
    def map[A, B](fa: ZIO[R, E, A])(f: A => B): ZIO[R, E, B] = fa.map(f)
  }

  final def runContext[A](fa: ZIO[R, E, A])(ctx: R): ZIO[Any, E, A]   = fa.provide(ctx)
  final def local[A](fa: ZIO[R, E, A])(project: R => R): ZIO[R, E, A] = fa.provideSome(project)
  final override def ask[A](f: R => A): ZIO[R, E, A]                  = ZIO.fromFunction(f)

  final def restore[A](fa: ZIO[R, E, A]): ZIO[R, E, Option[A]] = fa.option
  final def raise[A](err: E): ZIO[R, E, A]                     = ZIO.fail(err)
  final def lift[A](fa: ZIO[R, E, A]): ZIO[R, E, A]            = fa
  final def tryHandleWith[A](fa: ZIO[R, E, A])(f: E => Option[ZIO[R, E, A]]): ZIO[R, E, A] =
    fa.catchSome(CachedMatcher(f))
  final override def handleWith[A](fa: ZIO[R, E, A])(f: E => ZIO[R, E, A]): ZIO[R, E, A] = fa.catchAll(f)

  final def start[A](fa: ZIO[R, E, A]): ZIO[R, E, Fiber[ZIO[R, E, *], A]] = fa.fork.map(convertFiber)
  final def racePair[A, B](
      fa: ZIO[R, E, A],
      fb: ZIO[R, E, B]
  ): ZIO[R, E, Either[(A, Fiber[ZIO[R, E, *], B]), (Fiber[ZIO[R, E, *], A], B)]] =
    (fa raceWith fb)(
      { case (l, f) => l.fold(f.interrupt *> ZIO.halt(_), RIO.succeed(_)).map(lv => Left((lv, convertFiber(f)))) },
      { case (r, f) => r.fold(f.interrupt *> ZIO.halt(_), RIO.succeed(_)).map(rv => Right((convertFiber(f), rv))) }
    )

  final def race[A, B](fa: ZIO[R, E, A], fb: ZIO[R, E, B]): ZIO[R, E, Either[A, B]] = fa.raceEither(fb)
  final def never[A]: ZIO[R, E, A]                                                  = ZIO.never
  final def fireAndForget[A](fa: ZIO[R, E, A]): ZIO[R, E, Unit]                     = fa.fork.unit

  final def bracket[A, B, C](
      init: ZIO[R, E, A]
  )(action: A => ZIO[R, E, B])(release: (A, Boolean) => ZIO[R, E, C]): ZIO[R, E, B] =
    init.bracketExit[R, E, B]((a, e) => release(a, e.succeeded).ignore, action)

  final def finallyCase[A, B, C](
      init: ZIO[R, E, A]
  )(action: A => ZIO[R, E, B])(release: (A, Exit[E, B]) => ZIO[R, E, C]): ZIO[R, E, B] =
    init.bracketExit[R, E, B]((a, e) => release(a, e).ignore, action)

  def executionContext: ZIO[R, E, ExecutionContext] = ZIO.runtime[R].map(_.platform.executor.asEC)

  def deferFutureAction[A](f: ExecutionContext => Future[A]): ZIO[R, E, A] = ZIO.fromFuture(f).orDie
}

class RioTofuInstance[R] extends ZioTofuInstance[R, Throwable] {
  override def deferFutureAction[A](f: ExecutionContext => Future[A]): ZIO[R, Throwable, A] = ZIO.fromFuture(f)
}

class ZIOTofuTimeoutInstance[R <: Clock, E] extends Timeout[ZIO[R, E, *]] {
  final def timeoutTo[A](fa: ZIO[R, E, A], after: FiniteDuration, fallback: ZIO[R, E, A]): ZIO[R, E, A] =
    fa.timeoutTo[R, E, A, ZIO[R, E, A]](fallback)(ZIO.succeed(_))(zio.duration.Duration.fromScala(after)).flatten
}

class ZIOTofuRandomInstance[R <: Random, E] extends GenRandom[ZIO[R, E, *]] {
  override def nextLong: ZIO[R, E, Long]       = random.nextLong
  override def nextInt(n: Int): ZIO[R, E, Int] = random.nextInt(n)
}

class ZIOTofuConsoleInstance[R <: Console, E >: IOException] extends tofu.common.Console[ZIO[R, E, *]] {
  override def readStrLn: ZIO[R, E, String]           = console.getStrLn
  override def putStr(s: String): ZIO[R, E, Unit]     = console.putStr(s)
  override def putStrLn(s: String): ZIO[R, E, Unit]   = console.putStrLn(s)
  override def putErr(err: String): ZIO[R, E, Unit]   = console.putStr(err)
  override def putErrLn(err: String): ZIO[R, E, Unit] = console.putStrLn(err)
}

object ZioTofuInstance {
  def convertFiber[R, E, A](f: ZFiber[E, A]): Fiber[ZIO[R, E, *], A] = new Fiber[ZIO[R, E, *], A] {
    def cancel: CancelToken[ZIO[R, E, *]] = f.interrupt.unit
    def join: ZIO[R, E, A]                = f.join
  }
}

class ZioTofuErrorsToInstance[R, E, E1](implicit lens: Extract[E1, E])
    extends ErrorsTo[ZIO[R, E, *], ZIO[R, E1, *], E] {
  final def handleWith[A](fa: ZIO[R, E, A])(f: E => ZIO[R, E1, A]): ZIO[R, E1, A] = fa.catchAll(f)
  final def restore[A](fa: ZIO[R, E, A]): ZIO[R, E1, Option[A]]                   = fa.option
  final def raise[A](err: E): ZIO[R, E, A]                                        = ZIO.fail(err)
  final def lift[A](fa: ZIO[R, E1, A]): ZIO[R, E, A]                              = fa.mapError(lens.extract)

  final override def handle[A](fa: ZIO[R, E, A])(f: E => A)(implicit G: Applicative[ZIO[R, E1, *]]): ZIO[R, E1, A] =
    fa.catchAll(e => ZIO.succeed(f(e)))

  final override def attempt[A](
      fa: ZIO[R, E, A]
  )(implicit F: Functor[ZIO[R, E, *]], G: Applicative[ZIO[R, E1, *]]): ZIO[R, E1, Either[E, A]] =
    fa.either

}

class ZioTofuUnliftInstance[R1, R2, E](implicit lens: Contains[R2, R1]) extends Unlift[ZIO[R1, E, *], ZIO[R2, E, *]] {
  def lift[A](fa: ZIO[R1, E, A]): ZIO[R2, E, A] = fa.provideSome(lens.extract)
  def unlift: ZIO[R2, E, ZIO[R2, E, *] ~> ZIO[R1, E, *]] =
    ZIO.access[R2](r2 => funK(_.provideSome(r1 => lens.set(r2, r1))))
}
