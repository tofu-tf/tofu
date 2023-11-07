package tofu
package zioInstances

import cats.effect.kernel.Outcome
import cats.effect.{Fiber, LiftIO, IO => CIO}
import cats.{Applicative, Functor, ~>}
import glass.{Contains, Extract}
import tofu.generate.GenRandom
import tofu.internal.CachedMatcher
import tofu.lift.{Unlift, UnliftIO, UnsafeExecFuture}
import tofu.syntax.funk._
import tofu.zioInstances.ZioTofuInstance.convertFiber
import zio.ZIO._
import zio.{ZIO, Fiber => ZFiber, _}

import java.io.IOException
import scala.concurrent.duration.FiniteDuration
import scala.concurrent.{ExecutionContext, Future}

class ZioTofuInstance[R, E]
    extends Errors[ZIO[R, E, _], E] with GenStart[ZIO[R, E, _], E] with Finally[ZIO[R, E, _], Exit[E, _]]
    with Execute[ZIO[R, E, _]] with Delay[ZIO[R, E, _]] {
  final def restore[A](fa: ZIO[R, E, A]): ZIO[R, E, Option[A]]                             = fa.option
  final def raise[A](err: E): ZIO[R, E, A]                                                 = ZIO.fail(err)
  final def lift[A](fa: ZIO[R, E, A]): ZIO[R, E, A]                                        = fa
  final def tryHandleWith[A](fa: ZIO[R, E, A])(f: E => Option[ZIO[R, E, A]]): ZIO[R, E, A] =
    fa.catchSome(CachedMatcher(f))
  final override def handleWith[A](fa: ZIO[R, E, A])(f: E => ZIO[R, E, A]): ZIO[R, E, A]   = fa.catchAll(f)

  final def start[A](fa: ZIO[R, E, A]): ZIO[R, E, Fiber[ZIO[R, E, _], E, A]] =
    fa.interruptible.forkDaemon.map(convertFiber)

  final def racePair[A, B](fa: ZIO[R, E, A], fb: ZIO[R, E, B]): ZIO[R, E, Either[
    (Outcome[ZIO[R, E, _], E, A], Fiber[ZIO[R, E, _], E, B]),
    (Fiber[ZIO[R, E, _], E, A], Outcome[ZIO[R, E, _], E, B])
  ]] =
    (fa raceWith fb)(
      { case (l, f) =>
        l.foldZIO(
          e => f.interrupt *> ZIO.fail(e),
          r => ZIO.succeed(Outcome.Succeeded[ZIO[R, E, _], E, A](ZIO.succeed(r)))
        ).map(lv => Left((lv, convertFiber(f))))
      },
      { case (r, f) =>
        r.foldZIO(
          e => f.interrupt *> ZIO.fail(e),
          r => ZIO.succeed(Outcome.Succeeded[ZIO[R, E, _], E, B](ZIO.succeed(r)))
        ).map(rv => Right((convertFiber(f), rv)))
      }
    )

  final def race[A, B](fa: ZIO[R, E, A], fb: ZIO[R, E, B]): ZIO[R, E, Either[A, B]] = fa.raceEither(fb)
  final def never[A]: ZIO[R, E, A]                                                  = ZIO.never
  final def fireAndForget[A](fa: ZIO[R, E, A]): ZIO[R, E, Unit]                     = fa.forkDaemon.unit

  final def bracket[A, B, C](
      init: ZIO[R, E, A]
  )(action: A => ZIO[R, E, B])(release: (A, Boolean) => ZIO[R, E, C]): ZIO[R, E, B] =
    ZIO.acquireReleaseExitWith(init).apply[R, E, B]((a, e) => release(a, e.isSuccess).ignore)(action)

  final def finallyCase[A, B, C](
      init: ZIO[R, E, A]
  )(action: A => ZIO[R, E, B])(release: (A, Exit[E, B]) => ZIO[R, E, C]): ZIO[R, E, B] =
    ZIO.acquireReleaseExitWith(init).apply[R, E, B]((a, e) => release(a, e).ignore)(action)

  // Execute
  def runScoped[A](fa: ZIO[R, E, A]): ZIO[R, E, A] = fa

  def executionContext: ZIO[R, E, ExecutionContext] = ZIO.executor.map(_.asExecutionContext)

  def deferFutureAction[A](f: ExecutionContext => Future[A]): ZIO[R, E, A] = ZIO.fromFuture(f).orDie

  def delay[A](a: => A): ZIO[R, E, A] = ZIO.succeed(a)

}

class RioTofuInstance[R] extends ZioTofuInstance[R, Throwable] {
  override def deferFutureAction[A](f: ExecutionContext => Future[A]): ZIO[R, Throwable, A] = ZIO.fromFuture(f)

  override def delay[A](a: => A): ZIO[R, Throwable, A] = ZIO.attempt(a)
}

class ZioTofuWithRunInstance[R: Tag, E] extends WithRun[ZIO[R, E, _], ZIO[Any, E, _], R] {
  val context: ZIO[R, E, R]                                           = ZIO.service[R]
  val functor: Functor[ZIO[R, E, _]]                                  = new Functor[ZIO[R, E, _]] {
    def map[A, B](fa: ZIO[R, E, A])(f: A => B): ZIO[R, E, B] = fa.map(f)
  }
  final def runContext[A](fa: ZIO[R, E, A])(ctx: R): ZIO[Any, E, A]   = fa.provideEnvironment(ZEnvironment(ctx))
  final def local[A](fa: ZIO[R, E, A])(project: R => R): ZIO[R, E, A] =
    fa.provideSomeEnvironment(env => env.update(project))
  final override def ask[A](f: R => A): ZIO[R, E, A]                  = ZIO.serviceWith(f)
  final def lift[A](fa: ZIO[Any, E, A]): ZIO[R, E, A]                 = fa
}

class ZioTofuTimeoutInstance[R, E] extends Timeout[ZIO[R, E, _]] {
  final def timeoutTo[A](fa: ZIO[R, E, A], after: FiniteDuration, fallback: ZIO[R, E, A]): ZIO[R, E, A] =
    fa.timeoutTo(fallback)(ZIO.succeed(_))(zio.Duration.fromScala(after)).flatten
}

class ZioTofuRandomInstance[R, E] extends GenRandom[ZIO[R, E, _]] {
  override def nextLong: ZIO[R, E, Long]       = Random.nextLong
  override def nextInt(n: Int): ZIO[R, E, Int] = Random.nextIntBounded(n)
}

class ZioTofuConsoleInstance[R, E >: IOException] extends tofu.common.Console[ZIO[R, E, _]] {
  override def readStrLn: ZIO[R, E, String]           = Console.readLine
  override def putStr(s: String): ZIO[R, E, Unit]     = Console.print(s)
  override def putStrLn(s: String): ZIO[R, E, Unit]   = Console.printLine(s)
  override def putErr(err: String): ZIO[R, E, Unit]   = Console.printError(err)
  override def putErrLn(err: String): ZIO[R, E, Unit] = Console.printLineError(err)
}

object ZioTofuInstance {
  def convertFiber[R, E, A](f: ZFiber[E, A]): Fiber[ZIO[R, E, _], E, A] = new Fiber[ZIO[R, E, _], E, A] {
    override def cancel: ZIO[R, E, Unit]                      = f.interrupt.unit
    override def join: ZIO[R, E, Outcome[ZIO[R, E, _], E, A]] =
      f.join.fold(e => Outcome.Errored(e), r => Outcome.Succeeded(ZIO.succeed(r)))
  }
}

class ZioTofuErrorsToInstance[R, E, E1](implicit lens: Extract[E1, E])
    extends ErrorsTo[ZIO[R, E, _], ZIO[R, E1, _], E] {
  final def handleWith[A](fa: ZIO[R, E, A])(f: E => ZIO[R, E1, A]): ZIO[R, E1, A] = fa.catchAll(f)
  final def restore[A](fa: ZIO[R, E, A]): ZIO[R, E1, Option[A]]                   = fa.option
  final def raise[A](err: E): ZIO[R, E, A]                                        = ZIO.fail(err)
  final def lift[A](fa: ZIO[R, E1, A]): ZIO[R, E, A]                              = fa.mapError(lens.extract)

  final override def handle[A](fa: ZIO[R, E, A])(f: E => A)(implicit G: Applicative[ZIO[R, E1, _]]): ZIO[R, E1, A] =
    fa.catchAll(e => ZIO.succeed(f(e)))

  final override def attempt[A](
      fa: ZIO[R, E, A]
  )(implicit F: Functor[ZIO[R, E, _]], G: Applicative[ZIO[R, E1, _]]): ZIO[R, E1, Either[E, A]] =
    fa.either
}

class ZioTofuContainsUnliftInstance[R1: Tag, R2: Tag, E](implicit lens: Contains[R2, R1])
    extends Unlift[ZIO[R1, E, _], ZIO[R2, E, _]] {
  def lift[A](fa: ZIO[R1, E, A]): ZIO[R2, E, A]          =
    fa.provideSomeEnvironment[R2](r2env => ZEnvironment(lens.extract(r2env.get)))
  def unlift: ZIO[R2, E, ZIO[R2, E, _] ~> ZIO[R1, E, _]] =
    ZIO.serviceWith[R2](r2 => funK[ZIO[R2, E, _], ZIO[R1, E, _]](_.provideSomeEnvironment[R1](r1env => ZEnvironment(lens.set(r2, r1env.get)))))
}

class RioTofuUnliftIOInstance[R] extends UnliftIO[RIO[R, _]] {
  import cats.effect.unsafe.implicits._
  import zio.interop._
  import zio.interop.catz._

  def lift[A](fa: CIO[A]): RIO[R, A]   = implicitly[LiftIO[RIO[R, _]]].liftIO(fa)
  def unlift: RIO[R, RIO[R, _] ~> CIO] =
    ZIO.runtime[R].map(implicit runtime => funK[RIO[R, _], CIO](rio => rio.toEffect[CIO]))
}

class RioTofuUnsafeExecFutureInstance[R] extends UnsafeExecFuture[RIO[R, _]] {
  def lift[A](fa: Future[A]): RIO[R, A]   = ZIO.fromFuture(_ => fa)
  def unlift: RIO[R, RIO[R, _] ~> Future] =
    ZIO.runtime[R].map(r => Unsafe.unsafe(implicit unsafe => funK[RIO[R, _], Future](r.unsafe.runToFuture(_))))
}

class ZioTofuUnliftManyInstance[R, E, R1: Tag] extends WithRun[ZIO[R with R1, E, _], ZIO[R, E, _], R1] {
  override def functor: Functor[ZIO[R with R1, E, _]] = zio.interop.catz.monadErrorInstance

  override def runContext[A](fa: ZIO[R with R1, E, A])(ctx: R1): ZIO[R, E, A] =
    fa.provideSomeEnvironment[R](_.add[R1](ctx))

  override def context: ZIO[R with R1, E, R1] = ZIO.environmentWith[R with R1](_.get[R1])

  override def local[A](fa: ZIO[R with R1, E, A])(project: R1 => R1): ZIO[R with R1, E, A] =
    fa.provideSomeEnvironment(_.update(project))

  override def lift[A](fa: ZIO[R, E, A]): ZIO[R with R1, E, A] = fa
}

class ZioTofuBlockingInstance[R, E] extends BlockExec[ZIO[R, E, _]] {
  def runScoped[A](fa: ZIO[R, E, A]): ZIO[R, E, A] = blocking(fa)

  def executionContext: ZIO[R, E, ExecutionContext] = blockingExecutor.map(_.asExecutionContext)

  def deferFutureAction[A](f: ExecutionContext => Future[A]): ZIO[R, E, A] =
    blocking(ZIO.fromFuture(f).orDie)
}

class RioTofuBlockingInstance[R] extends ZioTofuBlockingInstance[R, Throwable] {
  override def deferFutureAction[A](f: ExecutionContext => Future[A]): RIO[R, A] =
    blocking(ZIO.fromFuture(f))
}
