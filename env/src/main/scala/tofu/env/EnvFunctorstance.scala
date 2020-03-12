package tofu
package env

import cats.effect._
import cats.{Functor, Monad}
import monix.eval.Task
import monix.execution.Scheduler
import tofu.memo.Memoize

import scala.concurrent.duration.{FiniteDuration, TimeUnit}
import scala.concurrent.{ExecutionContext, Future}

private[env] class EnvFunctorstance[E]
    extends Concurrent[Env[E, *]] with Timer[Env[E, *]] with WithRun[Env[E, *], Task, E] with Execute[Env[E, *]]
    with Memoize[Env[E, *]] with ContextShift[Env[E, *]] with Timeout[Env[E, *]] with Race[Env[E, *]] {
  import Env._

  override def functor: Functor[Env[E, *]] = this
  //Functor
  override def map[A, B](fa: Env[E, A])(f: A => B): Env[E, B] = fa.map(f)

  //Applicative
  def pure[A](x: A): Env[E, A]                                                        = Env.pure(x)
  override def unit: Env[E, Unit]                                                     = Env.unit
  override def ap[A, B](ff: Env[E, A => B])(fa: Env[E, A]): Env[E, B]                 = Env.map2(ff, fa)(_(_))
  override def map2[A, B, Z](fa: Env[E, A], fb: Env[E, B])(f: (A, B) => Z): Env[E, Z] = fa.map2(fb)(f)
  override def product[A, B](fa: Env[E, A], fb: Env[E, B]): Env[E, (A, B)]            = fa.zip(fb)

  //Monad
  override def flatMap[A, B](fa: Env[E, A])(f: A => Env[E, B]): Env[E, B]    = fa.flatMap(f)
  override def tailRecM[A, B](a: A)(f: A => Env[E, Either[A, B]]): Env[E, B] = Env.tailRecM(a)(f)

  //MonadError
  override def raiseError[A](e: Throwable): Env[E, A] = Env.raiseError[E, A](e)
  def handleErrorWith[A](fa: Env[E, A])(f: Throwable => Env[E, A]): Env[E, A] =
    fa.onErrorRecoverWith { case e => f(e) }
  override def ensure[A](fa: Env[E, A])(error: => Throwable)(predicate: A => Boolean): Env[E, A] =
    fa.ensure(error)(predicate)
  override def ensureOr[A](fa: Env[E, A])(error: A => Throwable)(predicate: A => Boolean): Env[E, A] =
    fa.ensureOr(error)(predicate)
  override def adaptError[A](fa: Env[E, A])(pf: PartialFunction[Throwable, Throwable]): Env[E, A] =
    fa.mapTask(_.onErrorRecoverWith { case e if pf.isDefinedAt(e) => Task.raiseError(pf(e)) })

  override def rethrow[A, EE <: Throwable](fa: Env[E, Either[EE, A]]): Env[E, A] =
    fa.mapTask(_.flatMap(Task.fromEither(_)))

  //Bracket
  override def bracketCase[A, B](
      acquire: Env[E, A]
  )(use: A => Env[E, B])(release: (A, ExitCase[Throwable]) => Env[E, Unit]): Env[E, B] =
    acquire.bracketСase(use)(release)
  override def bracket[A, B](acquire: Env[E, A])(use: A => Env[E, B])(release: A => Env[E, Unit]): Env[E, B] =
    acquire.bracket(use)(release)

  //Sync
  override def suspend[A](thunk: => Env[E, A]): Env[E, A] = Env.defer(thunk)
  override def delay[A](thunk: => A): Env[E, A]           = Env.delay(thunk)

  //Async
  override def shift: Env[E, Unit]                                       = fromTask(Task.shift)
  override def evalOn[A](ec: ExecutionContext)(fa: Env[E, A]): Env[E, A] = fa.mapTask(_.executeOn(Scheduler(ec)))
  override def liftIO[A](ioa: IO[A]): Env[E, A]                          = fromTask(Task.from(ioa))
  override def asyncF[A](k: (Either[Throwable, A] => Unit) => Env[E, Unit]): Env[E, A] =
    Env(ctx => Task.asyncF(cb => k(cb).run(ctx)))
  override def async[A](k: (Either[Throwable, A] => Unit) => Unit): Env[E, A] =
    fromTask(Task.async(k))

  //Concurrent
  override def cancelable[A](k: (Either[Throwable, A] => Unit) => CancelToken[Env[E, *]]): Env[E, A] =
    apply(e => Task.cancelable(cb => k(cb).run(e)))
  override def uncancelable[A](fa: Env[E, A]): Env[E, A] = fa.uncancelable

  override def start[A](fa: Env[E, A]): Env[E, Fiber[Env[E, *], A]]           = fa.start
  override def race[A, B](fa: Env[E, A], fb: Env[E, B]): Env[E, Either[A, B]] = Env.race(fa, fb)
  override def racePair[A, B](
      fa: Env[E, A],
      fb: Env[E, B]
  ): Env[E, Either[(A, Fiber[Env[E, *], B]), (Fiber[Env[E, *], A], B)]] =
    Env.racePair(fa, fb)

  //Timer
  override object clock extends Clock[Env[E, *]] {
    override def realTime(unit: TimeUnit): Env[E, Long]  = scheduler.map(_.clockRealTime(unit))
    override def monotonic(unit: TimeUnit): Env[E, Long] = scheduler.map(_.clockMonotonic(unit))
  }
  override def sleep(duration: FiniteDuration): Env[E, Unit] = Env.sleep(duration)

  //Context
  override val context: Env[E, E]                                                  = Env.fromFunc(identity)
  override def ask[A](f: E => A): Env[E, A]                                        = Env.fromFunc(f)
  override def local[A](fa: Env[E, A])(project: E => E): Env[E, A]                 = fa.local(project)
  override def askF[A](f: E => Env[E, A])(implicit F: Monad[Env[E, *]]): Env[E, A] = Env.withContext(f)

  //RunContext
  override def runContext[A](fa: Env[E, A])(ctx: E): Task[A] = fa.run(ctx)

  //Memoize
  override def memoize[A](fa: Env[E, A]): Env[E, Env[E, A]]          = fa.memo
  override def memoizeOnSuccess[A](fa: Env[E, A]): Env[E, Env[E, A]] = fa.memoSuccess

  //Execute
  override def executionContext: Env[E, ExecutionContext]                        = Env.deferTask(Task.deferAction(Task.pure))
  override def deferFutureAction[A](f: ExecutionContext => Future[A]): Env[E, A] = Env.deferFutureAction(f)
  override def deferFuture[A](f: => Future[A]): Env[E, A]                        = Env.deferFuture(f)

  //Timeout
  override def timeoutTo[A](fa: Env[E, A], after: FiniteDuration, fallback: Env[E, A]): Env[E, A] =
    fa.mapTask2(fallback)(_.timeoutTo(after, _))

  //Race
  def fireAndForget[A](fa: Env[E, A]): Env[E, Unit] = fa.startAndForget
}
