package tofu.env

import java.util.concurrent.TimeUnit.MILLISECONDS

import cats.Eval
import cats.data.ReaderT
import cats.effect._
import monix.eval.{Coeval, Task}
import monix.execution.Scheduler

import scala.concurrent.duration.FiniteDuration
import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try

private[env] trait EnvFunctions
    extends EnvProducts with EnvTraversing with EnvRacing with EnvSyntax with EnvTransformations {
  self: Env.type =>
  def apply[E, A](f: E => Task[A]): Env[E, A] = EnvCtx(f)
  def later[E, A](x: => A): Env[E, A]         = fromTask(Task.delay(x))
  def pure[E, A](x: A): Env[E, A]             = fromTask(Task.pure(x))

  def context[E]: Env[E, E]                            = Env(ctx => Task.now(ctx))
  def withContextNow[E, A](f: E => A): Env[E, A]       = Env(ctx => Task.now(f(ctx)))
  def withContext[E, A](f: E => Env[E, A]): Env[E, A]  = Env(ctx => f(ctx).run(ctx))
  def withContextFork[E, A](f: E => A): Env[E, A]      = Env(ctx => Task(f(ctx)))
  def withContextDelay[E, A](f: E => A): Env[E, A]     = Env(ctx => Task.delay(f(ctx)))
  private[this] val anyUnit: Env[Any, Unit]            = fromTask(Task.unit)
  def unit[E]: Env[E, Unit]                            = anyUnit.asInstanceOf[Env[E, Unit]]
  def scheduler[E]: Env[E, Scheduler]                  =
    fromTask(Task.deferAction(scheduler => Task.pure(scheduler)))
  def millis[E]: Env[E, Long]                          =
    fromTask(Task.deferAction(scheduler => Task.pure(scheduler.clockRealTime(MILLISECONDS))))
  def shift[E]: Env[E, Unit]                           = fromTask(Task.shift)
  def shift[E](ec: ExecutionContext): Env[E, Unit]     = fromTask(Task.shift(ec))
  def sleep[E](duration: FiniteDuration): Env[E, Unit] =
    fromTask(Task.sleep(duration))

  def fromTask[E, A](task: Task[A]): Env[E, A]                           = EnvTask(task)
  def delay[E, A](x: => A): Env[E, A]                                    = later(x)
  def defer[E, A](x: => Env[E, A]): Env[E, A]                            =
    Env(ctx => Task.defer(x.run(ctx)))
  def deferTask[E, A](x: => Task[A]): Env[E, A]                          = fromTask(Task.defer(x))
  def deferFuture[E, A](future: => Future[A]): Env[E, A]                 =
    EnvTask(Task.deferFuture(future))
  def deferFutureContext[E, A](future: E => Future[A]): Env[E, A]        =
    Env(ctx => Task.deferFuture(future(ctx)))
  def deferFutureAction[E, A](futAct: Scheduler => Future[A]): Env[E, A] =
    EnvTask(Task.deferFutureAction(futAct))
  def raiseError[E, A](throwable: Throwable): Env[E, A]                  =
    EnvTask(Task.raiseError(throwable))
  def attempt[E, A](e: Env[E, A]): Env[E, Either[Throwable, A]]          = e.attempt

  def fromFunc[E, A](f: E => A): Env[E, A]                          = Env.withContextNow(f)
  def fromReaderT[E, A](reader: ReaderT[Task, E, A]): Env[E, A]     =
    Env(reader.run)
  def fromTry[E, A](t: Try[A]): Env[E, A]                           = fromTask(Task.fromTry(t))
  def fromEither[E, A](e: Either[Throwable, A]): Env[E, A]          = fromTask(Task.fromTry(e.toTry))
  def fromFuture[E, A](future: Future[A]): Env[E, A]                = EnvTask(Task.fromFuture(future))
  def fromTryFunc[E, A](ft: E => Try[A]): Env[E, A]                 =
    Env(ctx => Task.fromTry(ft(ctx)))
  def fromEffect[F[_]: Effect, E, A](fa: F[A]): Env[E, A]           =
    fromTask(Task.fromEffect(fa))
  def fromEffectFunc[F[_]: Effect, E, A](ffa: E => F[A]): Env[E, A] =
    Env(ctx => Task.fromEffect(ffa(ctx)))
  def fromEval[E, A](ea: Eval[A]): Env[E, A]                        = fromTask(Task.from(ea))
  def fromEvalFunc[E, A](fea: E => Eval[A]): Env[E, A]              =
    Env(ctx => Task.from(fea(ctx)))
  def fromIO[E, A](ioa: IO[A]): Env[E, A]                           = fromTask(Task.from(ioa))
  def fromIOFunc[E, A](fioa: E => IO[A]): Env[E, A]                 =
    Env(ctx => Task.from(fioa(ctx)))
  def fromCoeval[E, A](ca: Coeval[A]): Env[E, A]                    = fromTask(ca.to[Task])
  def fromCoevalFunc[E, A](fca: E => Coeval[A]): Env[E, A]          =
    Env(ctx => fca(ctx).to[Task])

  def tailRecM[E, A, B](a: A)(f: (A) => Env[E, Either[A, B]]): Env[E, B] =
    Env(ctx => Task.tailRecM(a)(a1 => f(a1).run(ctx)))

  def when[E](cond: => Boolean)(io: => Env[E, _]): Env[E, Unit] =
    whenF(delay[E, Boolean](cond))(io)

  def whenF[E](cond: Env[E, Boolean])(io: => Env[E, _]): Env[E, Unit] =
    cond.flatMap(run => if (run) io.flatMap(_ => Env.unit[E]) else Env.unit[E])
}
