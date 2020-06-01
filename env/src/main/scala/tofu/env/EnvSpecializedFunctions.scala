package tofu.env

import cats.Eval
import cats.data.ReaderT
import cats.effect._
import monix.eval.{Coeval, Task}
import monix.execution.Scheduler
import monix.execution.compat.BuildFrom

import scala.concurrent.duration.FiniteDuration
import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try
import internal.CollectionMapper

trait EnvSpecializedFunctions[E] {
  type F[A] = Env[E, A]

  def apply[A](f: E => Task[A]): F[A]                            = Env(f)
  def later[A](x: => A): F[A]                                    = Env.later(x)
  def pure[A](x: A): F[A]                                        = Env.pure(x)
  val context: F[E]                                              = Env.context
  def withContextNow[A](f: E => A): F[A]                         = Env.withContextNow(f)
  def withContextFork[A](f: E => A): F[A]                        = Env.withContextFork(f)
  def withContextDelay[A](f: E => A): F[A]                       = Env.withContextDelay(f)
  val unit: F[Unit]                                              = Env.unit
  val scheduler: F[Scheduler]                                    = Env.scheduler
  val millis: F[Long]                                            = Env.millis
  val shift: F[Unit]                                             = Env.shift
  def shift(ec: ExecutionContext): F[Unit]                       = Env.shift(ec)
  def sleep(duration: FiniteDuration): F[Unit]                   = Env.sleep(duration)
  def marMap2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] = Env.parMap2(fa, fb)(f)
  def fromTask[A](task: Task[A]): F[A]                           = Env.fromTask(task)
  def delay[A](x: => A): F[A]                                    = Env.delay(x)
  def defer[A](x: => F[A]): F[A]                                 = Env.defer(x)
  def deferTask[A](x: => Task[A]): F[A]                          = Env.deferTask(x)
  def deferFuture[A](future: => Future[A]): F[A]                 = Env.deferFuture(future)
  def deferFutureContext[A](future: E => Future[A]): F[A]        = Env.deferFutureContext(future)
  def deferFutureAction[A](futAct: Scheduler => Future[A]): F[A] = Env.deferFutureAction(futAct)
  def raiseError[A](throwable: Throwable): F[A]                  = Env.raiseError(throwable)
  def attempt[A](f: F[A]): F[Either[Throwable, A]]               = f.attempt
  def fromReaderT[A](reader: ReaderT[Task, E, A]): F[A]          = Env.fromReaderT(reader)
  def fromFuture[A](future: Future[A]): F[A]                     = Env.fromFuture(future)
  def fromFunc[A](f: E => A): F[A]                               = Env.fromFunc(f)
  def fromTry[A](t: Try[A]): F[A]                                = Env.fromTry(t)
  def fromEither[A](e: Either[Throwable, A]): F[A]               = Env.fromEither(e)
  def fromTryFunc[A](ft: E => Try[A]): F[A]                      = Env.fromTryFunc(ft)
  def fromEffect[G[_]: Effect, A](fa: G[A]): F[A]                = Env.fromEffect(fa)
  def fromEffectFunc[G[_]: Effect, A](ffa: E => G[A]): F[A]      = Env.fromEffectFunc(ffa)
  def fromEval[A](ea: Eval[A]): F[A]                             = Env.fromEval(ea)
  def fromEvalFunc[A](fea: E => Eval[A]): F[A]                   = Env.fromEvalFunc(fea)
  def fromIO[A](ioa: IO[A]): F[A]                                = Env.fromIO(ioa)
  def fromIOFunc[A](fioa: E => IO[A]): F[A]                      = Env.fromIOFunc(fioa)
  def fromCoeval[A](ca: Coeval[A]): F[A]                         = Env.fromCoeval(ca)
  def fromCoevalFunc[A](fca: E => Coeval[A]): F[A]               = Env.fromCoevalFunc(fca)
  def tailRecM[A, B](a: A)(f: A => F[Either[A, B]]): F[B]        = Env.tailRecM(a)(f)
  def race[A, B](ta: F[A], tb: F[B]): F[Either[A, B]]            = Env.race(ta, tb)
  def raceMany[A](tta: Iterable[F[A]]): F[A]                     = Env.raceMany(tta)

  def racePair[A, B](ta: F[A], tb: F[B]): F[Either[(A, Fiber[F, B]), (Fiber[F, A], B)]]                            =
    Env.racePair(ta, tb)
  def sequence[A, M[X] <: Iterable[X]](in: M[F[A]])(implicit cbf: BuildFrom[M[F[A]], A, M[A]]): F[M[A]]            =
    Env.sequence(in)
  def traverse[A, B, M[X] <: Iterable[X]](in: M[A])(f: A => F[B])(implicit cbf: BuildFrom[M[A], B, M[B]]): F[M[B]] =
    Env.traverse(in)(f)
  def gather[A, M[X] <: Iterable[X]](in: M[F[A]])(implicit cbf: BuildFrom[M[F[A]], A, M[A]]): F[M[A]]              =
    Env.gather(in)
  def wander[A, B, M[X] <: Iterable[X]](in: M[A])(f: A => F[B])(implicit cbf: BuildFrom[M[A], B, M[B]]): F[M[B]]   =
    Env.wander(in)(f)
  def gatherUnordered[A](in: Iterable[F[A]]): F[List[A]]                                                           = Env.gatherUnordered(in)
  def wanderUnordered[A, B, M[X] <: Iterable[X]](in: M[A])(f: A => F[B]): F[List[B]]                               =
    Env.wanderUnordered(in)(f)

  /** Mirrored traversing operations, trying to create context-unaware Tasks whenever possible */
  object opt {
    def sequence[A, M[+X] <: Iterable[X]](
        in: M[Env[E, A]]
    )(implicit cbf: BuildFrom[M[Env[E, A]], A, M[A]]): Env[E, M[A]] =
      Env.opt.sequence(in)

    def traverse[A, B, M[+X] <: Iterable[X]](in: M[A])(
        f: A => Env[E, B]
    )(implicit mapper: CollectionMapper[A, Env[E, B], M], cbf2: BuildFrom[M[Env[E, B]], B, M[B]]): Env[E, M[B]] =
      Env.opt.traverse(in)(f)

    def parSequence[A, M[X] <: Iterable[X]](
        in: M[Env[E, A]]
    )(implicit cbf: BuildFrom[M[Env[E, A]], A, M[A]]): Env[E, M[A]] =
      Env.opt.parSequence(in)

    def parTraverse[A, B, M[X] <: Iterable[X]](in: M[A])(
        f: A => Env[E, B]
    )(implicit mapper: CollectionMapper[A, Env[E, B], M], cbf2: BuildFrom[M[Env[E, B]], B, M[B]]): Env[E, M[B]] =
      Env.opt.parTraverse(in)(f)

    def parSequenceUnordered[A](in: Iterable[Env[E, A]]): Env[E, List[A]] =
      Env.opt.parSequenceUnordered(in)

    def parTraverseUnordered[A, B, M[X] <: Iterable[X]](in: M[A])(f: A => Env[E, B]): Env[E, List[B]] =
      Env.opt.parTraverseUnordered(in)(f)

    @deprecated("use parSequence", since = "0.7.6")
    def gather[A, M[X] <: Iterable[X]](in: M[Env[E, A]])(implicit cbf: BuildFrom[M[Env[E, A]], A, M[A]]): Env[E, M[A]] =
      parSequence(in)

    @deprecated("use parTraverse", since = "0.7.6")
    def wander[A, B, M[X] <: Iterable[X]](in: M[A])(
        f: A => Env[E, B]
    )(implicit mapper: CollectionMapper[A, Env[E, B], M], cbf2: BuildFrom[M[Env[E, B]], B, M[B]]): Env[E, M[B]] =
      parTraverse(in)(f)

    @deprecated("use parSequenceUnordered", since = "0.7.6")
    def gatherUnordered[A](in: Iterable[Env[E, A]]): Env[E, List[A]] =
      parSequenceUnordered(in)

    @deprecated("use parTraverseUnordered", since = "0.7.6")
    def wanderUnordered[A, B, M[X] <: Iterable[X]](in: M[A])(f: A => Env[E, B]): Env[E, List[B]] =
      parTraverseUnordered(in)(f)
  }

  def parMap2[A1, A2, R](fa1: Env[E, A1], fa2: Env[E, A2])(f: (A1, A2) => R): Env[E, R] = Env.parMap2(fa1, fa2)(f)

  def parMap3[A1, A2, A3, R](fa1: Env[E, A1], fa2: Env[E, A2], fa3: Env[E, A3])(f: (A1, A2, A3) => R): Env[E, R] =
    Env.parMap3(fa1, fa2, fa3)(f)

  def parMap4[A1, A2, A3, A4, R](fa1: Env[E, A1], fa2: Env[E, A2], fa3: Env[E, A3], fa4: Env[E, A4])(
      f: (A1, A2, A3, A4) => R
  ): Env[E, R] = Env.parMap4(fa1, fa2, fa3, fa4)(f)

  def parMap5[A1, A2, A3, A4, A5, R](
      fa1: Env[E, A1],
      fa2: Env[E, A2],
      fa3: Env[E, A3],
      fa4: Env[E, A4],
      fa5: Env[E, A5]
  )(f: (A1, A2, A3, A4, A5) => R): Env[E, R] =
    Env.parMap5(fa1, fa2, fa3, fa4, fa5)(f)

  def parMap6[A1, A2, A3, A4, A5, A6, R](
      fa1: Env[E, A1],
      fa2: Env[E, A2],
      fa3: Env[E, A3],
      fa4: Env[E, A4],
      fa5: Env[E, A5],
      fa6: Env[E, A6]
  )(f: (A1, A2, A3, A4, A5, A6) => R): Env[E, R] =
    Env.parMap6(fa1, fa2, fa3, fa4, fa5, fa6)(f)

  def map2[A1, A2, R](fa1: Env[E, A1], fa2: Env[E, A2])(f: (A1, A2) => R): Env[E, R] =
    Env.map2(fa1, fa2)(f)

  def map3[A1, A2, A3, R](fa1: Env[E, A1], fa2: Env[E, A2], fa3: Env[E, A3])(f: (A1, A2, A3) => R): Env[E, R] =
    Env.map3(fa1, fa2, fa3)(f)

  def map4[A1, A2, A3, A4, R](fa1: Env[E, A1], fa2: Env[E, A2], fa3: Env[E, A3], fa4: Env[E, A4])(
      f: (A1, A2, A3, A4) => R
  ): Env[E, R] =
    Env.map4(fa1, fa2, fa3, fa4)(f)

  def map5[A1, A2, A3, A4, A5, R](fa1: Env[E, A1], fa2: Env[E, A2], fa3: Env[E, A3], fa4: Env[E, A4], fa5: Env[E, A5])(
      f: (A1, A2, A3, A4, A5) => R
  ): Env[E, R] =
    Env.map5(fa1, fa2, fa3, fa4, fa5)(f)

  def map6[A1, A2, A3, A4, A5, A6, R](
      fa1: Env[E, A1],
      fa2: Env[E, A2],
      fa3: Env[E, A3],
      fa4: Env[E, A4],
      fa5: Env[E, A5],
      fa6: Env[E, A6]
  )(f: (A1, A2, A3, A4, A5, A6) => R): Env[E, R] =
    Env.map6(fa1, fa2, fa3, fa4, fa5, fa6)(f)

  def parZip2[A1, A2](fa1: Env[E, A1], fa2: Env[E, A2]): Env[E, (A1, A2)] =
    Env.parZip2(fa1, fa2)

  def parZip3[A1, A2, A3](fa1: Env[E, A1], fa2: Env[E, A2], fa3: Env[E, A3]): Env[E, (A1, A2, A3)] =
    Env.parZip3(fa1, fa2, fa3)

  def parZip4[A1, A2, A3, A4](
      fa1: Env[E, A1],
      fa2: Env[E, A2],
      fa3: Env[E, A3],
      fa4: Env[E, A4]
  ): Env[E, (A1, A2, A3, A4)] =
    Env.parZip4(fa1, fa2, fa3, fa4)

  def parZip5[A1, A2, A3, A4, A5](
      fa1: Env[E, A1],
      fa2: Env[E, A2],
      fa3: Env[E, A3],
      fa4: Env[E, A4],
      fa5: Env[E, A5]
  ): Env[E, (A1, A2, A3, A4, A5)] =
    Env.parZip5(fa1, fa2, fa3, fa4, fa5)

  def parZip6[A1, A2, A3, A4, A5, A6](
      fa1: Env[E, A1],
      fa2: Env[E, A2],
      fa3: Env[E, A3],
      fa4: Env[E, A4],
      fa5: Env[E, A5],
      fa6: Env[E, A6]
  ): Env[E, (A1, A2, A3, A4, A5, A6)] =
    Env.parZip6(fa1, fa2, fa3, fa4, fa5, fa6)

  object conversions {
    implicit def taskAsEnv[A](task: Task[A]): Env[E, A]          = fromTask(task)
    implicit def futureAsEnv[A](future: => Future[A]): Env[E, A] =
      deferFuture(future)
  }

  object converters {
    implicit class TaskAsEnv[A](private val task: Task[A]) {
      def env: Env[E, A] = fromTask(task)
    }

    implicit class FutureAsEnv[A](future: => Future[A]) {
      def env: Env[E, A] = deferFuture(future)
    }
  }
}
