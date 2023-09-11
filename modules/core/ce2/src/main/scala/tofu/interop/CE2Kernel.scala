package tofu
package interop

import java.util.concurrent.TimeUnit
import scala.concurrent.duration.FiniteDuration
import scala.concurrent.{ExecutionContext, Future}
import cats.effect.concurrent.{MVar, Ref}
import cats.effect.{
  Async,
  Blocker,
  Bracket,
  Concurrent,
  ConcurrentEffect,
  ContextShift,
  Effect,
  ExitCase,
  Fiber,
  IO,
  Sync,
  Timer
}
import cats.{Functor, Id, Monad, Parallel, Traverse, ~>}
import tofu.compat.unused212
import tofu.concurrent._
import tofu.internal.NonTofu
import tofu.internal.carriers._
import tofu.lift.Lift
import tofu.syntax.monadic._
import tofu.internal.instances.PerformViaUnlift

object CE2Kernel {
  // 2.12 sometimes gets mad on Const partial alias during implicit search
  type CEExit[E, A] >: ExitCase[E] <: ExitCase[E]

  def delayViaSync[K[_]](implicit KS: Sync[K]): DelayCarrier2[K] =
    new DelayCarrier2[K] {
      def delay[A](a: => A): K[A] = KS.delay(a)
    }

  def unliftEffect[K[_]](implicit KE: Effect[K]): UnliftCarrier2[IO, K] =
    new UnliftCarrier2[IO, K] {
      def lift[A](fa: IO[A]): K[A] = Effect[K].liftIO(fa)
      def unlift: K[K ~> IO]       = Effect.toIOK[K].pure[K]
    }

  def timeout[F[_]: Concurrent: Timer](implicit @unused212 nonTofu: NonTofu[F]): TimeoutCE2Carrier[F] =
    new TimeoutCE2Carrier[F] {
      override def timeoutTo[A](fa: F[A], after: FiniteDuration, fallback: F[A]): F[A] =
        Concurrent.timeoutTo(fa, after, fallback)
    }

  final implicit def finallyFromBracket[F[_], E](implicit
      F: Bracket[F, E]
  ): FinallyCarrier2.Aux[F, E, CEExit[E, *]] =
    new FinallyCarrier2.Impl[F, E, CEExit[E, *]] {
      def finallyCase[A, B, C](init: F[A])(action: A => F[B])(release: (A, CEExit[E, B]) => F[C]): F[B] =
        F.bracketCase(init)(action) { case (a, exit) =>
          F.void(release(a, exit))
        }
      def bracket[A, B, C](init: F[A])(action: A => F[B])(release: (A, Boolean) => F[C]): F[B]          =
        F.bracketCase(init)(action) {
          case (a, ExitCase.Completed) => F.void(release(a, true))
          case (a, _)                  => F.void(release(a, false))
        }
    }

  final def startFromConcurrent[F[_]](implicit
      F: Concurrent[F],
      @unused212 _nonTofu: NonTofu[F]
  ): FibersCarrier2.Aux[F, Id, Fiber[F, *]] =
    new FibersCarrier2.Impl[F, Id, Fiber[F, *]] {
      def start[A](fa: F[A]): F[Fiber[F, A]]                                                = F.start(fa)
      def fireAndForget[A](fa: F[A]): F[Unit]                                               = F.void(start(fa))
      def racePair[A, B](fa: F[A], fb: F[B]): F[Either[(A, Fiber[F, B]), (Fiber[F, A], B)]] = F.racePair(fa, fb)
      def race[A, B](fa: F[A], fb: F[B]): F[Either[A, B]]                                   = F.race(fa, fb)
      def never[A]: F[A]                                                                    = F.never
    }

  final def makeExecute[Tag, F[_]](
      ec: ExecutionContext
  )(implicit cs: ContextShift[F], F: Async[F]): ScopedCarrier2[Tag, F] =
    new ScopedCarrier2[Tag, F] {
      def runScoped[A](fa: F[A]): F[A] = cs.evalOn(ec)(fa)

      def executionContext: F[ExecutionContext] = ec.pure[F]

      def deferFutureAction[A](f: ExecutionContext => Future[A]): F[A] =
        Async.fromFuture(runScoped(F.delay(f(ec))))
    }

  final def asyncExecute[F[_]](implicit
      ec: ExecutionContext,
      cs: ContextShift[F],
      F: Async[F]
  ): ScopedCarrier2[Scoped.Main, F] = makeExecute[Scoped.Main, F](ec)

  final def blockerExecute[F[_]](implicit
      cs: ContextShift[F],
      blocker: Blocker,
      F: Async[F]
  ): ScopedCarrier2[Scoped.Blocking, F] = makeExecute[Scoped.Blocking, F](blocker.blockingContext)

  final def atomBySync[I[_]: Sync, F[_]: Sync]: MkAtomCE2Carrier[I, F] =
    new MkAtomCE2Carrier[I, F] {
      def atom[A](a: A): I[Atom[F, A]] = Ref.in[I, F, A](a).map(AtomByRef(_))
    }

  final def qvarByConcurrent[I[_]: Sync, F[_]: Concurrent]: MkQVarCE2Carrier[I, F] =
    new MkQVarCE2Carrier[I, F] {
      def qvarOf[A](a: A): I[QVar[F, A]] = MVar.in[I, F, A](a).map(QVarByMVar(_))
      def qvarEmpty[A]: I[QVar[F, A]]    = MVar.emptyIn[I, F, A].map(QVarByMVar(_))
    }

  final def clock[F[_]](implicit C: cats.effect.Clock[F]): ClockCE2Carrier[F] =
    new ClockCE2Carrier[F] {
      def realTime(unit: TimeUnit): F[Long] = C.realTime(unit)

      def nanos: F[Long] = C.monotonic(TimeUnit.NANOSECONDS)
    }

  final def sleep[F[_]](implicit T: cats.effect.Timer[F]): SleepCE2Carrier[F] =
    new SleepCE2Carrier[F] {
      def sleep(duration: FiniteDuration): F[Unit] = T.sleep(duration)
    }

  final def performConcurrentEffect[F[_]](implicit
      F: ConcurrentEffect[F],
      @unused212 _nt: NonTofu[F]
  ): PerformCarrier2[F] =
    new ConcurrentEffectPerformer[F]

  final def performContextConcurrentEffect[F[_]](implicit
      F: ContextConcurrentEffect[F],
      @unused212 _nt: NonTofu[F]
  ): PerformCarrier2Context[F] =
    new PerformViaUnlift[F, F.Base, PerformOf.ExitCont[Throwable, *], Unit]()(
      performConcurrentEffect[F.Base](F.concurrentEffect, NonTofu.refute),
      F.unlift,
      F.apply
    ) with PerformCarrier2Context[F]

  final def agentByRefAndSemaphore[I[_]: Monad, F[_]: Fire: Monad](implicit
      makeRef: MakeRef[I, F],
      makeSemaphore: MakeSemaphore[I, F]
  ): MkAgentCE2Carrier[I, F] =
    new MkAgentCE2Carrier[I, F] {
      def agentOf[A](a: A): I[Agent[F, A]] =
        for {
          ref <- makeRef.refOf(a)
          sem <- makeSemaphore.semaphore(1)
        } yield SemRef(ref, sem)
    }

  final def serialAgentByRefAndSemaphore[I[_]: Monad, F[_]: Monad](implicit
      makeRef: MakeRef[I, F],
      makeSemaphore: MakeSemaphore[I, F]
  ): MkSerialAgentCE2Carrier[I, F] =
    new MkSerialAgentCE2Carrier[I, F] {
      override def serialAgentOf[A](a: A): I[SerialAgent[F, A]] =
        for {
          ref <- makeRef.refOf(a)
          sem <- makeSemaphore.semaphore(1)
        } yield SerialSemRef(ref, sem)
    }

  final def underlyingSerialAgentByRefAndSemaphore[I[_]: Sync, F[_]: Functor, G[_]: Monad](implicit
      makeRef: MakeRef[I, F],
      makeSemaphore: MakeSemaphore[I, F],
      lift: Lift[F, G]
  ): MkSerialAgentCE2Carrier[I, G] =
    new MkSerialAgentCE2Carrier[I, G] {
      override def serialAgentOf[A](a: A): I[SerialAgent[G, A]] =
        for {
          ref <- makeRef.refOf(a)
          sem <- makeSemaphore.semaphore(1)
        } yield UnderlyingSemRef[F, G, A](ref, sem)
    }

  def boundedParallel[F[_]: Async: Parallel]: BoundedParallelCarrierCE2[F] = new BoundedParallelCarrierCE2.Impl[F] {
    def parTraverse[T[_]: Traverse, A, B](in: T[A])(f: A => F[B]): F[T[B]] =
      Parallel.parTraverse(in)(f)

    def parTraverseN[T[_]: Traverse, A, B](in: T[A], n: Int)(f: A => F[B]): F[T[B]] =
      Async.parTraverseN(n.toLong)(in)(f)
  }
}
