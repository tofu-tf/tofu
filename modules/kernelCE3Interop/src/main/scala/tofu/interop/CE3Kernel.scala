package tofu.interop

import cats.effect.kernel._
import cats.effect.std.Dispatcher
import cats.effect.unsafe.IORuntime
import cats.effect.{Async, Fiber, IO, Sync}
import cats.{Functor, Monad}
import tofu.concurrent.impl.QVarSM
import tofu.concurrent._
import tofu.internal.NonTofu
import tofu.internal.carriers._
import tofu.lift.Lift
import tofu.syntax.monadic._
import tofu.{Fire, Scoped, WithContext}

import java.util.concurrent.TimeUnit
import scala.annotation.unused
import scala.concurrent.duration.FiniteDuration
import scala.concurrent.{ExecutionContext, Future}

object CE3Kernel {
  def delayViaSync[K[_]](implicit KS: Sync[K]): DelayCarrier3[K] =
    new DelayCarrier3[K] {
      def delay[A](a: => A): K[A] = KS.delay(a)
    }

  def unliftEffect[K[_]](implicit
      KD: WithContext[K, Dispatcher[K]],
      K: Async[K],
      KIO: WithContext[K, IORuntime]
  ): UnliftCarrier3[IO, K] = new UnliftIOImpl[K]

  def timeout[F[_]](implicit @unused nonTofu: NonTofu[F], F: GenTemporal[F, _]): TimeoutCE3Carrier[F] =
    new TimeoutCE3Carrier[F] {
      override def timeoutTo[A](fa: F[A], after: FiniteDuration, fallback: F[A]): F[A] =
        F.timeoutTo(fa, after, fallback)
    }

  final def finallyFromBracket[F[_], E](implicit
      F: MonadCancel[F, E]
  ): FinallyCarrier3.Aux[F, E, Outcome[F, E, *]] =
    new FinallyCarrier3.Impl[F, E, Outcome[F, E, *]] {
      def finallyCase[A, B, C](init: F[A])(action: A => F[B])(release: (A, Outcome[F, E, B]) => F[C]): F[B] =
        F.bracketCase(init)(action) { case (a, exit) =>
          F.void(release(a, exit))
        }

      def bracket[A, B, C](init: F[A])(action: A => F[B])(release: (A, Boolean) => F[C]): F[B] =
        F.bracketCase(init)(action) {
          case (a, Outcome.Succeeded(_)) => F.void(release(a, true))
          case (a, _)                    => F.void(release(a, false))
        }
    }

  final def startFromConcurrent[F[_], E](implicit
      F: GenConcurrent[F, E],
      @unused _nonTofu: NonTofu[F]
  ): FibersCarrier3.Aux[F, E, Outcome[F, E, *], Fiber[F, E, *]] =
    new FibersCarrier3.Impl[F, E, Outcome[F, E, *], Fiber[F, E, *]] {
      def start[A](fa: F[A]): F[Fiber[F, E, A]]                                            = F.start(fa)
      def fireAndForget[A](fa: F[A]): F[Unit]                                              = F.void(start(fa))
      def racePair[A, B](
          fa: F[A],
          fb: F[B]
      ): F[Either[(Outcome[F, E, A], Fiber[F, E, B]), (Fiber[F, E, A], Outcome[F, E, B])]] = F.racePair(fa, fb)
      def race[A, B](fa: F[A], fb: F[B]): F[Either[A, B]]                                  = F.race(fa, fb)
      def never[A]: F[A]                                                                   = F.never
    }

  final def makeExecute[Tag, F[_]](
      ec: ExecutionContext
  )(implicit F: Async[F]): ScopedCarrier3[Tag, F] =
    new ScopedCarrier3[Tag, F] {
      def runScoped[A](fa: F[A]): F[A] = F.evalOn(fa, ec)

      def executionContext: F[ExecutionContext] = F.pure(ec)

      def deferFutureAction[A](f: ExecutionContext => Future[A]): F[A] =
        F.fromFuture(runScoped(F.delay(f(ec))))
    }

  final def asyncExecute[F[_]](implicit
      ec: ExecutionContext,
      F: Async[F]
  ): ScopedCarrier3[Scoped.Main, F] = makeExecute[Scoped.Main, F](ec)

  final def blockerExecute[F[_]](implicit
      blocker: Blocker[F],
      F: Async[F]
  ): ScopedCarrier3[Scoped.Blocking, F] = makeExecute[Scoped.Blocking, F](blocker.ec)

  final def atomBySync[I[_]: Sync, F[_]: Sync]: MkAtomCE3Carrier[I, F] =
    new MkAtomCE3Carrier[I, F] {
      def atom[A](a: A): I[Atom[F, A]] = Ref.in[I, F, A](a).map(AtomByRef(_))
    }

  final def qvarByConcurrent[I[_]: Sync, F[_]: Async]: MkQVarCE3Carrier[I, F] =
    new MkQVarCE3Carrier[I, F] {
      private def qvarOpt[A](opt: Option[A]): I[QVar[F, A]] = for {
        ref <- Ref.in[I, F, QVarSM.State[A, Deferred[F, A]]](QVarSM.fromOption(opt))
      } yield new QVarCE3[F, A](ref)

      def qvarOf[A](a: A): I[QVar[F, A]] = qvarOpt(Some(a))
      def qvarEmpty[A]: I[QVar[F, A]]    = qvarOpt(None)
    }

  final def clock[F[_]: Functor](implicit C: cats.effect.Clock[F]): ClockCE3Carrier[F] =
    new ClockCE3Carrier[F] {
      def realTime(unit: TimeUnit): F[Long] = C.realTime.map(d => TimeUnit.MILLISECONDS.convert(d.toMillis, unit))

      def nanos: F[Long] = C.monotonic.map(_.toNanos)
    }

  final def sleep[F[_]](implicit T: GenTemporal[F, _]): SleepCE3Carrier[F] =
    new SleepCE3Carrier[F] {
      def sleep(duration: FiniteDuration): F[Unit] = T.sleep(duration)
    }

  final def agentByRefAndSemaphore[I[_]: Monad, F[_]: MonadCancelThrow: Fire](implicit
      makeRef: MakeRef[I, F],
      makeSemaphore: MakeSemaphore[I, F]
  ): MakeAgent[I, F] = {
    new MakeAgent[I, F] {
      def agentOf[A](a: A): I[Agent[F, A]] =
        for {
          ref <- makeRef.refOf(a)
          sem <- makeSemaphore.semaphore(1)
        } yield SemRef(ref, sem)
    }
  }

  final def serialAgentByRefAndSemaphore[I[_]: Monad, F[_]: MonadCancelThrow](implicit
      makeRef: MakeRef[I, F],
      makeSemaphore: MakeSemaphore[I, F]
  ): MakeSerialAgent[I, F] =
    new MakeSerialAgent[I, F] {
      override def serialAgentOf[A](a: A): I[SerialAgent[F, A]] =
        for {
          ref <- makeRef.refOf(a)
          sem <- makeSemaphore.semaphore(1)
        } yield SerialSemRef(ref, sem)
    }

  final def underlyingSerialAgentByRefAndSemaphore[I[_]: Monad, F[_]: MonadCancelThrow, G[_]: Monad](implicit
      makeRef: MakeRef[I, F],
      makeSemaphore: MakeSemaphore[I, F],
      lift: Lift[F, G]
  ): MakeSerialAgent[I, G] =
    new MakeSerialAgent[I, G] {
      override def serialAgentOf[A](a: A): I[SerialAgent[G, A]] =
        for {
          ref <- makeRef.refOf(a)
          sem <- makeSemaphore.semaphore(1)
        } yield UnderlyingSemRef[F, G, A](ref, sem)
    }
}
