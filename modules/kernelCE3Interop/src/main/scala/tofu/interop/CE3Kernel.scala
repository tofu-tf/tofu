package tofu.interop

import cats.effect.kernel.{GenTemporal, MonadCancel, Outcome}
import cats.effect.std.Dispatcher
import cats.effect.unsafe.IORuntime
import cats.effect.{Async, IO, Sync}
import tofu.{WithContext}
import tofu.internal.NonTofu
import tofu.internal.carriers.{DelayCarrier3, FinallyCarrier3, TimeoutCE3Carrier, UnliftCarrier3}

import scala.annotation.unused
import scala.concurrent.duration.FiniteDuration
import cats.effect.kernel.GenConcurrent
import tofu.internal.carriers.FibersCarrier3
import cats.effect.Fiber
import scala.concurrent.ExecutionContext
import tofu.ScopedExecute
import scala.concurrent.Future

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
  )(implicit F: Async[F]): ScopedExecute[Tag, F] =
    new ScopedExecute[Tag, F] {
      def runScoped[A](fa: F[A]): F[A] = F.evalOn(fa, ec)

      def executionContext: F[ExecutionContext] = F.pure(ec)

      def deferFutureAction[A](f: ExecutionContext => Future[A]): F[A] =
        F.fromFuture(runScoped(F.delay(f(ec))))
    }

}
