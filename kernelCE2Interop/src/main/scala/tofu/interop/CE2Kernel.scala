package tofu
package interop
import tofu.lift.Unlift

import cats.effect.Sync
import tofu.Delay
import cats.effect.Effect
import cats.effect.IO
import tofu.syntax.monadic._
import cats.~>
import cats.effect.Concurrent
import cats.effect.Timer
import cats.effect.Fiber
import scala.concurrent.duration.FiniteDuration
import tofu.internal.NonTofu
import tofu.compat.unused
import cats.effect.Bracket
import cats.effect.ExitCase
import tofu.internal.carriers._

object CE2Kernel {
  // 2.12 sometimes gets mad on Const partial alias during implicit search
  type CEExit[E, A] >: ExitCase[E] <: ExitCase[E]

  def delayViaSync[K[_]](implicit KS: Sync[K]): Delay[K] =
    new Delay[K] {
      def delay[A](a: => A): K[A] = KS.delay(a)
    }

  def unliftEffect[K[_]](implicit KE: Effect[K]): UnliftEffect[IO, K] =
    new UnliftEffect(
      new Unlift[IO, K] {
        def lift[A](fa: IO[A]): K[A] = Effect[K].liftIO(fa)
        def unlift: K[K ~> IO]       = Effect.toIOK[K].pure[K]
      }
    )

  def concurrentTimeout[F[_]: Concurrent: Timer](implicit @unused nonTofu: NonTofu[F]): Timeout[F] = new Timeout[F] {
    override def timeoutTo[A](fa: F[A], after: FiniteDuration, fallback: F[A]): F[A] =
      Concurrent.timeoutTo(fa, after, fallback)
  }

  final implicit def finallyFromBracket[F[_], E](implicit
      F: Bracket[F, E]
  ): FinallyCarrier.Aux[F, E, CEExit[E, *]] =
    FinallyCarrier[F, E, CEExit[E, *]](
      new Finally[F, CEExit[E, *]] {
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
    )

  final implicit def startFromConcurrent[F[_]](implicit
      F: Concurrent[F],
      @unused _nonTofu: NonTofu[F]
  ): FibersCarrier.Aux[F, Fiber[F, *]] =
    FibersCarrier[F, Fiber[F, *]](
      new Fibers[F, Fiber[F, *]] {
        def start[A](fa: F[A]): F[Fiber[F, A]]                                                = F.start(fa)
        def fireAndForget[A](fa: F[A]): F[Unit]                                               = F.void(start(fa))
        def racePair[A, B](fa: F[A], fb: F[B]): F[Either[(A, Fiber[F, B]), (Fiber[F, A], B)]] = F.racePair(fa, fb)
        def race[A, B](fa: F[A], fb: F[B]): F[Either[A, B]]                                   = F.race(fa, fb)
        def never[A]: F[A]                                                                    = F.never
      }
    )
}
