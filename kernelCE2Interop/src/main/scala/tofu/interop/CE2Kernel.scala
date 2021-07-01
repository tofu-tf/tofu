package tofu.interop
import tofu.lift.Unlift

import cats.effect.Sync
import tofu.Delay
import cats.effect.Effect
import cats.effect.IO
import tofu.syntax.monadic._
import cats.~>
import tofu.lift.UnliftEffect
import cats.effect.Concurrent
import cats.effect.Timer
import scala.concurrent.duration.FiniteDuration
import tofu.Timeout
import tofu.internal.NonTofu
import tofu.compat.unused
import cats.effect.Bracket
import tofu.Finally
import tofu.kernel.types._
import cats.effect.ExitCase

object CE2Kernel {
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
  
  final implicit def finallyFromBracket[F[_], E](implicit F: Bracket[F, E]): Finally[F, TConst[ExitCase[E], *]] =
    new Finally[F, TConst[ExitCase[E], *]] {
      def finallyCase[A, B, C](init: F[A])(action: A => F[B])(release: (A, ExitCase[E]) => F[C]): F[B] =
        F.bracketCase(init)(action) { case (a, exit) =>
          F.void(release(a, exit))
        }
      def bracket[A, B, C](init: F[A])(action: A => F[B])(release: (A, Boolean) => F[C]): F[B]         =
        F.bracketCase(init)(action) {
          case (a, ExitCase.Completed) => F.void(release(a, true))
          case (a, _)                  => F.void(release(a, false))
        }
    }
}
