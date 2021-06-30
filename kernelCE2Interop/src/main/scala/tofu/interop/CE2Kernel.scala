package tofu.interop
import tofu.lift.Unlift

import cats.effect.Sync
import tofu.Delay
import cats.effect.Effect
import cats.effect.IO
import tofu.syntax.monadic._
import cats.~>
import tofu.lift.UnliftEffect

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
}
