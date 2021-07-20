package tofu.interop

import tofu.Delay
import cats.effect.Sync
import cats.effect.std.Dispatcher
import tofu.internal.carriers.UnliftCarrier3
import cats.effect.IO
import cats.effect.Async
import tofu.WithContext
import cats.effect.unsafe.IORuntime

object CE3Kernel {
  def delayViaSync[K[_]](implicit KS: Sync[K]): Delay[K] =
    new Delay[K] {
      def delay[A](a: => A): K[A] = KS.delay(a)
    }

  def unliftEffect[K[_]](implicit
      KD: WithContext[K, Dispatcher[K]],
      K: Async[K],
      KIO: WithContext[K, IORuntime]
  ): UnliftCarrier3[IO, K] = new UnliftIOImpl[K]

}
