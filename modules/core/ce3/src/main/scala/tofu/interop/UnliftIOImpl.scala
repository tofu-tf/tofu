package tofu.interop

import tofu.WithContext
import cats.effect.std.Dispatcher
import cats.effect.Async
import cats.effect.unsafe.IORuntime
import cats.effect.IO
import tofu.internal.carriers.UnliftCarrier3
import tofu.syntax.monadic._
import cats.~>
import scala.concurrent.ExecutionContext
import tofu.syntax.funk.funK

final class UnliftIOImpl[K[_]](implicit
    KD: WithContext[K, Dispatcher[K]],
    K: Async[K],
    KIO: WithContext[K, IORuntime]
) extends UnliftCarrier3[IO, K] {
  type Callback[A] = Either[Throwable, A] => Unit

  def lift[A](io: IO[A]): K[A] = for {
    iort <- KIO.context
    ec   <- K.executionContext
    a    <- K.async[A](k => K.delay(Some(liftRun[A](io, k)(iort, ec))))
  } yield a

  def unlift: K[K ~> IO] = for {
    d    <- KD.context
    iort <- KIO.context
  } yield funK[K, IO](ka => IO.async(cb => IO.delay(Some(unliftRun(d, ka, cb)(iort.compute)))))

  @inline private[this] def liftRun[A](
      io: IO[A],
      callback: Callback[A]
  )(implicit iort: IORuntime, ec: ExecutionContext): K[Unit] = {
    val (res, cancel) = io.unsafeToFutureCancelable()
    res.onComplete(t => callback(t.toEither))
    K.fromFuture(K.delay(cancel()))
  }

  @inline private[this] def unliftRun[A](d: Dispatcher[K], ka: K[A], callback: Callback[A])(implicit
      ec: ExecutionContext
  ): IO[Unit] = {
    val (res, cancel) = d.unsafeToFutureCancelable(ka)
    res.onComplete(t => callback(t.toEither))
    IO.fromFuture(IO.delay(cancel()))
  }
}
