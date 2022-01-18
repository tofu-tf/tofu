package tofu.interop
import tofu.internal.carriers.PerformCarrier2
import tofu.Performer
import tofu.concurrent.Exit
import cats.effect.{ConcurrentEffect, IO}

final class ConcurrentEffectPerformer[F[_]](implicit val functor: ConcurrentEffect[F])
    extends PerformCarrier2[F] with Performer.OfExit[F, Throwable] {

  def performer: F[Performer.OfExit[F, Throwable]] = functor.pure(this)

  def perform[A](cont: Exit[Throwable, A] => Unit)(f: F[A]): F[Unit] =
    functor.runCancelable(f)(e => IO(cont(Exit.fromEither(e)))).unsafeRunSync()
}
