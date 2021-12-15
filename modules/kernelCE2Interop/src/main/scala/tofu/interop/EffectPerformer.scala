package tofu.interop
import tofu.internal.carriers.PerformCarrier2
import tofu.Performer
import tofu.concurrent.Exit
import cats.effect.IO
import cats.effect.Effect

final class EffectPerformer[F[_]](implicit val functor: Effect[F])
    extends PerformCarrier2[F, Throwable] with Performer.OfExit[F, Throwable] {

  def performer: F[Performer.OfExit[F, Throwable]] = functor.pure(this)

  def perform[A](cont: Exit[Throwable, A] => Unit)(f: F[A]): F[Unit] =
    functor.delay(functor.runAsync(f)(e => IO(cont(Exit.fromEither(e)))).unsafeRunSync())
}
