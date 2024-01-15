package tofu.interop

import cats.effect.std.Dispatcher
import tofu.Performer
import tofu.kernel.types.PerformThrow
import tofu.concurrent.Exit
import cats.effect.Async
import scala.concurrent.ExecutionContext
import cats.Apply
import tofu.WithContext
import tofu.lift.Unlift
import tofu.internal.instances.UnliftPerformer
import tofu.kernel.types.PerformExitCont

class DispatchPerform[F[_], B[_]](implicit
    async: Async[B],
    val functor: Apply[F],
    dispatcher: WithContext[F, Dispatcher[B]],
    unlift: Unlift[B, F]
) extends PerformThrow[F] {

  def performer: F[Performer.OfExit[F, Throwable]] =
    functor.map3(
      unlift.lift(async.executionContext),
      dispatcher.context,
      unlift.unlift
    ) { (ec, dispatcher, unlifter) =>
      val performer = new DispatchPerformer[B](dispatcher)(async, ec)
      new UnliftPerformer[F, B, PerformExitCont[Throwable, _], Unit](performer, unlifter, unlift)
    }
}

class DispatchPerformer[F[_]](dispatcher: Dispatcher[F])(implicit F: Async[F], ec: ExecutionContext)
    extends Performer.OfExit[F, Throwable] {

  def perform[A](cont: Exit[Throwable, A] => Unit)(fa: F[A]): F[Unit] = {
    val (res, cancel) = dispatcher.unsafeToFutureCancelable(fa)
    res.onComplete(t => cont(Exit.fromTry(t)))
    F.fromFuture(F.delay(cancel()))
  }

}
