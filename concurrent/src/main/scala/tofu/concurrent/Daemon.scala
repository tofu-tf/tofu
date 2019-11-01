package tofu
package concurrent

import cats.data.StateT
import cats.effect._
import cats.effect.concurrent.{MVar, TryableDeferred}
import cats.effect.syntax.bracket._
import cats.syntax.applicativeError._
import cats.tagless.autoApplyK
import cats.{Apply, FlatMap, Monad}
import tofu.higherKind.Function2K
import tofu.syntax.monadic._
import tofu.syntax.start._

trait Daemonic[F[_], E] {
  def daemonize[A](process: F[A]): F[Daemon[F, E, A]]
}

object Daemonic extends DaemonicInstances {
  def apply[F[_], E](implicit D: Daemonic[F, E]): Daemonic[F, E] = D

  private[tofu] type Promise[F[_], E, A] = TryableDeferred[F, Exit[E, A]]
  private[tofu] type Maker[F[_], E]      = Function2K[Fiber[F, *], Promise[F, E, *], Daemon[F, E, *]]

  private[tofu] def mkInstance[F[_]: Start: TryableDeferreds: Bracket[*[_], E], E](maker: Maker[F, E]): Daemonic[F, E] =
    new Daemonic[F, E] {
      override def daemonize[A](process: F[A]): F[Daemon[F, E, A]] =
        for {
          exit <- MakeDeferred.tryable[F, Exit[E, A]]
          fiber <- process
                    .flatTap(a => exit.complete(Exit.Completed(a)))
                    .guaranteeCase {
                      case ExitCase.Completed => unit
                      case ExitCase.Canceled  => exit.complete(Exit.Canceled)
                      case ExitCase.Error(e)  => exit.complete(Exit.Error(e))
                    }
                    .start
        } yield maker(fiber, exit)
    }

  /** instance making safe Daemons that throws exception on joining canceled fiber */
  implicit def safeInstance[F[_]: Start: TryableDeferreds: BracketThrow]: Daemonic[F, Throwable] =
    mkInstance[F, Throwable](
      Function2K[Fiber[F, *], Promise[F, Throwable, *], Daemon[F, Throwable, *]](
        (fib, promise) => new Daemon.SafeImpl(fib, promise)
      )
    )

}
trait DaemonicInstances { self: Daemonic.type =>

  /** instance making Daemons keeping default underlying behaviour*/
  implicit def nativeInstance[F[_]: Start: TryableDeferreds: Bracket[*[_], E], E]: Daemonic[F, E] =
    mkInstance[F, E](Function2K[Fiber[F, *], Promise[F, E, *], Daemon[F, E, *]]((fib, promise) => new Daemon.Impl(fib, promise)))
}

@autoApplyK
trait Daemon[F[_], E, A] extends Fiber[F, A] {
  def join: F[A]
  def cancel: F[Unit]
  def poll: F[Option[Exit[E, A]]]
  def exit: F[Exit[E, A]]

  final def process: Fiber[F, A] = this
}

/** Probably Infinite processes */
object Daemon {
  private[tofu] class Impl[F[_], E, A](process: Fiber[F, A], end: TryableDeferred[F, Exit[E, A]])
      extends Daemon[F, E, A] {

    override def join: F[A]                  = process.join
    override def cancel: F[Unit]             = process.cancel
    override def poll: F[Option[Exit[E, A]]] = end.tryGet
    override def exit: F[Exit[E, A]]         = end.get
  }

  /** safe implementation that would throw an exception on joining canceled process */
  private[tofu] class SafeImpl[F[_]: MonadThrow, A](process: Fiber[F, A], end: TryableDeferred[F, Exit[Throwable, A]])
      extends Impl[F, Throwable, A](process, end) {
    override def join: F[A] = exit.flatMap {
      case Exit.Error(e)     => e.raiseError
      case Exit.Completed(a) => a.pure
      case Exit.Canceled     => new TofuCanceledJoinException[F, A](this).raiseError
    }
  }

  implicit class DaemonOps[F[_], E, A](val self: Daemon[F, E, A]) extends AnyVal {
    def bindTo[B](daemon: Daemon[F, E, B])(implicit FS: Start[F], F: Apply[F]): F[Unit] =
      (daemon.exit *> self.cancel).start.void

    def kill = self.cancel
  }

  def apply[F[_], E, A](process: F[A])(implicit D: Daemonic[F, E]): F[Daemon[F, E, A]] = D.daemonize(process)

  def repeat[F[_]: Monad: Daemonic[*[_], E], E, A, B](step: F[A]): F[Daemon[F, E, B]] = apply(step.foreverM)

  def iterate[F[_]: Monad: Daemonic[*[_], E], E, A, B](init: A)(step: A => F[A]): F[Daemon[F, E, B]] =
    apply(init.iterateForeverM(step))

  def state[F[_]: Monad: Daemonic[*[_], E], E, S, A, B](init: S)(state: StateT[F, S, A]): F[Daemon[F, E, B]] =
    iterate(init)(state.runS)

  def resource[F[_]: Monad: Daemonic[*[_], E], E, A](daemon: F[Daemon[F, E, A]]): Resource[F, Daemon[F, E, A]] =
    Resource.make(daemon)(_.cancel)
}

final class Actor[F[_], E, A] private (queue: MVar[F, A], val daemon: Daemon[F, E, Void]) {

  /** fire message waiting for receive */
  def !!(message: A): F[Unit] = queue.put(message)

  /** fire and forget */
  def !(message: A)(implicit F: Start[F]): F[Fiber[F, Unit]] = queue.put(message).start

  /** ask pattern with error handling */
  def ??[B](make: (Either[Throwable, B] => Unit) => A)(implicit F: Concurrent[F]): F[B] =
    F.asyncF(cb => !!(make(cb)))

  /** ask pattern without error handling */
  def ?[B](make: (B => Unit) => A)(implicit F: Concurrent[F]): F[B] =
    F.asyncF(cb => !!(make(b => cb(Right(b)))))

  def onStop(action: F[Unit])(implicit FS: Start[F], F: FlatMap[F]): F[Fiber[F, Unit]] = watch(_ => action)

  def watch(action: Exit[E, Void] => F[Unit])(implicit FS: Start[F], F: FlatMap[F]): F[Fiber[F, Unit]] =
    (daemon.exit >>= action).start

  def send(message: A): F[Unit] = this !! message

  def tell(message: A)(implicit F: Start[F]): F[Fiber[F, Unit]] = this ! message
}

object Actor {
  final case class Behavior[F[_], A](receive: A => F[Option[Behavior[F, A]]])

  def spawn[F[_]: MVars: Monad: Daemonic[*[_], E], E, A](behavior: Behavior[F, A]): F[Actor[F, E, A]] =
    for {
      mvar <- MakeMVar[F, F].empty[A]
      daemon: Daemon[F, E, Void] <- Daemon.iterate(behavior)(
                                     b =>
                                       for {
                                         a <- mvar.take
                                         r <- behavior.receive(a)
                                       } yield r.getOrElse(b)
                                   )
    } yield new Actor(mvar, daemon)

  def apply[F[_]: MVars: Monad: Daemonic[*[_], E], E, A](receive: A => F[Unit]): F[Actor[F, E, A]] =
    for {
      mvar                       <- MakeMVar[F, F].empty[A]
      daemon: Daemon[F, E, Void] <- Daemon.repeat(mvar.take >>= receive)
    } yield new Actor(mvar, daemon)

  def sync[F[_]: Concurrent, A](receive: A => Unit): F[Actor[F, Throwable, A]] = apply(a => receive(a).pure[F])

  def syncSupervise[F[_]: Concurrent, A](
      receive: A => Unit
  )(strategy: Throwable => F[Unit]): F[Actor[F, Throwable, A]] =
    apply(a => Sync[F].delay(receive(a)).handleErrorWith(strategy))
}

final class TofuCanceledJoinException[F[_], A] private[tofu] (val daemon: Daemon[F, Throwable, A])
    extends InterruptedException("trying to join canceled fiber")
