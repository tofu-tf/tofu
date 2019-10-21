package tofu
package concurrent

import cats.data.StateT
import cats.effect._
import cats.effect.concurrent.{MVar, TryableDeferred}
import cats.effect.syntax.bracket._
import cats.syntax.applicativeError._
import cats.tagless.autoApplyK
import cats.{Apply, FlatMap, Monad}
import tofu.syntax.monadic._
import tofu.syntax.start._

trait Daemonic[F[_], E] {
  def daemonize[A](process: F[A]): F[Daemon[F, E, A]]
}

object Daemonic {
  def apply[F[_], E](implicit D: Daemonic[F, E]): Daemonic[F, E] = D

  implicit def instance[F[_]: Start: TryableDeferreds: Bracket[*[_], E], E]: Daemonic[F, E] =
    new Daemonic[F, E] {
      override def daemonize[A](process: F[A]): F[Daemon[F, E, A]] =
        for {
          exit  <- MakeDeferred.tryable[F, Daemon.Exit[E]]
          fiber <- process.guaranteeCase(exit.complete).start
        } yield new Daemon.Impl(fiber, exit)
    }

}

@autoApplyK
trait Daemon[F[_], E, A] extends Fiber[F, A] {
  def join: F[A]
  def cancel: F[Unit]
  def poll: F[Option[Daemon.Exit[E]]]
  def exit: F[Daemon.Exit[E]]

  final def process: Fiber[F, A] = this
}

/** Probably Infinite processes */
object Daemon {
  type Exit[E] = ExitCase[E]

  private[tofu] final class Impl[F[_], E, A](process: Fiber[F, A], end: TryableDeferred[F, Daemon.Exit[E]])
      extends Daemon[F, E, A] {

    override def join: F[A]               = process.join
    override def cancel: F[Unit]          = process.cancel
    override def poll: F[Option[Exit[E]]] = end.tryGet
    override def exit: F[Exit[E]]         = end.get
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

  def watch(action: Daemon.Exit[E] => F[Unit])(implicit FS: Start[F], F: FlatMap[F]): F[Fiber[F, Unit]] =
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
