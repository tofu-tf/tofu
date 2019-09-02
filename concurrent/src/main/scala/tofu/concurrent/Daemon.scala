package tofu
package concurrent

import cats.data.StateT
import cats.effect._
import cats.effect.concurrent.MVar
import cats.effect.syntax.bracket._
import cats.syntax.applicativeError._
import cats.tagless.FunctorK
import cats.{Apply, FlatMap, Monad, ~>}
import simulacrum.typeclass
import tofu.syntax.monadic._
import tofu.syntax.start._

@typeclass
trait Daemonic[F[_]] {
  def daemonize(process: F[Unit]): F[Daemon[F]]
}

object Daemonic {
  implicit def instance[F[_]: Start: Deferreds: BracketThrow]: Daemonic[F] =
    process =>
      for {
        exit  <- MakeDeferred[F, F, Daemon.Exit]
        fiber <- process.guaranteeCase(exit.complete).start
      } yield Daemon.Impl(fiber, exit.get)
}

trait Daemon[F[_]] {
  def process: Fiber[F, Unit]
  def exit: F[Daemon.Exit]
}

/** Probably Infinite processes */
object Daemon {
  type Exit = ExitCase[Throwable]

  final case class Impl[F[_]](process: Fiber[F, Unit], exit: F[Daemon.Exit]) extends Daemon[F]

  implicit class DaemonOps[F[_]](val self: Daemon[F]) extends AnyVal {
    def bindTo(daemon: Daemon[F])(implicit FS: Start[F], F: Apply[F]): F[Unit] = (daemon.exit *> kill).start.void

    def kill = self.process.cancel
  }

  implicit val functorKInstance: FunctorK[Daemon] = new FunctorK[Daemon] {
    def mapK[F[_], G[_]](af: Daemon[F])(fk: F ~> G): Daemon[G] = new Daemon[G] {
      val process: Fiber[G, Unit] = af.process.mapK(fk)
      val exit: G[Exit]           = fk(af.exit)
    }
  }

  def apply[F[_]](process: F[Unit])(implicit D: Daemonic[F]): F[Daemon[F]] = D.daemonize(process)

  def repeat[F[_]: Monad: Daemonic](step: F[Unit]): F[Daemon[F]] = apply(step.foreverM)

  def iterate[F[_]: Monad: Daemonic, A](init: A)(step: A => F[A]): F[Daemon[F]] =
    apply(init.iterateWhileM(step)(_ => true).void)

  def state[F[_]: Monad: Daemonic, S](init: S)(state: StateT[F, S, Unit]): F[Daemon[F]] =
    iterate(init)(state.runS)

  def resource[F[_]: Monad: Daemonic, A](daemon: F[Daemon[F]]): Resource[F, Daemon[F]] =
    Resource.make(daemon)(_.process.cancel)
}

final class Actor[F[_], A] private (queue: MVar[F, A], val daemon: Daemon[F]) {

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

  def watch(action: Daemon.Exit => F[Unit])(implicit FS: Start[F], F: FlatMap[F]): F[Fiber[F, Unit]] =
    (daemon.exit >>= action).start

  def send(message: A): F[Unit] = this !! message

  def tell(message: A)(implicit F: Start[F]): F[Fiber[F, Unit]] = this ! message
}

object Actor {
  final case class Behavior[F[_], A](receive: A => F[Option[Behavior[F, A]]])

  def spawn[F[_]: MVars: Monad: Daemonic, A](behavior: Behavior[F, A]): F[Actor[F, A]] =
    for {
      mvar <- MakeMVar[F, F].empty[A]
      daemon <- Daemon.iterate(behavior)(b =>
                 for {
                   a <- mvar.take
                   r <- behavior.receive(a)
                 } yield r.getOrElse(b))
    } yield new Actor(mvar, daemon)

  def apply[F[_]: MVars: Monad: Daemonic, A](receive: A => F[Unit]): F[Actor[F, A]] =
    for {
      mvar   <- MakeMVar[F, F].empty[A]
      daemon <- Daemon.repeat(mvar.take >>= receive)
    } yield new Actor(mvar, daemon)

  def sync[F[_]: Concurrent, A](receive: A => Unit): F[Actor[F, A]] = apply(a => receive(a).pure[F])

  def syncSupervise[F[_]: Concurrent, A](receive: A => Unit)(strategy: Throwable => F[Unit]): F[Actor[F, A]] =
    apply(a => Sync[F].delay(receive(a)).handleErrorWith(strategy))
}
