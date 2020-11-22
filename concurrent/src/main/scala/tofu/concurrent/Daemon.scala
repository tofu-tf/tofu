package tofu
package concurrent

import cats.data.StateT
import cats.effect._
import cats.effect.concurrent.{MVar, TryableDeferred}
import cats.effect.syntax.bracket._
import cats.syntax.applicativeError._
import cats.{Applicative, Apply, FlatMap, Monad}
import tofu.control.ApplicativeZip
import tofu.higherKind.{Function2K, RepresentableK}
import tofu.syntax.monadic._
import tofu.syntax.start._
import tofu.concurrent.syntax.deferred._

import scala.annotation.nowarn

trait Daemonic[F[_], E] {
  def daemonize[A](process: F[A]): F[Daemon[F, E, A]]
}

object Daemonic extends DaemonicInstances {
  def apply[F[_], E](implicit D: Daemonic[F, E]): Daemonic[F, E] = D

  private[tofu] type Promise[F[_], E, A] = TryableDeferred[F, Exit[E, A]]
  private[tofu] type Maker[F[_], E]      = Function2K[Fiber[F, *], Promise[F, E, *], Daemon[F, E, *]]

  private[tofu] def mkInstance[F[_]: Start: TryableDeferreds: Bracket[*[_], E], E](
      maker: Maker[F, E]
  ): Daemonic[F, E] =
    new Daemonic[F, E] {
      override def daemonize[A](process: F[A]): F[Daemon[F, E, A]] =
        for {
          exit  <- MakeDeferred.tryable[F, Exit[E, A]]
          fiber <- process
                     .flatTap(a => exit.tryComplete(Exit.Completed(a)))
                     .guaranteeCase {
                       case ExitCase.Completed => unit[F]
                       case ExitCase.Canceled  => exit.tryComplete(Exit.Canceled)
                       case ExitCase.Error(e)  => exit.tryComplete(Exit.Error(e))
                     }
                     .start
        } yield maker(fiber, exit)
    }

  /** instance making safe Daemons that throws exception on joining canceled fiber */
  implicit def safeInstance[F[_]: Start: TryableDeferreds: BracketThrow]: Daemonic[F, Throwable] =
    mkInstance[F, Throwable](
      Function2K[Fiber[F, *], Promise[F, Throwable, *]]((fib, promise) => new Daemon.SafeImpl(fib, promise))
    )

}
trait DaemonicInstances { self: Daemonic.type =>

  /** instance making Daemons keeping default underlying behaviour */
  implicit def nativeInstance[F[_]: Start: TryableDeferreds: Bracket[*[_], E], E]: Daemonic[F, E] =
    mkInstance[F, E](
      Function2K[Fiber[F, *], Promise[F, E, *], Daemon[F, E, *]]((fib, promise) => new Daemon.Impl(fib, promise))
    )
}

trait Daemon[F[_], E, A] extends Fiber[F, A]     {
  def join: F[A]
  def cancel: F[Unit]
  def poll: F[Option[Exit[E, A]]]
  def exit: F[Exit[E, A]]

  final def process: Fiber[F, A] = this
}

/** Probably Infinite processes */
object Daemon            extends DaemonInstances {
  private[tofu] class Impl[F[_]: Restore: Apply, E, A](process: Fiber[F, A], end: TryableDeferred[F, Exit[E, A]])
      extends Daemon[F, E, A] {

    override def join: F[A]                  = process.join
    override def cancel: F[Unit]             = end.tryComplete(Exit.Canceled) *> process.cancel
    override def poll: F[Option[Exit[E, A]]] = end.tryGet
    override def exit: F[Exit[E, A]]         = end.get
  }

  /** safe implementation that would throw an exception on joining canceled process */
  private[tofu] class SafeImpl[F[_]: MonadThrow, A](process: Fiber[F, A], end: TryableDeferred[F, Exit[Throwable, A]])
      extends Impl[F, Throwable, A](process, end) {
    override def join: F[A] = exit.flatMap {
      case Exit.Error(e)     => e.raiseError
      case Exit.Completed(a) => a.pure[F]
      case Exit.Canceled     => new TofuCanceledJoinException[F, A](this).raiseError
    }
  }

  implicit final class DaemonOps[F[_], E, A](private val self: Daemon[F, E, A]) extends AnyVal {
    def bindTo[B](daemon: Daemon[F, E, B])(implicit FS: Start[F], F: Apply[F]): F[Unit] =
      (daemon.exit *> self.cancel).start.void

    def kill: F[Unit] = self.cancel
  }

  def apply[F[_], E, A](process: F[A])(implicit D: Daemonic[F, E]): F[Daemon[F, E, A]] = D.daemonize(process)

  def repeat[F[_]: Monad: Daemonic[*[_], E], E, A, B](step: F[A]): F[Daemon[F, E, B]] = apply(step.foreverM)

  def repeatUnit[F[_]: Monad: Daemonic[*[_], E], E, A](step: F[A]): F[Daemon[F, E, Unit]] = repeat(step)

  def repeatThrow[F[_]: Monad: DaemonicThrow, A, B](step: F[A]): F[DaemonThrow[F, B]] = repeat(step)

  def repeatTask[F[_]: Monad: DaemonicThrow, A](step: F[A]): F[DaemonTask[F]] = repeat(step)

  def iterate[F[_]: Monad: Daemonic[*[_], E], E, A, B](init: A)(step: A => F[A]): F[Daemon[F, E, B]] =
    apply(init.iterateForeverM(step))

  def iterateUnit[F[_]: Monad: Daemonic[*[_], E], E, A](init: A)(step: A => F[A]): F[Daemon[F, E, Unit]] =
    iterate(init)(step)

  def iterateThrow[F[_]: Monad: DaemonicThrow, A, B](init: A)(step: A => F[A]): F[DaemonThrow[F, B]] =
    iterate(init)(step)

  def iterateTask[F[_]: Monad: DaemonicThrow, A](init: A)(step: A => F[A]): F[DaemonTask[F]] = iterate(init)(step)

  def state[F[_]: Monad: Daemonic[*[_], E], E, S, A, B](init: S)(state: StateT[F, S, A]): F[Daemon[F, E, B]] =
    iterate(init)(state.runS)

  def resource[F[_]: Monad: Daemonic[*[_], E], E, A](daemon: F[Daemon[F, E, A]]): Resource[F, Daemon[F, E, A]] =
    Resource.make(daemon)(_.cancel)

}

trait Actor[F[_], E, A] {

  /** fire message waiting for receive */
  def !!(message: A): F[Unit]

  /** fire and forget */
  def !(message: A)(implicit F: Start[F]): F[Fiber[F, Unit]]

  /** ask pattern with error handling */
  def ??[B](make: (Either[Throwable, B] => Unit) => A)(implicit F: Concurrent[F]): F[B]

  /** ask pattern without error handling */
  def ?[B](make: (B => Unit) => A)(implicit F: Concurrent[F]): F[B]

  def onStop(action: F[Unit])(implicit FS: Start[F], F: FlatMap[F]): F[Fiber[F, Unit]]

  def watch(action: Exit[E, Void] => F[Unit])(implicit FS: Start[F], F: FlatMap[F]): F[Fiber[F, Unit]]

  def send(message: A): F[Unit] = this !! message

  def tell(message: A)(implicit F: Start[F]): F[Fiber[F, Unit]] = this ! message
}

object Actor {

  final case class Behavior[F[_], A](receive: A => F[Option[Behavior[F, A]]])

  /** Default in-memory Actor implementation.
    */
  @nowarn("cat=deprecation")
  final class LocalActor[F[_], E, A] private[concurrent] (queue: MVar[F, A], val daemon: Daemon[F, E, Void])
      extends Actor[F, E, A] {

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
  }

  def spawn[F[_]: MVars: Monad: Daemonic[*[_], E], E, A](behavior: Behavior[F, A]): F[Actor[F, E, A]] =
    for {
      mvar                       <- MakeMVar[F, F].empty[A]
      daemon: Daemon[F, E, Void] <- Daemon.iterate(behavior)(b =>
                                      for {
                                        a <- mvar.take
                                        r <- behavior.receive(a)
                                      } yield r.getOrElse(b)
                                    )
    } yield new LocalActor(mvar, daemon)

  def apply[F[_]: MVars: Monad: Daemonic[*[_], E], E, A](receive: A => F[Unit]): F[Actor[F, E, A]] =
    for {
      mvar                       <- MakeMVar[F, F].empty[A]
      daemon: Daemon[F, E, Void] <- Daemon.repeat(mvar.take >>= receive)
    } yield new LocalActor(mvar, daemon)

  def sync[F[_]: Concurrent, A](receive: A => Unit): F[Actor[F, Throwable, A]] = apply(a => receive(a).pure[F])

  def syncSupervise[F[_]: Concurrent, A](
      receive: A => Unit
  )(strategy: Throwable => F[Unit]): F[Actor[F, Throwable, A]] =
    apply(a => Sync[F].delay(receive(a)).handleErrorWith(strategy))
}

final class TofuCanceledJoinException[F[_], A] private[tofu] (val daemon: Daemon[F, Throwable, A])
    extends InterruptedException("trying to join canceled fiber")

trait DaemonInstances {
  private[this] val representableAny = higherKind.derived.genRepresentableK[Daemon[*[_], Any, Any]]

  final implicit def daemonRepresentable[E, A]: RepresentableK[Daemon[*[_], E, A]] =
    representableAny.asInstanceOf[RepresentableK[Daemon[*[_], E, A]]]

  final implicit def daemonApplicative[F[_], E](implicit F: Monad[F]): Applicative[Daemon[F, E, *]] =
    new ApplicativeZip[Daemon[F, E, *]] {
      def pure[A](x: A): Daemon[F, E, A] = new Daemon[F, E, A] {
        def join: F[A]                  = F.pure(x)
        def cancel: F[Unit]             = F.unit
        def poll: F[Option[Exit[E, A]]] = F.pure(Some(Exit.Completed(x)))
        def exit: F[Exit[E, A]]         = F.pure(Exit.Completed(x))
      }

      override def map[A, B](fa: Daemon[F, E, A])(f: A => B): Daemon[F, E, B] = new Daemon[F, E, B] {
        def join: F[B]                  = fa.join.map(f)
        def cancel: F[Unit]             = fa.cancel
        def poll: F[Option[Exit[E, B]]] = fa.poll.map(_.map(_.map(f)))
        def exit: F[Exit[E, B]]         = fa.exit.map(_.map(f))
      }

      def zipWith[A, B, C](fa: Daemon[F, E, A], fb: Daemon[F, E, B])(f: (A, B) => C): Daemon[F, E, C] =
        new Daemon[F, E, C] {
          def join: F[C]                  = fa.join.map2(fb.join)(f)
          def cancel: F[Unit]             = fa.cancel *> fb.cancel
          def poll: F[Option[Exit[E, C]]] = fa.poll.flatMap {
            case fe @ (None | Some(_: Exit.Incomplete[E])) => F.pure(fe.asInstanceOf[Option[Exit[E, C]]])
            case Some(Exit.Completed(a))                   =>
              fb.poll.map {
                case fe @ (None | Some(_: Exit.Incomplete[E])) => fe.asInstanceOf[Option[Exit[E, C]]]
                case Some(Exit.Completed(b))                   => Some(Exit.Completed(f(a, b)))
              }
          }
          def exit: F[Exit[E, C]]         = fa.exit.map2(fb.exit)(_.map2(_)(f))
        }
    }
}
