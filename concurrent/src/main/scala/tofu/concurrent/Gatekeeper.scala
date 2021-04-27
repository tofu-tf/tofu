package tofu.concurrent
import cats.effect.concurrent.Semaphore
import cats.effect.syntax.bracket._
import cats.effect.{Async, Concurrent, Sync}
import cats.{InvariantMonoidal, Monad}
import tofu.BracketThrow
import tofu.concurrent.Gatekeeper.{BlockedMutex, MutexBySemaphore, StoplightBySemaphore}
import tofu.syntax.monadic._

/** Semaphore-like structure */
trait Gatekeeper[F[_], A] {

  /** Returns the number of permits currently available.
    * May be out of date the instant after it is retrieved.
    */
  def available: F[A]

  /** Obtains a snapshot of the currently taken.
    *
    * Like [[available]] when permits are available but returns the number of permits
    * callers are waiting for when there are no permits available.
    */
  def taken: F[A]

  /** Returns an effect that acquires a permit, runs the supplied effect, and then releases the permit.
    */
  def withPermit[B](t: F[B]): F[B]

  /** Returns an effect that acquires a permit, runs the supplied effect, and then releases the permit.
    */
  def withPermitN[B](take: A)(t: F[B]): F[B]
}

object Gatekeeper {
  implicit def gatekeeperApplicative[F[_]: Monad]: InvariantMonoidal[Gatekeeper[F, *]] =
    new InvariantMonoidal[Gatekeeper[F, *]] {
      object unit extends Gatekeeper[F, Unit] {
        def available: F[Unit]                        = Monad[F].unit
        def taken: F[Unit]                            = Monad[F].unit
        def withPermit[B](t: F[B]): F[B]              = t
        def withPermitN[B](take: Unit)(t: F[B]): F[B] = t
      }

      def product[A, B](fa: Gatekeeper[F, A], fb: Gatekeeper[F, B]): Gatekeeper[F, (A, B)] = new Gatekeeper[F, (A, B)] {
        def available: F[(A, B)]                        = fa.available.product(fb.available)
        def taken: F[(A, B)]                            = fa.taken.product(fb.taken)
        def withPermit[C](t: F[C]): F[C]                = fa.withPermit(fb.withPermit(t))
        def withPermitN[C](take: (A, B))(t: F[C]): F[C] = fa.withPermitN(take._1)(fb.withPermitN(take._2)(t))
      }

      def imap[A, B](fa: Gatekeeper[F, A])(f: A => B)(g: B => A): Gatekeeper[F, B] = new Gatekeeper[F, B] {
        def available: F[B]                        = fa.available.map(f)
        def taken: F[B]                            = fa.taken.map(f)
        def withPermit[C](t: F[C]): F[C]           = fa.withPermit(t)
        def withPermitN[C](take: B)(t: F[C]): F[C] = fa.withPermitN(g(take))(t)
      }
    }

  final case class StoplightBySemaphore[F[_]: BracketThrow](semaphore: Semaphore[F]) extends Gatekeeper[F, Long] {
    override def available: F[Long]                        = semaphore.available
    override def taken: F[Long]                            = semaphore.count
    override def withPermit[B](t: F[B]): F[B]              = semaphore.withPermit(t)
    override def withPermitN[B](take: Long)(t: F[B]): F[B] =
      semaphore.acquireN(take).bracket(_ => t)(_ => semaphore.releaseN(take))
  }

  final case class MutexBySemaphore[F[_]: BracketThrow](semaphore: Semaphore[F]) extends Gatekeeper[F, Boolean] {
    override def available: F[Boolean]                        = semaphore.available.map(_ > 0)
    override def taken: F[Boolean]                            = semaphore.count.map(_ > 0)
    override def withPermit[B](t: F[B]): F[B]                 = semaphore.withPermit(t)
    override def withPermitN[B](take: Boolean)(t: F[B]): F[B] = if (!take) t else withPermit(t)
  }

  final class BlockedMutex[F[_]: Async] extends Gatekeeper[F, Boolean] {
    override def available: F[Boolean]                        = false.pure[F]
    override def taken: F[Boolean]                            = true.pure[F]
    override def withPermit[B](t: F[B]): F[B]                 = Async[F].never
    override def withPermitN[B](take: Boolean)(t: F[B]): F[B] = if (!take) t else withPermit(t)
  }
}

trait MakeGatekeeper[I[_], F[_], A] {
  def gatekeeper(available: A): I[Gatekeeper[F, A]]
}

object Stoplights {
  def apply[F[_]](implicit stoplights: Stoplights[F]): MakeGatekeeper.Maker[F, F, Long] =
    new MakeGatekeeper.Maker(stoplights)
}

object Mutexes {
  def apply[F[_]](implicit mutexes: Mutexes[F]): MakeGatekeeper.Maker[F, F, Boolean] =
    new MakeGatekeeper.Maker(mutexes)
}

object MakeStoplight {
  def apply[I[_], F[_]](implicit maker: MakeStoplight[I, F]): MakeGatekeeper.Maker[I, F, Long] =
    new MakeGatekeeper.Maker(maker)
}

object MakeMutex {
  def apply[I[_], F[_]](implicit maker: MakeMutex[I, F]): MakeGatekeeper.Maker[I, F, Boolean] =
    new MakeGatekeeper.Maker(maker)
}

object MakeGatekeeper {
  def apply[I[_], F[_]]: Applier[I, F]                                           = new Applier[I, F](true)
  def mk[I[_], F[_], A](implicit maker: MakeGatekeeper[I, F, A]): Maker[I, F, A] = new Maker[I, F, A](maker)

  class Applier[I[_], F[_]](private val dummy: Boolean) extends AnyVal {
    def of[A](count: A)(implicit maker: MakeGatekeeper[I, F, A]): I[Gatekeeper[F, A]] = maker.gatekeeper(count)
  }

  class Maker[I[_], F[_], A](private val maker: MakeGatekeeper[I, F, A]) extends AnyVal {
    def of(count: A): I[Gatekeeper[F, A]] = maker.gatekeeper(count)
  }

  implicit def concurrentStoplight[I[_]: Sync, F[_]: Concurrent]: MakeStoplight[I, F] =
    new MakeGatekeeper[I, F, Long] {
      def gatekeeper(count: Long): I[Gatekeeper[F, Long]] = Semaphore.in[I, F](count).map(StoplightBySemaphore(_))
    }

  implicit def concurrentMutex[I[_]: Sync, F[_]: Concurrent]: MakeMutex[I, F] =
    new MakeGatekeeper[I, F, Boolean] {
      def gatekeeper(available: Boolean): I[Gatekeeper[F, Boolean]] =
        if (available) Semaphore.in[I, F](1).map(MutexBySemaphore(_)) else Monad[I].pure(new BlockedMutex[F])
    }
}
