package tofu.concurrent
import cats.effect.{Bracket, Concurrent, Sync}
import cats.effect.concurrent.Semaphore
import tofu.BracketThrow
import tofu.syntax.monadic._
import cats.effect.syntax.bracket._
import tofu.concurrent.Gatekeeper.GateKeeperBySemaphore

trait Gatekeeper[F[_], A] {

  /**
    * Returns the number of permits currently available.
    * May be out of date the instant after it is retrieved.
    * Use `[[tryAcquire]]` or `[[tryAcquireN]]` if you wish to attempt an
    * acquire, returning immediately if the current count is not high enough
    * to satisfy the request.
    */
  def available: F[A]

  /**
    * Obtains a snapshot of the currently taken.
    *
    * Like [[available]] when permits are available but returns the number of permits
    * callers are waiting for when there are no permits available.
    */
  def taken: F[A]

  /**
    * Returns an effect that acquires a permit, runs the supplied effect, and then releases the permit.
    */
  def withPermit[B](t: F[B]): F[B]

  /**
    * Returns an effect that acquires a permit, runs the supplied effect, and then releases the permit.
    */
  def withPermitN[B](take: A, t: F[B]): F[B]
}

object Gatekeeper {
  final case class GateKeeperBySemaphore[F[_]: BracketThrow](semaphore: Semaphore[F]) extends Gatekeeper[F, Long] {
    override def available: F[Long]           = semaphore.available
    override def taken: F[Long]               = semaphore.count
    override def withPermit[B](t: F[B]): F[B] = semaphore.withPermit(t)
    override def withPermitN[B](take: Long, t: F[B]): F[B] =
      semaphore.acquireN(take).bracket(_ => t)(_ => semaphore.releaseN(take))
  }
}

trait MakeGatekeeper[I[_], F[_], A] {
  def gatekeeper(available: A): I[Gatekeeper[F, A]]
}

object MakeGatekeeper {
  def apply[I[_], F[_]] = new Applier[I, F](true)

  class Applier[I[_], F[_]](private val dummy: Boolean) extends AnyVal {
    def of[A](count: A)(implicit mksem: MakeGatekeeper[I, F, A]): I[Gatekeeper[F, A]] = mksem.gatekeeper(count)
  }

  implicit def concurrentSemaphore[I[_]: Sync, F[_]: Concurrent]: MakeGatekeeper[I, F, Long] =
    new MakeGatekeeper[I, F, Long] {
      def gatekeeper(count: Long): I[Gatekeeper[F, Long]] = Semaphore.in[I, F](count).map(GateKeeperBySemaphore(_))
    }
}
