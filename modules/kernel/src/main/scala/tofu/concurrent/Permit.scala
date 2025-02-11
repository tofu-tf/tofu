package tofu.concurrent

import tofu.internal.instances.MakePermitInstance

/** A purely functional semaphore. A semaphore has a non-negative number of permits available. Acquiring a permit
  * decrements the current number of permits and releasing a permit increases the current number of permits. An acquire
  * that occurs when there are no permits available results in semantic blocking until a permit becomes available.
  * Blocking `withPermit` are cancelable.
  */
trait Permit[F[_]] {

  /** Returns an effect that acquires a permit, runs the supplied effect, and then releases the permit. The returned
    * effect semantically blocks until permit are available. Note that acquires are statisfied in strict FIFO order.
    */
  def withPermit[A](fa: F[A]): F[A]

}

object Permit {
  type Make[F[_]] = MakePermit[F, F]

  /** A helper for creating instances of [[tofu.concurrent.Permit]] that use the same effect during construction and
    * work. If you want to use different effect to construct `Permit` use [[tofu.concurrent.MakePermit]]
    */
  def Make[F[_]](implicit makePermit: Make[F]): MakePermit.PermitApplier[F, F] =
    new MakePermit.PermitApplier[F, F](makePermit)
}

/** A creator of [[tofu.concurrent.Permit]] that supports effectful construction
  * @tparam I
  *   effect for creation of agent
  * @tparam F
  *   effect on which agent will run
  */
trait MakePermit[I[_], F[_]] {

  /** Creates instance of [[tofu.concurrent.Permit]], initialized with `limit` available permits
    *
    * @param limit
    *   maximum concurrent permits
    * @return
    *   `I[ Permit[F] ]`
    */
  def permitOf(limit: Long): I[Permit[F]]
}

/** A helper for creating instances of [[tofu.concurrent.Permit]] that use different effects during construction and
  * work. If you want to use same effect to construct and run `Permit` use [[tofu.concurrent.Permit.Make]]
  */
object MakePermit extends MakePermitInstance {

  def apply[I[_], F[_]](implicit mkPermit: MakePermit[I, F]): PermitApplier[I, F] =
    new PermitApplier[I, F](mkPermit)

  final class PermitApplier[I[_], F[_]](private val mkPermit: MakePermit[I, F]) extends AnyVal {
    def of(limit: Long): I[Permit[F]] = mkPermit.permitOf(limit)
  }
}
