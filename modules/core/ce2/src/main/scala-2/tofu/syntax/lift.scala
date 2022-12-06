package tofu.syntax

import cats.Functor
import cats.effect.concurrent.{Deferred, MVar, MVar2, Ref, Semaphore}
import tofu.lift.{IsoK, Lift, Unlift}

import scala.annotation.nowarn

object lift extends KernelLiftSyntax {
  @nowarn("cat=deprecation")
  implicit final class MVarLiftSyntax[F[_], A](private val mvar: MVar[F, A]) extends AnyVal {
    def lift[G[_]](implicit lift: Lift[F, G]): MVar[G, A] = mvar.mapK(lift.liftF)
  }

  implicit final class MVar2LiftSyntax[F[_], A](private val mvar: MVar2[F, A]) extends AnyVal {
    def ilift[G[_]](implicit lift: IsoK[F, G]): MVar2[G, A] = mvar.imapK(lift.tof, lift.fromF)
  }

  implicit final class RefLiftSyntax[F[_], A](private val ref: Ref[F, A]) extends AnyVal {
    def lift[G[_]](implicit lift: Lift[F, G], F: Functor[F]): Ref[G, A] = ref.mapK(lift.liftF)
  }

  implicit final class DeferredLiftSyntax[F[_], A](private val deferred: Deferred[F, A]) extends AnyVal {
    def lift[G[_]](implicit lift: Lift[F, G]): Deferred[G, A] = deferred.mapK(lift.liftF)
  }

  implicit final class SemaphoreLiftSyntax[F[_]](private val semaphore: Semaphore[F]) extends AnyVal {
    def ilift[G[_]](implicit lift: IsoK[F, G]): Semaphore[G]                        = semaphore.imapK(lift.tof, lift.fromF)
    def unlift[G[_]](implicit unlift: Unlift[F, G], G: Functor[G]): G[Semaphore[G]] =
      G.map(unlift.unlift)(backf => semaphore.imapK(unlift.liftF, backf))
  }
}
