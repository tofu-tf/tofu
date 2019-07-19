package tofu.syntax

import cats.Functor
import cats.effect.concurrent.{Deferred, MVar, Ref, Semaphore}
import tofu.lift.{IsoK, Lift, Unlift}
import cats.tagless.{FunctorK, InvariantK}
import tofu.lift.{IsoK, Lift, Unlift}

object lift {
  implicit class MVarLiftSyntax[F[_], A](val mvar: MVar[F, A]) extends AnyVal {
    def lift[G[_]](implicit lift: Lift[F, G]): MVar[G, A] = mvar.mapK(lift.liftF)
  }

  implicit class RefLiftSyntax[F[_], A](val ref: Ref[F, A]) extends AnyVal {
    def lift[G[_]](implicit lift: Lift[F, G], F: Functor[F]): Ref[G, A] = ref.mapK(lift.liftF)
  }

  implicit class DeferredLiftSyntax[F[_], A](val deferred: Deferred[F, A]) extends AnyVal {
    def lift[G[_]](implicit lift: Lift[F, G]): Deferred[G, A] = deferred.mapK(lift.liftF)
  }

  implicit class SemaphoreLiftSyntax[F[_]](val semaphore: Semaphore[F]) extends AnyVal {
    def ilift[G[_]](implicit lift: IsoK[F, G]): Semaphore[G] = semaphore.imapK(lift.tof, lift.fromF)
    def unlift[G[_]](implicit unlift: Unlift[F, G], G: Functor[G]): G[Semaphore[G]] =
      G.map(unlift.unlift)(backf => semaphore.imapK(unlift.liftF, backf))
  }

  implicit class CatsFunctorKLiftSyntax[T[_[_]], F[_]](val tf: T[F]) extends AnyVal {
    def lift[G[_]](implicit lift: Lift[F, G], fk: FunctorK[T]): T[G]    = fk.mapK(tf)(lift.liftF)
    def ilift[G[_]](implicit lift: IsoK[F, G], fk: InvariantK[T]): T[G] = fk.imapK(tf)(lift.tof)(lift.fromF)
    def unlift[G[_]](implicit unlift: Unlift[F, G], G: Functor[G], fk: InvariantK[T]): G[T[G]] =
      G.map(unlift.unlift)(backf => fk.imapK(tf)(unlift.liftF)(backf))
  }
}
