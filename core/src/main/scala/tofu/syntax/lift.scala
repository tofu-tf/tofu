package tofu.syntax

import cats.{Functor, ~>}
import cats.effect.concurrent.{Deferred, MVar, MVar2, Ref, Semaphore}
import cats.tagless.{FunctorK, InvariantK}
import tofu.lift.{IsoK, Lift, Unlift}

import scala.annotation.nowarn

object lift {
  implicit final class LiftSyntax[F[_], A](private val fa: F[A]) extends AnyVal {
    def lift[G[_]](implicit lift: Lift[F, G]): G[A] = lift.lift(fa)
  }

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

  implicit final class CatsTaglessLiftSyntax[T[_[_]], F[_]](private val tf: T[F]) extends AnyVal {
    def lift[G[_]](implicit lift: Lift[F, G], fk: FunctorK[T]): T[G]                           = fk.mapK(tf)(lift.liftF)
    def ilift[G[_]](implicit lift: IsoK[F, G], fk: InvariantK[T]): T[G]                        = fk.imapK(tf)(lift.tof)(lift.fromF)
    def unlift[G[_]](implicit unlift: Unlift[F, G], G: Functor[G], fk: InvariantK[T]): G[T[G]] =
      G.map(unlift.unlift)(backf => fk.imapK(tf)(unlift.liftF)(backf))
  }

  implicit final class CatsTagless1LiftSyntax[T[_[_], _], F[_], A](private val tf: T[F, A]) extends AnyVal {
    def mapK1[G[_]](f: F ~> G)(implicit fk: FunctorK[T[*[_], A]]): T[G, A]               = fk.mapK(tf)(f)
    def imapK1[G[_]](f: F ~> G)(g: G ~> F)(implicit fk: InvariantK[T[*[_], A]]): T[G, A] = fk.imapK(tf)(f)(g)

    def lift1[G[_]](implicit lift: Lift[F, G], fk: FunctorK[T[*[_], A]]): T[G, A]                           = fk.mapK(tf)(lift.liftF)
    def ilift1[G[_]](implicit lift: IsoK[F, G], fk: InvariantK[T[*[_], A]]): T[G, A]                        =
      fk.imapK(tf)(lift.tof)(lift.fromF)
    def unlift1[G[_]](implicit unlift: Unlift[F, G], G: Functor[G], fk: InvariantK[T[*[_], A]]): G[T[G, A]] =
      G.map(unlift.unlift)(backf => fk.imapK(tf)(unlift.liftF)(backf))
  }

  implicit final class CatsTagless2LiftSyntax[T[_[_], _, _], F[_], A, B](private val tf: T[F, A, B]) extends AnyVal {
    def mapK2[G[_]](f: F ~> G)(implicit fk: FunctorK[T[*[_], A, B]]): T[G, A, B]               = fk.mapK(tf)(f)
    def imapK2[G[_]](f: F ~> G)(g: G ~> F)(implicit fk: InvariantK[T[*[_], A, B]]): T[G, A, B] = fk.imapK(tf)(f)(g)

    def lift2[G[_]](implicit lift: Lift[F, G], fk: FunctorK[T[*[_], A, B]]): T[G, A, B]                           = fk.mapK(tf)(lift.liftF)
    def ilift2[G[_]](implicit lift: IsoK[F, G], fk: InvariantK[T[*[_], A, B]]): T[G, A, B]                        =
      fk.imapK(tf)(lift.tof)(lift.fromF)
    def unlift2[G[_]](implicit unlift: Unlift[F, G], G: Functor[G], fk: InvariantK[T[*[_], A, B]]): G[T[G, A, B]] =
      G.map(unlift.unlift)(backf => fk.imapK(tf)(unlift.liftF)(backf))
  }
}
