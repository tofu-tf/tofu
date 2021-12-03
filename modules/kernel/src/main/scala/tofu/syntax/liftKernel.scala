package tofu.syntax
import cats.tagless.{FunctorK, InvariantK}
import cats.{Functor, ~>}
import tofu.lift.{IsoK, Lift, Unlift}

object liftKernel extends KernelLiftSyntax

trait KernelLiftSyntax extends Any {
  final implicit def LiftSyntax[F[_], A](fa: F[A]): LiftSyntax[F, A] = new LiftSyntax(fa)

  final implicit def CatsTaglessLiftSyntax[T[_[_]], F[_]](tf: T[F]): CatsTaglessLiftSyntax[T, F] =
    new CatsTaglessLiftSyntax(tf)

  final implicit def CatsTagless1LiftSyntax[T[_[_], _], F[_], A](tf: T[F, A]): CatsTagless1LiftSyntax[T, F, A] =
    new CatsTagless1LiftSyntax(tf)

  final implicit def CatsTagless2LiftSyntax[T[_[_], _, _], F[_], A, B](
      tf: T[F, A, B]
  ): CatsTagless2LiftSyntax[T, F, A, B] =
    new CatsTagless2LiftSyntax(tf)
}

final class LiftSyntax[F[_], A](private val fa: F[A]) extends AnyVal {
  def lift[G[_]](implicit lift: Lift[F, G]): G[A] = lift.lift(fa)
}

final class CatsTaglessLiftSyntax[T[_[_]], F[_]](private val tf: T[F]) extends AnyVal {
  def lift[G[_]](implicit lift: Lift[F, G], fk: FunctorK[T]): T[G]                           = fk.mapK(tf)(lift.liftF)
  def ilift[G[_]](implicit lift: IsoK[F, G], fk: InvariantK[T]): T[G]                        = fk.imapK(tf)(lift.tof)(lift.fromF)
  def unlift[G[_]](implicit unlift: Unlift[F, G], G: Functor[G], fk: InvariantK[T]): G[T[G]] =
    G.map(unlift.unlift)(backf => fk.imapK(tf)(unlift.liftF)(backf))
}

final class CatsTagless1LiftSyntax[T[_[_], _], F[_], A](private val tf: T[F, A]) extends AnyVal {
  def mapK1[G[_]](f: F ~> G)(implicit fk: FunctorK[T[*[_], A]]): T[G, A]               = fk.mapK(tf)(f)
  def imapK1[G[_]](f: F ~> G)(g: G ~> F)(implicit fk: InvariantK[T[*[_], A]]): T[G, A] = fk.imapK(tf)(f)(g)

  def lift1[G[_]](implicit lift: Lift[F, G], fk: FunctorK[T[*[_], A]]): T[G, A]                           = fk.mapK(tf)(lift.liftF)
  def ilift1[G[_]](implicit lift: IsoK[F, G], fk: InvariantK[T[*[_], A]]): T[G, A]                        =
    fk.imapK(tf)(lift.tof)(lift.fromF)
  def unlift1[G[_]](implicit unlift: Unlift[F, G], G: Functor[G], fk: InvariantK[T[*[_], A]]): G[T[G, A]] =
    G.map(unlift.unlift)(backf => fk.imapK(tf)(unlift.liftF)(backf))
}

final class CatsTagless2LiftSyntax[T[_[_], _, _], F[_], A, B](private val tf: T[F, A, B]) extends AnyVal {
  def mapK2[G[_]](f: F ~> G)(implicit fk: FunctorK[T[*[_], A, B]]): T[G, A, B]               = fk.mapK(tf)(f)
  def imapK2[G[_]](f: F ~> G)(g: G ~> F)(implicit fk: InvariantK[T[*[_], A, B]]): T[G, A, B] = fk.imapK(tf)(f)(g)

  def lift2[G[_]](implicit lift: Lift[F, G], fk: FunctorK[T[*[_], A, B]]): T[G, A, B]                           = fk.mapK(tf)(lift.liftF)
  def ilift2[G[_]](implicit lift: IsoK[F, G], fk: InvariantK[T[*[_], A, B]]): T[G, A, B]                        =
    fk.imapK(tf)(lift.tof)(lift.fromF)
  def unlift2[G[_]](implicit unlift: Unlift[F, G], G: Functor[G], fk: InvariantK[T[*[_], A, B]]): G[T[G, A, B]] =
    G.map(unlift.unlift)(backf => fk.imapK(tf)(unlift.liftF)(backf))
}
