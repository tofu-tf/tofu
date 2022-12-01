package tofu.control.impl
import cats.{Contravariant, Functor}

trait FunctorDelegate[F[_]] extends Functor[F] {
  val F: Functor[F]

  final override def map[A, B](fa: F[A])(f: A => B): F[B]                                                     = F.map(fa)(f)
  final override def widen[A, B >: A](fa: F[A]): F[B]                                                         = F.widen(fa)
  final override def lift[A, B](f: A => B): F[A] => F[B]                                                      = F.lift(f)
  final override def void[A](fa: F[A]): F[Unit]                                                               = F.void(fa)
  final override def fproduct[A, B](fa: F[A])(f: A => B): F[(A, B)]                                           = F.fproduct(fa)(f)
  final override def as[A, B](fa: F[A], b: B): F[B]                                                           = F.as(fa, b)
  final override def tupleLeft[A, B](fa: F[A], b: B): F[(B, A)]                                               = F.tupleLeft(fa, b)
  final override def tupleRight[A, B](fa: F[A], b: B): F[(A, B)]                                              = F.tupleRight(fa, b)
  final override def compose[G[_]](implicit G: Functor[G]): Functor[λ[α => F[G[α]]]]                          = F.compose[G]
  final override def composeContravariant[G[_]](implicit G: Contravariant[G]): Contravariant[λ[α => F[G[α]]]] =
    F.composeContravariant[G]
}
