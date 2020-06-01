package tofu.control.impl
import cats.{Applicative, ContravariantMonoidal}

trait ApplicativeDelegate[F[_]] extends Applicative[F] with ApplyDelegate[F] {
  val F: Applicative[F]

  final def pure[A](x: A): F[A]                                                                                        = F.pure(x)
  final override def unit: F[Unit]                                                                                     = F.unit
  final override def replicateA[A](n: Int, fa: F[A]): F[List[A]]                                                       = F.replicateA(n, fa)
  final override def compose[G[_]: Applicative]: Applicative[λ[α => F[G[α]]]]                                          = F.compose[G]
  final override def composeContravariantMonoidal[G[_]: ContravariantMonoidal]: ContravariantMonoidal[λ[α => F[G[α]]]] =
    F.composeContravariantMonoidal[G]
  final override def unlessA[A](cond: Boolean)(f: => F[A]): F[Unit]                                                    = F.unlessA(cond)(f)
  final override def whenA[A](cond: Boolean)(f: => F[A]): F[Unit]                                                      = F.whenA(cond)(f)
  final override def point[A](a: A): F[A]                                                                              = F.point(a)
}
