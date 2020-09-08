package tofu.streams

import cats.{Applicative, Foldable, MonoidK}

trait Emits[F[_]] {

  val monoidK: MonoidK[F]
  val applicative: Applicative[F]

  def emits[C[_], A](as: C[A])(implicit C: Foldable[C]): F[A] =
    C.foldLeft(as, monoidK.empty[A])((acc, a) => monoidK.combineK(acc, applicative.pure(a)))
}

object Emits {

  implicit def instance[F[_]: MonoidK: Applicative]: Emits[F] =
    new Emits[F] {
      override val monoidK: MonoidK[F]         = implicitly
      override val applicative: Applicative[F] = implicitly
    }
}
