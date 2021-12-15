package tofu.interop

import cats.Apply
import cats.data.ReaderT
import cats.effect.Effect
import tofu.lift.Unlift

abstract class ContextEffect[F[_]] {
  type Base[_]

  implicit def effect: Effect[Base]
  implicit def unlift: Unlift[Base, F]
  implicit def apply: Apply[F]
}

object ContextEffect {
  final class Impl[F[_], B[_]](implicit val effect: Effect[B], val unlift: Unlift[B, F], val apply: Apply[F])
      extends ContextEffect[F] {
    final type Base[A] = B[A]
  }

  implicit def resolveReaderTEffect[F[_]: Effect, C]: Impl[ReaderT[F, C, *], F] = new Impl[ReaderT[F, C, *], F]
}
