package tofu.interop

import cats.Apply
import cats.data.ReaderT
import cats.effect.ConcurrentEffect
import tofu.lift.Unlift

abstract class ContextConcurrentEffect[F[_]] {
  type Base[_]

  implicit def concurrentEffect: ConcurrentEffect[Base]
  implicit def unlift: Unlift[Base, F]
  implicit def apply: Apply[F]
}

object ContextConcurrentEffect {
  final class Impl[F[_], B[_]](implicit
      val concurrentEffect: ConcurrentEffect[B],
      val unlift: Unlift[B, F],
      val apply: Apply[F]
  ) extends ContextConcurrentEffect[F] {
    final type Base[A] = B[A]
  }

  implicit def resolveReaderTConcurrentEffect[F[_]: ConcurrentEffect, C]: Impl[ReaderT[F, C, *], F] =
    new Impl[ReaderT[F, C, *], F]
}
