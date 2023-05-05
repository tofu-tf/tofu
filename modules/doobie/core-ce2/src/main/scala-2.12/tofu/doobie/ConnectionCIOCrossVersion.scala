package tofu.doobie

import cats.Functor

import tofu.kernel.types.Perform
import tofu.PerformOf
import tofu.PerformVia

trait ConnectionCIOCrossVersion {
  // A hint to overcome 2.12 incompentence in implicit resolution
  @inline final implicit def readerPerformer[F[_]: Functor, E](implicit
      FP: Perform[F, E]
  ): Perform[ConnectionCIO[F, *], E] =
    PerformVia.performReader[F, PerformOf.ExitCont[E, *], ConnectionCIO.Cont[F], Unit]
}
