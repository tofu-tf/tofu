package tofu

import cats.effect.{Async, Concurrent, ConcurrentEffect, Effect}
import tofu.lift.Unlift
import tofu.syntax.unlift._

object UnliftSuite {

  private def needsEffect[G[_]: Effect]: G[Unit] = Effect[G].unit

  private def needsConcurrentEffect[G[_]: ConcurrentEffect]: G[Unit] = ConcurrentEffect[G].unit

  def checkUnliftEffectSyntax[F[_]: Effect, G[_]: Async](implicit U: Unlift[F, G]): Unit = {
    Unlift[F, G].effect: G[Effect[G]]
    Unlift[F, G].effectWith(implicit E => needsEffect[G])
    ()
  }

  def checkUnliftCESyntax[F[_]: ConcurrentEffect, G[_]: Concurrent](implicit U: Unlift[F, G]): Unit = {
    Unlift[F, G].concurrentEffect: G[ConcurrentEffect[G]]
    Unlift[F, G].concurrentEffectWith(implicit CE => needsConcurrentEffect[G])
    ()
  }

}
