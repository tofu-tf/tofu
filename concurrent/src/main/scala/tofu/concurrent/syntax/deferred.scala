package tofu.concurrent.syntax

import tofu.Restore
import tofu.syntax.handle._
import tofu.syntax.monadic._
import cats.Functor
import cats.effect.Deferred

object deferred {
  implicit class TofuDeferredSyntax[F[_], A](private val deferred: Deferred[F, A]) extends AnyVal {
    def tryComplete(a: A)(implicit FR: Restore[F], F: Functor[F]): F[Unit] = deferred.complete(a).restore.void
  }
}
