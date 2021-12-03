package tofu
package syntax

import cats.FlatMap
import cats.effect.Blocker

object scoped extends ScopedSyntax {
  def withBlocker[F[_]: BlockExec: FlatMap, A](f: Blocker => F[A]): F[A] =
    withScopedEc[Scoped.Blocking](ec => f(Blocker.liftExecutionContext(ec)))
}
