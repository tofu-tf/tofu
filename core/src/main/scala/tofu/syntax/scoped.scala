package tofu
package syntax

import cats.effect.Blocker
import cats.FlatMap

object scoped extends ScopedSyntax {
  def withBlocker[F[_]: BlockExec: FlatMap, A](f: Blocker => F[A]): F[A] =
    withScopedEc[Scoped.Blocking](ec => f(Blocker.liftExecutionContext(ec)))
}
