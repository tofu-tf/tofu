package tofu
package syntax

import cats.FlatMap
import tofu.interop.Blocker

object scoped extends ScopedSyntax {
  def withBlocker[F[_]: BlockExec: FlatMap, A](f: Blocker[F] => F[A]): F[A] =
    withScopedEc[Scoped.Blocking](ec => f(Blocker(ec)))
}
