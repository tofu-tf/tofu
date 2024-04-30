package tofu.logging

import eu.timepit.refined.api.RefType
import eu.timepit.refined.api.RefType.ops.toRefTypeOps

package object refined {
  implicit def deriveRefinedLoggable[T: Loggable, P, F[_, _]: RefType]: Loggable[F[T, P]] = {
    Loggable[T].contramap(_.unwrap)
  }
}
