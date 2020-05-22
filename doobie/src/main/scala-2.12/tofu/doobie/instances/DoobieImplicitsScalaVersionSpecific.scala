package tofu.doobie.instances

import doobie.ConnectionIO
import tofu.lift.Lift
import tofu.WithProvide

// Scala 2.12 cannot derive HasProvide[G, F, R] if F is not specified
trait DoobieImplicitsScalaVersionSpecific {
  @inline final implicit def liftProvideToConnectionRIOImplicit[F[_], G[_], R](implicit
      WP: WithProvide[G, F, R],
      L: Lift[F, ConnectionIO]
  ): LiftProvideToConnectionRIO[F, G, R] = liftProvideToConnectionRIO
}
