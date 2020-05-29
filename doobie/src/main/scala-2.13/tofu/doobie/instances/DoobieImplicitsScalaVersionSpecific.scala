package tofu.doobie.instances

import doobie.ConnectionIO
import tofu.HasProvide
import tofu.lift.Lift

trait DoobieImplicitsScalaVersionSpecific {
  @inline final implicit def liftProvideToConnectionRIOImplicit[F[_], G[_], R](implicit
      HP: HasProvide[G, F, R],
      L: Lift[F, ConnectionIO]
  ): LiftProvideToConnectionRIO[F, G, R] = liftProvideToConnectionRIO
}
