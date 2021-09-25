package tofu.doobie.instances

import cats.effect.{Effect, IO, SyncEffect}
import doobie.ConnectionIO
import tofu.WithProvide
import tofu.lift.Lift

object implicits extends DoobieImplicits1

private[instances] trait DoobieImplicits1 extends DoobieImplicits2 {
  @inline final implicit def liftToConnectionIOViaIOImplicit[F[_]: Lift[*[_], IO]]: LiftToConnectionIOViaIO[F] =
    liftToConnectionIOViaIO
}

private[instances] trait DoobieImplicits2 extends DoobieImplicits3 {
  @inline final implicit def liftEffectToConnectionIOImplicit[F[_]: Effect]: LiftEffectToConnectionIO[F]             =
    liftEffectToConnectionIO

  @inline final implicit def liftSyncEffectToConnectionIOImplicit[F[_]: SyncEffect]: LiftSyncEffectToConnectionIO[F] =
    liftSyncEffectToConnectionIO

  @inline final implicit def liftToConnectionRIOImplicit[F[_], R](implicit
      L: Lift[F, ConnectionIO]
  ): LiftToConnectionRIO[F, R] = liftToConnectionRIO
}

private[instances] trait DoobieImplicits3 {
  @inline final implicit def liftProvideToConnectionRIOImplicit[F[_], G[_], R](implicit
      WP: WithProvide[G, F, R],
      L: Lift[F, ConnectionIO]
  ): LiftProvideToConnectionRIO[F, G, R] = liftProvideToConnectionRIO
}
