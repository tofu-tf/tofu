package tofu.doobie

import doobie.ConnectionIO
import tofu.lift.Lift

object DoobieInstancesSuite {

  def summonLiftConnectionIO[R, F[_]](): Any = {
    LiftConnectionIO[ConnectionIO]
    LiftConnectionIO[ConnectionCIO[F, *]]
  }

  def summonLiftToConnectionCIO[F[_]](): Any = {
    Lift[F, ConnectionCIO[F, *]]
  }

}
