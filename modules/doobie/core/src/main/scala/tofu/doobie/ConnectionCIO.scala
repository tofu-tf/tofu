package tofu.doobie

import cats.data.Kleisli
import cats.~>
import doobie.ConnectionIO
import tofu.kernel.types.AnyK

object ConnectionCIO {
  trait Cont[F[_]] extends (ConnectionIO ~> F)
  object Cont {
    private val liftConnectionIOToConnectionCIOAny: LiftConnectionIO[ConnectionCIO[AnyK, *]]                =
      new LiftConnectionIO[ConnectionCIO[AnyK, *]] {
        def lift[A](ca: ConnectionIO[A]): ConnectionCIO[AnyK, A] = Kleisli(k => k(ca))
      }
    @inline final implicit def liftConnectionIOToConnectionCIO[F[_]]: LiftConnectionIO[ConnectionCIO[F, *]] =
      liftConnectionIOToConnectionCIOAny.asInstanceOf[LiftConnectionIO[ConnectionCIO[F, *]]]
  }
}
