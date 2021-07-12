package tofu.doobie.instances

import cats.data.{Kleisli, ReaderT}
import cats.effect.{Effect, IO, SyncEffect}
import cats.effect.syntax.effect._
import cats.effect.syntax.syncEffect._
import doobie.ConnectionIO
import doobie.free.connection.AsyncConnectionIO
import tofu.WithProvide
import tofu.doobie.{ConnectionCIO, ConnectionRIO, LiftConnectionIO}
import tofu.lift.Lift

private[instances] trait DoobieInstances {
  final def liftToConnectionIOViaIO[F[_]: Lift[*[_], IO]]: LiftToConnectionIOViaIO[F] = new LiftToConnectionIOViaIO

  final def liftEffectToConnectionIO[F[_]: Effect]: LiftEffectToConnectionIO[F] = new LiftEffectToConnectionIO

  final def liftSyncEffectToConnectionIO[F[_]: SyncEffect]: LiftSyncEffectToConnectionIO[F] =
    new LiftSyncEffectToConnectionIO

  final def liftToConnectionRIO[F[_], R](implicit L: Lift[F, ConnectionIO]): LiftToConnectionRIO[F, R] =
    new LiftToConnectionRIO

  final def liftProvideToConnectionRIO[F[_], G[_], R](implicit
      WP: WithProvide[G, F, R],
      L: Lift[F, ConnectionIO]
  ): LiftProvideToConnectionRIO[F, G, R] = new LiftProvideToConnectionRIO

  final def liftConnectionIOToConnectionCIO[F[_]]: LiftConnectionIOToConnectionCIO[F] =
    new LiftConnectionIOToConnectionCIO
}

final class LiftToConnectionIOViaIO[F[_]](implicit L: Lift[F, IO]) extends Lift[F, ConnectionIO] {
  def lift[A](fa: F[A]): ConnectionIO[A] = AsyncConnectionIO.liftIO(L.lift(fa))
}

final class LiftEffectToConnectionIO[F[_]: Effect] extends Lift[F, ConnectionIO] {
  def lift[A](fa: F[A]): ConnectionIO[A] = AsyncConnectionIO.liftIO(fa.toIO)
}

final class LiftSyncEffectToConnectionIO[F[_]: SyncEffect] extends Lift[F, ConnectionIO] {
  def lift[A](fa: F[A]): ConnectionIO[A] = fa.runSync[ConnectionIO]
}

final class LiftToConnectionRIO[F[_], R](implicit L: Lift[F, ConnectionIO]) extends Lift[F, ConnectionRIO[R, *]] {
  def lift[A](fa: F[A]): ConnectionRIO[R, A] = ReaderT.liftF(L.lift(fa))
}

final class LiftProvideToConnectionRIO[F[_], G[_], R](implicit WP: WithProvide[G, F, R], L: Lift[F, ConnectionIO])
    extends Lift[G, ConnectionRIO[R, *]] {
  def lift[A](fa: G[A]): ConnectionRIO[R, A] = ReaderT(ctx => L.lift(WP.runContext(fa)(ctx)))
}

final class LiftConnectionIOToConnectionCIO[F[_]] extends LiftConnectionIO[ConnectionCIO[F, *]] {
  def lift[A](fa: ConnectionIO[A]): ConnectionCIO[F, A] = Kleisli(k => k(fa))
}
