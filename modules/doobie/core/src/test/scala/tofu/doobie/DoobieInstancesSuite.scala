package tofu.doobie

import cats.Applicative
import cats.data.ReaderT
import cats.effect.{IO, SyncIO}
import doobie.ConnectionIO
import monix.eval.{Coeval, Task}
import monix.execution.Scheduler
import tofu.compat.unused
import tofu.doobie.instances.implicits._
import tofu.env.Env
import tofu.lift.Lift
import tofu.zioInstances.implicits._
import zio.interop.catz._

object DoobieInstancesSuite {

  def summonImplicitsViaLiftToIO[F[_]: Applicative, R](implicit L: Lift[F, IO]): Any = {
    Lift[F, ConnectionIO]
    Lift[F, ConnectionRIO[R, *]]
    Lift[ReaderT[F, R, *], ConnectionRIO[R, *]]
  }

  def summonCatsEffectImplicits[R](): Any = {
    Lift[SyncIO, ConnectionIO]
    Lift[SyncIO, ConnectionRIO[R, *]]
    Lift[ReaderT[SyncIO, R, *], ConnectionRIO[R, *]]

    Lift[IO, ConnectionIO]
    Lift[IO, ConnectionRIO[R, *]]
    Lift[ReaderT[IO, R, *], ConnectionRIO[R, *]]
  }

  def summonMonixImplicitsViaScheduler[R](implicit sc: Scheduler): Any = {
    Lift[Coeval, ConnectionIO]
    Lift[Coeval, ConnectionRIO[R, *]]
    Lift[ReaderT[Coeval, R, *], ConnectionRIO[R, *]]

    Lift[Task, ConnectionIO]
    Lift[Task, ConnectionRIO[R, *]]
    Lift[ReaderT[Task, R, *], ConnectionRIO[R, *]]

    Lift[Env[R, *], ConnectionRIO[R, *]]
  }

  def summonMonixImplicitsUnambiguously[R](implicit @unused sc: Scheduler, L: Lift[Task, IO]): Any = {
    Lift[Task, ConnectionIO]
    Lift[Task, ConnectionRIO[R, *]]
    Lift[ReaderT[Task, R, *], ConnectionRIO[R, *]]
  }

  def summonZioImplicits[R](): zio.Task[Any] =
    zio.Task.concurrentEffect.map { implicit CE =>
      Lift[zio.Task, ConnectionIO]
      Lift[zio.Task, ConnectionRIO[R, *]]
      Lift[zio.RIO[R, *], ConnectionRIO[R, *]]
    }

  def summonLiftConnectionIO[R, F[_]](): Any = {
    LiftConnectionIO[ConnectionIO]
    LiftConnectionIO[ConnectionRIO[R, *]]
    LiftConnectionIO[ConnectionCIO[F, *]]
  }

  def summonLiftToConnectionCIO[F[_]](): Any = {
    Lift[F, ConnectionCIO[F, *]]
  }

}
