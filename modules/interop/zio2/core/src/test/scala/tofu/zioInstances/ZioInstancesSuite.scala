package tofu.zioInstances

import glass.{Contains, Extract}
import tofu._
import tofu.lift.{Lift, Unlift, UnliftIO, UnsafeExecFuture}
import tofu.zioInstances.implicits._
import zio._

import scala.annotation.nowarn

object ZioInstancesSuite {

  def summonZioInstances[E1, E2: Extract[_, E1], R1: Tag, R2: Tag: Contains[_, R1]](): Unit = {
    implicitly[Lift[IO[E1, _], ZIO[R1, E1, _]]]
    implicitly[Lift[URIO[R1, _], ZIO[R1, E1, _]]]

    implicitly[Unlift[IO[E1, _], ZIO[R1, E1, _]]]
    implicitly[Unlift[ZIO[R1, E1, _], ZIO[R2, E1, _]]]

    implicitly[UnliftIO[RIO[R1, _]]]
    implicitly[UnsafeExecFuture[RIO[R1, _]]]

    implicitly[ErrorsTo[IO[E1, _], IO[E1, _], E1]]
    implicitly[ErrorsTo[ZIO[R1, E1, _], ZIO[R1, E1, _], E1]]

    implicitly[ErrorsTo[IO[E1, _], UIO, E1]]
    implicitly[ErrorsTo[ZIO[R1, E1, _], URIO[R1, _], E1]]

    implicitly[ErrorsTo[IO[E1, _], IO[E2, _], E1]]
    implicitly[ErrorsTo[ZIO[R1, E1, _], ZIO[R1, E2, _], E1]]

    implicitly[R1 In RIO[R1, _]]
    implicitly[WithRun[RIO[R1, _], Task, R1]]

    implicitly[R1 In RIO[R1, _]]
    implicitly[WithRun[URIO[R1, _], UIO, R1]]

    implicitly[R1 In RIO[R1, _]]
    implicitly[WithRun[ZIO[R1, E1, _], IO[E1, _], R1]]
    ()
  }

  @nowarn("cat=unused-params")
  def summonZioInstances[E, Env: Tag, Ctx: Tag](): Unit = {
    implicitly[WithRun[ZIO[Env with Ctx, E, _], ZIO[Env, E, _], Ctx]]
    implicitly[WithRun[ZIO[Ctx with Env, E, _], ZIO[Env, E, _], Ctx]]
    implicitly[WithLocal[ZIO[Env with Ctx, E, _], Ctx]]
    implicitly[WithLocal[ZIO[Ctx with Env, E, _], Ctx]]
    ()
  }

  @nowarn("cat=unused-params")
  def taskAmbiguity: Any = {
    import cats.effect.Sync
    import tofu.Raise
    import tofu.zioInstances.implicits.rioTofuImplicit
    import zio.Task
    import zio.interop.catz.asyncInstance

    class SyncDependency[F[_]: Sync]

    class RaiseDependency[F[_]: ({ type L[x[_]] = Raise[x, RuntimeException] })#L]
    class ErrorsDependency[F[_]: ({ type L[x[_]] = Errors[x, RuntimeException] })#L]

    new SyncDependency[Task]
    new RaiseDependency[Task]
    new ErrorsDependency[Task]

    implicitly[Throws[Task]]
  }

}
