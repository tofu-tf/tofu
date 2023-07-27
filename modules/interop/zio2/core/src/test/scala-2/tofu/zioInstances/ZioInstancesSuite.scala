package tofu.zioInstances

import glass.{Contains, Extract}
import tofu._
import tofu.lift.{Lift, Unlift, UnliftIO, UnsafeExecFuture}
import tofu.zioInstances.implicits._
import zio._

import scala.annotation.nowarn

object ZioInstancesSuite {

  def summonZioInstances[E1, E2: * Extract E1, R1: Tag, R2: Tag: * Contains R1](): Unit = {
    implicitly[Lift[IO[E1, *], ZIO[R1, E1, *]]]
    implicitly[Lift[URIO[R1, *], ZIO[R1, E1, *]]]

    implicitly[Unlift[IO[E1, *], ZIO[R1, E1, *]]]
    implicitly[Unlift[ZIO[R1, E1, *], ZIO[R2, E1, *]]]

    implicitly[UnliftIO[RIO[R1, *]]]
    implicitly[UnsafeExecFuture[RIO[R1, *]]]

    implicitly[ErrorsTo[IO[E1, *], IO[E1, *], E1]]
    implicitly[ErrorsTo[ZIO[R1, E1, *], ZIO[R1, E1, *], E1]]

    implicitly[ErrorsTo[IO[E1, *], UIO, E1]]
    implicitly[ErrorsTo[ZIO[R1, E1, *], URIO[R1, *], E1]]

    implicitly[ErrorsTo[IO[E1, *], IO[E2, *], E1]]
    implicitly[ErrorsTo[ZIO[R1, E1, *], ZIO[R1, E2, *], E1]]

    implicitly[R1 In RIO[R1, *]]
    implicitly[WithRun[RIO[R1, *], Task, R1]]

    implicitly[R1 In RIO[R1, *]]
    implicitly[WithRun[URIO[R1, *], UIO, R1]]

    implicitly[R1 In RIO[R1, *]]
    implicitly[WithRun[ZIO[R1, E1, *], IO[E1, *], R1]]
    ()
  }

  @nowarn("cat=unused-params")
  def summonZioInstances[E, Env: Tag, Ctx: Tag](): Unit = {
    implicitly[WithRun[ZIO[Env with Ctx, E, *], ZIO[Env, E, *], Ctx]]
    implicitly[WithRun[ZIO[Ctx with Env, E, *], ZIO[Env, E, *], Ctx]]
    implicitly[WithLocal[ZIO[Env with Ctx, E, *], Ctx]]
    implicitly[WithLocal[ZIO[Ctx with Env, E, *], Ctx]]
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

    class RaiseDependency[F[_]: Raise[*[_], RuntimeException]]
    class ErrorsDependency[F[_]: Errors[*[_], RuntimeException]]

    new SyncDependency[Task]
    new RaiseDependency[Task]
    new ErrorsDependency[Task]

    implicitly[Throws[Task]]
  }

}
