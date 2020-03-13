package tofu.zioInstances

import tofu.{ErrorsTo, HasContextRun, RunContext, WithRun}
import tofu.lift.{Lift, Unlift}
import tofu.optics.{Contains, Extract}
import tofu.zioInstances.implicits._
import zio.{IO, RIO, Task, UIO, URIO, ZIO}

object ZioInstancesSuite {

  def summonZioInstances[E1, E2: * Extract E1, R1, R2: * Contains R1](): Unit = {
    implicitly[Lift[IO[E1, *], ZIO[R1, E1, *]]]
    implicitly[Lift[URIO[R1, *], ZIO[R1, E1, *]]]

    implicitly[Unlift[IO[E1, *], ZIO[R1, E1, *]]]
    implicitly[Unlift[ZIO[R1, E1, *], ZIO[R2, E1, *]]]

    implicitly[ErrorsTo[IO[E1, *], IO[E1, *], E1]]
    implicitly[ErrorsTo[ZIO[R1, E1, *], ZIO[R1, E1, *], E1]]

    implicitly[ErrorsTo[IO[E1, *], UIO, E1]]
    implicitly[ErrorsTo[ZIO[R1, E1, *], URIO[R1, *], E1]]

    implicitly[ErrorsTo[IO[E1, *], IO[E2, *], E1]]
    implicitly[ErrorsTo[ZIO[R1, E1, *], ZIO[R1, E2, *], E1]]

    implicitly[RunContext[RIO[R1, *]]]
    implicitly[HasContextRun[RIO[R1, *], Task, R1]]
    implicitly[WithRun[RIO[R1, *], Task, R1]]

    implicitly[RunContext[URIO[R1, *]]]
    implicitly[HasContextRun[URIO[R1, *], UIO, R1]]
    implicitly[WithRun[URIO[R1, *], UIO, R1]]

    implicitly[RunContext[ZIO[R1, E1, *]]]
    implicitly[HasContextRun[ZIO[R1, E1, *], IO[E1, *], R1]]
    implicitly[WithRun[ZIO[R1, E1, *], IO[E1, *], R1]]
    ()
  }

}
