package tofu.zioInstances

import tofu.{HasContextRun, WithRun}
import tofu.lift.{Lift, Unlift}
import tofu.optics.Contains
import tofu.zioInstances.implicits._
import zio.{IO, RIO, Task, UIO, URIO, ZIO}

object ZioInstancesSuite {

  def summonZioInstances[E, R1, R2: * Contains R1](): Unit = {
    implicitly[Lift[IO[E, *], ZIO[R1, E, *]]]
    implicitly[Lift[URIO[R1, *], ZIO[R1, E, *]]]
    implicitly[Unlift[IO[E, *], ZIO[R1, E, *]]]
    implicitly[Unlift[ZIO[R1, E, *], ZIO[R2, E, *]]]
    implicitly[HasContextRun[RIO[R1, *], Task, R1]]
    implicitly[WithRun[RIO[R1, *], Task, R1]]
    implicitly[HasContextRun[URIO[R1, *], UIO, R1]]
    implicitly[WithRun[URIO[R1, *], UIO, R1]]
    implicitly[HasContextRun[ZIO[R1, E, *], IO[E, *], R1]]
    implicitly[WithRun[ZIO[R1, E, *], IO[E, *], R1]]
    ()
  }

}
