package tofu.zioInstances

import tofu.HasContextRun
import tofu.lift.{Lift, Unlift}
import tofu.optics.Contains
import tofu.zioInstances.implicits._
import zio.{IO, RIO, Task, URIO, ZIO}

object ZioInstancesSuit {

  def summonZioInstances[E, R1, R2: * Contains R1](): Unit = {
    implicitly[Lift[IO[E, *], ZIO[R1, E, *]]]
    implicitly[Lift[URIO[R1, *], ZIO[R1, E, *]]]
    implicitly[Unlift[IO[E, *], ZIO[R1, E, *]]]
    implicitly[Unlift[ZIO[R1, E, *], ZIO[R2, E, *]]]
    implicitly[HasContextRun[RIO[R1, *], Task, R1]]
    ()
  }

}
