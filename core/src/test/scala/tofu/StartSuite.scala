package tofu

import cats.effect.Concurrent
import scala.annotation.nowarn
import cats.effect.IO
import cats.effect.ContextShift

@nowarn("msg=parameter")
object StartSuite {
  def summonInstancesForConcurrent[F[_]: Concurrent] = {
    Fire[F]
    Start[F]
    Race[F]
  }

  def summonInstancesForIO(implicit cs: ContextShift[IO]) = {
    Fire[IO]
    Start[IO]
    Race[IO]
  }
}
