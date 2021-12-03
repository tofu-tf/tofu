package tofu

import cats.effect.{Concurrent, ContextShift, IO}
import tofu.concurrent.{MakeAgent, MakeSerialAgent}
import tofu.syntax.monadic._
import tofu.syntax.start._

import scala.annotation.nowarn

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
    MakeAgent[IO, IO]
    MakeSerialAgent[IO, IO]
  }

  def testStartSyntaxCheck[A, B, F[_]: Concurrent](fa: F[A], fb: F[B]): F[(A, B)] = {
    fa.racePair(fb).flatMap {
      case Left((a, eb))  => eb.join.tupleLeft(a)
      case Right((ea, b)) => ea.join.tupleRight(b)
    }
  }
}
