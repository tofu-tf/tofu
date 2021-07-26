package tofu

import cats.effect.Concurrent
import cats.effect.IO
import tofu.syntax.start._
import tofu.syntax.monadic._

object StartSuite {
  def summonInstancesForConcurrent[F[_]: Concurrent] = {
    Fire[F]
    Start[F]
    Race[F]
  }

  def summonInstancesForIO = {
    Fire[IO]
    Start[IO]
    Race[IO]
  }

  def testStartSyntaxCheck[A, B, F[_]: Concurrent](fa: F[A], fb: F[B]): F[(A, B)] = {
    fa.racePair(fb).flatMap {
      case Left((oa, eb))  => (oa.embedNever, eb.join.flatMap(_.embedNever)).tupled
      case Right((ea, ob)) => (ea.join.flatMap(_.embedNever), ob.embedNever).tupled
    }
  }
}
