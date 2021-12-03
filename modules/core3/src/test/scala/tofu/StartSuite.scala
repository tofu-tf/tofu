package tofu

import cats.effect.kernel.{MonadCancelThrow, Outcome}
import cats.effect.{Concurrent, IO}
import tofu.concurrent.{MakeAgent, MakeSerialAgent}
import tofu.syntax.monadic._
import tofu.syntax.start._

object StartSuite {
  def summonInstancesForConcurrent[F[_]: Concurrent] = {
    Fire[F]
    Start[F]
    Race[F]
    MakeAgent[IO, IO]
  }

  def summonInstancesForIO = {
    Fire[IO]
    Start[IO]
    Race[IO]
    MakeAgent[IO, IO]
    MakeSerialAgent[IO, IO]
  }

  private def withInterrupt[F[_], A](oa: Outcome[F, Throwable, A])(implicit F: MonadCancelThrow[F]): F[A] =
    oa.embed(F.canceled *> F.raiseError(new InterruptedException))

  def testStartSyntaxCheck[A, B, F[_]: Concurrent](fa: F[A], fb: F[B]): F[(A, B)] = {
    fa.racePair(fb).flatMap {
      case Left((oa, eb))  => (withInterrupt(oa), eb.join.flatMap(withInterrupt(_))).tupled
      case Right((ea, ob)) => (ea.join.flatMap(withInterrupt(_)), withInterrupt(ob)).tupled
    }
  }
}
