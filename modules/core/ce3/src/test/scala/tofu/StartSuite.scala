package tofu

import cats.Applicative
import cats.data.*
import cats.effect.{Concurrent, IO}
import cats.effect.kernel.{Fiber, MonadCancelThrow, Outcome}
import tofu.concurrent.{MakeAgent, MakeSerialAgent}
import tofu.syntax.monadic.*
import tofu.syntax.start.*

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

  def fireForDataF[F[_]: Applicative: Fire] = {
    Fire[ReaderT[F, String, _]]
    Fire[OptionT[F, _]]
    Fire[EitherT[F, String, _]]
    Fire[IorT[F, String, _]]
    Fire[WriterT[F, String, _]]
  }

  def raceForDataF[F[_]: Applicative: Race] = {
    Race[ReaderT[F, String, _]]
    Race[OptionT[F, _]]
    Race[EitherT[F, String, _]]
    Race[IorT[F, String, _]]
    Race[WriterT[F, String, _]]
  }

  private def withInterrupt[F[_], A](oa: Outcome[F, Throwable, A])(implicit F: MonadCancelThrow[F]): F[A] =
    oa.embed(F.canceled *> F.raiseError(new InterruptedException))

  def testStartSyntaxCheck[A, B, F[_]: Concurrent](fa: F[A], fb: F[B]): F[(A, B)] = {
    fa.racePair[F, B, Outcome[F, Throwable, _], Fiber[F, Throwable, _]](fb).flatMap {
      case Left((oa, eb))  => (withInterrupt(oa), eb.join.flatMap(withInterrupt(_))).tupled
      case Right((ea, ob)) => (ea.join.flatMap(withInterrupt(_)), withInterrupt(ob)).tupled
    }
  }
}
