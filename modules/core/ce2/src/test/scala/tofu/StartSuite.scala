package tofu

import cats.data.*
import cats.effect.{Concurrent, ContextShift, Fiber, IO}
import cats.{Applicative, Id}
import tofu.concurrent.{MakeAgent, MakeSerialAgent}
import tofu.syntax.monadic.*
import tofu.syntax.start.*

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

  def testStartSyntaxCheck[A, B, F[_]: Concurrent](fa: F[A], fb: F[B]): F[(A, B)] =
    fa.racePair[F, B, Id, Fiber[F, _]](fb).flatMap {
      case Left((a, eb))  => eb.join.tupleLeft(a)
      case Right((ea, b)) => ea.join.tupleRight(b)
    }

}
