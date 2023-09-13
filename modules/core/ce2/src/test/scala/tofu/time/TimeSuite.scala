package tofu.time

import cats.data.ReaderT
import cats.effect.{Concurrent, IO, Timer}

import scala.annotation.nowarn
import scala.concurrent.ExecutionContext
import cats.effect.ContextShift

@nowarn
class TimeSuite {

  implicit val ioTimer: Timer[IO]     = IO.timer(ExecutionContext.global)
  implicit val ioCS: ContextShift[IO] = IO.contextShift(ExecutionContext.global)

  def summon[F[_]: Timer] = {
    Clock[F]
    Sleep[F]
  }

  def timeout[F[_]: Timer: Concurrent] = {
    Timeout[F]
  }

  def io = {
    Clock[IO]
    Sleep[IO]
    Timeout[IO]
  }

  def readerIO = {
    Clock[ReaderT[IO, Unit, _]]
    Sleep[ReaderT[IO, Unit, _]]
    Timeout[ReaderT[IO, Unit, _]]
  }
}
