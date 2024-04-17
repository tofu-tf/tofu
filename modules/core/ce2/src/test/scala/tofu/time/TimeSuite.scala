package tofu.time

import cats.data.ReaderT
import cats.effect.{Concurrent, IO, Timer}

import scala.concurrent.ExecutionContext
import cats.effect.ContextShift
import tofu.compat.unused

@unused
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

  def readerF[F[_]: Clock: Sleep] = {
    Clock[ReaderT[F, Unit, _]]
    Sleep[ReaderT[F, Unit, _]]
  }
}
