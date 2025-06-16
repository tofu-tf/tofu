package tofu.time

import cats.Monad
import cats.data.*
import cats.effect.{Concurrent, ContextShift, IO, Timer}

import scala.concurrent.ExecutionContext

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
    TimeZone[IO]
  }

  def readerIO = {
    Clock[ReaderT[IO, Unit, _]]
    Sleep[ReaderT[IO, Unit, _]]
    Timeout[ReaderT[IO, Unit, _]]
    TimeZone[ReaderT[IO, Unit, _]]
  }

  def readerF[F[_]: Clock: Sleep] = {
    Clock[ReaderT[F, Unit, _]]
    Sleep[ReaderT[F, Unit, _]]
  }

  def dataF[F[_]: Monad: Clock: Sleep: TimeZone] = {
    Clock[WriterT[F, Unit, _]]
    Sleep[StateT[F, Unit, _]]
    TimeZone[OptionT[F, _]]
    Clock[EitherT[F, Unit, _]]
    Sleep[IorT[F, Unit, _]]
    TimeZone[ContT[F, Unit, _]]
    Clock[RWST[F, Unit, Unit, Unit, _]]
  }
}
