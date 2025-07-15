package tofu.time

import cats.Monad
import cats.data.*
import cats.effect.{IO, Temporal}

class TimeSuite {

  def summon[F[_]: Temporal] = {
    Clock[F]
    Sleep[F]
  }

  def timeout[F[_]: Temporal] = {
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
