package tofu.time

import cats.data.ReaderT
import cats.effect.{IO, Temporal}

import tofu.compat.unused

@unused
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
