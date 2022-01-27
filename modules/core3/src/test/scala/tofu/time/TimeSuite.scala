package tofu.time

import scala.annotation.nowarn

import cats.data.ReaderT
import cats.effect.{IO, Temporal}

@nowarn
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
    Clock[ReaderT[IO, Unit, *]]
    Sleep[ReaderT[IO, Unit, *]]
    Timeout[ReaderT[IO, Unit, *]]
  }
}
