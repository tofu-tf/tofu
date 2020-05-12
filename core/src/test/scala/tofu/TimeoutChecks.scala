package tofu

import cats.data.ReaderT
import cats.effect.{ContextShift, IO, Timer}

object TimeoutChecks {
  def checks(implicit cs: ContextShift[IO], t: Timer[IO]) = {
    type T[A] = ReaderT[IO, String, A]
    ContextShift[T]
    Timer[T]

    Timeout[T]
    Timeout[IO]
  }
}
