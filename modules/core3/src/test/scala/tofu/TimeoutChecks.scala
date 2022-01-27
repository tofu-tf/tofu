package tofu

import cats.data.ReaderT
import cats.effect.{IO, Temporal}

object TimeoutChecks {
  def checks(implicit cs: Temporal[IO]) = {
    type T[A] = ReaderT[IO, String, A]
    Temporal[T]

    Timeout[T]
    Timeout[IO]
  }
}
