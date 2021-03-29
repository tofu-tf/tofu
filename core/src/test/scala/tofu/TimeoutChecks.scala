package tofu

import cats.data.ReaderT
import cats.effect.IO
import cats.effect.Temporal

object TimeoutChecks {
  def checks(implicit t: Temporal[IO]) = {
    type T[A] = ReaderT[IO, String, A]
    ContextShift[T]
    Temporal[T]

    Timeout[T]
    Timeout[IO]
  }
}
