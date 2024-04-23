package tofu

import cats.Monad
import cats.data.*
import cats.effect.{ContextShift, IO, Timer}

object TimeoutChecks {
  def checks(implicit cs: ContextShift[IO], t: Timer[IO]) = {
    type T[A] = ReaderT[IO, String, A]
    ContextShift[T]
    Timer[T]

    Timeout[T]
    Timeout[IO]
    dataF[IO]
    readerF[IO]
  }

  private def readerF[F[_]: Monad: Timeout] = {
    type T[A] = ReaderT[F, String, A]
    Timeout[T]
  }

  private def dataF[F[_]: Timeout] = {
    Timeout[OptionT[F, _]]
    Timeout[EitherT[F, String, _]]
    Timeout[IorT[F, String, _]]
    Timeout[WriterT[F, String, _]]
  }
}
