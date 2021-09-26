package tofu

import cats.data.ReaderT
import cats.effect.{IO}

class IOSummonChecks {
  implicitly[Fire[IO]]
  implicitly[Start[IO]]
  implicitly[Race[IO]]
  implicitly[Errors[IO, Throwable]]

  implicitly[Fire[ReaderT[IO, Unit, *]]]
  implicitly[Start[ReaderT[IO, Unit, *]]]
  implicitly[Race[ReaderT[IO, Unit, *]]]
  implicitly[Errors[ReaderT[IO, Unit, *], Throwable]]
}
