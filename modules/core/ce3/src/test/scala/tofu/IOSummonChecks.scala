package tofu

import cats.data.ReaderT
import cats.effect.{IO}

class IOSummonChecks {
  implicitly[Fire[IO]]
  implicitly[Start[IO]]
  implicitly[Race[IO]]
  implicitly[Errors[IO, Throwable]]
  implicitly[BoundedParallel[IO]]

  implicitly[Fire[ReaderT[IO, Unit, _]]]
  implicitly[Start[ReaderT[IO, Unit, _]]]
  implicitly[Race[ReaderT[IO, Unit, _]]]
  implicitly[Errors[ReaderT[IO, Unit, _], Throwable]]
}
