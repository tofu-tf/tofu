package tofu.lift

import cats.Applicative
import cats.data.ReaderT

object UnliftSuite {

  def summonUnliftInstaces[F[_]: Applicative, R](): Unit = {
    implicitly[Lift[F, F]]
    implicitly[Unlift[F, F]]
    implicitly[Lift[F, ReaderT[F, R, *]]]
    implicitly[Unlift[F, ReaderT[F, R, *]]]
    ()
  }

}
