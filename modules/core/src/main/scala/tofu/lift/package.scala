package tofu

import scala.concurrent.Future

import cats.effect.IO

package object lift {
  type UnliftIO[F[_]]         = Unlift[IO, F]
  type UnsafeExecFuture[F[_]] = Unlift[Future, F]
}
