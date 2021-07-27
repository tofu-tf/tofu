package tofu

import cats.effect.IO

import scala.concurrent.Future

package object lift {
  type UnliftIO[F[_]]         = Unlift[IO, F]
  type UnsafeExecFuture[F[_]] = Unlift[Future, F]
}
