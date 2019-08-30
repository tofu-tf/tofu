package tofu
import cats.data.{Chain, ReaderT}
import tofu.config.ConfigErrors.Message

import scala.annotation.unchecked.uncheckedVariance

package object config {
  type Path = Vector[Key]
  type MessageList = Vector[(Path, Message)]
  type ConfigT[F[+_], +A] = ReaderT[F, ConfigTContext[F], A @uncheckedVariance]
}
