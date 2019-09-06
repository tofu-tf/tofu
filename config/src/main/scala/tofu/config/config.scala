package tofu
import cats.data.ReaderT

package object config {
  type Path              = Vector[Key]
  type MessageList       = Vector[ConfigParseMessage]
  type ConfigT[F[_], A]  = ReaderT[F, ConfigTContext[F], A]
  type ConfigRaise[F[_]] = Raise[F, ConfigError]
  type ErrorsFail[F[_]]  = Errors[F, ConfigTContext.Fail.type]
}
