package tofu.syntax.doobie.log

import cats.Id
import doobie.util.log._
import tofu.doobie.log.LogHandlerF
import tofu.doobie.log.instances._
import tofu.kernel.types.AnyK
import tofu.logging.{Logging, Logs}

object handler {
  implicit class MkLogHandlerF(private val lhf: LogHandlerF.type) extends AnyVal {
    def loggable[F[_]: Logs.Universal](level: Logging.Level): LogHandlerF[F] = { evt =>
      val logging: Logging[F] = Logs[Id, F].forService[LogHandlerF[AnyK]]
      evt match {
        case _: Success           => logging.write(level, "{}", evt)
        case e: ProcessingFailure => logging.errorCause("{}", e.failure, evt)
        case e: ExecFailure       => logging.errorCause("{}", e.failure, evt)
      }
    }
  }
}
