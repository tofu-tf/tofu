package tofu.syntax.doobie.log

import doobie.util.log._
import tofu.doobie.log.LogHandlerF
import tofu.doobie.log.instances._
import tofu.kernel.types.AnyK
import tofu.logging.Logging

object handler {
  implicit class MkLogHandlerF(private val lhf: LogHandlerF.type) extends AnyVal {
    def loggable[F[_]: Logging.Make](level: Logging.Level): LogHandlerF[F] = { evt =>
      val logging: Logging[F] = Logging.Make[F].forService[LogHandlerF[AnyK]]
      evt match {
        case _: Success           => logging.write(level, "{}", evt)
        case e: ProcessingFailure => logging.errorCause("{}", e.failure, evt)
        case e: ExecFailure       => logging.errorCause("{}", e.failure, evt)
      }
    }
  }
}
