package tofu.syntax.doobie.log

import doobie.util.log._
import tofu.doobie.log.instances._
import tofu.kernel.types.AnyK
import tofu.logging.Logging

object handler {
  implicit class MkLogHandlerF(private val lhf: LogHandler.type) extends AnyVal {
    def loggable[F[_]: Logging.Make](level: Logging.Level): LogHandler[F] = { evt =>
      val logging: Logging[F] = Logging.Make[F].forService[LogHandler[AnyK]]
      evt match {
        case _: Success           => logging.write(level, "{}", evt)
        case e: ProcessingFailure => logging.errorCause("{}", e.failure, evt)
        case e: ExecFailure       => logging.errorCause("{}", e.failure, evt)
      }
    }
  }
}
