package tofu

import org.slf4j.LoggerFactory
import tofu.logging.Logging.loggerForService
import tofu.logging.impl.{UIOZLogging, URIOZLoggingImpl}
import zio.{UIO, URIO}

import scala.reflect.ClassTag

package object logging {
  type ZLogs[R]    = Logs[UIO, URIO[R, *]]
  type ZLogging[R] = Logging[URIO[R, *]]

  private[logging] object ZLogsOps {
    val uio: ZLogs[Any] = new ZLogs[Any] {
      def forService[Svc: ClassTag]: UIO[Logging[UIO]] =
        UIO.effectTotal(new UIOZLogging(loggerForService[Svc]))
      def byName(name: String): UIO[Logging[UIO]] = UIO.effectTotal(new UIOZLogging(LoggerFactory.getLogger(name)))
    }

    def withContext[R: Loggable]: ZLogs[R] = new ZLogs[R] {
      def forService[Svc: ClassTag]: UIO[ZLogging[R]] =
        UIO.effectTotal(new URIOZLoggingImpl(loggerForService[Svc]))
      override def byName(name: String): UIO[ZLogging[R]] =
        UIO.effectTotal(new URIOZLoggingImpl[R](LoggerFactory.getLogger(name)))
    }
  }

  val ZLogs: ZLogsOps.type = ZLogsOps
}
