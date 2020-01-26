package tofu.logging.zlogs

import org.slf4j.LoggerFactory
import tofu.logging.{Loggable, Logging}
import tofu.logging.Logging.loggerForService
import tofu.logging.zlogs.impl.{UIOZLogging, URIOZLoggingImpl}
import zio.UIO
import scala.reflect.ClassTag

object ZLogs {
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
