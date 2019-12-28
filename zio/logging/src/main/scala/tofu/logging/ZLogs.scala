package tofu.logging
import org.slf4j.LoggerFactory
import tofu.logging.Logging.loggerForService
import tofu.logging.impl.{UIOZLogging, URIOZLoggingImpl}
import zio.{UIO, URIO}

import scala.reflect.ClassTag

trait ZLogs[R] extends Logs[UIO, URIO[R, *]]

object ZLogs {
  val uio: Logs[UIO, UIO] = new Logs[UIO, UIO] {
    def forService[Svc: ClassTag]: UIO[Logging[UIO]] =
      UIO.effectTotal(new UIOZLogging(loggerForService[Svc]))
    def byName(name: String): UIO[Logging[UIO]] = UIO.effectTotal(new UIOZLogging(LoggerFactory.getLogger(name)))
  }

  def withContext[R: Loggable]: Logs[UIO, URIO[R, *]] = new Logs[UIO, URIO[R, *]] {
    def forService[Svc: ClassTag]: UIO[Logging[URIO[R, *]]] =
      UIO.effectTotal(new URIOZLoggingImpl(loggerForService[Svc]))
    override def byName(name: String): UIO[Logging[URIO[R, *]]] =
      UIO.effectTotal(new URIOZLoggingImpl[R](LoggerFactory.getLogger(name)))
  }
}
