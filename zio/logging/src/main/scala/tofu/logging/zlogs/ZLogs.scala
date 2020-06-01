package tofu.logging.zlogs

import izumi.reflect.Tag
import org.slf4j.LoggerFactory
import tofu.logging.Logging.loggerForService
import tofu.logging.zlogs.impl.{UIOZLogging, URIOZLoggingImpl}
import tofu.logging.{Loggable, Logging}
import zio.{Has, UIO, ULayer, ZIO, ZLayer}

import scala.reflect.ClassTag
import zio.interop.catz._

object ZLogs {
  val uio: ZLogs[Any] = new ZLogs[Any] {
    def forService[Svc: ClassTag]: UIO[Logging[UIO]] =
      UIO.effectTotal(new UIOZLogging(loggerForService[Svc]))
    def byName(name: String): UIO[Logging[UIO]]      = UIO.effectTotal(new UIOZLogging(LoggerFactory.getLogger(name)))
  }

  def withContext[R: Loggable]: ZLogs[R] = new ZLogs[R] {
    def forService[Svc: ClassTag]: UIO[ZLogging[R]]     =
      UIO.effectTotal(new URIOZLoggingImpl(loggerForService[Svc]))
    override def byName(name: String): UIO[ZLogging[R]] =
      UIO.effectTotal(new URIOZLoggingImpl[R](LoggerFactory.getLogger(name)))
  }

  val build = new ZioHasBuilder[Any](Loggable.empty)

  def named[R: Tag](logs: ZLogs[R], name: String): ULayer[ZLog[R]] =
    ZLayer.fromEffect(logs.byName(name))

  def service[R: Tag, S: ClassTag](logs: ZLogs[R]): ULayer[ZLog[R]] =
    ZLayer.fromEffect(logs.forService[S])

  def access[R <: ZLog[U] with U: Tag, U <: Has[_]: Tag]: ZLogging[R] =
    Logging.loggingRepresentable.embed(ZIO.access[ZLog[U]](_.get[ZLogging[U]].widen))
}
