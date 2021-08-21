package tofu.logging.zlogs

import scala.reflect.ClassTag
import izumi.reflect.Tag
import org.slf4j.LoggerFactory
import tofu.logging.zlogs.impl.{UIOZLogging, URIOZLoggingImpl}
import tofu.logging.{Loggable, Logging, ServiceLogging}
import zio.interop.catz._
import zio.{Has, UIO, ULayer, ZIO, ZLayer}

import scala.annotation.nowarn

object ZLogs {

  val uio: ZLogs[Any] = new ZLogs[Any] {
    def byName(name: String): UIO[Logging[UIO]] = UIO.effectTotal(new UIOZLogging(LoggerFactory.getLogger(name)))
  }

  def withContext[R: Loggable]: ZLogs[R] = new ZLogs[R] {
    override def byName(name: String): UIO[ZLogging[R]] =
      UIO.effectTotal(new URIOZLoggingImpl[R](LoggerFactory.getLogger(name)))
  }

  val build = new ZioHasBuilder[Any](Loggable.empty)

  @nowarn("cat=unused-params") //Tag
  def named[R: Tag](logs: ZLogs[R], name: String): ULayer[ZLog[R]] =
    ZLayer.fromEffect(logs.byName(name))

  @nowarn("cat=unused-params") //Tag
  def service[R: Tag, S: ClassTag](logs: ZLogs[R]): ULayer[ZLog[R]] =
    ZLayer.fromEffect(logs.forService[S])

  @nowarn("cat=unused-params") //Tag
  def access[R <: ZLog[U] with U: Tag, U <: Has[_]: Tag]: ZLogging[R] =
    Logging.loggingRepresentable.embed(ZIO.access[ZLog[U]](_.get[ZLogging[U]].widen))
}
