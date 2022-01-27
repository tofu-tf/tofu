package tofu.logging.zlogs

import scala.annotation.nowarn
import scala.reflect.ClassTag

import izumi.reflect.Tag
import tofu.logging.impl.{ZUniversalContextLogging, ZUniversalLogging}
import tofu.logging.{Loggable, Logging}
import zio.interop.catz._
import zio.{Has, UIO, ULayer, ZIO, ZLayer}

object ZLogs {
  val uio: ZLogs[Any] = (name: String) => UIO.effectTotal(new ZUniversalLogging(name))

  def withContext[R: Loggable]: ZLogs[R] =
    (name: String) => UIO.effectTotal(new ZUniversalContextLogging(name, ZIO.environment[R]))

  val build = new ZioHasBuilder[Any](Loggable.empty)

  @nowarn("cat=unused-params") // Tag
  def named[R: Tag](logs: ZLogs[R], name: String): ULayer[ZLog[R]] =
    ZLayer.fromEffect(logs.byName(name))

  @nowarn("cat=unused-params") // Tag
  def service[R: Tag, S: ClassTag](logs: ZLogs[R]): ULayer[ZLog[R]] =
    ZLayer.fromEffect(logs.forService[S])

  @nowarn("cat=unused-params") // Tag
  def access[R <: ZLog[U] with U: Tag, U <: Has[_]: Tag]: ZLogging[R] =
    Logging.loggingRepresentable.embed(ZIO.access[ZLog[U]](_.get[ZLogging[U]].widen))
}
