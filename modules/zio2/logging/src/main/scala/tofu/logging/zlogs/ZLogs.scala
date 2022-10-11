package tofu.logging.zlogs

import org.slf4j.{Logger, LoggerFactory}
import tofu.logging.impl.ZContextLogging
import tofu.logging.{Loggable, Logs}
import zio.{Tag, _}

import scala.annotation.unused

object ZLogs {

  private def make[R](f: Logger => ZLogging[R]) =
    new Logs[UIO, URIO[R, *]] {
      override def byName(name: String): UIO[ZLogging[R]] =
        ZIO.succeed(LoggerFactory.getLogger(name)).map(f)
    }

  def layerPlain: ULayer[ULogs] =
    ZLayer.succeed(make(new ZContextLogging(_, ZIO.unit)))

  def layerContextual[R: Loggable](implicit @unused RT: Tag[R]): ULayer[ZLogs[R]] =
    ZLayer.succeed(make(new ZContextLogging(_, ZIO.service[R])))

  val layerPlainWithContext: URLayer[ZLogContext.LoggedValuesExtractor, ULogs] = ZLayer(
    ZIO.serviceWith[ZLogContext.LoggedValuesExtractor](_.loggedValue)
      .map(ctxZIO => make(new ZContextLogging(_, ctxZIO)))
  )

}
