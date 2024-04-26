package tofu.logging.zlogs

import org.slf4j.{Logger, LoggerFactory}
import tofu.logging.impl.ZContextLogging
import tofu.logging.{Loggable, Logs}
import zio._

import scala.annotation.unused

object ZLogs {

  private def make[R](f: Logger => ZLogging[R]) =
    new Logs[UIO, URIO[R, _]] {
      override def byName(name: String): UIO[ZLogging[R]] =
        ZIO.succeed(LoggerFactory.getLogger(name)).map(f)
    }

  /** Creates layer with a helper producing simple [[tofu.logging.Logging]].
    */
  def layerPlain: ULayer[ULogs] =
    ZLayer.succeed(make(new ZContextLogging(_, ZIO.unit)))

  /** Creates layer with a helper [[ZLogs]] producing logging `ZLogging[R]`. Logging methods will add the context from
    * the ZIO environment.
    *
    * @tparam R
    *   the context, an environment of the logging methods.
    */
  def layerContextual[R: Loggable](implicit @unused RT: Tag[R]): ULayer[ZLogs[R]] =
    ZLayer.succeed(make(new ZContextLogging(_, ZIO.service[R])))

  /** Creates layer with a helper [[ULogs]] producing plain `ULogging` with encapsulated context. You have to provide an
    * implementation of `ContextProvider`.
    */
  val layerPlainWithContext: URLayer[ContextProvider, ULogs] = ZLayer(
    ZIO.serviceWith[ContextProvider](cp => make(new ZContextLogging(_, cp.getCtx)))
  )

  val layerEmpty: ULayer[ULogs] = ZLayer.succeed(make(_ => ZLogging.empty[Any]))

}
