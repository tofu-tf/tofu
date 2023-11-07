package tofu.logging.zlogs

import tofu.logging._
import tofu.logging.impl.ZUniversalContextLogging
import zio._

import scala.annotation.unused

object ZLogging {

  type ZMake[R] = Logging.Make[URIO[R, _]]

  type Make = Logging.Make[UIO]

  def empty[R]: ZLogging[R] = new Logging[URIO[R, _]] {
    override def write(level: Logging.Level, message: String, values: LoggedValue*): URIO[R, Unit] = ZIO.unit
  }

  /** Unlike [[ZLogs]] layers, these helpers are effect-less.
    */
  object Make {

    /** Creates layer with a helper producing simple [[tofu.logging.Logging]].
      */
    val layerPlain: ULayer[ZLogging.Make] = ZLayer.succeed(new ZUniversalContextLogging(_, ZIO.unit))

    /** Creates layer with a helper [[ZMake]] producing logging `ZLogging[R]`. Logging methods will add the context from
      * the ZIO environment.
      *
      * @tparam R
      *   the context, an environment of the logging methods.
      */
    def layerContextual[R: Loggable](implicit @unused RT: Tag[R]): ULayer[ZLogging.ZMake[R]] = ZLayer.succeed(
      new ZUniversalContextLogging(_, ZIO.service[R])
    )

    /** Creates layer with a helper [[Make]] producing plain `ULogging` with encapsulated context. You have to provide
      * an implementation of `ContextProvider`.
      */
    def layerPlainWithContext: URLayer[ContextProvider, ZLogging.Make] =
      ZLayer(
        ZIO.serviceWith[ContextProvider](cp => new ZUniversalContextLogging(_, cp.getCtx))
      )

    val layerEmpty: ULayer[ZLogging.Make] = ZLayer.succeed(_ => empty[Any])
  }

}
