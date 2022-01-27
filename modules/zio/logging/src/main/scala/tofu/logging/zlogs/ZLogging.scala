package tofu.logging.zlogs

import scala.annotation.unused

import tofu.logging.impl.{ZUniversalContextLogging, ZUniversalLogging}
import tofu.logging.{Loggable, Logging}
import zio._
import zio.interop.catz._

object ZLogging {
  type ZMake[R] = Logging.Make[URIO[R, *]]

  type Make = Logging.Make[UIO]

  object Make {

    /** Creates layer with a helper producing simple [[tofu.logging.Logging]].
      */
    val layerPlain: ULayer[Has[ZLogging.Make]] = ZLayer.succeed(new ZUniversalLogging(_))

    /** Creates layer with a helper [[ZMake]] producing logging `Logging[URIO[R, *]]`. Logging methods will add a
      * context from the ZIO environment.
      * @tparam R
      *   the context, an environment of the logging methods.
      */
    def layerContextual[R: Loggable](implicit @unused RT: Tag[R]): ULayer[Has[ZLogging.ZMake[R]]] = ZLayer.succeed(
      new ZUniversalContextLogging(_, ZIO.environment[R])
    )

    /** Creates layer with a helper [[Make]] producing plain `Logging[UIO]` with encapsulated context. You have to
      * provide a way to get the context from the abstract `ContextService`. It would be [[zio.FiberRef]], [[zio.Ref]]
      * or whatever you want.
      */
    def layerPlainWithContext[Ctx: Loggable, ContextService: Tag](
        getContext: ContextService => UIO[Ctx]
    ): URLayer[Has[ContextService], Has[ZLogging.Make]] = {
      ZLayer.fromService { cs: ContextService =>
        new ZUniversalContextLogging[Any, Ctx](_, getContext(cs))
      }
    }

    val layerEmpty: ULayer[Has[ZLogging.Make]] = ZLayer.succeed(_ => Logging.empty[UIO])
  }
}
