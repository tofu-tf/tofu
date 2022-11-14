package tofu.logging.zlogs

import tofu.logging.{LogAnnotation, LoggedValue}
import zio.{Fiber, UIO, ULayer, ZIO, ZLayer}

trait ContextProvider {
  def getCtx: UIO[LoggedValue]
}

class TofuDefaultContext() extends ContextProvider {
  def getValue[A](key: LogAnnotation[A]): UIO[Option[A]] =
    TofuAnnotatedContextRef.get
      .map(_.get(key).asInstanceOf[Option[A]])

  protected def getLoggedValuesMap: UIO[Map[String, LoggedValue]] =
    TofuAnnotatedContextRef.get
      .map(_.map { case (k, v) =>
        (k.name, k -> v.asInstanceOf[k.Value])
      })

  def getCtx: UIO[LoggedValue] = getLoggedValuesMap.map(x => x)
}
