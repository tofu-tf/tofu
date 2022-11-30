package tofu.logging.impl

import tofu.logging.zlogs.TofuDefaultContext.AnnotatedContextRef
import tofu.logging.zlogs.{TofuDefaultContext, ValueContextProvider}
import tofu.logging.{LogAnnotation, LoggedValue}
import zio.UIO

class TofuDefaultContextImpl extends ValueContextProvider[Map[String, LoggedValue]] with TofuDefaultContext {
  override protected def getA: UIO[Map[String, LoggedValue]] =
    AnnotatedContextRef.get
      .map(_.map { case (k, v) =>
        (k.name, k -> v.asInstanceOf[k.Value])
      })

  def getValue[A](key: LogAnnotation[A]): UIO[Option[A]] =
    AnnotatedContextRef.get.map(
      TofuDefaultContext.getValueUnsafe(key)
    ) // ok, values are added via typed ZLogAnnotation.apply
}
