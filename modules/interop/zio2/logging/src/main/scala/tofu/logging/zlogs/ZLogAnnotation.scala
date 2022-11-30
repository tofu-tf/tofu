package tofu.logging.zlogs

import tofu.logging.{LogAnnotation, Loggable}
import zio.{Trace, ZIO, ZIOAspect}

/** The [[LogAnnotation]] extension allows you add structured log annotations into the default log context. The stored
  * values are accessible via [[TofuDefaultContext]] and would be logged by [[TofuZLogger]].
  *
  * __Attention!__ Please be advised that creating different annotations with the same name can lead to duplicated keys
  * in your structured log and its corruption. Don't use keys defined in the companion object below.
  */
class ZLogAnnotation[V](name: String, valueLoggable: Loggable[V]) extends LogAnnotation[V](name, valueLoggable) {
  self =>

  def apply(value: V): ZIOAspect[Nothing, Any, Nothing, Any, Nothing, Any] =
    new ZIOAspect[Nothing, Any, Nothing, Any, Nothing, Any] {
      def apply[R, E, A](zio: ZIO[R, E, A])(implicit trace: Trace): ZIO[R, E, A] = {
        TofuDefaultContext.AnnotatedContextRef
          .locallyWith(_ + ((self, value)))(zio)
      }
    }

  def apply(value: Option[V]): ZIOAspect[Nothing, Any, Nothing, Any, Nothing, Any] =
    value match {
      case Some(v) => apply(v)
      case None    => ZLogAnnotation.NoopAspect
    }

}

object ZLogAnnotation {

  private[zlogs] val NoopAspect = new ZIOAspect[Nothing, Any, Nothing, Any, Nothing, Any] {
    def apply[R, E, A](zio: ZIO[R, E, A])(implicit trace: Trace): ZIO[R, E, A] = zio
  }

  def make[A](name: String)(implicit L: Loggable[A]): ZLogAnnotation[A] =
    new ZLogAnnotation[A](name, L)

  // Don't use these keys
  private val LoggerNameKey              = "_zLoggerName"
  private[logging] val ZioAnnotationsKey = "zAnnotations"
  private[logging] val ZioLogSpansKey    = "zSpans"

  /** Specifies the loggerName used by [[TofuZLogger]]
    */
  val loggerName: ZLogAnnotation[String] = make[String](LoggerNameKey)(Loggable.empty)
}
