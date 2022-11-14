package tofu.logging.zlogs

import tofu.logging.{LogAnnotation, Loggable}
import zio.{Trace, ZIO, ZIOAspect}

class ZLogAnnotation[A](name: String, valueLoggable: Loggable[A]) extends LogAnnotation[A](name, valueLoggable) {
  self =>

  def apply(value: A): ZIOAspect[Nothing, Any, Nothing, Any, Nothing, Any] =
    new ZIOAspect[Nothing, Any, Nothing, Any, Nothing, Any] {
      def apply[R, E, A](zio: ZIO[R, E, A])(implicit trace: Trace): ZIO[R, E, A] = {
        TofuAnnotatedContextRef.locallyWith(_ + ((self, value)))(zio)
      }
    }

  def apply(value: Option[A]): ZIOAspect[Nothing, Any, Nothing, Any, Nothing, Any] =
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
}
