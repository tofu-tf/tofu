package tofu.logging.zlogs

import tofu.logging.LogAnnotation.AnnotatedValue
import tofu.logging._
import zio.{Trace, ZIO, ZIOAspect}

class ZLogAspect private(
    appendedContext: Map[LogAnnotation[_], Any],
) extends ZIOAspect[Nothing, Any, Nothing, Any, Nothing, Any] {

  override def apply[R, E, A](zio: ZIO[R, E, A])(implicit trace: Trace): ZIO[R, E, A] = {
    if (appendedContext.isEmpty) zio
    else {
      ZLogContextLive.TofuLogContextRef
        .locallyWith(_ ++ appendedContext)(zio)
    }
  }

}

object ZLogAspect {
  def apply(value: AnnotatedValue, values: AnnotatedValue*)(valuesOpt: Option[AnnotatedValue]*): ZLogAspect = {
    new ZLogAspect(
      (value +: values).toMap ++ valuesOpt.flatten.toMap
    )
  }
}
