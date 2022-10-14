package tofu.logging.zlogs

import tofu.logging.TofuAnnotation.AnnotatedValue
import tofu.logging._
import zio._

class TofuZAspect private(
    appendedContext: Map[TofuAnnotation[_], Any],
) extends ZIOAspect[Nothing, Any, Nothing, Any, Nothing, Any] {

  override def apply[R, E, A](zio: ZIO[R, E, A])(implicit trace: Trace): ZIO[R, E, A] = {
    if (appendedContext.isEmpty) zio
    else {
      ZLogContextLive.TofuLogContextRef
        .locallyWith(_ ++ appendedContext)(zio)
    }
  }

}

object TofuZAspect {
  def apply(values: AnnotatedValue*)(valuesOpt: Option[AnnotatedValue]*): TofuZAspect = {
    new TofuZAspect(
      values.toMap ++ valuesOpt.flatten.toMap
    )
  }

  def apply(value: AnnotatedValue, values: AnnotatedValue*): TofuZAspect =
    apply((values :+ value): _*)()

}
