package tofu.logging.zlogs

import tofu.logging.LogAnnotation
import tofu.logging.impl.{TofuDefaultContextImpl, TofuDefaultWithZIOContextImpl}
import zio._

/** A [[ContextProvider]] with the tofu annotations context access.
  */
trait TofuDefaultContext extends ContextProvider {

  /** Returns the value added in the context by [[ZLogAnnotation]]. Returns `None` if the value is absent.
    */
  def getValue[A](key: LogAnnotation[A]): UIO[Option[A]]
}

object TofuDefaultContext {

  /** The [[AnnotatedContextRef]] fiber reference is used to store typed, structured log annotations, which can be
    * utilized by backends to enrich log messages. This have to be a global value to pretty updates by
    * [[ZLogAnnotation]]. This way the logContext is implemented in official zio-logging library, see
    * `zio.logging#logContext`.
    */
  private[logging] lazy val AnnotatedContextRef: FiberRef[Map[LogAnnotation[_], Any]] =
    Unsafe.unsafe(implicit unsafe =>
      Runtime.default.unsafe
        .run(ZIO.scoped(FiberRef.make[Map[LogAnnotation[_], Any]](Map.empty)))
        .getOrThrow()
    )

  private[logging] def getValueUnsafe[A](key: LogAnnotation[A])(m: Map[LogAnnotation[_], Any]): Option[A] =
    m.get(key).asInstanceOf[Option[A]]

  /** This [[ContextProvider]] logs just tofu annotations (values added via [[ZLogAnnotation]]) */
  val layerZioContextOff: ULayer[TofuDefaultContext] = ZLayer.succeed(new TofuDefaultContextImpl)

  /** This [[ContextProvider]] logs tofu annotations as well as zio annotations and [[zio.LogSpan]]-s */
  val layerZioContextOn: ULayer[TofuDefaultContext] = ZLayer.succeed(new TofuDefaultWithZIOContextImpl)
}
