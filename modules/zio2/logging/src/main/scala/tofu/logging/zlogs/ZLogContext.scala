package tofu.logging.zlogs

import ZLogContextLive.TofuLogContextRef
import tofu.logging.{LoggedValue, LogAnnotation}
import zio._

trait ZLogContext {

  /** Adds the couple of `LogAnnotation` and corresponding value to the context.
    */
  def update[A](key: LogAnnotation[A], value: A): UIO[Unit]

  /** Adds a new value and returns it if the annotation is absent in the context, returns an actual value from the
    * context otherwise.
    */
  def getOrUpdate[A](key: LogAnnotation[A], value: A): UIO[A]

  /** Returns a value from the context if the annotation exists.
    */
  def get[A](key: LogAnnotation[A]): UIO[Option[A]]

  /** Scopes the result of calling the specified function only for the given effect.
    */
  def locally[R, E, B](updates: ZLogContext => UIO[Unit])(zio: ZIO[R, E, B]): ZIO[R, E, B]

}

object ZLogContext {
  trait LoggedValuesExtractor {
    def loggedValue: UIO[LoggedValue]
  }

  def live: ULayer[ZLogContextLive] = ZLayer.succeed(
    new ZLogContextLive(TofuLogContextRef)
  )
}
