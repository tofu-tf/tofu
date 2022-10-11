package tofu.logging.zlogs

import tofu.logging.{LogAnnotation, LoggedValue}
import zio.{FiberRefs, UIO}

trait ZLogContext {

  /** Adds the couple of `LogAnnotation` and corresponding value to the context.
    */
  def update[A](pair: (LogAnnotation[A], A)): UIO[Unit]

  /**
    * Adds a new value and returns it if the annotation is absent in the context,
    * returns an actual value from the context otherwise.
    */
  def getOrUpdate[A](pair: (LogAnnotation[A], A)): UIO[A]

  /**
    * Returns a value from the context if the annotation exists.
    */
  def get[A](key: LogAnnotation[A]): UIO[Option[A]]

}

object ZLogContext {
  trait LoggedValuesExtractor {
    def loggedValueFromFiberRefs(fiberRefs: FiberRefs): Option[LoggedValue]

    def loggedValue: UIO[LoggedValue]
  }
}
