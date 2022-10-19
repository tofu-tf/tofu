package tofu.logging

import tofu.logging.LogAnnotation.AnnotatedValue

/** Type-safe annotation for loggable values.
  *
  * In particular this solves two problems:
  *
  *   1. limit the number of fields your app logs
  *   1. set the type of log field (logging the same field as a string and as an object in the same application can lead
  *      to problems in your logging platform)
  *
  * @example
  *   {{{
  *   val count = LogAnnotation.make[Int]("count")
  *   logger.info("Count has type Int", count -> 100)
  *   // logger.error("This line wouldn't be compiled", count -> "100")
  *   }}}
  */
final class LogAnnotation[A](val name: String, valueLoggable: Loggable[A]) {
  self =>

  type Value = A

  implicit private lazy val loggable: Loggable[A] = valueLoggable.named(name)

  override def hashCode: Int = name.hashCode

  override def equals(other: Any): Boolean = other match {
    case other: LogAnnotation[_] => name == other.name
    case _                       => false
  }

  /** Returns `LoggedValue` using original `Loggable` provided when this annotation created.
    */
  def unnamed(value: Value): LoggedValue = valueLoggable.loggedValue(value)

  /** Make `LoggedValue` as named single field with corresponding value.
    */
  def apply(value: Value): LoggedValue = value

  /** Make `LoggedValue` as named single field with corresponding value (if it's non-empty).
    */
  def apply(value: Option[Value]): LoggedValue = value

  /** Pretty syntax for use in `Logging` methods (like `apply`) as well as other places requiring a tuple of
    * (LogAnnotation[A], A)
    */
  def ->(value: Value): AnnotatedValue = AnnotatedValue(self, value)

  def ->(value: Option[Value]): Option[AnnotatedValue] = value.map(v => self -> v)
}

object LogAnnotation {
  def make[A: Loggable](name: String): LogAnnotation[A] =
    new LogAnnotation[A](name, Loggable[A])

  type AnnotatedValue = AnnotatedValue.Type

  /** Typed tuple of (LogAnnotation[A], A) with no runtime overhead.
    */
  object AnnotatedValue {
    trait Tagged extends Any

    type Repr = (LogAnnotation[_], Any)
    type Type <: Repr with Tagged

    def apply[A](annot: LogAnnotation[A], value: A): AnnotatedValue =
      (annot, value).asInstanceOf[AnnotatedValue]

    implicit def annotatedValue2LoggedValue(pair: AnnotatedValue): LoggedValue = {
      val annot: LogAnnotation[_] = pair._1
      val value: annot.Value      = pair._2.asInstanceOf[annot.Value]
      annot(value)
    }

    implicit def annotatedOption2LoggedValue(pair: Option[AnnotatedValue]): LoggedValue =
      pair match {
        case None    => ()
        case Some(p) => annotatedValue2LoggedValue(p)
      }
  }
}
