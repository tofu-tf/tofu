package tofu.logging

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
  *   val count = TofuAnnotation.make[Int]("count")
  *   logger.info("Count has type Int", count -> 100)
  *   // logger.error("This line wouldn't be compiled", count -> "100")
  *   }}}
  */
final class TofuAnnotation[A](val name: String, valueLoggable: Loggable[A]) {

  type Type = A

  implicit private lazy val loggable: Loggable[A] = valueLoggable.named(name)

  override def hashCode: Int = name.hashCode

  override def equals(other: Any): Boolean = other match {
    case other: TofuAnnotation[_] => name == other.name
    case _                        => false
  }

  /** Returns `LoggedValue` using original `Loggable` provided when this annotation created.
    */
  def unnamed(value: A): LoggedValue = valueLoggable.loggedValue(value)

  /** Make `LoggedValue` as named single field with corresponding value.
    */
  def apply(value: A): LoggedValue = value

  /** Make `LoggedValue` as named single field with corresponding value (if it's non-empty).
    */
  def apply(value: Option[A]): LoggedValue = value
}

object TofuAnnotation {
  def make[A: Loggable](name: String): TofuAnnotation[A] =
    new TofuAnnotation[A](name, Loggable[A])
}