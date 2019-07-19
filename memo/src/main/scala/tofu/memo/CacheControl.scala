package tofu.memo

final case class CacheControl(invalidated: InvalidationTS) extends CacheKeyControl[Any]{
  override def keyInvalidated(key: Any): InvalidationTS = invalidated
}

object CacheControl {
  def empty = CacheControl(InvalidationTS.zero)
}

final case class InvalidationTS(millis: Long) extends AnyVal

object InvalidationTS {
  val zero = InvalidationTS(0L)
}

trait CacheKeyControl[-K]{
  def keyInvalidated(key: K): InvalidationTS
}
