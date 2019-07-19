package tofu.memo

sealed trait CacheVal[+A] {
  def filter(after: Long): CacheVal[A] = if (valid(after)) this else CacheVal.None
  def valid(after: Long): Boolean
  def expired(after: Long): Boolean
  def isEmpty: Boolean
  def getOption: Option[A]
  def get: A
  def fold[B](empty: => B)(some: (Long, A) => B): B
}

object CacheVal {
  def none[A]: CacheVal[A]                 = None
  def some[A](ts: Long, x: A): CacheVal[A] = Some(ts, x)

  final case class Some[+A](updatedTime: Long, value: A) extends CacheVal[A] {
    override def valid(after: Long): Boolean          = after <= updatedTime
    override def isEmpty: Boolean                     = false
    override def get: A                               = value
    override def getOption: Option[A]                 = scala.Some(value)
    override def expired(after: Long): Boolean        = !valid(after)
    def fold[B](empty: => B)(some: (Long, A) => B): B = some(updatedTime, value)
  }

  case object None extends CacheVal[Nothing] {
    override def valid(after: Long): Boolean                = false
    override def isEmpty: Boolean                           = true
    override def get: Nothing                               = throw new NoSuchElementException
    override def getOption: Option[Nothing]                 = scala.None
    override def expired(after: Long): Boolean              = false
    def fold[B](empty: => B)(some: (Long, Nothing) => B): B = empty
  }
}
