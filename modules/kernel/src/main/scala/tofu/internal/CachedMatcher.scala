package tofu.internal

private[tofu] final case class CachedMatcher[-A, +B](f: A => Option[B]) extends PartialFunction[A, B] {
  private[this] var cachedInput: Option[A]            = None
  private[this] var cachedOutput: Option[B]           = None
  @inline private[this] def calc(input: A): Option[B] = {
    if (!cachedInput.contains(input)) {
      cachedInput = Some(input)
      cachedOutput = f(input)
    }
    cachedOutput
  }

  def isDefinedAt(x: A): Boolean = calc(x).isDefined
  def apply(x: A): B             = calc(x).get

}
