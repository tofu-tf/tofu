package tofu

/** Initialize value of type A in type F
  *
  * @tparam F
  *   initialization effect type
  * @tparam A
  *   result type
  */
trait Init[F[_], A] {
  def init: F[A]
}
