package tofu.logging.shapeless

import shapeless.tag.@@
import tofu.logging.Loggable

object implicits {
  implicit def deriveTaggedLoggable[T, P](implicit loggable: Loggable[T]): Loggable[T @@ P] =
    Loggable[T].contramap(identity(_: T))
}
