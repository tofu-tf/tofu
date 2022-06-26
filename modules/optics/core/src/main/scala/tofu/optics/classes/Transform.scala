package tofu.optics.classes

import tofu.optics.{Extract, Upcast}

object Transform extends TransformInstances {
  final implicit def fromExtract[A, B](implicit
      extract: Extract[A, B]
  ): Transform[A, B] = extract.extract _
}

/** precisely implicit A => B deriveable from both Extract and Downcast */
trait Transform[A, B] {
  def apply(a: A): B
}

trait TransformInstances {
  final implicit def fromUpcast[A, B](implicit
      upcast: Upcast[A, B]
  ): Transform[B, A] = upcast.upcast _
}
