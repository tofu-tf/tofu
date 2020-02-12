package tofu

import fs2._

package object fs2Instances {
  private[this] val fs2InstanceAny = new FS2StreamInstance[Any]

  final implicit def fs2StreamInstance[A]: FS2StreamInstance[A] = fs2InstanceAny.asInstanceOf[FS2StreamInstance[A]]
  final implicit def fs2StreamContext[F[_], R](implicit fctx: F HasContext R): HasContext[Stream[F, *], R] =
    new FS2Context[F, R]
}
