package tofu

import izumi.reflect.Tag
import tofu.optics.PContains
import zio.Has

object zioOptics {
  def hasContains[R, A: Tag, B: Tag]: PContains[R with Has[A], R with Has[A] with Has[B], A, B] =
    new PContains[R with Has[A], R with Has[A] with Has[B], A, B] {
      def set(s: R with Has[A], b: B): R with Has[A] with Has[B] = s.add(b)

      def extract(s: R with Has[A]): A = s.get[A]
    }
}
