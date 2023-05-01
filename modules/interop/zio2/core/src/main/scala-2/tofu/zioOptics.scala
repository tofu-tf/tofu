package tofu

import glass.PContains
import zio.{Tag, ZEnvironment}

object zioOptics {
  def envContains[A: Tag, B: Tag]: PContains[ZEnvironment[A], ZEnvironment[A with B], A, B] =
    new PContains[ZEnvironment[A], ZEnvironment[A with B], A, B] {
      override def set(s: ZEnvironment[A], b: B): ZEnvironment[A with B] =
        s.add[B](b)

      override def extract(s: ZEnvironment[A]): A = s.get[A]
    }
}
