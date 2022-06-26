package tofu.optics

package object compat {
  type LazySeq[+A] = Stream[A]

  val lazySeqInstances = cats.instances.stream

  type uv212 = scala.annotation.unchecked.uncheckedVariance

  type unused = scala.annotation.nowarn

  type unused212 = scala.annotation.nowarn
}
