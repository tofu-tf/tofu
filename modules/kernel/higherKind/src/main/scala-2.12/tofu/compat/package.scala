package tofu.compat

import cats.data.NonEmptyStream

object `package` {
  type LazySeq[+A]  = Stream[A]
  type NELazySeq[A] = NonEmptyStream[A]

  val lazySeqInstances = cats.instances.stream

  type uv212 = scala.annotation.unchecked.uncheckedVariance

  type unused = scala.annotation.nowarn

  type unused212 = scala.annotation.nowarn
  val LazySeq: Stream.type = Stream
}
