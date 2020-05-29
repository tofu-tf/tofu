package tofu

import cats.data.NonEmptyStream

package object compat {
  type LazySeq[+A]  = Stream[A]
  type NELazySeq[A] = NonEmptyStream[A]

  val lazySeqInstances = cats.instances.stream
}
