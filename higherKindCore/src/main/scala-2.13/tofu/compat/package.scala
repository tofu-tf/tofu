package tofu

import cats.data.NonEmptyLazyList
import scala.collection.immutable.ArraySeq

package object compat {
  type LazySeq[+A]   = LazyList[A]
  type NELazySeq[+A] = NonEmptyLazyList[A]

  val lazySeqInstances = cats.instances.lazyList
}
