package tofu

import cats.data.NonEmptyLazyList
import scala.collection.immutable.ArraySeq
import scala.annotation.StaticAnnotation
import scala.annotation.compileTimeOnly

package object compat {
  type LazySeq[+A]   = LazyList[A]
  type NELazySeq[+A] = NonEmptyLazyList[A]

  val lazySeqInstances = cats.instances.lazyList
}

package compat {
  class uv212 extends StaticAnnotation
}
