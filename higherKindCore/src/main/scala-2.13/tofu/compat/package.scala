package tofu.compat

import cats.data.NonEmptyLazyList

import scala.annotation.StaticAnnotation

object `package` {
  type LazySeq[+A]   = LazyList[A]
  type NELazySeq[+A] = NonEmptyLazyList[A]

  // this should be preferred over scala.annotation.unused, since it's tracked by -Wunused:nowarn
  type unused = scala.annotation.nowarn
  val lazySeqInstances       = cats.instances.lazyList
  val LazySeq: LazyList.type = LazyList
}

class uv212 extends StaticAnnotation

class unused212 extends StaticAnnotation
