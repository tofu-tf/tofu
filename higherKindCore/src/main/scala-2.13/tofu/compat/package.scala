package tofu.compat

import cats.data.NonEmptyLazyList

import scala.annotation.StaticAnnotation

object `package` {
  type LazySeq[+A]   = LazyList[A]
  type NELazySeq[+A] = NonEmptyLazyList[A]

  val lazySeqInstances = cats.instances.lazyList

  // this should be preferred over scala.annotation.unused, since it's tracked by -Wunused:nowarn
  type unused = scala.annotation.nowarn
}

class uv212 extends StaticAnnotation

class unused212 extends StaticAnnotation