package tofu.optics

package object compat {
  type LazySeq[+A] = LazyList[A]

  // this should be preferred over scala.annotation.unused, since it's tracked by -Wunused:nowarn
  type unused = scala.annotation.nowarn
  val lazySeqInstances = cats.instances.lazyList
}
