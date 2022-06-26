package tofu.optics.compat

import scala.annotation.StaticAnnotation

type LazySeq[+A] = LazyList[A]

// this should be preferred over scala.annotation.unused, since it's tracked by -Wunused:nowarn
type unused = scala.annotation.nowarn
val lazySeqInstances = cats.instances.lazyList

class uv212 extends StaticAnnotation

class unused212 extends StaticAnnotation
