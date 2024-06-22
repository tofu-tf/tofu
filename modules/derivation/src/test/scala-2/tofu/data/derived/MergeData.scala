package tofu.data.derived

import derevo.derive

object MergeData {
  @derive(Merge)
  final case class Foo(a: Int, b: Option[String], c: Option[Double])

  @derive(Merge)
  final case class Bar(x: Foo, y: Option[Foo])
}
