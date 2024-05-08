package tofu.data.derived


object MergeData {
  final case class Foo(a: Int, b: Option[String], c: Option[Double]) derives Merge

  final case class Bar(x: Foo, y: Option[Foo]) derives Merge
}
