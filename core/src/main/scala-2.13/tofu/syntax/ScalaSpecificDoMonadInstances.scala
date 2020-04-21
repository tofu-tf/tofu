package tofu.syntax

class ScalaSpecificDoMonadInstances {
  import cats.instances.lazyList._

  implicit val lazyListDoMonad: Do[LazyList] = new DoMonad(catsStdInstancesForLazyList)
}
