package tofu.alias

class ScalaSpecificDoMonadInstances {

  implicit val lazyListDoMonad: Do[LazyList] = new DoMonad(catsStdInstancesForLazyList)
}
