package tofu.syntax

class ScalaSpecificDoMonadInstances {
  import cats.instances.stream._

  implicit val lazyListDoMonad: Do[Stream] = new DoMonad(catsStdInstancesForStream)
}
