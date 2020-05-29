package tofu.alias

class ScalaSpecificDoMonadInstances {
  import cats.instances.stream._

  implicit val streamDoMonad: Do[Stream] = new DoMonad(catsStdInstancesForStream)
}
