package tofu.alias

import tofu.alias.DoMonad

class ScalaSpecificDoMonadInstances {
  import cats.instances.stream._

  implicit val streamDoMonad: Do[Stream] = new DoMonad(catsStdInstancesForStream)
}
