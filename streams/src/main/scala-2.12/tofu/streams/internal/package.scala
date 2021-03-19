package tofu.streams

import scala.collection.generic.CanBuildFrom

package object internal {
  private[tofu] type Factory[-A, +C] = CanBuildFrom[Nothing, A, C]
}
