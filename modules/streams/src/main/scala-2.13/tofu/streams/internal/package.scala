package tofu.streams

package object internal {
  private[tofu] type Factory[-A, +C] = scala.collection.Factory[A, C]
}
