package tofu.optics.classes

trait Category2[O[S, T, A, B]] {
  def id[A, B]: O[A, B, A, B]
  def compose[S, T, A, B, U, V](f: O[A, B, U, V], g: O[S, T, A, B]): O[S, T, U, V]
}
