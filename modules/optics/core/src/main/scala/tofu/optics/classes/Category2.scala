package tofu.optics.classes

trait Category2[O[S, T, A, B]] {
  def id[A, B]: O[A, B, A, B]
  def compose[S, T, A, B, U, V](f: O[A, B, U, V], g: O[S, T, A, B]): O[S, T, U, V]
}

object Category2 {
  def id[O[_, _, _, _], A, B](implicit cat: Category2[O]): O[A, B, A, B] = cat.id
  def compose[O[_, _, _, _], S, T, A, B, U, V](f: O[A, B, U, V], g: O[S, T, A, B])(implicit
      cat: Category2[O]
  ): O[S, T, U, V] = cat.compose(f, g)
}
