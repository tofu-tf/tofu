package tofu.optics.classes

trait Delayed[O[S, T, A, B]] {
  def delayed[S, T, A, B](o: () => O[S, T, A, B]): O[S, T, A, B]
}
