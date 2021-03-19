package tofu.optics

import tofu.optics.classes.Category2
import scala.annotation.unchecked.{uncheckedVariance => uv212}

trait PBase[+O[-s, +t, +a, -b] <: PBase[O, s, t, a, b], -S, +T, +A, -B] { self: O[S, T, A, B] =>
  def label[label]: this.type with Label[label] = this.asInstanceOf[this.type with Label[label]]

  def andThen[O1[-s, +t, +a, -b] >: O[s, t, a, b] @uv212, U, V](o1: O1[A, B, U, V])(implicit
      category2: Category2[O1]
  ): O1[S, T, U, V] = category2.compose(o1, this)

  def >>[O1[-s, +t, +a, -b] >: O[s, t, a, b] @uv212: Category2, U, V](o1: O1[A, B, U, V]): O1[S, T, U, V] = andThen(o1)
}

class PComposed[+O[-s, +t, +a, -b] <: PBase[O, s, t, a, b], -S, +T, A, B, +U, -V](
    x: PBase[O, S, T, A, B],
    y: PBase[O, A, B, U, V]
) extends PBase[O, S, T, U, V] { self: O[S, T, U, V] =>
  override def toString(): String = s"($x) >> ($y)"
}
