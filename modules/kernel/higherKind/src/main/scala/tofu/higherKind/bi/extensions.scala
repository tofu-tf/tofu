package tofu.higherKind.bi

class MonoidBKOps[F[_, _], A, B](private val fab: F[A, B]) extends AnyVal {
  def combinebk[F1[x, y] >: F[x, y]](other: F1[A, B])(implicit FSG: SemigroupBK[F1]): F1[A, B] =
    FSG.combinebk(fab, other)
}
