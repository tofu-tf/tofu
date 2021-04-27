package tofu.optics
import cats.data.NonEmptyList
import cats.kernel.Semigroup
import cats.{Applicative, Apply, Monoid}
import tofu.optics.classes.Category2
import tofu.optics.tags.{PTagApply, Tagger}

final case class Applied[O[_, _, _, _], S, T, A, B](s: S, o: O[S, T, A, B]) {
  def >>[O1[s, t, a, b] >: O[s, t, a, b]: Category2, U, V](o1: O1[A, B, U, V]): Applied[O1, S, T, U, V] =
    Applied(s, Category2.compose(o1, o))

  def >[O1[s, t, a, b] >: O[s, t, a, b]](tagger: Tagger[O1]): AppliedWithTag[O1, S, T, A, B, tagger.Tag] =
    AppliedWithTag(s, o)

  def to[O1[s, t, a, b] >: O[s, t, a, b]](tagger: Tagger[O1]): AppliedWithTag[O1, S, T, A, B, tagger.Tag] =
    AppliedWithTag(s, o)

  def put(b: B)(implicit ev: O[S, T, A, B] <:< PUpdate[S, T, A, B]): T = ev(o).put(s, b)

  def update(f: A => B)(implicit ev: O[S, T, A, B] <:< PUpdate[S, T, A, B]): T = ev(o).update(s, f)

  def get(implicit ev: O[S, T, A, B] <:< PExtract[S, T, A, B]): A = ev(o).extract(s)

  def downcast(implicit ev: O[S, T, A, B] <:< PDowncast[S, T, A, B]): Option[A] = ev(o).downcast(s)

  def traverse[G[+_]: Applicative](f: A => G[B])(implicit ev: O[S, T, A, B] <:< PItems[S, T, A, B]): G[T] =
    ev(o).traverse(s)(f)

  def foldMap[M: Monoid](f: A => M)(implicit ev: O[S, T, A, B] <:< PFolded[S, T, A, B]): M =
    ev(o).foldMap(s)(f)

  def reduceMap[M: Semigroup](f: A => M)(implicit ev: O[S, T, A, B] <:< PReduced[S, T, A, B]): M =
    ev(o).reduceMap(s)(f)

  def getAll1(implicit ev: O[S, T, A, B] <:< PReduced[S, T, A, B]): NonEmptyList[A] =
    ev(o).getAll1(s)

  def getAll(implicit ev: O[S, T, A, B] <:< PFolded[S, T, A, B]): List[A] =
    ev(o).getAll(s)

  def traverse1[G[+_]: Apply](f: A => G[B])(implicit ev: O[S, T, A, B] <:< PRepeated[S, T, A, B]): G[T] =
    ev(o).traverse1(s)(f)
}

final case class WithTag[O[s, t, a, b], S, T, A, B, Tag](private val o: O[S, T, A, B]) extends AnyVal {
  def >@[X, U, V](x: X)(implicit tag: PTagApply[O, A, B, U, V, Tag, X], cat: Category2[O]): O[S, T, U, V] =
    cat.compose(tag.continue(x), o)

  def apply[X, U, V](x: X)(implicit tag: PTagApply[O, A, B, U, V, Tag, X], cat: Category2[O]): O[S, T, U, V] =
    >@(x)

  def app[X, U, V](x: X)(implicit tag: PTagApply[O, A, B, U, V, Tag, X], cat: Category2[O]): O[S, T, U, V] =
    >@(x)

  def >[O1[s, t, a, b] >: O[s, t, a, b], U, V](
      tagger: Tagger[O1]
  )(implicit tag: PTagApply[O, A, B, U, V, Tag, Unit], cat: Category2[O]): WithTag[O1, S, T, U, V, tagger.Tag] =
    new WithTag(end)

  def to[O1[s, t, a, b] >: O[s, t, a, b], U, V](
      tagger: Tagger[O1]
  )(implicit tag: PTagApply[O, A, B, U, V, Tag, Unit], cat: Category2[O]): WithTag[O1, S, T, U, V, tagger.Tag] =
    new WithTag(end)

  def >>[O1[s, t, a, b] >: O[s, t, a, b], U, V, X, Y](o1: O1[U, V, X, Y])(implicit
      tag: PTagApply[O, A, B, U, V, Tag, Unit],
      cat1: Category2[O],
      cat: Category2[O1],
  ): O1[S, T, X, Y] =
    cat.compose(o1, end)

  def andThen[O1[s, t, a, b] >: O[s, t, a, b], U, V, X, Y](o1: O1[U, V, X, Y])(implicit
      tag: PTagApply[O, A, B, U, V, Tag, Unit],
      cat1: Category2[O],
      cat: Category2[O1],
  ): O1[S, T, X, Y] =
    cat.compose(o1, end)

  def end[U, V](implicit tag: PTagApply[O, A, B, U, V, Tag, Unit], cat: Category2[O]): O[S, T, U, V] =
    >@ {}
}

final case class AppliedWithTag[O[_, _, _, _], S, T, A, B, Tag](s: S, o: O[S, T, A, B]) {
  def >@[X, U, V](x: X)(implicit tag: PTagApply[O, A, B, U, V, Tag, X], cat: Category2[O]): Applied[O, S, T, U, V] =
    Applied(s, cat.compose(tag.continue(x), o))

  def app[X, U, V](x: X)(implicit tag: PTagApply[O, A, B, U, V, Tag, X], cat: Category2[O]): Applied[O, S, T, U, V] =
    >@(x)

  def >[O1[s, t, a, b] >: O[s, t, a, b], U, V](
      tagger: Tagger[O1]
  )(implicit tag: PTagApply[O, A, B, U, V, Tag, Unit], cat: Category2[O]): AppliedWithTag[O1, S, T, U, V, tagger.Tag] =
    end > tagger

  def to[O1[s, t, a, b] >: O[s, t, a, b], U, V](
      tagger: Tagger[O1]
  )(implicit tag: PTagApply[O, A, B, U, V, Tag, Unit], cat: Category2[O]): AppliedWithTag[O1, S, T, U, V, tagger.Tag] =
    end > tagger

  def >>[O1[s, t, a, b] >: O[s, t, a, b]: Category2, U, V, X, Y](o1: O1[U, V, X, Y])(implicit
      tag: PTagApply[O, A, B, U, V, Tag, Unit],
      cat: Category2[O]
  ): Applied[O1, S, T, X, Y] =
    end >> o1

  def end[U, V](implicit tag: PTagApply[O, A, B, U, V, Tag, Unit], cat: Category2[O]): Applied[O, S, T, U, V] =
    >@ {}
}

object chain {
  def to[T]          = new ChainTo[T]
  def apply[S](s: S) = Applied(s, PSame.id[S, S])

  class ChainTo[T] {
    def apply[S](s: S) = Applied(s, PSame.id[S, T])
  }
}
