package tofu.bi

import cats.Bifunctor
import tofu.optics.Extract
import tofu.optics.Same

trait BiContext[F[_, _], X, C] {
  def bifunctor: Bifunctor[F]

  def context: F[X, C]

  def extract[E, A](err: Extract[X, E], res: Extract[C, A]): BiContext[F, E, A] =
    new BiContextExtractInstance[F, X, C, E, A](this, err, res)

  def lextraxt[A](ex: Extract[C, A]): BiContext[F, X, A] = extract(Same.id, ex)

  def rextract[E](ex: Extract[X, E]): BiContext[F, E, C] = extract(ex, Same.id)
}

object BiContext {
  def apply[F[_, _], X, C](implicit ctx: BiContext[F, X, C]): BiContext[F, X, C] = ctx
}

class BiContextExtractInstance[F[_, _], X, C, E, A](ctx: BiContext[F, X, C], exErr: Extract[X, E], exRes: Extract[C, A])
    extends BiContext[F, E, A] {

  override def bifunctor: Bifunctor[F] = ctx.bifunctor

  override def context: F[E, A] = bifunctor.bimap(ctx.context)(exErr.extract, exRes.extract)
}
