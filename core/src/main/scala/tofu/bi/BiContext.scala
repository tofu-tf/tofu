package tofu.bi

import cats.Bifunctor
import tofu.optics.Extract
import tofu.optics.Same
import tofu.optics.Contains
import tofu.bi.lift.{BiLift, BiDisclose}
import tofu.higherKind.bi.FunBK

trait BiContext[F[_, _], X, C] {
  def bifunctor: Bifunctor[F]

  def context: F[X, C]

  def extract[E, A](err: Extract[X, E], res: Extract[C, A]): BiContext[F, E, A] =
    new BiContextExtractInstance[F, X, C, E, A](this, err, res)

  def lextraxt[A](ex: Extract[C, A]): BiContext[F, X, A] = extract(Same.id, ex)

  def rextract[E](ex: Extract[X, E]): BiContext[F, E, C] = extract(ex, Same.id)
}

object BiContext {
  def apply[F[_, _], X, C](implicit inst: BiContext[F, X, C]): BiContext[F, X, C] = inst
}

trait BiLocal[F[_, _], X, C] extends BiContext[F, X, C] {
  def bilocal[E, A](fea: F[E, A])(lproj: X => X, rproj: C => C): F[E, A]

  def local[E, A](fea: F[E, A])(proj: C => C): F[E, A]    = bilocal(fea)(identity, proj)
  def errLocal[E, A](fea: F[E, A])(proj: X => X): F[E, A] = bilocal(fea)(proj, identity)

  def sub[E, A](err: X Contains E, res: C Contains A): BiLocal[F, E, A] =
    new BiLocalSubInstance[F, X, C, E, A](this, err, res)

  def rsub[A](cts: C Contains A): BiLocal[F, X, A] = sub(Same.id, cts)
  def lsub[E](cts: X Contains E): BiLocal[F, E, C] = sub(cts, Same.id)
}

object BiLocal {
  def apply[F[_, _], X, C](implicit inst: BiLocal[F, X, C]): BiLocal[F, X, C] = inst
}

trait BiProvide[F[_, _], G[_, _], X, C] extends BiLift[G, F] {
  def runLeft[E, A](fa: F[E, A])(x: X): G[E, A]
  def runRight[E, A](fa: F[E, A])(c: C): G[E, A]
  def runEither[E, A](fa: F[E, A])(ctx: Either[X, C]): G[E, A] =
    ctx match {
      case Left(x)  => runLeft(fa)(x)
      case Right(r) => runRight(fa)(r)
    }
  def runEitherK(ctx: Either[X, C]): F FunBK G                 = FunBK[F](runEither(_)(ctx))
}

object BiProvide {
  def apply[F[_, _], G[_, _], X, C](implicit inst: BiProvide[F, G, X, C]): BiProvide[F, G, X, C] = inst
}

class BiContextExtractInstance[F[_, _], X, C, E, A](ctx: BiContext[F, X, C], lext: Extract[X, E], rext: Extract[C, A])
    extends BiContext[F, E, A] {

  override def bifunctor: Bifunctor[F] = ctx.bifunctor

  override def context: F[E, A] = bifunctor.bimap(ctx.context)(lext.extract, rext.extract)

  override def extract[E1, A1](err: tofu.optics.Extract[E, E1], res: tofu.optics.Extract[A, A1]): BiContext[F, E1, A1] =
    ctx.extract(lext >> err, rext >> res)
}

class BiLocalSubInstance[F[_, _], X, C, E, A](ctx: BiLocal[F, X, C], lcts: Contains[X, E], rcts: Contains[C, A])
    extends BiContextExtractInstance[F, X, C, E, A](ctx, lcts, rcts) with BiLocal[F, E, A] {
  override def bilocal[E1, A1](fea: F[E1, A1])(lproj: E => E, rproj: A => A): F[E1, A1] =
    ctx.bilocal(fea)(lcts.update(_, lproj), rcts.update(_, rproj))

  override def sub[E1, A1](err: tofu.optics.Contains[E, E1], res: tofu.optics.Contains[A, A1]): BiLocal[F, E1, A1] =
    ctx.sub(lcts >> err, rcts >> res)
}

trait BiRun[F[_, _], G[_, _], X, C] extends BiProvide[F, G, X, C] with BiLocal[F, X, C] with BiDisclose[G, F]

object BiRun {
  def apply[F[_, _], G[_, _], X, C](implicit inst: BiRun[F, G, X, C]): BiRun[F, G, X, C] = inst
}
