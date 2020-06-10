package tofu.bi.lift

import tofu.higherKind.bi.BiFunK
import cats.Bifunctor
import tofu.bi.BiConst

trait BiLift[F[_, _], G[_, _]] {
  def gfunctor: Bifunctor[G]

  def lift[E, A](fa: F[E, A]): G[E, A]

  def liftF: BiFunK[F, G] = BiFunK.apply(lift(_))
}

object BiLift {
  def apply[F[_, _], G[_, _]](implicit lift: BiLift[F, G]): BiLift[F, G] = lift
  def trans[F[_, _], G[_, _]](implicit lift: BiLift[F, G]): F BiFunK G   = lift.liftF
}

trait LUnlift[F[_, _], G[_, _], R] extends BiLift[F, G] with BiUnlift[F, BiConst[*, *, R], G] {
  def lunlift: G[G BiFunK F, R]
  def biunlift: G[BiFunK[G, F], BiFunK[G, BiConst[*, *, R]]] =
    gfunctor.bimap(lunlift)(identity, r => BiFunK.apply(_ => r))
}

trait RUnlift[F[_, _], G[_, _], X] extends BiLift[F, G] with BiUnlift[BiConst[*, *, X], F, G] {
  def runlift: G[X, G BiFunK F]
  def biunlift: G[BiFunK[G, BiConst[*, *, X]], BiFunK[G, F]] =
    gfunctor.leftMap(runlift)(x => BiFunK.apply(_ => x))
}

trait BiUnlift[F[_, _], G[_, _], H[_, _]] {
  def liftL[E, A](fa: F[E, A]): H[E, A]
  def liftR[E, A](fa: G[E, A]): H[E, A]

  def biunlift: H[H BiFunK F, H BiFunK G]
}
