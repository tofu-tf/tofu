package tofu.bi.lift

import tofu.higherKind.bi.BiFunK
import cats.Bifunctor
import tofu.bi.BiConst
import tofu.higherKind.bi.BiFun2K

trait BiLift[F[_, _], G[_, _]] {
  def gfunctor: Bifunctor[G]

  def lift[E, A](fa: F[E, A]): G[E, A]

  def liftF: BiFunK[F, G] = BiFunK.apply(lift(_))
}

object BiLift {
  def apply[F[_, _], G[_, _]](implicit lift: BiLift[F, G]): BiLift[F, G] = lift
  def trans[F[_, _], G[_, _]](implicit lift: BiLift[F, G]): F BiFunK G   = lift.liftF
}

trait BiDisclose[F[_, _], G[_, _]] extends BiLift[F, G] {
  def disclose[E, A](k: (G BiFunK F) => F[E, A]): G[E, A]
}
