package tofu.bi.lift

import tofu.higherKind.bi.FunBK
import cats.Bifunctor
import tofu.control.Bind

trait BiLift[F[_, _], G[_, _]] {
  def bifunctor: Bifunctor[G]

  def lift[E, A](fa: F[E, A]): G[E, A]

  def liftF: FunBK[F, G] = FunBK.apply[F](lift(_))
}

object BiLift {
  def apply[F[_, _], G[_, _]](implicit lift: BiLift[F, G]): BiLift[F, G] = lift
  def trans[F[_, _], G[_, _]](implicit lift: BiLift[F, G]): F FunBK G    = lift.liftF
}

trait BiUnlift[F[_, _], G[_, _]] extends BiLift[F, G] {
  def bifunctor: Bind[G]

  def disclose[E, A](k: (G FunBK F) => G[E, A]): G[E, A]

  def discloseBase[E, A](k: (G FunBK F) => F[E, A]): G[E, A] = disclose(ul => lift(k(ul)))

  def unlift[E]: G[E, G FunBK F]    = disclose(bifunctor.pure)
  def unliftErr[A]: G[G FunBK F, A] = disclose(bifunctor.raise)
}
