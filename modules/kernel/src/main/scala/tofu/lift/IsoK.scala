package tofu.lift

import cats.~>
import tofu.syntax.funk._

/** bidirectional transformation */
trait IsoK[F[_], G[_]] { self =>
  def to[A](fa: F[A]): G[A]
  def from[A](ga: G[A]): F[A]

  def tof: F ~> G   = makeFunctionK[F, G](to(_))
  def fromF: G ~> F = makeFunctionK[G, F](from(_))

  def inverse: IsoK[G, F] = new IsoK[G, F] {
    def to[A](fa: G[A]): F[A]   = self.from(fa)
    def from[A](ga: F[A]): G[A] = self.to(ga)

    override def inverse: IsoK[F, G] = self
  }

  def andThen[H[_]](that: IsoK[G, H]): IsoK[F, H] =
    new IsoK[F, H] {
      def to[A](fa: F[A]): H[A]   = that.to(self.to(fa))
      def from[A](ga: H[A]): F[A] = self.from(that.from(ga))
    }
}

object IsoK {
  def apply[F[_], G[_]](implicit isoK: IsoK[F, G]): IsoK[F, G] = isoK

  def id[F[_]]: IsoK[F, F] = new IsoK[F, F] {
    def to[A](fa: F[A]): F[A]   = fa
    def from[A](ga: F[A]): F[A] = ga
  }

  def byFunK[F[_], G[_]](fg: F ~> G)(gf: G ~> F): IsoK[F, G] =
    new IsoK[F, G] {
      def to[A](fa: F[A]): G[A]   = fg(fa)
      def from[A](ga: G[A]): F[A] = gf(ga)
    }
}
