package tofu
package lift

import cats.arrow.FunctionK
import cats.data.ReaderT
import cats.~>
import syntax.funk._

trait Lift[F[_], G[_]] {
  def lift[A](fa: F[A]): G[A]

  def liftF: FunctionK[F, G] = funK(lift(_))
}

object Lift extends LiftInstances1 {
  def apply[F[_], G[_]](implicit lift: Lift[F, G]): Lift[F, G] = lift
  def trans[F[_], G[_]](implicit lift: Lift[F, G]): F ~> G     = lift.liftF

  def byFunK[F[_], G[_]](fk: F ~> G): Lift[F, G] =
    new Lift[F, G] {
      def lift[A](fa: F[A]): G[A] = fk(fa)
    }

  private val liftIdentityAny: Lift[AnyK, AnyK] = new Lift[AnyK, AnyK] {
    def lift[A](fa: Any): Any = fa
  }
  implicit def liftIdentity[F[_]]: Lift[F, F]   = liftIdentityAny.asInstanceOf[Lift[F, F]]

  private val liftReaderTAny: Lift[AnyK, ReaderT[AnyK, Any, *]] = {
    type RT[a] = ReaderT[AnyK, Any, a]
    new Lift[AnyK, RT] {
      def lift[A](fa: Any): RT[A] = ReaderT.liftF(fa)
    }
  }
  implicit def liftReaderT[F[_], R]: Lift[F, ReaderT[F, R, *]] = liftReaderTAny.asInstanceOf[Lift[F, ReaderT[F, R, *]]]
}

private[lift] trait LiftInstances1 extends LiftInstances2 {
  implicit def byIso[F[_], G[_]](implicit iso: IsoK[F, G]): Lift[F, G] =
    new Lift[F, G] {
      def lift[A](fa: F[A]): G[A] = iso.to(fa)
    }
}

private[lift] trait LiftInstances2 {
  implicit def byIsoInverse[F[_], G[_]](implicit iso: IsoK[F, G]): Lift[G, F] =
    new Lift[G, F] {
      def lift[A](ga: G[A]): F[A] = iso.from(ga)
    }
}
