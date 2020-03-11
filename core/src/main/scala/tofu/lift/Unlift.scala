package tofu
package lift

import cats.arrow.FunctionK
import cats.data.ReaderT
import cats.{Applicative, Functor, Monad, ~>}
import syntax.functionK._
import tofu.syntax.monadic._

trait Lift[F[_], G[_]] {
  def lift[A](fa: F[A]): G[A]

  def liftF: FunctionK[F, G] = makeFunctionK(lift(_))
}

object Lift {
  def apply[F[_], G[_]](implicit lift: Lift[F, G]): Lift[F, G] = lift
  def trans[F[_], G[_]](implicit lift: Lift[F, G]): F ~> G     = lift.liftF

  private val liftIdentityAny: Lift[AnyK, AnyK] = new Lift[AnyK, AnyK] {
    def lift[A](fa: Any): Any = fa
  }
  implicit def liftIdentity[F[_]]: Lift[F, F] = liftIdentityAny.asInstanceOf[Lift[F, F]]

  private val liftReaderTAny: Lift[AnyK, ReaderT[AnyK, Any, *]] = {
    type RT[a] = ReaderT[AnyK, Any, a]
    new Lift[AnyK, RT] {
      def lift[A](fa: Any): RT[A] = ReaderT.liftF(fa)
    }
  }
  implicit def liftReaderT[F[_], R]: Lift[F, ReaderT[F, R, *]] = liftReaderTAny.asInstanceOf[Lift[F, ReaderT[F, R, *]]]

  def byIso[F[_], G[_]](iso: IsoK[F, G]): Lift[F, G] =
    new Lift[F, G] {
      def lift[A](fa: F[A]): G[A] = iso.to(fa)
    }
}

/** embedded transformation
  * can be used instead of direct F ~> G
  * especially useful Unlift[_[_], IO] as replacement for `Effect` typeclass */
trait Unlift[F[_], G[_]] extends Lift[F, G] {
  self =>
  def lift[A](fa: F[A]): G[A]
  def unlift: G[G ~> F]

  def subIso(implicit G: Functor[G]): G[IsoK[F, G]] =
    unlift.map(gf =>
      new IsoK[F, G] {
        def to[A](fa: F[A]): G[A]   = self.lift(fa)
        def from[A](ga: G[A]): F[A] = gf(ga)
      }
    )

  def andThen[H[_]: Monad](ugh: Unlift[G, H]): Unlift[F, H] =
    new Unlift[F, H] {
      def lift[A](fa: F[A]): H[A] = ugh.lift(self.lift(fa))
      def unlift: H[H ~> F] =
        for {
          tfg <- ugh.lift(self.unlift)
          tgh <- ugh.unlift
        } yield tfg compose tgh
    }
}

object Unlift {
  implicit def unliftIdentity[F[_]: Applicative]: Unlift[F, F] = new Unlift[F, F] {
    def lift[A](fa: F[A]): F[A] = fa
    def unlift: F[F ~> F]       = FunctionK.id[F].pure[F]
  }

  implicit def unliftReaderT[F[_]: Applicative, R]: Unlift[F, ReaderT[F, R, *]] = {
    type RT[a] = ReaderT[F, R, a]
    new Unlift[F, RT] {
      def lift[A](fa: F[A]): RT[A] = ReaderT.liftF(fa)
      def unlift: RT[RT ~> F]      = ReaderT(r => makeFunctionK[RT, F](_.run(r)).pure[F])
    }
  }

  def byIso[F[_], G[_]: Applicative](iso: IsoK[F, G]): Unlift[F, G] =
    new Unlift[F, G] {
      def lift[A](fa: F[A]): G[A] = iso.to(fa)
      def unlift: G[G ~> F]       = iso.fromF.pure[G]
    }
}
