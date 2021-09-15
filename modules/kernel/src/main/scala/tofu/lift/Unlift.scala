package tofu
package lift

import cats.arrow.FunctionK
import cats.data.ReaderT
import cats.{Applicative, FlatMap, Functor, Monad, ~>}
import syntax.funk._
import tofu.optics.Contains
import tofu.syntax.monadic._
import kernel.types._
import tofu.internal.ContextBase

trait Lift[F[_], G[_]] {
  def lift[A](fa: F[A]): G[A]

  def liftF: FunctionK[F, G] = funK(lift(_))
}

object Lift extends LiftInstances1 {
  type AnyKK[_] = Any

  def apply[F[_], G[_]](implicit lift: Lift[F, G]): Lift[F, G] = lift
  def trans[F[_], G[_]](implicit lift: Lift[F, G]): F ~> G     = lift.liftF

  def byFunK[F[_], G[_]](fk: F ~> G): Lift[F, G] =
    new Lift[F, G] {
      def lift[A](fa: F[A]): G[A] = fk(fa)
    }

  private val liftIdentityAny: Lift[AnyKK, AnyKK] = new Lift[AnyKK, AnyKK] {
    def lift[A](fa: Any): Any = fa
  }
  implicit def liftIdentity[F[_]]: Lift[F, F]     = liftIdentityAny.asInstanceOf[Lift[F, F]]

  private val liftReaderTAny: Lift[AnyKK, ReaderT[Any, Any, *]] = {
    type RT[a] = ReaderT[Any, Any, a]
    new Lift[Any, RT] {
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

/** Embedded transformation. Can be used instead of a direct F ~> G. Especially useful one is `UnliftIO`, a replacement
  * for the `Effect` typeclass.
  */
trait Unlift[F[_], G[_]] extends Lift[F, G] with ContextBase { self =>
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
      def unlift: H[H ~> F]       =
        for {
          tfg <- ugh.lift(self.unlift)
          tgh <- ugh.unlift
        } yield tfg compose tgh
    }
}

object Unlift {
  def apply[F[_], G[_]](implicit unlift: Unlift[F, G]): Unlift[F, G] = unlift

  def byIso[F[_], G[_]: Applicative](iso: IsoK[F, G]): Unlift[F, G] =
    new Unlift[F, G] {
      def lift[A](fa: F[A]): G[A] = iso.to(fa)
      def unlift: G[G ~> F]       = iso.fromF.pure[G]
    }

  def subContextUnlift[F[_], G[_], I[_], A, B](implicit
      wrF: WithRun[F, I, A],
      wrG: WithRun[G, I, B],
      lens: A Contains B,
      F: FlatMap[F],
      G: FlatMap[G]
  ): Unlift[G, F] = new Unlift[G, F] {
    override def lift[X](fa: G[X]): F[X] =
      wrF.askF(a => wrF.lift(wrG.runContext(fa)(lens.get(a))))

    override def unlift: F[F ~> G] =
      wrF.ask(a => funK(fa => wrG.askF(b => wrG.lift(wrF.runContext(fa)(lens.set(a, b))))))
  }
}
