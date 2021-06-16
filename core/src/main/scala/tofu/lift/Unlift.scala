package tofu
package lift

import cats.arrow.FunctionK
import cats.data.ReaderT
import cats.effect.{Effect, IO}
import cats.{Applicative, FlatMap, Functor, Monad, ~>}
import syntax.funk._
import tofu.optics.Contains
import tofu.syntax.monadic._

/** Embedded transformation. Can be used instead of a direct F ~> G.
  * Especially useful one is `UnliftIO`, a replacement for the `Effect` typeclass.
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

object Unlift extends UnliftInstances1 {
  def apply[F[_], G[_]](implicit unlift: Unlift[F, G]): Unlift[F, G] = unlift

  implicit def unliftIdentity[F[_]: Applicative]: Unlift[F, F] = new Unlift[F, F] {
    def lift[A](fa: F[A]): F[A] = fa
    def unlift: F[F ~> F]       = FunctionK.id[F].pure[F]
  }

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

  implicit def unliftIOReaderTViaUnliftIO[F[_]: UnliftIO: Functor, R]: UnliftIO[ReaderT[F, R, *]] = {
    type RT[a] = ReaderT[F, R, a]
    new UnliftIO[RT] {
      def lift[A](fa: IO[A]): RT[A] = ReaderT.liftF(UnliftIO[F].lift(fa))
      def unlift: RT[RT ~> IO]      = ReaderT(r => UnliftIO[F].unlift.map(toIO => funK[RT, IO](rt => toIO(rt.run(r)))))
    }
  }
}

private[lift] trait UnliftInstances1 {
  final implicit def unliftIOEffect[F[_]: Effect]: UnliftIO[F] = new UnliftIO[F] {
    def lift[A](fa: IO[A]): F[A] = Effect[F].liftIO(fa)
    def unlift: F[F ~> IO]       = Effect.toIOK[F].pure[F]
  }
}
