package tofu.internal


import cats.arrow.FunctionK
import cats.data.ReaderT
import cats.{Applicative, Functor, Monad, ~>}
import tofu.WithRun
import tofu.internal.carriers.UnliftEffect
import tofu.lift.Unlift
import tofu.syntax.monadic._

/** Common base for instances */
trait ContextBase

object ContextBase extends ContextBaseInstances1 {
  implicit def unliftIdentity[F[_] : Applicative]: Unlift[F, F] = new Unlift[F, F] {
    def lift[A](fa: F[A]): F[A] = fa

    def unlift: F[F ~> F] = FunctionK.id[F].pure[F]
  }


}
trait ContextBaseInstances1 extends ContextBaseInstances2{
  final implicit def readerTContext[F[_] : Applicative, C]: WithRun[ReaderT[F, C, *], F, C] =
    new WithRun[ReaderT[F, C, *], F, C] {
      def lift[A](fa: F[A]): ReaderT[F, C, A] = ReaderT.liftF(fa)

      def runContext[A](fa: ReaderT[F, C, A])(ctx: C): F[A] = fa.run(ctx)

      def local[A](fa: ReaderT[F, C, A])(project: C => C): ReaderT[F, C, A] = fa.local(project)

      val functor: Functor[ReaderT[F, C, *]] = Functor[ReaderT[F, C, *]]
      val context: ReaderT[F, C, C] = ReaderT.ask[F, C]
    }
}

trait ContextBaseInstances2 extends ContextBaseInstances3 {
  final implicit def unliftReaderCompose[F[_]: Monad, G[_], R](implicit FG: Unlift[G, F]): Unlift[G, ReaderT[F, R, *]] =
    FG.andThen(ContextBase.readerTContext[F, R])
}

trait ContextBaseInstances3 {
  final implicit def unliftIOEffect[F[_], G[_]](implicit carrier: UnliftEffect[F, G]): Unlift[F, G] =
    carrier.value
}

