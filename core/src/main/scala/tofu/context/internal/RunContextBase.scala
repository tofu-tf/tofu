package tofu.context.internal
import cats.Functor
import cats.data.ReaderT
import tofu.WithProvide
import cats.Applicative

trait RunContextBase

object RunContextBase {
  final implicit def readerTContext[F[_]: Applicative, C]: WithRun[ReaderT[F, C, *], F, C] =
    new WithRun[ReaderT[F, C, *], F, C] {
      override def lift[A](fa: F[A]): ReaderT[F, C, A] = ReaderT.liftF(fa)

      override def runContext[A](fa: ReaderT[F, C, A])(ctx: C): F[A] = fa.run(ctx)

      override def local[A](fa: ReaderT[F, C, A])(project: C => C): ReaderT[F, C, A] = fa.local(project)

      override val functor: Functor[ReaderT[F, C, *]] = Functor[ReaderT[F, C, *]]
      override val context: ReaderT[F, C, C]          = ReaderT.ask[F, C]
    }
}
