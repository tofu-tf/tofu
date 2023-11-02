package tofu.internal

import cats.arrow.FunctionK
import cats.data.ReaderT
import cats.{Applicative, Functor, Monad, ~>}
import tofu.internal.instances.ContextBaseCarrierInstance
import tofu.lift.Unlift
import tofu.syntax.monadic._
import tofu.{WithContext, WithLocal, WithProvide, WithRun}

/** Common base for instances */
trait ContextBase

object ContextBase extends ContextBaseInstances1 {
  implicit def unliftIdentity[F[_]: Applicative]: Unlift[F, F] = new Unlift[F, F] {
    def lift[A](fa: F[A]): F[A] = fa

    def unlift: F[F ~> F] = FunctionK.id[F].pure[F]
  }

}

trait ContextBaseInstances1 extends ContextBaseInstances2 {
  final implicit def readerTContext[F[_]: Applicative, C]: WithRun[ReaderT[F, C, _], F, C] =
    new WithRun[ReaderT[F, C, _], F, C] {
      def lift[A](fa: F[A]): ReaderT[F, C, A] = ReaderT.liftF(fa)

      def runContext[A](fa: ReaderT[F, C, A])(ctx: C): F[A] = fa.run(ctx)

      def local[A](fa: ReaderT[F, C, A])(project: C => C): ReaderT[F, C, A] = fa.local(project)

      val functor: Functor[ReaderT[F, C, _]] = Functor[ReaderT[F, C, _]]
      val context: ReaderT[F, C, C]          = ReaderT.ask[F, C]
    }
}

trait ContextBaseInstances2 extends ContextBaseInstances3 {
  final implicit def runReaderTWrapped[F[_], G[_], C, R](implicit
      F: WithRun[F, G, C]
  ): WithRun[ReaderT[F, R, _], ReaderT[G, R, _], C] =
    new WithRun[ReaderT[F, R, _], ReaderT[G, R, _], C] {
      val functor: Functor[ReaderT[F, R, _]]                                = ReaderT.catsDataFunctorForKleisli(F.functor)
      val context: ReaderT[F, R, C]                                         = ReaderT.liftF(F.context)
      def lift[A](fa: ReaderT[G, R, A]): ReaderT[F, R, A]                   = ReaderT(r => F.liftF(fa.run(r)))
      def local[A](fa: ReaderT[F, R, A])(project: C => C): ReaderT[F, R, A] = ReaderT(r => F.local(fa.run(r))(project))
      def runContext[A](fa: ReaderT[F, R, A])(ctx: C): ReaderT[G, R, A]     = ReaderT(r => F.runContext(fa.run(r))(ctx))
    }
}

trait ContextBaseInstances3 extends ContextBaseInstances4 {
  final implicit def localReaderTWrapped[F[_], C, R](implicit
      F: WithLocal[F, C]
  ): WithLocal[ReaderT[F, R, _], C] =
    new WithLocal[ReaderT[F, R, _], C] {
      val functor: Functor[ReaderT[F, R, _]]                                = ReaderT.catsDataFunctorForKleisli(F.functor)
      val context: ReaderT[F, R, C]                                         = ReaderT.liftF(F.context)
      def local[A](fa: ReaderT[F, R, A])(project: C => C): ReaderT[F, R, A] = ReaderT(r => F.local(fa.run(r))(project))
    }

  final implicit def provideReaderTWrapped[F[_], G[_], C, R](implicit
      F: WithProvide[F, G, C]
  ): WithProvide[ReaderT[F, R, _], ReaderT[G, R, _], C] =
    new WithProvide[ReaderT[F, R, _], ReaderT[G, R, _], C] {
      def lift[A](fa: ReaderT[G, R, A]): ReaderT[F, R, A]               = ReaderT(r => F.liftF(fa.run(r)))
      def runContext[A](fa: ReaderT[F, R, A])(ctx: C): ReaderT[G, R, A] = ReaderT(r => F.runContext(fa.run(r))(ctx))
    }
}

trait ContextBaseInstances4 extends ContextBaseInstances5 {
  final implicit def contextReaderTWrapped[F[_], C, R](implicit
      F: WithContext[F, C]
  ): WithContext[ReaderT[F, R, _], C] =
    new WithContext[ReaderT[F, R, _], C] {
      val functor: Functor[ReaderT[F, R, _]] = ReaderT.catsDataFunctorForKleisli(F.functor)
      val context: ReaderT[F, R, C]          = ReaderT.liftF(F.context)
    }
}

trait ContextBaseInstances5 extends ContextBaseCarrierInstance {
  final implicit def unliftReaderCompose[F[_]: Monad, G[_], R](implicit FG: Unlift[G, F]): Unlift[G, ReaderT[F, R, _]] =
    FG.andThen(ContextBase.readerTContext[F, R])
}
