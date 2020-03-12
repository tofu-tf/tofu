package tofu

import cats.Applicative
import cats.data.ReaderT

object ContextSuite {
  type Ctx = Map[String, String]

  def testInstancesForReaderT[F[_]: Applicative](): Unit = {
    implicitly[HasContext[ReaderT[F, Ctx, *], Ctx]]
    implicitly[WithContext[ReaderT[F, Ctx, *], Ctx]]
    implicitly[HasLocal[ReaderT[F, Ctx, *], Ctx]]
    implicitly[WithLocal[ReaderT[F, Ctx, *], Ctx]]
    implicitly[HasContextRun[ReaderT[F, Ctx, *], F, Ctx]]
    implicitly[WithRun[ReaderT[F, Ctx, *], F, Ctx]]
    ()
  }

  def testRunContextSyntax[F[_], G[_], A](fa: F[A])(implicit rc: HasContextRun[F, G, Ctx]): G[A] = {
    import syntax.context._
    runContext(fa)(Map.empty[String, String])
  }

}
