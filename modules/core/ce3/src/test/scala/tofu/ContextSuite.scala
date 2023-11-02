package tofu

import cats.Applicative
import cats.data.ReaderT
import glass.Contains

object ContextSuite {
  type Ctx >: Unit

  val ctx: Ctx = ()

  def testInstancesForReaderT[F[_]: Applicative](): Unit = {
    implicitly[HasContext[ReaderT[F, Ctx, _], Ctx]]
    implicitly[WithContext[ReaderT[F, Ctx, _], Ctx]]
    implicitly[HasLocal[ReaderT[F, Ctx, _], Ctx]]
    implicitly[WithLocal[ReaderT[F, Ctx, _], Ctx]]
    implicitly[HasProvide[ReaderT[F, Ctx, _], F, Ctx]]
    implicitly[WithProvide[ReaderT[F, Ctx, _], F, Ctx]]
    implicitly[HasContextRun[ReaderT[F, Ctx, _], F, Ctx]]
    implicitly[WithRun[ReaderT[F, Ctx, _], F, Ctx]]
    ()
  }

  def testRunContextSyntax[F[_], G[_], A](fa: F[A])(implicit rc: HasProvide[F, G, Ctx]): G[A] = {
    import syntax.context._
    runContext(fa)(ctx)
  }

  case class B()
  case class D(a: B)

  object D extends Context.Companion[D]

  import D._

  def contextFromLocalSummon[F[_]: ({ type L[x[_]] = WithLocal[x, D] })#L](implicit lens: D Contains B): Unit = {
    implicitly[WithContext[F, B]]
    ()
  }

  def contextFromContextSummon[F[_]: ({ type L[x[_]] = WithLocal[x, D] })#L](implicit lens: D Contains B): Unit = {
    implicitly[WithContext[F, B]]
    ()
  }
}
