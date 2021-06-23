package tofu.context.internal

import cats.Applicative
import cats.data.ReaderT
import cats.Functor
import tofu.optics.Contains
import tofu.optics.Extract
import tofu._

/** Common base for instances */
trait ContextBase

object ContextBase {
  final implicit def readerTWithProvide[F[_], C]: WithProvide[ReaderT[F, C, *], F, C]       =
    new WithProvide[ReaderT[F, C, *], F, C] {
      override def lift[A](fa: F[A]): ReaderT[F, C, A] = ReaderT.liftF(fa)

      override def runContext[A](fa: ReaderT[F, C, A])(ctx: C): F[A] = fa.run(ctx)
    }
  final implicit def readerTWithLocal[F[_]: Applicative, C]: WithLocal[ReaderT[F, C, *], C] =
    new WithLocal[ReaderT[F, C, *], C] {

      override def local[A](fa: ReaderT[F, C, A])(
          project: C => C
      ): ReaderT[F, C, A]                             = fa.local(project)
      override def functor: Functor[ReaderT[F, C, *]] = Functor[ReaderT[F, C, *]]
      override def context: ReaderT[F, C, C]          = ReaderT.ask[F, C]
    }
}

private[tofu] class ContextExtractInstance[F[_], C1, C2](ctx: Context.Aux[F, C1], extract: C1 Extract C2)
    extends WithContext[F, C2] {
  def functor: Functor[F] = ctx.functor

  def context: F[C2] = functor.map(ctx.context)(extract.extract)
}

private[tofu] class LocalContainsInstance[F[_], C1, C2](ctx: Local.Aux[F, C1], contains: C1 Contains C2)
    extends ContextExtractInstance[F, C1, C2](ctx, contains) with WithLocal[F, C2] {
  def local[A](fa: F[A])(project: C2 => C2): F[A] = ctx.local(fa)(contains.update(_, project))
}

private[tofu] class ProvideExtractInstance[F[_], G[_], C1, C2](
    ctx: Provide.Aux[F, G, C1],
    extract: C2 Extract C1
) extends WithProvide[F, G, C2] {
  def runContext[A](fa: F[A])(c: Ctx): G[A] =
    ctx.runContext(fa)(extract.extract(c))

  def lift[A](ga: G[A]): F[A] = ctx.lift(ga)
}
