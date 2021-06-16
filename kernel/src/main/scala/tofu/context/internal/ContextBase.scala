package tofu.context.internal

import cats.data.ReaderT
import cats.{Applicative, Functor}
import tofu.optics.{Contains, Equivalent, Extract}
import tofu._

/** Common base for instances */
trait ContextBase

object ContextBase {
  final implicit def readerTContext[F[_]: Applicative, C]: WithProvide[ReaderT[F, C, *], F, C] =
    new WithProvide[ReaderT[F, C, *], F, C] {
      override def lift[A](fa: F[A]): ReaderT[F, C, A] = ReaderT.liftF(fa)

      override def runContext[A](fa: ReaderT[F, C, A])(ctx: C): F[A] = fa.run(ctx)

    }
}

private[tofu] class ContextExtractInstance[F[_], C1, C2](ctx: F HasContext C1, extract: C1 Extract C2)
    extends WithContext[F, C2] {
  def functor: Functor[F] = ctx.functor

  def context: F[C2] = functor.map(ctx.context)(extract.extract)
}

private[tofu] class LocalContainsInstance[F[_], C1, C2](ctx: F HasLocal C1, contains: C1 Contains C2)
    extends ContextExtractInstance[F, C1, C2](ctx, contains) with WithLocal[F, C2] {
  def local[A](fa: F[A])(project: C2 => C2): F[A] = ctx.local(fa)(contains.update(_, project))
}

private[tofu] class ProvideExtractInstance[F[_], G[_], C1, C2](
    ctx: HasProvide[F, G, C1],
    extract: C2 Extract C1
) extends WithProvide[F, G, C2] {
  def runContext[A](fa: F[A])(c: Ctx): G[A] =
    ctx.runContext(fa)(extract.extract(c))

  def lift[A](ga: G[A]): F[A] = ctx.lift(ga)
}
