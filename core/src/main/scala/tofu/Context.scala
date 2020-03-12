package tofu

import cats.{Applicative, Functor, Monad}
import cats.data.ReaderT
import tofu.optics.{Contains, Equivalent, Extract}

/** lightweight version of ApplicativeAsk */
trait Context[F[_]] {
  def functor: Functor[F]

  type Ctx
  def context: F[Ctx]

  def ask[A](f: Ctx => A): F[A] = functor.map(context)(f)

  def askF[A](f: Ctx => F[A])(implicit F: Monad[F]): F[A] = F.flatMap(context)(f)

  def extract[A](extr: Extract[Ctx, A]): HasContext[F, A] = new ContextExtractInstance[F, Ctx, A](this, extr)
}

object Context extends ContextInstances[HasContext] {
  def apply[F[_]](implicit ctx: Context[F]): HasContext[F, ctx.Ctx] = ctx
  type Aux[F[_], C] = HasContext[F, C]

  def const[F[_]: Applicative, C](c: C): HasContext[F, C] = new Context[F] {
    type Ctx = C
    val functor: Functor[F] = Functor[F]
    val context: F[C]       = Applicative[F].pure(c)
  }
}

trait WithContext[F[_], C] extends Context[F] {
  override type Ctx = C
}

object WithContext extends ContextInstances[WithContext] {
  def apply[F[_], C](implicit ctx: WithContext[F, C]): WithContext[F, C] = ctx

}

trait ContextInstances[TC[f[_], r] >: WithContext[f, r]] extends LocalInstances[TC]

trait Local[F[_]] extends Context[F] {
  def local[A](fa: F[A])(project: Ctx => Ctx): F[A]

  def subcontext[A](contains: Ctx Contains A): HasLocal[F, A] = new LocalContainsInstance[F, Ctx, A](this, contains)
}

object Local extends LocalInstances[HasLocal] {
  def apply[F[_]](implicit ctx: Local[F]): HasLocal[F, ctx.Ctx] = ctx
  type Aux[F[_], C] = HasLocal[F, C]
}

trait WithLocal[F[_], C] extends Local[F] with WithContext[F, C]

object WithLocal extends LocalInstances[WithLocal] {
  def apply[F[_], C](implicit ctx: WithLocal[F, C]): WithLocal[F, C] = ctx
}

trait LocalInstances[TCA[f[_], r] >: WithLocal[f, r]] extends RunContextInstances[λ[(`f[_]`, `g[_]`, r) => TCA[f, r]]]

trait Provide[F[_]] {
  type Ctx
  type Lower[A]

  def runContext[A](fa: F[A])(ctx: Ctx): Lower[A]

  def runExtract[A](extract: A Extract Ctx): HasProvide[F, Lower, A] =
    new ProvideExtractInstance[F, Lower, Ctx, A](this, extract)
}

object Provide extends ProvideInstances[HasProvide] {
  def apply[F[_]](implicit p: Provide[F]): HasProvide[F, p.Lower, p.Ctx] = p
  type Aux[F[_], G[_], C] = HasProvide[F, G, C]
}

trait WithProvide[F[_], G[_], C] extends Provide[F] {
  override type Lower[A] = G[A]
  override type Ctx      = C
}
object WithProvide extends ProvideInstances[WithProvide] {
  def apply[F[_], G[_], C](implicit p: WithProvide[F, G, C]): WithProvide[F, G, C] = p
}

trait ProvideInstances[TCA[f[_], g[_], r] >: WithProvide[f, g, r]] extends RunContextInstances[TCA]

trait RunContext[F[_]] extends Local[F] with Provide[F] {
  def runEquivalent[A](eq: Equivalent[Ctx, A]): HasContextRun[F, Lower, A] =
    new RunContextEquivalentInstance[F, Lower, Ctx, A](this, eq)
}

object RunContext extends RunContextInstances[HasContextRun] {
  def apply[F[_]](implicit ctx: RunContext[F]): HasContextRun[F, ctx.Lower, ctx.Ctx] = ctx
  type Aux[F[_], G[_], C] = HasContextRun[F, G, C]
}

trait WithRun[F[_], G[_], C] extends WithProvide[F, G, C] with WithLocal[F, C] with RunContext[F] {
  override type Ctx = C
}

object WithRun extends RunContextInstances[WithRun] {
  def apply[F[_], G[_], C](implicit ctx: WithRun[F, G, C]): WithRun[F, G, C] = ctx
}

trait RunContextInstances[TCA[f[_], g[_], r] >: WithRun[f, g, r]] {
  implicit def readerTContext[C, F[_]: Applicative]: TCA[ReaderT[F, C, *], F, C] = new WithRun[ReaderT[F, C, *], F, C] {
    override def runContext[A](fa: ReaderT[F, C, A])(ctx: C): F[A]                 = fa.run(ctx)
    override def local[A](fa: ReaderT[F, C, A])(project: C => C): ReaderT[F, C, A] = fa.local(project)
    val functor: Functor[ReaderT[F, C, *]]                                         = Functor[ReaderT[F, C, *]]
    def context: ReaderT[F, C, C]                                                  = ReaderT.ask[F, C]
  }
}

private[tofu] class ContextExtractInstance[F[_], C1, C2](ctx: F HasContext C1, extract: C1 Extract C2)
    extends WithContext[F, C2] {
  def functor: Functor[F] = ctx.functor
  def context: F[C2]      = functor.map(ctx.context)(extract.extract)
}

private[tofu] class LocalContainsInstance[F[_], C1, C2](ctx: F HasLocal C1, contains: C1 Contains C2)
    extends ContextExtractInstance[F, C1, C2](ctx, contains) with WithLocal[F, C2] {
  def local[A](fa: F[A])(project: C2 => C2): F[A] = ctx.local(fa)(contains.update(_, project))
}

private[tofu] class ProvideExtractInstance[F[_], G[_], C1, C2](
    ctx: HasProvide[F, G, C1],
    extract: C2 Extract C1
) extends Provide[F] {
  type Ctx      = C2
  type Lower[a] = G[a]

  def runContext[A](fa: F[A])(c: Ctx): G[A] =
    ctx.runContext(fa)(extract.extract(c))
}

private[tofu] class RunContextEquivalentInstance[F[_], G[_], C1, C2](
    ctx: HasContextRun[F, G, C1],
    equivalent: C1 Equivalent C2
) extends LocalContainsInstance[F, C1, C2](ctx, equivalent) with WithRun[F, G, C2] {
  def runContext[A](fa: F[A])(c: C2): G[A] = ctx.runContext(fa)(equivalent.upcast(c))
}

object HasContext {
  def apply[F[_], C](implicit hc: HasContext[F, C]): HasContext[F, C] = hc
}

object HasLocal {
  def apply[F[_], C](implicit hl: HasLocal[F, C]): HasLocal[F, C] = hl
}

object HasContextRun {
  def apply[F[_], G[_], C](implicit hcr: HasContextRun[F, G, C]): HasContextRun[F, G, C] = hcr
}
