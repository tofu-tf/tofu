package tofu

import cats.{Applicative, FlatMap, Functor, ~>}
import cats.data.ReaderT
import tofu.lift.{Lift, Unlift}
import tofu.optics.{Contains, Equivalent, Extract}
import tofu.syntax.funk._

/** Common base for instances */
trait ContextBase

object ContextBase {
  final implicit def readerTContext[F[_]: Applicative, C]: WithRun[ReaderT[F, C, *], F, C] =
    new WithRun[ReaderT[F, C, *], F, C] {
      def lift[A](fa: F[A]): ReaderT[F, C, A]                               = ReaderT.liftF(fa)
      def runContext[A](fa: ReaderT[F, C, A])(ctx: C): F[A]                 = fa.run(ctx)
      def local[A](fa: ReaderT[F, C, A])(project: C => C): ReaderT[F, C, A] = fa.local(project)
      val functor: Functor[ReaderT[F, C, *]]                                = Functor[ReaderT[F, C, *]]
      val context: ReaderT[F, C, C]                                         = ReaderT.ask[F, C]
    }
}

/** lightweight version of ApplicativeAsk
  * consider to use `WithContext` or `In` for better type inference
  */
trait Context[F[_]] extends ContextBase {
  def functor: Functor[F]

  type Ctx
  def context: F[Ctx]

  def ask[A](f: Ctx => A): F[A] = functor.map(context)(f)

  def askF[A](f: Ctx => F[A])(implicit F: FlatMap[F]): F[A] = F.flatMap(context)(f)

  def extract[A](extr: Extract[Ctx, A]): WithContext[F, A] = new ContextExtractInstance[F, Ctx, A](this, extr)
}

object Context {
  def apply[F[_]](implicit ctx: Context[F]): HasContext[F, ctx.Ctx] = ctx
  type Aux[F[_], C] = HasContext[F, C]

  def const[F[_]: Applicative, C](c: C): WithContext[F, C] = new WithContext[F, C] {
    val functor: Functor[F] = Functor[F]
    val context: F[C]       = Applicative[F].pure(c)
  }

  def make[F[_]: Functor, A](fa: F[A]): WithContext[F, A] = new WithContext[F, A] {
    val functor: Functor[F] = implicitly
    val context: F[A]       = fa
  }

  /** a mix-in for supplying environment data type companions with useful things
    * {{{
    * // @ClassyOptics
    * case class MyContext(...)
    * object MyContext extends Context.Companion[MyContext]
    * }}}
    */
  trait Companion[C] {
    type Has[F[_]] = WithContext[F, C]

    implicit def tofuPromoteContextStructure[F[_], A](implicit
        context: WithLocal[F, C],
        field: C Contains A
    ): WithLocal[F, A] =
      context.subcontext(field)
  }
}

/** lightweight version of ApplicativeAsk
  *  Can be understood as functional replacement for global variable
  * or implicit F[C]
  */
trait WithContext[F[_], C] extends Context[F] {
  override type Ctx = C
}

object WithContext {
  def apply[F[_], C](implicit ctx: WithContext[F, C]): WithContext[F, C] = ctx
}

/** lightweight version of ApplicativeLocal
  * consider using `WithLocal` for better type inference
  */
trait Local[F[_]] extends Context[F] {
  def local[A](fa: F[A])(project: Ctx => Ctx): F[A]

  def subcontext[A](contains: Ctx Contains A): WithLocal[F, A] = new LocalContainsInstance[F, Ctx, A](this, contains)
}

object Local {
  def apply[F[_]](implicit ctx: Local[F]): HasLocal[F, ctx.Ctx] = ctx
  type Aux[F[_], C] = HasLocal[F, C]
}

/** lightweight version of ApplicativeLocal
  * allows to modify value of `C` locally for some subprocess
  */
trait WithLocal[F[_], C] extends Local[F] with WithContext[F, C]

object WithLocal {
  def apply[F[_], C](implicit ctx: WithLocal[F, C]): WithLocal[F, C] = ctx
}

trait Provide[F[_]] extends ContextBase {
  type Ctx
  type Lower[A]

  def runContext[A](fa: F[A])(ctx: Ctx): Lower[A]
  def runContextK(ctx: Ctx): F ~> Lower = funK(runContext(_)(ctx))

  def lift[A](la: Lower[A]): F[A]

  def runExtract[A](extract: A Extract Ctx): WithProvide[F, Lower, A] =
    new ProvideExtractInstance[F, Lower, Ctx, A](this, extract)
}

object Provide {
  def apply[F[_]](implicit p: Provide[F]): HasProvide[F, p.Lower, p.Ctx] = p
  type Aux[F[_], G[_], C] = HasProvide[F, G, C]

  final implicit def readerTContext[F[_]: Applicative, C]: HasProvide[ReaderT[F, C, *], F, C] =
    ContextBase.readerTContext[F, C]
}

trait WithProvide[F[_], G[_], C] extends Provide[F] with Lift[G, F] {
  override type Lower[A] = G[A]
  override type Ctx      = C
}
object WithProvide {
  def apply[F[_], G[_], C](implicit p: WithProvide[F, G, C]): WithProvide[F, G, C] = p
}

trait RunContext[F[_]] extends Local[F] with Provide[F] {
  def unlift: F[F ~> Lower]                                          = ask(ctx => funK(runContext(_)(ctx)))
  def runEquivalent[A](eq: Equivalent[Ctx, A]): WithRun[F, Lower, A] =
    new RunContextEquivalentInstance[F, Lower, Ctx, A](this, eq)
}

object RunContext {
  def apply[F[_]](implicit ctx: RunContext[F]): HasContextRun[F, ctx.Lower, ctx.Ctx] = ctx
  type Aux[F[_], G[_], C] = HasContextRun[F, G, C]

  final implicit def readerTContext[F[_]: Applicative, C]: HasContextRun[ReaderT[F, C, *], F, C] =
    ContextBase.readerTContext[F, C]
}

trait WithRun[F[_], G[_], C] extends WithProvide[F, G, C] with WithLocal[F, C] with RunContext[F] with Unlift[G, F] {
  override type Ctx = C
}

object WithRun {
  def apply[F[_], G[_], C](implicit ctx: WithRun[F, G, C]): WithRun[F, G, C] = ctx
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
) extends WithProvide[F, G, C2] {
  def runContext[A](fa: F[A])(c: Ctx): G[A] =
    ctx.runContext(fa)(extract.extract(c))
  def lift[A](ga: G[A]): F[A]               = ctx.lift(ga)
}

private[tofu] class RunContextEquivalentInstance[F[_], G[_], C1, C2](
    ctx: HasContextRun[F, G, C1],
    equivalent: C1 Equivalent C2
) extends LocalContainsInstance[F, C1, C2](ctx, equivalent) with WithRun[F, G, C2] {
  def runContext[A](fa: F[A])(c: C2): G[A] = ctx.runContext(fa)(equivalent.upcast(c))
  def lift[A](ga: G[A]): F[A]              = ctx.lift(ga)
}

object HasContext {
  def apply[F[_], C](implicit hc: HasContext[F, C]): HasContext[F, C] = hc
}

object HasLocal {
  def apply[F[_], C](implicit hl: HasLocal[F, C]): HasLocal[F, C] = hl
}

object HasProvide {
  def apply[F[_], G[_], C](implicit hp: HasProvide[F, G, C]): HasProvide[F, G, C] = hp
}

object HasContextRun {
  def apply[F[_], G[_], C](implicit hcr: HasContextRun[F, G, C]): HasContextRun[F, G, C] = hcr
}
