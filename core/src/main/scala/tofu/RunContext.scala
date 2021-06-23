package tofu

import cats.data.ReaderT
import cats.Applicative
import cats.~>
import tofu.lift.Unlift
import tofu.optics.Equivalent
import tofu.syntax.funk.funK
import tofu.context.internal._

/** Combination of [[Local]] and [[Provide]]
  *
  * @tparam F context-aware effect e.g.`ReaderT[Lower, Ctx, *]`
  */
trait RunContext[F[_]] extends Local[F] with Provide[F] {

  /** Allows to convert some context-unaware computation into contextual one.
    *
    * @example {{{
    *   trait ProcessHandler[G[_]] {
    *     def mapK[M[_]](fk: G ~> M): ProcessHandler[M] = ???
    *     //...other methods
    *   }
    *
    *   type WithMyContext[F[_], A] = ReaderT[F, MyCtx, A]
    *
    *   val processHandler: ProcessHandler[IO WithMyContext *] = ???
    *
    *   val contextualHandler: IO WithMyContext ProcessHandler[IO] =
    *     processHandler.mapK(
    *       WithRun[WithMyContext[IO, *], IO, MyCtx].unlift.map(fk => processHandler.mapK(fk))
    *     ) //now it is able to process MyCtx but is wrapped in IO WithMyContext *
    * }}}
    * @return
    */
  def unlift: F[F ~> Lower] = ask(ctx => funK(runContext(_)(ctx)))

  /** Allows to focus [[Provide]] on inner parts of its context with equivalence lens.
    *
    * @param eq lens that can convert from `Ctx` value of type `A`
    */
  def runEquivalent[A](eq: Equivalent[Ctx, A]): WithRun[F, Lower, A] =
    new RunContextEquivalentInstance[F, Lower, Ctx, A](this, eq)
}

private[tofu] class RunContextEquivalentInstance[F[_], G[_], C1, C2](
    ctx: HasContextRun[F, G, C1],
    equivalent: C1 Equivalent C2
) extends LocalContainsInstance[F, C1, C2](ctx, equivalent) with WithRun[F, G, C2] {
  def runContext[A](fa: F[A])(c: C2): G[A] = ctx.runContext(fa)(equivalent.upcast(c))

  def lift[A](ga: G[A]): F[A] = ctx.lift(ga)
}

/** Companion object for [[RunContext]] */
object RunContext {
  def apply[F[_]](implicit ctx: RunContext[F]): HasContextRun[F, ctx.Lower, ctx.Ctx] = ctx

  type Aux[F[_], G[_], C] = RunContext[F] { type Lower[A] = G[A]; type Ctx = C; }

  final implicit def readerTContext[F[_]: Applicative, C]: HasContextRun[ReaderT[F, C, *], F, C] =
    RunContextBase.readerTContext[F, C]
}

/** Synonym for both [[RunContext]] and [[Unlift]] with explicit `C` as `Ctx` and `G` as `Lower` for better type inference
  *
  * Can be seen as transformation `F[*] = C => G[*]`
  */
trait WithRun[F[_], G[_], C] extends WithProvide[F, G, C] with WithLocal[F, C] with RunContext[F] with Unlift[G, F] {
  override type Ctx = C
}

/** Companion object for [[WithRun]] */
object WithRun       {
  def apply[F[_], G[_], C](implicit ctx: WithRun[F, G, C]): WithRun[F, G, C] = ctx
}
object HasContextRun {
  def apply[F[_], G[_], C](implicit hcr: HasContextRun[F, G, C]): HasContextRun[F, G, C] = hcr
}
