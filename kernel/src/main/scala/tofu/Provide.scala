package tofu

import cats.data.ReaderT
import cats.{Applicative, ~>}
import tofu.context.internal._
import tofu.lift.Lift
import tofu.optics.Extract
import tofu.syntax.funk.funK

/** Allows to evaluate contextual computation with some context
  *
  * The main use case for it is to obtain some context `Lower[Ctx]`,
  * and then to run another contextual computation F[A] that can use `F[Ctx]` inside.
  *
  * @tparam F context-aware effect e.g.`ReaderT[Lower, Ctx, *]`
  */
trait Provide[F[_]] extends ContextBase {
  type Ctx

  /** Result effect type without any notion of context */
  type Lower[A]

  /** Runs `fa` providing `ctx` to it.
    *
    * One can treat F as function of type `Ctx => Lower[A]` so this method applies it to `ctx`.
    *
    * @example
    * Example of usage is to hide sensitive information across a part of service {{{
    *   import tofu.syntax.context._
    *
    *   case class UserContext(
    *       id: String,
    *       phoneNumber: String //very sensitive
    *   )
    *   def hidePartOfPhoneNumber: String => String = ???
    *
    *   def contextualLogInfo[F[_]](message: String)(implicit hasUserContext: UserContext In F): F[Unit] =
    *     ??? //logs both message AND UserContext
    *
    *   def program[F[_]: WithRun[*[_], G[_], UserContext], G[_]: Monad] = for {
    *     user   <- obtainUserSomehow[G]
    *     logUser = contextualLogInfo[F](s"Successfully obtained user")
    *     _      <- runContext[F](logUser)(user.toContext) //G[Unit]
    *   } yield ()
    * }}}
    * @param fa Contextual computation
    * @return Result of running fa and providing it context
    */
  def runContext[A](fa: F[A])(ctx: Ctx): Lower[A]

  /** Same as [[runContext]] but higher-kinded.
    *
    * @example {{{
    *   trait ProcessHandler[G[_]] {
    *     def mapK[M[_]](fk: G ~> M): ProcessHandler[M] = ???
    *     //...other methods
    *   }
    *   type WithMyContext[F[_], A] = ReaderT[F, MyCtx, A]
    *   val contextualProcessHandler: ProcessHandler[IO WithMyContext *] = ???
    *
    *   val processHandler: ProcessHandler[IO] =
    *     processHandler.mapK(
    *       WithProvide[IO WithMyContext *, IO, MyCtx].runContextK
    *     )
    * }}}
    */
  def runContextK(ctx: Ctx): F ~> Lower = funK(runContext(_)(ctx))

  /** Converts context-unaware computation into contextual one by ignoring context
    *
    * One can treat `F[A]` as function of type `Ctx => Lower[A]` so this method
    * creates  something like `_  => la` which has type `Ctx => Lower[A]`
    *
    * @return
    */
  def lift[A](la: Lower[A]): F[A]

  /** Allows to focus [[Provide]] on inner parts of its context with lens.
    *
    * @param extract lens that can extract from `Ctx` value of type `A`
    */
  def runExtract[A](extract: A Extract Ctx): WithProvide[F, Lower, A] =
    new ProvideExtractInstance[F, Lower, Ctx, A](this, extract)
}

/** Companion object for [[Provide]] */
object Provide {
  def apply[F[_]](implicit p: Provide[F]): HasProvide[F, p.Lower, p.Ctx] = p

  type Aux[F[_], G[_], C] = HasProvide[F, G, C]

  final implicit def readerTContext[F[_]: Applicative, C]: HasProvide[ReaderT[F, C, *], F, C] =
    ContextBase.readerTContext[F, C]
}

/** Synonym for [[Provide]] with explicit `C` as `Ctx` and `G` as `Lower` for better type inference
  *
  * Can be seen as transformation `F[*] = C => G[*]`
  */
trait WithProvide[F[_], G[_], C] extends Provide[F] with Lift[G, F] {
  override type Lower[A] = G[A]
  override type Ctx      = C
}

/** Companion object for [[WithProvide]] */
object WithProvide {
  def apply[F[_], G[_], C](implicit p: WithProvide[F, G, C]): WithProvide[F, G, C] = p
}
