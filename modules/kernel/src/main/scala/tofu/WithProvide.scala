package tofu

import cats.~>
import tofu.lift.Lift
import glass.Extract
import tofu.syntax.funk.funK

/** Synonym for [[Provide]] with explicit `C` as `Ctx` and `G` as `Lower` for better type inference
  *
  * Can be seen as transformation `F[*] = C => G[*]`
  */
trait WithProvide[F[_], G[_], C] extends Provide[F] with Lift[G, F] {
  override type Lower[A] = G[A]
  override type Ctx      = C

  /** Runs `fa` providing `ctx` to it.
    *
    * One can treat F as function of type `Ctx => Lower[A]` so this method applies it to `ctx`.
    *
    * @example
    *   Example of usage is to hide sensitive information across a part of service {{{ import tofu.syntax.context._
    *
    * case class UserContext( id: String, phoneNumber: String //very sensitive ) def hidePartOfPhoneNumber: String =>
    * String = ???
    *
    * def contextualLogInfo[F[_]](message: String)(implicit hasUserContext: UserContext In F): F[Unit] = ??? //logs both
    * message AND UserContext
    *
    * def program[F[_]: WithRun[*[_], G[_], UserContext], G[_]: Monad] = for { user <- obtainUserSomehow[G] logUser =
    * contextualLogInfo[F](s"Successfully obtained user") _ <- runContext[F](logUser)(user.toContext) //G[Unit] } yield
    * () }}}
    * @param fa
    *   Contextual computation
    * @return
    *   Result of running fa and providing it context
    */
  override def runContext[A](fa: F[A])(ctx: Ctx): Lower[A]

  /** Same as [[runContext]] but higher-kinded.
    *
    * @example
    *   {{{ trait ProcessHandler[G[_]] { def mapK[M[_]](fk: G ~> M): ProcessHandler[M] = ??? //...other methods } type
    *   WithMyContext[F[_], A] = ReaderT[F, MyCtx, A] val contextualProcessHandler: ProcessHandler[IO WithMyContext *] =
    *   ???
    *
    * val processHandler: ProcessHandler[IO] = processHandler.mapK( WithProvide[IO WithMyContext *, IO,
    * MyCtx].runContextK ) }}}
    */
  override def runContextK(ctx: Ctx): F ~> Lower = funK[F, Lower](runContext(_)(ctx))

  /** Converts context-unaware computation into contextual one by ignoring context
    *
    * One can treat `F[A]` as function of type `Ctx => Lower[A]` so this method creates something like `_ => la` which
    * has type `Ctx => Lower[A]`
    *
    * @return
    */
  def lift[A](la: Lower[A]): F[A]

  /** Allows to focus [[Provide]] on inner parts of its context with lens.
    *
    * @param extract
    *   lens that can extract from `Ctx` value of type `A`
    */
  override def runExtract[A](extract: A Extract Ctx): WithProvide[F, Lower, A] =
    new ProvideExtractInstance[F, Lower, Ctx, A](self, extract)

  def self = this
}

/** Companion object for [[WithProvide]] */
object WithProvide {
  def apply[F[_], G[_], C](implicit p: WithProvide[F, G, C]): WithProvide[F, G, C] = p
}

private[tofu] class ProvideExtractInstance[F[_], G[_], C1, C2](
    ctx: WithProvide[F, G, C1],
    extract: C2 Extract C1
) extends WithProvide[F, G, C2] {
  def runContext[A](fa: F[A])(c: Ctx): G[A] =
    ctx.runContext(fa)(extract.extract(c))

  def lift[A](ga: G[A]): F[A] = ctx.lift(ga)
}
