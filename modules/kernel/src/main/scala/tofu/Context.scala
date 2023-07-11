package tofu

import cats.{Applicative, FlatMap, Functor, ~>}
import tofu.internal.ContextBase
import tofu.kernel.types._
import glass.{Contains, Equivalent, Extract}
import tofu.syntax.funk._

/** Declares that [[F]] can provide value of type Ctx
  *
  * In other words this trait tells you that F is some kind of Reader or function of type `Ctx => F[A]`. The best way to
  * use it is through `WithContext` or `In`
  *
  * This can be seen as global value for your Application or for a part of it.
  *
  * @example
  *   One common use of this is to make contextual logging:
  *   {{{
  *     import tofu.syntax.console._
  *
  *   def contexualConsolling[F[_]: Console: FlatMap](message: String)(implicit hasMyCtx: F HasContext MyCtx): F[Unit] =
  *     hasMyCtx.askF(ctx => puts"$message (Also context: $ctx)")
  *   }}}
  *   so when you have `MyCtx(3)` the call of `contextualConsolling("Hi!")` prints `Hi! (Also MyCtx(3))`
  */
@deprecated("Migrate to With* typeclasses", "0.10.3")
trait Context[F[_]] extends ContextBase { self =>
  def functor: Functor[F]

  type Ctx

  def context: F[Ctx]

  /** Returns context modified by pure function `f`.
    *
    * For example one can use it like that:
    * {{{
    *   Context[F].ask(myCtx => myCtx.toString)
    * }}}
    *
    * Also there is more convenient syntax:
    * {{{
    *   import tofu.syntax.context._
    *
    *   ask[F](myCtx => myCtx.toString)
    * }}}
    *
    * @note
    *   It does not affect context itself.
    */
  def ask[A](f: Ctx => A): F[A] = functor.map(context)(f)

  /** Same as [[ask]] but `f` is effectual */
  def askF[A](f: Ctx => F[A])(implicit F: FlatMap[F]): F[A] = F.flatMap(context)(f)

  /** Allows to focus context on its inside with lens.
    *
    * @param extract
    *   lens that can extract value of type `A` from `Ctx`
    */
  def extract[A](extract: Extract[Ctx, A]): WithContext[F, A] =
    new ContextExtractInstance[F, Ctx, A](asWithContext, extract)

  private[tofu] def asWithContext: WithContext[F, Ctx]
}

/** Companion object for [[Context]] */
@deprecated("Migrate to With* typeclasses", "0.10.3")
object Context {
  def apply[F[_]](implicit ctx: Context[F]): HasContext[F, ctx.Ctx] = ctx

  type Aux[F[_], C] = HasContext[F, C]

  /** Creates constant Context of type C into F
    *
    * Allows to put some value into context of F even when F itself does not have any way to store it
    *
    * @example
    *   {{{ case class MyCtx(id: Int)
    *
    * val program: Option[String] = { implicit val ctx = Context.const[Option, MyCtx](MyCtx(3))
    *
    * Context[Option].ask(ctx => ctx.id.toString) } }}}
    */
  def const[F[_]: Applicative, C](c: C): WithContext[F, C] = WithContext.const(c)

  /** Same as const but context is effectual here
    *
    * @example
    *   {{{ def handleRequest[F[_]: Monad: *[_] WithContext TraceId](httpRequest: Request) = ???
    *
    * def myProgram[F[_]: Monad] = for { request <- getRequest[F] implicit0(ctx: TraceId In F) <-
    * Context.make(genTraceId[F]) _ <- handleRequest[F](request) } yield () }}}
    */
  def make[F[_]: Functor, A](fa: F[A]): WithContext[F, A] = WithContext.make(fa)

  /** A mix-in for supplying environment data type companions with useful things
    *
    * @example
    *   {{{
    * @ClassyOptics
    *   case class MyContext(id: Int, date: String)
    *
    * object MyContext extends Context.Companion[MyContext]
    *   }}}
    */
  trait Companion[C] extends WithContext.Companion[C]
}

/** Allows to run some computation with notion of altered context consider using `WithLocal` for better type inference
  */
@deprecated("Migrate to With* typeclasses", "0.10.3")
trait Local[F[_]] extends Context[F] { self =>

  /** Alters context for computation
    *
    * @example
    *   Example of usage is to hide sensitive information {{{ case class UserContext( id: String, phoneNumber: String
    *   //very sensitive )
    *
    * def hidePartOfPhoneNumber: String => String = ???
    *
    * def contextualLogInfo[F[_]](message: String)(implicit hasUserContext: UserContext In F): F[Unit] = ??? //logs both
    * message AND UserContext
    *
    * def program[F[_]: Monad] = for { user <- obtainUserSomehow[F] implicit0(hasUserContext: In[User, F]) <-
    * Context.const[F, User](user.toContext) logUser = contextualLogInfo[F](s"Successfully obtained user") _ <-
    * logUser.local(hidePartOfPhoneNumber) //logs only part of phone number } yield ()
    *
    * }}}
    * @param fa
    *   computation that is going to run with different context
    * @param project
    *   pure function
    * @return
    *   result of ran computation `fa`
    */
  def local[A](fa: F[A])(project: Ctx => Ctx): F[A]

  /** Allows to focus [[Local]] on inner parts of its context with lens.
    *
    * @param contains
    *   lens that can extract from `Ctx` or set some value of type `A`
    */
  def subcontext[A](contains: Ctx Contains A): WithLocal[F, A] =
    new LocalContainsInstance[F, Ctx, A](asWithLocal, contains)

  private[tofu] def asWithLocal: WithLocal[F, Ctx] = {
    new WithLocal[F, Ctx] {
      override def context: F[Ctx] = self.context

      override def functor: Functor[F] = self.functor

      override def local[B](fa: F[B])(project: Ctx => Ctx): F[B] = self.local(fa)(project)
    }
  }
}

@deprecated("Migrate to With* typeclasses", "0.10.3")
object Local {
  def apply[F[_]](implicit ctx: Local[F]): HasLocal[F, ctx.Ctx] = ctx

  type Aux[F[_], C] = HasLocal[F, C]
}

/** Allows to evaluate contextual computation with some context
  *
  * The main use case for it is to obtain some context `Lower[Ctx]`, and then to run another contextual computation F[A]
  * that can use `F[Ctx]` inside.
  *
  * @tparam F
  *   context-aware effect e.g.`ReaderT[Lower, Ctx, *]`
  */
@deprecated("Migrate to With* typeclasses", "0.10.3")
trait Provide[F[_]] extends ContextBase {
  type Ctx

  /** Result effect type without any notion of context */
  type Lower[A]

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
  def runContext[A](fa: F[A])(ctx: Ctx): Lower[A]

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
  def runContextK(ctx: Ctx): F ~> Lower = funK[F, Lower](a => runContext(a)(ctx))

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
  def runExtract[A](extract: A Extract Ctx): WithProvide[F, Lower, A] =
    new ProvideExtractInstance[F, Lower, Ctx, A](self, extract)

  def self: WithProvide[F, Lower, Ctx]
}

/** Companion object for [[Provide]] */
@deprecated("Migrate to With* typeclasses", "0.10.3")
object Provide {
  def apply[F[_]](implicit p: Provide[F]): HasProvide[F, p.Lower, p.Ctx] = p.self

  type Aux[F[_], G[_], C] = HasProvide[F, G, C]
}

/** Combination of [[Local]] and [[Provide]]
  *
  * @tparam F
  *   context-aware effect e.g.`ReaderT[Lower, Ctx, *]`
  */
@deprecated("Migrate to With* typeclasses", "0.10.3")
trait RunContext[F[_]] extends Local[F] with Provide[F] {

  /** Allows to convert some context-unaware computation into contextual one.
    *
    * @example
    *   {{{ trait ProcessHandler[G[_]] { def mapK[M[_]](fk: G ~> M): ProcessHandler[M] = ??? //...other methods }
    *
    * type WithMyContext[F[_], A] = ReaderT[F, MyCtx, A]
    *
    * val processHandler: ProcessHandler[IO WithMyContext *] = ???
    *
    * val contextualHandler: IO WithMyContext ProcessHandler[IO] = processHandler.mapK( WithRun[WithMyContext[IO, *],
    * IO, MyCtx].unlift.map(fk => processHandler.mapK(fk)) ) //now it is able to process MyCtx but is wrapped in IO
    * WithMyContext * }}}
    * @return
    */
  def unlift: F[F ~> Lower] = ask(ctx => funK[F, Lower](runContext(_)(ctx)))

  def self: WithRun[F, Lower, Ctx]

  /** Allows to focus [[Provide]] on inner parts of its context with equivalence lens.
    *
    * @param eq
    *   lens that can convert from `Ctx` to `A`
    */
  def runEquivalent[A](eq: Equivalent[Ctx, A]): WithRun[F, Lower, A] =
    new RunContextEquivalentInstance[F, Lower, Ctx, A](self, eq)

}

/** Companion object for [[RunContext]] */
@deprecated("Migrate to With* typeclasses", "0.10.3")
object RunContext {
  def apply[F[_]](implicit ctx: RunContext[F]): HasContextRun[F, ctx.Lower, ctx.Ctx] = ctx.self

  type Aux[F[_], G[_], C] = HasContextRun[F, G, C]
}
