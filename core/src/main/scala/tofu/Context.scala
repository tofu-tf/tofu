package tofu

import cats.data.ReaderT
import cats.{Applicative, FlatMap, Functor, ~>}
import tofu.lift.{Lift, Unlift}
import tofu.optics.{Contains, Equivalent, Extract}
import tofu.syntax.funk._

/** Declares that [[F]] can provide value of type  Ctx
  *
  * In other words this trait tells you that F is some kind of Reader
  * or function of  type `Ctx => F[A]`.
  * The best way to use it is through  `WithContext` or `In`
  *
  * This can be seen as global value for your Application or for a part of it.
  *
  * @example
  * One common use of this is to make contextual logging:
  * {{{
  *     import tofu.syntax.console._
  *
  *   def contexualConsolling[F[_]: Console: FlatMap](message: String)(implicit hasMyCtx: F HasContext MyCtx): F[Unit] =
  *     hasMyCtx.askF(ctx => puts"$message (Also context: $ctx)")
  * }}}
  * so when you have `MyCtx(3)`
  * the call of `contextualConsolling("Hi!")` prints `Hi! (Also MyCtx(3))`
  */
trait Context[F[_]] extends ContextBase {
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
    * @note It does not affect context itself.
    */
  def ask[A](f: Ctx => A): F[A] = functor.map(context)(f)

  /** Same as [[ask]] but `f` is effectual */
  def askF[A](f: Ctx => F[A])(implicit F: FlatMap[F]): F[A] = F.flatMap(context)(f)

  /** Allows to focus context on its inside with lens.
    *
    * @param extract lens that can extract value of type `A` from `Ctx`
    */
  def extract[A](extract: Extract[Ctx, A]): WithContext[F, A] = new ContextExtractInstance[F, Ctx, A](this, extract)
}

/** Companion object for [[Context]] */
object Context {
  def apply[F[_]](implicit ctx: Context[F]): HasContext[F, ctx.Ctx] = ctx

  type Aux[F[_], C] = HasContext[F, C]

  /** Creates constant Context of type C into F
    *
    * Allows to put some value into context of F
    * even when F itself does not have any way to store it
    *
    * @example {{{
    *   case class MyCtx(id: Int)
    *
    *   val program: Option[String] = {
    *     implicit val ctx = Context.const[Option, MyCtx](MyCtx(3))
    *
    *     Context[Option].ask(ctx => ctx.id.toString)
    *   }
    * }}}
    */
  def const[F[_]: Applicative, C](c: C): WithContext[F, C] = new WithContext[F, C] {
    val functor: Functor[F] = Functor[F]
    val context: F[C]       = Applicative[F].pure(c)
  }

  /** Same as const but context is effectual here
    *
    * @example {{{
    *   def handleRequest[F[_]: Monad: *[_] WithContext TraceId](httpRequest: Request) = ???
    *
    *   def myProgram[F[_]: Monad] =
    *     for {
    *       request                                 <- getRequest[F]
    *       implicit0(ctx: TraceId In F) <- Context.make(genTraceId[F])
    *       _                                       <- handleRequest[F](request)
    *     } yield ()
    * }}}
    */
  def make[F[_]: Functor, A](fa: F[A]): WithContext[F, A] = new WithContext[F, A] {
    val functor: Functor[F] = implicitly
    val context: F[A]       = fa
  }

  /** A mix-in for supplying environment data type companions with useful things
    *
    * @example {{{
    * @ClassyOptics
    * case class MyContext(id: Int, date: String)
    *
    * object MyContext extends Context.Companion[MyContext]
    * }}}
    */
  trait Companion[C] extends ContextInstances[C] {
    type Has[F[_]] = WithContext[F, C]

    implicit def promoteContextStructure[F[_], A](implicit
        withContext: WithContext[F, C],
        field: C Contains A
    ): WithContextContainsInstance[F, C, A] =
      new WithContextContainsInstance[F, C, A]
  }

  trait ContextInstances[C] {

    implicit def promoteLocalStructure[F[_], A](implicit
        context: WithLocal[F, C],
        field: C Contains A
    ): WithLocal[F, A] =
      context.subcontext(field)
  }
}

final class WithContextContainsInstance[F[_], A, B](implicit wc: WithContext[F, A], lens: A Contains B)
    extends WithContext[F, B] {
  def functor: Functor[F] = wc.functor
  def context: F[B]       = wc.extract(lens).context
}

/** Synonym for [[Context]] with explicit C as Ctx for better type inference
  *
  * There is also a nice type alias: {{{
  * import tofu.In
  *
  * val fHasMyCtx: MyCtx In F = ???
  * }}}
  */
trait WithContext[F[_], C] extends Context[F] {
  override type Ctx = C
}

/** Companion object for [[WithContext]] */
object WithContext {
  def apply[F[_], C](implicit ctx: WithContext[F, C]): WithContext[F, C] = ctx
}

/** Allows to run some computation with notion of altered context
  * consider using `WithLocal` for better type inference
  */
trait Local[F[_]] extends Context[F] {

  /** Alters context for computation
    *
    * @example
    * Example of usage is to hide sensitive information {{{
    *   case class UserContext(
    *       id: String,
    *       phoneNumber: String //very sensitive
    *   )
    *
    *   def hidePartOfPhoneNumber: String => String = ???
    *
    *   def contextualLogInfo[F[_]](message: String)(implicit hasUserContext: UserContext In F): F[Unit] =
    *     ??? //logs both message AND UserContext
    *
    *   def program[F[_]: Monad] = for {
    *     user                                   <- obtainUserSomehow[F]
    *     implicit0(hasUserContext: In[User, F]) <- Context.const[F, User](user.toContext)
    *     logUser                                 = contextualLogInfo[F](s"Successfully obtained user")
    *     _                                      <- logUser.local(hidePartOfPhoneNumber) //logs only part of phone number
    *   } yield ()
    *
    * }}}
    * @param fa      computation that is going to run with different context
    * @param project pure function
    * @return result of ran computation `fa`
    */
  def local[A](fa: F[A])(project: Ctx => Ctx): F[A]

  /** Allows to focus [[Local]] on inner parts of its context with lens.
    *
    * @param contains lens that can extract from `Ctx` or set some value of type `A`
    */
  def subcontext[A](contains: Ctx Contains A): WithLocal[F, A] = new LocalContainsInstance[F, Ctx, A](this, contains)
}

object Local {
  def apply[F[_]](implicit ctx: Local[F]): HasLocal[F, ctx.Ctx] = ctx

  type Aux[F[_], C] = HasLocal[F, C]
}

/** Synonym for [[Local]] with explicit C as Ctx for better type inference */
trait WithLocal[F[_], C] extends Local[F] with WithContext[F, C]

/** Companion object for [[WithLocal]] */
object WithLocal {
  def apply[F[_], C](implicit ctx: WithLocal[F, C]): WithLocal[F, C] = ctx
}

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

/** Companion object for [[RunContext]] */
object RunContext {
  def apply[F[_]](implicit ctx: RunContext[F]): HasContextRun[F, ctx.Lower, ctx.Ctx] = ctx

  type Aux[F[_], G[_], C] = HasContextRun[F, G, C]

  final implicit def readerTContext[F[_]: Applicative, C]: HasContextRun[ReaderT[F, C, *], F, C] =
    ContextBase.readerTContext[F, C]
}

/** Synonym for both [[RunContext]] and [[Unlift]] with explicit `C` as `Ctx` and `G` as `Lower` for better type inference
  *
  * Can be seen as transformation `F[*] = C => G[*]`
  */
trait WithRun[F[_], G[_], C] extends WithProvide[F, G, C] with WithLocal[F, C] with RunContext[F] with Unlift[G, F] {
  override type Ctx = C
}

/** Companion object for [[WithRun]] */
object WithRun {
  def apply[F[_], G[_], C](implicit ctx: WithRun[F, G, C]): WithRun[F, G, C] = ctx
}

/** Common base for instances */
trait ContextBase

object ContextBase {
  final implicit def readerTContext[F[_]: Applicative, C]: WithRun[ReaderT[F, C, *], F, C] =
    new WithRun[ReaderT[F, C, *], F, C] {
      def lift[A](fa: F[A]): ReaderT[F, C, A] = ReaderT.liftF(fa)

      def runContext[A](fa: ReaderT[F, C, A])(ctx: C): F[A] = fa.run(ctx)

      def local[A](fa: ReaderT[F, C, A])(project: C => C): ReaderT[F, C, A] = fa.local(project)

      val functor: Functor[ReaderT[F, C, *]] = Functor[ReaderT[F, C, *]]
      val context: ReaderT[F, C, C]          = ReaderT.ask[F, C]
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

private[tofu] class RunContextEquivalentInstance[F[_], G[_], C1, C2](
    ctx: HasContextRun[F, G, C1],
    equivalent: C1 Equivalent C2
) extends LocalContainsInstance[F, C1, C2](ctx, equivalent) with WithRun[F, G, C2] {
  def runContext[A](fa: F[A])(c: C2): G[A] = ctx.runContext(fa)(equivalent.upcast(c))

  def lift[A](ga: G[A]): F[A] = ctx.lift(ga)
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
