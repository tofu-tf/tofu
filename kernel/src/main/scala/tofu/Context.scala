package tofu

import cats.{Applicative, FlatMap, Functor}
import tofu.context.internal.{ContextBase, _}
import tofu.optics.{Contains, Extract}

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

    /** Access [[C]] in [[F]].
      */
    final def access[F[_]](implicit has: Has[F]): F[C] =
      has.context
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

object HasContext {
  def apply[F[_], C](implicit hc: HasContext[F, C]): HasContext[F, C] = hc
}

object HasLocal {
  def apply[F[_], C](implicit hl: HasLocal[F, C]): HasLocal[F, C] = hl
}

object HasProvide {
  def apply[F[_], G[_], C](implicit hp: HasProvide[F, G, C]): HasProvide[F, G, C] = hp
}
