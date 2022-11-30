package tofu

import cats.{Applicative, FlatMap, Functor}
import glass.{Contains, Extract}

/** Synonym for [[Context]] with explicit C as Ctx for better type inference
  *
  * There is also a nice type alias: {{{ import tofu.In
  *
  * val fHasMyCtx: MyCtx In F = ??? }}}
  */
trait WithContext[F[_], C] extends Context[F] {
  override type Ctx = C

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
  override def ask[A](f: Ctx => A): F[A] = functor.map(context)(f)

  /** Same as [[ask]] but `f` is effectual */
  override def askF[A](f: Ctx => F[A])(implicit F: FlatMap[F]): F[A] = F.flatMap(context)(f)

  /** Allows to focus context on its inside with lens.
    *
    * @param extract
    *   lens that can extract value of type `A` from `Ctx`
    */
  override def extract[A](extract: Extract[Ctx, A]): ContextExtractInstance[F, Ctx, A] =
    new ContextExtractInstance[F, Ctx, A](this, extract)

  override def asWithContext: WithContext[F, C] = this
}

/** Companion object for [[WithContext]] */
object WithContext {
  def apply[F[_], C](implicit ctx: WithContext[F, C]): WithContext[F, C] = ctx

  /** Creates constant Context of type C in F
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
  def const[F[_]: Applicative, C](c: C): F WithContext C = new WithContext[F, C] {
    val functor: Functor[F] = Functor[F]
    val context: F[C]       = Applicative[F].pure(c)
  }

  /** Same as const but context is effectual here
    *
    * @example
    *   {{{ def handleRequest[F[_]: Monad: *[_] WithContext TraceId](httpRequest: Request) = ???
    *
    * def myProgram[F[_]: Monad] = for { request <- getRequest[F] implicit0(ctx: TraceId In F) <-
    * Context.make(genTraceId[F]) _ <- handleRequest[F](request) } yield () }}}
    */
  def make[F[_]: Functor, A](fa: F[A]): WithContext[F, A] = new WithContext[F, A] {
    val functor: Functor[F] = implicitly
    val context: F[A]       = fa
  }

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
  trait Companion[C] extends ContextInstances1[C] {

    type Has[F[_]] = WithContext[F, C]

    /** Access context `C` in F.
      */
    final def access[F[_]](implicit has: Has[F]): F[C] =
      has.context
  }

  trait ContextInstances1[C] {
    final implicit def promoteContextStructure[F[_], A](implicit
        withContext: WithContext[F, C],
        field: C Contains A
    ): WithContext[F, A] = withContext.extract(field)
  }

  trait ContextInstances2[C] {
    final implicit def promoteLocalStructure[F[_], A](implicit
        context: WithLocal[F, C],
        field: C Contains A
    ): WithLocal[F, A] =
      context.subcontext(field)
  }
}

private[tofu] class ContextExtractInstance[F[_], C1, C2](ctx: F WithContext C1, extract: C1 Extract C2)
    extends WithContext[F, C2] {
  def functor: Functor[F] = ctx.functor

  def context: F[C2] = functor.map(ctx.context)(extract.extract)
}
