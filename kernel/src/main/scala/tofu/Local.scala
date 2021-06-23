package tofu

import tofu.optics.Contains
import tofu.context.internal._

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
  def apply[F[_]](implicit ctx: Local[F]): Aux[F, ctx.Ctx] = ctx

  type Aux[F[_], C] = Local[F] { type Ctx = C }
}

/** Synonym for [[Local]] with explicit C as Ctx for better type inference */
trait WithLocal[F[_], C] extends Local[F] with WithContext[F, C]

/** Companion object for [[WithLocal]] */
object WithLocal {
  def apply[F[_], C](implicit ctx: WithLocal[F, C]): WithLocal[F, C] = ctx
}
