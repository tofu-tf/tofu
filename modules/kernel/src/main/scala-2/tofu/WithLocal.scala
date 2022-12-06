package tofu

import glass.Contains

/** Synonym for [[Local]] with explicit C as Ctx for better type inference */
trait WithLocal[F[_], C] extends Local[F] with WithContext[F, C] {

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
  override def subcontext[A](contains: Ctx Contains A): WithLocal[F, A] =
    new LocalContainsInstance[F, Ctx, A](this, contains)

  override private[tofu] def asWithLocal: WithLocal[F, C] = this
}

/** Companion object for [[WithLocal]] */
object WithLocal {
  def apply[F[_], C](implicit ctx: WithLocal[F, C]): WithLocal[F, C] = ctx
}

private[tofu] class LocalContainsInstance[F[_], C1, C2](ctx: F WithLocal C1, contains: C1 Contains C2)
    extends ContextExtractInstance[F, C1, C2](ctx, contains) with WithLocal[F, C2] {
  def local[A](fa: F[A])(project: C2 => C2): F[A] = ctx.local(fa)(contains.update(_, project))
}
