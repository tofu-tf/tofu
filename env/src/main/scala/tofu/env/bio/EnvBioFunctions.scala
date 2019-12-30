package tofu.env.bio

import monix.eval.Task

private[bio] trait EnvBioFunctions extends EnvBioProducts { self: EnvBio.type =>
  def apply[R, E, A](f: R => Task[Either[E, A]]): EnvBio[R, E, A] = (ctx: R) =>
    f(ctx).flatMap {
      case Left(err)    => Task.raiseError(UserError(err))
      case Right(value) => Task.pure(value)
    }

  // todo naming
  def applyFatal[R, E, A](f: R => Task[A]): EnvBio[R, E, A] = f(_)

  def pure[A](x: A): EnvBio[Any, Nothing, A]       = fromTask(Task.pure(x))
  def raiseError[E](e: E): EnvBio[Any, E, Nothing] = fromTask(Task.raiseError(UserError(e)))

  /** Returns current context, embedded in `EnvBio`.
    * Example:
    * {{{
    *   import scala.concurrent.duration.Duration
    *   import tofu.env.bio.EnvBio
    *
    *   val env: EnvBio[String, Nothing, String] =
    *     for {
    *       _   <- EnvBio.unit
    *       ctx <- EnvBio.context
    *     } yield ctx
    *
    *   env.run("ctx").runSyncUnsafe(Duration.Inf)
    *   // will result in Right("ctx")
    * }}}*/
  def context[R]: EnvBio[R, Nothing, R] = Task.now

  /** Creates `EnvBio` from total, non-failing `Task`.
    * Any error raised by Task will result in fatal error being thrown. */
  def fromTask[A](task: Task[A]): EnvBio[Any, Nothing, A] = _ => task

  /** Creates `EnvBio` from total, non-throwing effect, resulting in a value of type `A`.
    * Any error thrown by `x` will result in fatal error being thrown. */
  def delayTotal[A](x: => A): EnvBio[Any, Nothing, A] =
    fromTask(Task.delay(x))

  /** Creates `EnvBio` from total effect.
    * Any error thrown by `x` will result in failed `EnvBio` fixed to Left side. */
  def delay[A](x: => A): EnvBio[Any, Throwable, A] =
    fromTask(Task.delay(x).onErrorHandleWith(e => Task.raiseError(UserError(e))))

  private[this] val anyUnit: EnvBio[Any, Nothing, Unit] = fromTask(Task.unit)
  def unit: EnvBio[Any, Nothing, Unit]                  = anyUnit.asInstanceOf[EnvBio[Any, Nothing, Unit]]
}
