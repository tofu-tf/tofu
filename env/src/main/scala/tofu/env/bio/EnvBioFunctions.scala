package tofu.env.bio

import monix.eval.Task
import scala.concurrent.duration.FiniteDuration
import scala.concurrent.ExecutionContext

private[bio] trait EnvBioFunctions extends EnvBioProducts { self: EnvBio.type =>
  def apply[R, E, A](f: R => Task[Either[E, A]]): EnvBio[R, E, A] = (ctx: R) =>
    f(ctx).flatMap {
      case Left(err)    => Task.raiseError(UserError(err))
      case Right(value) => Task.pure(value)
    }

  def applyFatal[R, E, A](eb: EnvBio[R, E, A]): EnvBio[R, E, A] = eb

  def pure[A](x: A): EnvBio[Any, Nothing, A] = _ => Task.pure(x)

  /** Returns `EnvBio` that on execution is resulting in specified domain error being returned as `Left`
    * Example:
    * {{{
    *   import monix.execution.Scheduler.Implicits.global
    *
    *   val env: EnvBio[Any, String, Nothing] = EnvBio.raiseError("error")
    *   val result: Either[String, Nothing]   = env.run(()).runSyncUnsafe(Duration.Inf)
    *   // result is Left("err")
    * }}}
    */
  def raiseError[E](e: E): EnvBio[Any, E, Nothing] = _ => Task.raiseError(UserError(e))

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
    * }}}
    */
  def context[R]: EnvBio[R, Nothing, R] = Task.now

  /** Creates `EnvBio` from total, non-failing `Task`.
    * Any `Throwable` raised by Task will result in fatal error being thrown.
    * Example:
    * {{{
    *   import monix.eval.Task
    *   import monix.execution.Scheduler.Implicits.global
    *   import tofu.env.bio.EnvBio
    *   import scala.concurrent.duration.Duration
    *
    *   val env: EnvBio[Any, Nothing, Unit] = EnvBio.fromTaskTotal(Task.delay(1))
    *   val res = env.run(()).runSyncUnsafe(Duration.Inf)
    *
    *   // res = Right(1)
    * }}}
    */
  def fromTaskTotal[A](task: Task[A]): EnvBio[Any, Nothing, A] = _ => task

  def fromTaskEither[E, A](task: Task[Either[E, A]]): EnvBio[Any, E, A] = _ =>
    task.flatMap {
      case Left(e)  => Task.raiseError(UserError(e))
      case Right(a) => Task.pure(a)
    }

  /** Creates `EnvBio` from `Task`.
    * Any `Throwable` raised by Task will result in failed `EnvBio`, fixed to Left side.
    */
  def fromTask[A](task: Task[A]): EnvBio[Any, Throwable, A] =
    _ => task.onErrorHandleWith(e => Task.raiseError(UserError(e)))

  /** Creates `EnvBio` from total, non-throwing effect, resulting in a value of type `A`.
    * Any `Throwable` raised by `x` will result in fatal error being thrown.
    */
  def delayTotal[A](x: => A): EnvBio[Any, Nothing, A] =
    fromTaskTotal(Task.delay(x))

  /** Creates `EnvBio` from total effect.
    * Any `Throwable` raised by `x` will result in failed `EnvBio` fixed to Left side.
    */
  def delay[A](x: => A): EnvBio[Any, Throwable, A] =
    fromTask(Task.delay(x))

  /** Introduces an asynchronous boundary, effectfully shifting execution
    * to another thread or call stack.
    * An implementaion delegates to Monix's implementation (see [[monix.eval.Task.shift]])
    */
  def shift: EnvBio[Any, Nothing, Unit] = fromTaskTotal(Task.shift)

  /** Introduces an asynchronous boundary, effectfully shifting execution
    * to another thread or call stack. This implementation differs in that underlying
    * Monix `Task` will be executed on injected `Scheduler`.
    * An implementaion delegates to Monix's implementation (see [[monix.eval.Task.shift]])
    */
  def shift(ec: ExecutionContext): EnvBio[Any, Nothing, Unit] = fromTaskTotal(Task.shift(ec))

  /** Creates a new `EnvBio` that will sleep for given duration, and then continue execution.
    * This operation doesn't block a thread, the blocking is semantic.
    *
    * @param duration duration for 'sleeping', after which a tick will be emitted and execution will go on
    */
  def sleep(duration: FiniteDuration): EnvBio[Any, Nothing, Unit] =
    fromTaskTotal(Task.sleep(duration))

  private[this] val anyUnit: EnvBio[Any, Nothing, Unit] = fromTaskTotal(Task.unit)
  def unit: EnvBio[Any, Nothing, Unit]                  = anyUnit.asInstanceOf[EnvBio[Any, Nothing, Unit]]
}
