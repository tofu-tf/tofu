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

  def context[R]: EnvBio[R, Nothing, R]                    = Task.now
  def fromTask[R, A](task: Task[A]): EnvBio[R, Nothing, A] = _ => task
  def delayTotal[Any, A](x: => A): EnvBio[Any, Nothing, A] =
    fromTask(Task.delay(x))
  def delay[Any, A](x: => A): EnvBio[Any, Throwable, A] =
    fromTask(Task.delay(x).onErrorHandleWith(e => Task.raiseError(UserError(e))))

  private[this] val anyUnit: EnvBio[Any, Nothing, Unit] = fromTask(Task.unit)
  def unit: EnvBio[Any, Nothing, Unit]                  = anyUnit.asInstanceOf[EnvBio[Any, Nothing, Unit]]
}
