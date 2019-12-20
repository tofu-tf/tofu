package tofu.env.bio

import monix.eval.Task

private[bio] trait EnvBioFunctions { self: EnvBio.type =>
  def apply[R, E, A](f: R => Task[Either[E, A]]): EnvBio[R, E, A] = (ctx: R) =>
    f(ctx).flatMap {
      case Left(err)    => Task.raiseError(UserError(err))
      case Right(value) => Task.pure(value)
    }

  // todo naming
  def applyFatal[R, E, A](f: R => Task[A]): EnvBio[R, E, A] = (ctx: R) => f(ctx)

  def later[A](x: => A): EnvBio[Any, Nothing, A]   = fromTask(Task.delay(x))
  def pure[A](x: A): EnvBio[Any, Nothing, A]       = fromTask(Task.pure(x))
  def raiseError[E](e: E): EnvBio[Any, E, Nothing] = apply(_ => Task.pure(Left(e)))

  def context[R, E]: EnvBio[R, E, R]                       = applyFatal(Task.now)
  def fromTask[R, A](task: Task[A]): EnvBio[R, Nothing, A] = applyFatal(_ => task)
}
