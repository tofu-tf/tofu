package tofu.env.bio

import monix.eval.Task

private[bio] trait EnvBioFunctions { self: EnvBio.type =>
  def apply[R, E, A](f: R => Task[Either[E, A]]): EnvBio[R, E, A] = EnvBioCtx(f)
  def applyFatal[R, A](f: R => Task[A]): EnvBio[R, Nothing, A]    = EnvUioCtx(f)
  def later[A](x: => A): EnvBio[Any, Nothing, A]                  = fromTask(Task.delay(x))
  def pure[A](x: A): EnvBio[Any, Nothing, A]                      = fromTask(Task.pure(x))
  def raiseError[E](e: E): EnvBio[Any, E, Nothing]                = apply(_ => Task.pure(Left(e)))

  def context[R]: EnvBio[R, Any, R]                        = applyFatal(Task.now)
  def fromTask[R, A](task: Task[A]): EnvBio[R, Nothing, A] = EnvUioCtx(_ => task)
}
