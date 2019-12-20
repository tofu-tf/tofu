package tofu.env.bio

import monix.eval.Task

sealed trait EnvBio[-R, +E, +A] {
  def run(ctx: R): Task[Either[E, A]]

  def map[B](f: A => B): EnvBio[R, E, B] =
    flatMap(value => EnvBio.pure(f(value)))

  def flatMap[R1 <: R, E1 >: E, B](f: A => EnvBio[R1, E1, B]): EnvBio[R1, E1, B] =
    EnvBio(ctx => run(ctx).flatMap(_.fold(raiseUserError, f(_).run(ctx))))

  def mapTask[E1 >: E, B](f: Task[Either[E, A]] => Task[Either[E1, B]]): EnvBio[R, E1, B] =
    EnvBio(ctx => f(run(ctx)))

  private def raiseUserError[E1](e: E1): Task[Either[E1, Nothing]] = Task.raiseError(UserError(e))
}

object EnvBio extends EnvBioFunctions {}

/** Context aware variation of `EnvBio` that allows specifying error type */
final case class EnvBioCtx[R, +E, +A](runF: R => Task[Either[E, A]]) extends EnvBio[R, E, A] {
  def run(ctx: R): Task[Either[E, A]] = Task.defer(runF(ctx)).attempt.flatMap {
    case Left(UserError(err: E)) => Task.pure(Left(err))
    case Left(fatalErr)          => Task.raiseError(fatalErr)
    case Right(Left(userErr))    => Task.pure(Left(userErr))
    case Right(Right(value))     => Task.pure(Right(value))
  }
}

/** Context aware variation of `Env` that has no notion of error type, thus all errors to underlying 'Task' are considered fatal */
final case class EnvUioCtx[R, +A](runF: R => Task[A]) extends EnvBio[R, Nothing, A] {
  def run(ctx: R): Task[Either[Nothing, A]] = Task.defer(runF(ctx).map(Right.apply))
}

/** Context independent variation of `EnvBio` that allows specifying error type for underlying 'Task' */
final case class EnvBioTask[R, +E, +A](ta: Task[Either[E, A]]) extends EnvBio[R, E, A] {
  def run(ctx: R): Task[Either[E, A]] = ta
}

final case class UserError[E](err: E) extends Throwable
