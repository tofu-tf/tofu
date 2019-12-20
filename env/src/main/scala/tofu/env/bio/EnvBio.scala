package tofu.env.bio

import monix.eval.Task

abstract class EnvBio[-R, +E, +A] {
  // todo should expose this to user?
  protected def runF(ctx: R): Task[A]

  final def run(ctx: R): Task[Either[E, A]] = runF(ctx).attempt.flatMap {
    case Left(UserError(err: E)) => Task.pure(Left(err)) // todo erasure elimination?
    case Left(fatalErr)          => Task.raiseError(fatalErr)
    case Right(value)            => Task.pure(Right(value))
  }

  def map[B](f: A => B): EnvBio[R, E, B] =
    flatMap(value => EnvBio.pure(f(value)))

  def flatMap[R1 <: R, E1 >: E, B](f: A => EnvBio[R1, E1, B]): EnvBio[R1, E1, B] =
    EnvBio.applyFatal(ctx => runF(ctx).flatMap(f(_).runF(ctx)))

  def handleErrorWith[R1 <: R, E1 >: E, A1 >: A](f: E => EnvBio[R1, E1, A1]): EnvBio[R1, E1, A1] =
    EnvBio.applyFatal(ctx =>
      runF(ctx).onErrorHandleWith {
        case UserError(e: E) => f(e).runF(ctx)
        case t               => Task.raiseError(t)
      }
    )

  def mapError[E1 >: E](f: E => E1): EnvBio[R, E1, A] =
    handleErrorWith(e => EnvBio.raiseError(f(e)))
}

object EnvBio extends EnvBioFunctions {}

private[bio] final case class UserError[E](err: E) extends Throwable
