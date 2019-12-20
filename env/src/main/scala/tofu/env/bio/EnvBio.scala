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

  def flatTap[R1 <: R, E1 >: E, B](f: A => EnvBio[R1, E1, B]): EnvBio[R1, E1, A] =
    flatMap(a => f(a).map(_ => a))

  def map2[B, C, R1 <: R, E1 >: E](eb: EnvBio[R1, E1, B])(f: (A, B) => C): EnvBio[R1, E1, C] =
    flatMap(a => eb.map(f(a, _)))

  def map3[B, C, D, R1 <: R, E1 >: E](eb: EnvBio[R1, E1, B], ec: EnvBio[R1, E1, C])(
      f: (A, B, C) => D
  ): EnvBio[R1, E1, D] =
    map2(eb)((a, b) => f(a, b, _)).map2(ec)((fun, c) => fun(c))

  def parMap2[B, C, R1 <: R, E1 >: E](eb: EnvBio[R1, E1, B])(f: (A, B) => C): EnvBio[R1, E1, C] =
    EnvBio.applyFatal(ctx => Task.parMap2(runF(ctx), eb.runF(ctx))(f))

  def parMap3[B, C, D, R1 <: R, E1 >: E](eb: EnvBio[R1, E1, B], ec: EnvBio[R1, E1, C])(
      f: (A, B, C) => D
  ): EnvBio[R1, E1, D] =
    EnvBio.applyFatal(ctx => Task.parMap3(runF(ctx), eb.runF(ctx), ec.runF(ctx))(f))

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
