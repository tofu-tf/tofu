package tofu.env.bio

import monix.eval.Task
import scala.concurrent.duration.FiniteDuration

abstract class EnvBio[-R, +E, +A] {
  // todo should expose this to user?
  protected def runF(ctx: R): Task[A]

  final def run(ctx: R): Task[Either[E, A]] = runF(ctx).attempt.flatMap {
    case Left(UserError(err: E @unchecked)) => Task.pure(Left(err)) // todo erasure elimination?
    case Left(fatalErr)                     => Task.raiseError(fatalErr)
    case Right(value)                       => Task.pure(Right(value))
  }

  def mapTask[B](f: Task[A] => Task[B]): EnvBio[R, E, B] =
    EnvBio.applyFatal(ctx => f(runF(ctx)))

  def map[B](f: A => B): EnvBio[R, E, B] =
    mapTask(_.map(f))

  def flatMap[R1 <: R, E1 >: E, B](f: A => EnvBio[R1, E1, B]): EnvBio[R1, E1, B] =
    EnvBio.applyFatal(ctx => runF(ctx).flatMap(f(_).runF(ctx)))

  def >>[R1 <: R, E1 >: E, B](fb: => EnvBio[R1, E1, B]): EnvBio[R1, E1, B] =
    flatMap(_ => fb)

  def <<[R1 <: R, E1 >: E](fb: => EnvBio[R1, E1, Any]): EnvBio[R1, E1, A] =
    flatTap(_ => fb)

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

  def onErrorHandleWith[R1 <: R, E1, A1 >: A](f: E => EnvBio[R1, E1, A1]): EnvBio[R1, E1, A1] =
    EnvBio.applyFatal(ctx =>
      runF(ctx).onErrorHandleWith {
        case UserError(e: E @unchecked) => f(e).runF(ctx)
        case t                          => Task.raiseError(t)
      }
    )

  def onErrorHandle[A1 >: A](f: E => A1): EnvBio[R, Nothing, A1] =
    onErrorHandleWith(e => EnvBio.pure(f(e)))

  def onErrorRecoverWith[R1 <: R, E1 >: E, A2 >: A](f: PartialFunction[E, EnvBio[R1, E1, A2]]): EnvBio[R1, E1, A2] =
    EnvBio.applyFatal(ctx =>
      runF(ctx).onErrorRecoverWith {
        case UserError(e: E @unchecked) if f.isDefinedAt(e) => f(e).runF(ctx)
        case t                                              => Task.raiseError(t)
      }
    )

  def onErrorRecover[A1 >: A](f: PartialFunction[E, A1]): EnvBio[R, E, A1] =
    onErrorRecoverWith(f.andThen(a1 => EnvBio.pure(a1)))

  def mapError[E1](f: E => E1): EnvBio[R, E1, A] =
    onErrorHandleWith(e => EnvBio.raiseError(f(e)))

  def tapError[R1 <: R, E1 >: E, A1 >: A](f: E => EnvBio[R1, E1, A1]): EnvBio[R1, E1, A1] =
    onErrorHandleWith(e => f(e) >> EnvBio.raiseError(e))

  def tapHandle[R1 <: R, E1, A1 >: A](f: E => EnvBio[R1, E1, A1]): EnvBio[R1, E, A1] =
    onErrorHandleWith(e => f(e).mapError(_ => e))

  /** Creates new EnvBio with polymorphic environment switching */
  def localP[R1](f: R1 => R): EnvBio[R1, Nothing, A]              =
    EnvBio.applyFatal(ctx => runF(f(ctx)))
  def localPT[R1](f: R1 => Task[R]): EnvBio[R1, E, A]             =
    EnvBio.applyFatal(ctx => f(ctx).flatMap(runF))
  def localPB[R1](f: R1 => EnvBio[Any, Any, R]): EnvBio[R1, E, A] =
    EnvBio.applyFatal(ctx => f(ctx).runF(ctx).flatMap(runF))

  /** Convenient alias for [[localP]] for better type inference.
    * Creates a new `EnvBio` that uses transformed context `R1`
    * which is a result of applying given function to current context.
    */
  def local[R1 <: R](f: R1 => R1): EnvBio[R1, Nothing, A] =
    localP[R1](f)

  /** Times the `EnvBio` execution, returning elapsed time along with computed value */
  def timed: EnvBio[R, E, (FiniteDuration, A)] = mapTask(_.timed)

  def foldWith[X, B, R1 <: R](
      h: E => EnvBio[R1, X, B],
      f: A => EnvBio[R1, X, B]
  ): EnvBio[R1, X, B] =
    ctx =>
      this.runF(ctx).attempt.flatMap {
        case Left(UserError(e: E @unchecked)) => h(e).runF(ctx)
        case Left(err)                        => Task.raiseError(err)
        case Right(x)                         => f(x).runF(ctx)
      }
}

object EnvBio extends EnvBioFunctions with EnvBioInstances {}

private[bio] final case class UserError(err: Any) extends Throwable
