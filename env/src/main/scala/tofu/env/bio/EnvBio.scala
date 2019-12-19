package tofu.env.bio

import monix.eval.Task

sealed trait EnvBio[R, +E, +A] {
  type Lower[+A1] = Task[Either[E, A1]]

  def run(ctx: R): Lower[A]

  def map[B](f: A => B): EnvBio[R, E, B]

  def mapTask[B](f: Lower[A] => Lower[B]): EnvBio[R, E, B] =

}

/** Context aware variation of `Env` */
final case class EnvBioCtx[R, +E, +A](runF: R => Task[Either[E, A]]) extends EnvBio[R, E, A] {
  def run(ctx: R): Task[Either[E, A]] = Task.defer(runF(ctx))
}

/** Context aware variation of `Env` */
final case class EnvUioCtx[R, +A](runF: R => Task[A]) extends EnvBio[R, Nothing, A] {
  def run(ctx: R): Task[Either[Nothing, A]] = Task.defer(runF(ctx).map(Right.apply))
}

/** Context independent variation of `Env` */
final case class EnvBioTask[R, +E, +A](ta: Task[Either[E, A]]) extends EnvBio[R, E, A] {
  def run(ctx: R): Task[Either[E, A]] = ta
}

final case class UserError[E](err: E) extends Throwable
