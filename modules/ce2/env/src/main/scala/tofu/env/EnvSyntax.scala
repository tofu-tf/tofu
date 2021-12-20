package tofu.env

import monix.eval.Task

private[env] trait EnvSyntax {
  implicit def toEnvOptionOps[E, A](env: Env[E, Option[A]]): EnvOptionOps[E, A] = new EnvOptionOps(env)
}

class EnvOptionOps[E, A](val ea: Env[E, Option[A]]) extends AnyVal {
  import EnvOptionOps._
  def orElseF[B >: A](eb: Env[E, Option[B]]): Env[E, Option[B]] =
    (ea, eb) match {
      case (EnvTask(ta), EnvTask(tb)) =>
        EnvTask(ta.flatMap {
          case None        => tb
          case s @ Some(_) => Task.pure(s)
        })
      case _                          =>
        ea.flatMap {
          case None        => eb
          case s @ Some(_) => Env.pure(s)
        }
    }

  def orElseT[B >: A](tb: Task[Option[B]]): Env[E, Option[B]] =
    ea.mapTask(_.flatMap {
      case None        => tb
      case s @ Some(_) => Task.pure(s)
    })

  def orElse[B >: A](b: => Option[B]): Env[E, Option[B]] =
    ea.mapTask(_.map(_.orElse(b)))

  def getOrElseF[B >: A](eb: Env[E, B]): Env[E, B] =
    eb match {
      case EnvTask(tb) => ea.mapTask(getOrElseTask(tb))
      case _           => Env(ctx => getOrElseTask(eb.run(ctx))(ea.run(ctx)))
    }
  def getOrElseT[B >: A](tb: Task[B]): Env[E, B]   = ea.mapTask(getOrElseTask(tb))
  def getOrElse[B >: A](a: => B): Env[E, B]        = ea.map(_.getOrElse(a))

  def orRaise(e: Throwable): Env[E, A] =
    ea.mapTask(_.flatMap(_.fold(Task.raiseError[A](e))(Task.pure)))
}

object EnvOptionOps {
  @inline private def getOrElseTask[A](tb: Task[A])(ta: Task[Option[A]]): Task[A] =
    ta.flatMap(_.fold(tb)(Task.pure))
}
