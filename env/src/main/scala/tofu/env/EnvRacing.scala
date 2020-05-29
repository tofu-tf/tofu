package tofu.env

import cats.effect._
import monix.eval.Task
import scala.collection.compat._

private[env] trait EnvRacing {
  self: Env.type =>
  type Racing[F[_], A, B] = Either[(A, Fiber[F, B]), (Fiber[F, A], B)]

  private def convertRacingFibers[E, A, B]: Racing[Task, A, B] => Racing[Env[E, *], A, B] = {
    case Left((a, fibB))  => Left((a, EnvFiber(fibB)))
    case Right((fibA, b)) => Right((EnvFiber(fibA), b))
  }

  def racePair[E, A, B](
      ta: Env[E, A],
      tb: Env[E, B]
  ): Env[E, Either[(A, Fiber[Env[E, *], B]), (Fiber[Env[E, *], A], B)]] =
    (ta, tb) match {
      case (EnvTask(taskA), EnvTask(taskB)) =>
        fromTask(Task.racePair(taskA, taskB).map(convertRacingFibers))
      case _                                =>
        Env(ctx => Task.racePair(ta.run(ctx), tb.run(ctx)).map(convertRacingFibers))
    }

  def race[E, A, B](ta: Env[E, A], tb: Env[E, B]): Env[E, Either[A, B]] =
    (ta, tb) match {
      case (EnvTask(taskA), EnvTask(taskB)) => fromTask(Task.race(taskA, taskB))
      case _                                => Env(ctx => Task.race(ta.run(ctx), tb.run(ctx)))
    }

  def raceMany[E, A](tta: IterableOnce[Env[E, A]]): Env[E, A] = {
    val taskAccum = Array.newBuilder[Task[A]]
    val funcAccum = Array.newBuilder[E => Task[A]]
    tta.iterator.foreach {
      case EnvTask(tt) => taskAccum += tt
      case env         =>
        funcAccum += env.run
    }
    val tasks     = taskAccum.result()
    val funcs     = funcAccum.result()
    if (funcs.isEmpty) EnvTask(Task.raceMany(tasks))
    else Env(ctx => Task.raceMany(funcs.map(_(ctx)) ++ tasks))
  }
}

final private[env] case class EnvFiber[E, A](taskFiber: Fiber[Task, A]) extends Fiber[Env[E, *], A] {
  override def cancel: Env[E, Unit] = Env.fromTask(taskFiber.cancel)
  override def join: Env[E, A]      = Env.fromTask(taskFiber.join)
}
