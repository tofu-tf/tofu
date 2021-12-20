package tofu.env

import monix.catnap.MVar
import monix.eval.Task

final class Memoization[A] private (memo: MVar[Task, Option[A]]) {
  private def initWith(x: Task[A]): Task[A] =
    memo.take.bracketE(_.fold(x)(Task.pure)) {
      case (_, Right(b)) => memo.put(Some(b))
      case (prev, _)     => memo.put(prev)
    }

  def getOrElse(x: Task[A]): Task[A] =
    for {
      state <- memo.read
      value <- state.fold(initWith(x))(Task.pure)
    } yield value
}

object Memoization {
  def apply[A]: Task[Task[Memoization[A]]] =
    Task.delay(unsafe())

  def unsafe[A](): Task[Memoization[A]] =
    MVar[Task].of(Option.empty[A]).map(mvar => new Memoization[A](mvar)).memoize
}
