package tofu.sim

import cats.Functor

sealed trait Result[+E, +A]
final case class Done[+E, +A](res: Either[E, A]) extends Result[E, A]

sealed trait StepStatus

sealed trait StepExit extends StepStatus with Result[Nothing, Nothing]

case object Finished extends StepStatus
case object Deadlock extends StepExit
case object Ready    extends StepStatus

sealed trait Exit[+A]
case object Lock                      extends Exit[Nothing]
case class Sleep[+A](a: A, dur: Long) extends Exit[A]
case class Success[+A](a: A)          extends Exit[A]
case class Panic(str: String)         extends StepExit with Exit[Nothing]

object Exit {
  implicit val functor: Functor[Exit] = new Functor[Exit] {
    def map[A, B](fa: Exit[A])(f: A => B): Exit[B] = fa match {
      case Lock           => Lock
      case Panic(message) => Panic(message)
      case Sleep(a, dur)  => Sleep(f(a), dur)
      case Success(a)     => Success(f(a))
    }
  }
}
