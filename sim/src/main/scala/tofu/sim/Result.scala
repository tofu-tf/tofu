package tofu.sim
import cats.Functor

sealed trait Result
case object Finished extends Result
case object Deadlock extends Result
case object Ready    extends Result

sealed trait Exit[+A]
case object Lock                      extends Exit[Nothing]
case class Sleep[+A](a: A, dur: Long) extends Exit[A]
case class Success[+A](a: A)          extends Exit[A]
case class Panic(str: String)         extends Result with Exit[Nothing]

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
