package tofu.concurrent

import cats.effect.ExitCase

sealed trait Exit[+E, +A] {
  def exitCase: ExitCase[E]
}

object Exit {
  case object Canceled extends Exit[Nothing, Nothing] {
    def exitCase = ExitCase.Canceled
  }
  final case class Error[+E](e: E) extends Exit[E, Nothing] {
    def exitCase = ExitCase.Error(e)
  }
  final case class Completed[+A](a: A) extends Exit[Nothing, A] {
    override def exitCase = ExitCase.Completed
  }


}
