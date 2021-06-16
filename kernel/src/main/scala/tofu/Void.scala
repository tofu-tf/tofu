package tofu

final abstract class Void {
  def absurd[A]: A
}

object Void {
  def fromNothing(n: Nothing): Void = n

  def mergeEither[A](x: Either[Void, A]): A = x match {
    case Left(value)  => value.absurd
    case Right(value) => value
  }

}
