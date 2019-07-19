package tofu

final abstract class Void {
  def absurd[A]: A
}

object Void {
  def fromNothing(n: Nothing): Void = n
}
