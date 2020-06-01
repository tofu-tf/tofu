package tofu.syntax
import cats.syntax.SemigroupSyntax
import cats.{Monoid, Semigroup}

object monoid extends SemigroupSyntax {
  def empty[A](implicit A: Monoid[A]): A = A.empty

  implicit final class TofuSemigroupOps[A](private val lhs: A) extends AnyVal {
    def |+|(rhs: A)(implicit A: Semigroup[A]): A        = A.combine(lhs, rhs)
    def combine(rhs: A)(implicit A: Semigroup[A]): A    = A.combine(lhs, rhs)
    def combineN(rhs: Int)(implicit A: Semigroup[A]): A = A.combineN(lhs, rhs)
  }
}
