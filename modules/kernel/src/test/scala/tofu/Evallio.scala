package tofu

import cats.Eval
import cats.Defer
import cats.Applicative
import org.scalatest.funsuite.AnyFunSuite

trait Evallio[F[_]] {
  def eval[A](a: => A): F[A]
}

object Evallio extends EvallioInstances {
  def byDelay[F[_]: Delay]: Evallio[F] = new Evallio[F] {
    def eval[A](a: => A): F[A] = Delay[F].delay(a)
  }

  def byDeferWithApplicative[F[_]: Defer: Applicative]: Evallio[F] = new Evallio[F] {
    def eval[A](a: => A): F[A] = Defer[F].defer(Applicative[F].pure(a))
  }
}

//this test verifies that any code that can depend on the Delay
//in the project without CE2 support and optional instance via Sync
class EvallioSuite extends AnyFunSuite {
  test("evallio feels ok without Delay implicit") {
    assert(implicitly[Evallio[Eval]].eval(2).value === 2)
  }
}
