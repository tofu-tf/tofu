package tofu.lift

import cats.Applicative
import cats.data.ReaderT
import cats.syntax.option._
import org.scalatest.flatspec.AnyFlatSpec

class UnliftSuite extends AnyFlatSpec {
  "Lift implicit def implementations" should "cast instances properly" in {
    assert(
      implicitly[Lift[Option, Option]].lift(42.some) === Some(42)
    )
    assert(
      implicitly[Lift[Option, ReaderT[Option, String, *]]].lift(42.some).run("x") === Some(42)
    )
  }
}

object UnliftSuite {

  def summonLiftInstances[F[_], R](): Unit = {
    implicitly[Lift[F, F]]
    implicitly[Lift[F, ReaderT[F, R, *]]]
    ()
  }

  def summonUnliftInstances[F[_]: Applicative, R](): Unit = {
    implicitly[Unlift[F, F]]
    implicitly[Unlift[F, ReaderT[F, R, *]]]
    ()
  }

}
