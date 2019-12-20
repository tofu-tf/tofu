package tofu.env.bio

import monix.execution.Scheduler.Implicits.global
import org.scalatest.{FlatSpec, Matchers}

import scala.concurrent.duration.Duration

class EnvBioSuite extends FlatSpec with Matchers {
  "pure" should "lift constant value into EnvBio context" in {
    EnvBio.pure("str").run(()).runSyncUnsafe(Duration.Inf) shouldBe Right("str")
  }

  "raiseError" should "raise user-defined error" in {
    EnvBio.raiseError("Error").run(()).runSyncUnsafe(Duration.Inf) shouldBe Left("Error")
  }

  it should "fail chain of computations and return user-defined error" in {
    (for {
      _ <- EnvBio.pure(1)
      _ <- EnvBio.raiseError("Error")
      _ <- EnvBio.pure(2)
    } yield ()).run(()).runSyncUnsafe(Duration.Inf) shouldBe Left("Error")
  }

  "map" should "map value of computation" in {
    EnvBio.pure(1).map(_ + 1).map(_ + 1).run(()).runSyncUnsafe(Duration.Inf) shouldBe Right(3)
  }

  it should "raise fatal error if" in {
    val ex = new Exception("test")
    EnvBio.pure(1).map(_ + 1).map(_ => throw ex).run(()).attempt.runSyncUnsafe(Duration.Inf) shouldBe Left(ex)
  }

  "flatMap" should "chain computations" in {
    (for {
      v1 <- EnvBio.pure(1)
      v2 <- EnvBio.pure(2)
      v3 <- EnvBio.pure(3)
    } yield v1 + v2 + v3).run(()).runSyncUnsafe(Duration.Inf) shouldBe Right(6)
  }

  "context" should "return computation context" in {
    EnvBio.context[String, Nothing].run("ctx").runSyncUnsafe(Duration.Inf) shouldBe Right("ctx")
  }

  "mapError" should "transform error value" in {
    EnvBio
      .raiseError("err1")
      .mapError(_ => "err2")
      .mapError(_ => "err3")
      .run(())
      .runSyncUnsafe(Duration.Inf) shouldBe Left("err3")
  }

  "handleErrorWith" should "recover failed computation with given function" in {
    EnvBio
      .raiseError("err")
      .handleErrorWith(e => EnvBio.pure(e + "1"))
      .run(())
      .runSyncUnsafe(Duration.Inf) shouldBe Right("err1")
  }

  it should "return error value if recover results in failure" in {
    EnvBio
      .raiseError("err")
      .handleErrorWith(e => EnvBio.raiseError(e + "1"))
      .run(())
      .runSyncUnsafe(Duration.Inf) shouldBe Left("err1")
  }
}
