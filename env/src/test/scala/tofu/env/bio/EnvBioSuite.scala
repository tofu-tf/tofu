package tofu.env.bio

import cats.effect.concurrent.Ref
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global

import scala.concurrent.duration.Duration
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class EnvBioSuite extends AnyFlatSpec with Matchers {
  "pure" should "lift constant value into EnvBio context" in {
    EnvBio.pure("str").run(()).runSyncUnsafe(Duration.Inf) shouldBe Right("str")
  }

  "fromTask" should "return Left(Throwable) on failed Task" in {
    val ex = new Exception("test")
    EnvBio.fromTask(Task.raiseError(ex)).run(()).runSyncUnsafe(Duration.Inf) shouldBe Left(ex)
  }

  it should "return Right on successful Task" in {
    EnvBio.fromTask(Task.pure(1)).run(()).runSyncUnsafe(Duration.Inf) shouldBe Right(1)
  }

  "fromTaskTotal" should "fail with fatal error on failed Task" in {
    val ex = new Exception("test")
    EnvBio.fromTaskTotal(Task.raiseError(ex)).run(()).attempt.runSyncUnsafe(Duration.Inf) shouldBe Left(ex)
  }

  it should "return Right on successful Task" in {
    EnvBio.fromTaskTotal(Task.pure(1)).run(()).runSyncUnsafe(Duration.Inf) shouldBe Right(1)
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

  "mapTask" should "transform value in Task context" in {
    EnvBio.pure(1).mapTask(_.map(_ => 2)).run(()).runSyncUnsafe(Duration.Inf) shouldBe Right(2)
  }

  it should "result in fatal error if function returns failed Task" in {
    val ex = new Exception("Test")
    EnvBio.pure(1).mapTask(_ => Task.raiseError(ex)).run(()).attempt.runSyncUnsafe(Duration.Inf) shouldBe Left(ex)
  }

  "flatMap" should "chain computations" in {
    (for {
      v1 <- EnvBio.pure(1)
      v2 <- EnvBio.pure(2)
      v3 <- EnvBio.pure(3)
    } yield v1 + v2 + v3).run(()).runSyncUnsafe(Duration.Inf) shouldBe Right(6)
  }

  "context" should "return computation context" in {
    EnvBio.context[String].run("ctx").runSyncUnsafe(Duration.Inf) shouldBe Right("ctx")
  }

  "mapError" should "transform error value" in {
    EnvBio
      .raiseError("err1")
      .mapError(_ => "err2")
      .mapError(_ => "err3")
      .run(())
      .runSyncUnsafe(Duration.Inf) shouldBe Left("err3")
  }

  "tapError" should "effectfully peek at error without changing original error" in {
    EnvBio.raiseError("Err1").tapError(_ => EnvBio.pure(1)).run(()).runSyncUnsafe(Duration.Inf) shouldBe Left("Err1")
  }

  it should "return new error if effectful computation fails" in {
    EnvBio
      .raiseError("Err1")
      .tapError(_ => EnvBio.raiseError("Err2"))
      .run(())
      .runSyncUnsafe(Duration.Inf) shouldBe Left("Err2")
  }

  "tapHandle" should "effectfully peek at error without changing original error" in {
    EnvBio.raiseError("Err1").tapHandle(_ => EnvBio.pure(1)).run(()).runSyncUnsafe(Duration.Inf) shouldBe Right(1)
  }

  it should "return new error if effectful computation fails" in {
    EnvBio
      .raiseError("Err1")
      .tapHandle(_ => EnvBio.raiseError("Err2"))
      .run(())
      .runSyncUnsafe(Duration.Inf) shouldBe Left("Err1")
  }

  "onErrorHandleWith" should "recover failed computation with given function" in {
    EnvBio
      .raiseError("err")
      .onErrorHandleWith(e => EnvBio.pure(e + "1"))
      .run(())
      .runSyncUnsafe(Duration.Inf) shouldBe Right("err1")
  }

  it should "return error value if recover results in failure" in {
    EnvBio
      .raiseError("err")
      .onErrorHandleWith(e => EnvBio.raiseError(e + "1"))
      .run(())
      .runSyncUnsafe(Duration.Inf) shouldBe Left("err1")
  }

  "onErrorHandle" should "recover failed computation" in {
    EnvBio.raiseError("err").onErrorHandle(_ + "1").run(()).runSyncUnsafe(Duration.Inf) shouldBe Right("err1")
  }

  "onErrorRecover" should "recover using partial function" in {
    sealed trait Error
    case object Err1 extends Error
    case object Err2 extends Error

    Err2 // just for using

    EnvBio
      .raiseError[Error](Err1)
      .onErrorRecover { case Err1 => "Result" }
      .run(())
      .runSyncUnsafe(Duration.Inf) shouldBe Right("Result")
  }

  it should "ignore recover function if partial function is not defined at error" in {
    sealed trait Error
    case object Err1 extends Error
    case object Err2 extends Error

    EnvBio
      .raiseError[Error](Err1)
      .onErrorRecover { case Err2 => "Result" }
      .run(())
      .runSyncUnsafe(Duration.Inf) shouldBe Left(Err1)
  }

  "onErrorRecoverWith" should "recover using effectful partial function" in {
    sealed trait Error
    case object Err1 extends Error
    case object Err2 extends Error

    identity(Err2) // mention to avoid warning

    EnvBio
      .raiseError[Error](Err1)
      .onErrorRecoverWith { case Err1 => EnvBio.pure("Result") }
      .run(())
      .runSyncUnsafe(Duration.Inf) shouldBe Right("Result")
  }

  it should "ignore recover function if partial function is not defined at error" in {
    sealed trait Error
    case object Err1 extends Error
    case object Err2 extends Error

    EnvBio
      .raiseError[Error](Err1)
      .onErrorRecoverWith { case Err2 => EnvBio.pure("Result") }
      .run(())
      .runSyncUnsafe(Duration.Inf) shouldBe Left(Err1)
  }

  "map2" should "transform values" in {
    val bio1 = EnvBio.pure(1)
    val bio2 = EnvBio.pure(1)
    EnvBio.map2(bio1, bio2)(_ + _).run(()).runSyncUnsafe(Duration.Inf) shouldBe Right(2)
  }

  it should "fail if either of computation fails" in {
    val bio1: EnvBio[Any, String, Int] = EnvBio.pure(1)
    val bio2: EnvBio[Any, String, Int] = EnvBio.raiseError("Error")
    EnvBio.map2(bio1, bio2)(_ + _).run(()).runSyncUnsafe(Duration.Inf) shouldBe Left("Error")
  }

  "map3" should "transform values" in {
    val bio1 = EnvBio.pure(1)
    val bio2 = EnvBio.pure(1)
    val bio3 = EnvBio.pure(1)
    EnvBio.map3(bio1, bio2, bio3)(_ + _ + _).run(()).runSyncUnsafe(Duration.Inf) shouldBe Right(3)
  }

  it should "fail if either of computation fails" in {
    val bio1: EnvBio[Any, Nothing, Int] = EnvBio.pure(1)
    val bio2: EnvBio[Any, String, Int]  = EnvBio.raiseError("Error")
    val bio3: EnvBio[Any, Nothing, Int] = EnvBio.pure(1)
    EnvBio.map3(bio1, bio2, bio3)(_ + _ + _).run(()).runSyncUnsafe(Duration.Inf) shouldBe Left("Error")
  }

  "parMap2" should "transform values in parallel" in {
    val bio1 = EnvBio.pure(1)
    val bio2 = EnvBio.pure(1)
    EnvBio.parMap2(bio1, bio2)(_ + _).run(()).runSyncUnsafe(Duration.Inf) shouldBe Right(2)
  }

  it should "fail if either of computation fails" in {
    val bio1: EnvBio[Any, String, Int] = EnvBio.pure(1)
    val bio2: EnvBio[Any, String, Int] = EnvBio.raiseError("Error")
    EnvBio.parMap2(bio1, bio2)(_ + _).run(()).runSyncUnsafe(Duration.Inf) shouldBe Left("Error")
  }

  "parMap3" should "transform values" in {
    val bio1 = EnvBio.pure(1)
    val bio2 = EnvBio.pure(1)
    val bio3 = EnvBio.pure(1)
    EnvBio.parMap3(bio1, bio2, bio3)(_ + _ + _).run(()).runSyncUnsafe(Duration.Inf) shouldBe Right(3)
  }

  it should "fail if either of computation fails" in {
    val bio1: EnvBio[Any, Nothing, Int] = EnvBio.pure(1)
    val bio2: EnvBio[Any, String, Int]  = EnvBio.raiseError("Error")
    val bio3: EnvBio[Any, Nothing, Int] = EnvBio.pure(1)
    EnvBio.parMap3(bio1, bio2, bio3)(_ + _ + _).run(()).runSyncUnsafe(Duration.Inf) shouldBe Left("Error")
  }

  "local" should "change context of computation" in {
    val strRef: Ref[Task, String] = Ref.unsafe("")
    EnvBio.unit
    EnvBio
      .context[String]
      .mapTask(ctx => ctx.flatMap(strRef.set))
      .local[String](_ + "1")
      .run("ctx")
      .runSyncUnsafe(Duration.Inf)

    strRef.get.runSyncUnsafe(Duration.Inf) shouldBe "ctx1"
  }

  "delay" should "use thrown exception as typed error" in {
    val ex = new Exception("Test")
    EnvBio.delay(throw ex).run(()).runSyncUnsafe(Duration.Inf) shouldBe Left(ex)
  }

  "delayTotal" should "throw exception when being run" in {
    val ex = new Exception("Test")
    EnvBio.delayTotal(throw ex).run(()).attempt.runSyncUnsafe(Duration.Inf) shouldBe Left(ex)
  }
}
