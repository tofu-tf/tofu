package tofu.concurrent

import org.scalatest.flatspec.AnyFlatSpec
import cats.effect.ContextShift
import cats.effect.IO
import scala.concurrent.ExecutionContext
import tofu.concurrent.syntax.deferred._

class DeferredSyntaxSuite extends AnyFlatSpec {
  implicit val cs: ContextShift[IO] = IO.contextShift(ExecutionContext.global)

  "tryComplete" should "finish process successfully after double sequential complete" in {
    val process = for {
      d <- Deferreds[IO, Int]
      _ <- d.tryComplete(1)
      _ <- d.tryComplete(2)
      r <- d.get
    } yield r

    assert(process.unsafeRunSync() === 1)
  }

  it should "finish process successfully after double concurrent complete" in {
    val process = for {
      d  <- Deferreds[IO, Int]
      s  <- Deferreds[IO, Unit]
      f1 <- (s.get *> d.tryComplete(1)).start
      f2 <- (s.get *> d.tryComplete(2)).start
      _  <- s.complete(())
      _  <- f2.join
      _  <- f1.join
      r  <- d.get
    } yield r

    val res = process.unsafeRunSync()
    assert(res === 1 || res === 2)
  }
}
