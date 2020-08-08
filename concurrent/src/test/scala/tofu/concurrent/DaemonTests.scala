package tofu.concurrent

import java.util.concurrent.Executors

import cats.effect.{ContextShift, IO}
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import tofu.concurrent.syntax.daemon._

import scala.concurrent.ExecutionContext

class DaemonTests extends AnyWordSpec with Matchers {

  "Daemon" should {

    "not fail when it is cancelled while finishing" in {
      val ec: ExecutionContext          = ExecutionContext.fromExecutor(Executors.newFixedThreadPool(2))
      implicit val cs: ContextShift[IO] = IO.contextShift(ec)

      val io = IO.pure(()).daemonize.flatMap(_.cancel)
      noException shouldBe thrownBy {
        for { _ <- 1 to 100 } io.unsafeRunSync()
      }
    }

  }

}
