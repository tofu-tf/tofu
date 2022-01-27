package tofu.env

import scala.concurrent.duration._

import cats.Parallel
import cats.instances.list._
import monix.execution.Scheduler.Implicits.global
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ParEnvSuite extends AnyFlatSpec with Matchers {
  "parSequence" should "not stack overflow on large collection" in {

    Parallel
      .parSequence(
        List
          .range(1, 10000)
          .map(i => Env.fromFunc((j: Int) => i * j))
      )
      .run(3)
      .runSyncUnsafe(Duration.Inf) shouldBe List.range(3, 30000, 3)
  }
}
