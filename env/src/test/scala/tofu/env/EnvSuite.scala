package tofu.env

import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import tofu.{HasContextRun, RunContext, WithRun}
import tofu.lift.{Lift, Unlift, UnliftIO}

import scala.concurrent.duration._

class EnvSuite extends AnyFlatSpec with Matchers {
  "flatMap" should "not stack overflow on folding large collection" in {
    (0 to 100000).toList
      .foldLeft(Env.pure[Unit, Int](3)) { case (acc, _) =>
        acc.flatMap(Env.pure)
      }
      .run(())
      .runSyncUnsafe(Duration.Inf) shouldBe 3
  }

  "map" should "not stack overflow on folding large collection" in {
    (0 to 100000).toList
      .foldLeft(Env.pure[Unit, Int](3)) { case (acc, _) =>
        acc.map(identity)
      }
      .run(())
      .runSyncUnsafe(Duration.Inf) shouldBe 3
  }

  "map2" should "not stack overflow on folding large collection" in {
    (0 to 100000).toList
      .foldLeft(Env.pure[Unit, Int](3)) { case (acc, _) =>
        acc.map2(Env.unit[Unit])((a, _) => a)
      }
      .run(())
      .runSyncUnsafe(Duration.Inf) shouldBe 3
  }

  "map3" should "not stack overflow on folding large collection" in {
    (0 to 100000).toList
      .foldLeft(Env.pure[Unit, Int](3)) { case (acc, _) =>
        acc.map3(Env.unit[Unit], Env.unit[Unit])((a, _, _) => a)
      }
      .run(())
      .runSyncUnsafe(Duration.Inf) shouldBe 3
  }

  "parMap2" should "not stack overflow on folding large collection" in {
    (0 to 100000).toList
      .foldLeft(Env.pure[Unit, Int](3)) { case (acc, _) =>
        acc.parMap2(Env.unit[Unit])((a, _) => a)
      }
      .run(())
      .runSyncUnsafe(Duration.Inf) shouldBe 3
  }

  "parMap3" should "not stack overflow on folding large collection" in {
    (0 to 100000).toList
      .foldLeft(Env.pure[Unit, Int](3)) { case (acc, _) =>
        acc.parMap3(Env.unit[Unit], Env.unit[Unit])((a, _, _) => a)
      }
      .run(())
      .runSyncUnsafe(Duration.Inf) shouldBe 3
  }
}

object EnvSuite {
  def summonEnvInstances[E](): Unit = {
    implicitly[Lift[Task, Env[E, *]]]
    implicitly[Unlift[Task, Env[E, *]]]

    implicitly[UnliftIO[Env[E, *]]]

    implicitly[RunContext[Env[E, *]]]
    implicitly[HasContextRun[Env[E, *], Task, E]]
    implicitly[WithRun[Env[E, *], Task, E]]
    ()
  }
}
