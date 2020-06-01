package tofu.env

import cats.effect.concurrent.Ref
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class EnvTraverseSuite extends AnyFlatSpec with Matchers {
  object IntRef extends EnvSpecializedFunctions[Ref[Task, Int]]

  "Env" should "traverse" in {
    val traverser = IntRef.traverse(IndexedSeq(1, 2, 3))(x => IntRef(_.update(_ + x))) >> IntRef(_.get)

    Ref[Task].of(0).flatMap(traverser.run).runSyncUnsafe() shouldBe 6
  }

  it should "traverse optimized" in {
    val optTraverser = IntRef.opt.traverse(IndexedSeq(1, 2, 3))(x => IntRef(_.update(_ + x))) >> IntRef(_.get)

    Ref[Task].of(0).flatMap(optTraverser.run).runSyncUnsafe() shouldBe 6
  }

  it should "sequence" in {
    val sequencer = IntRef.sequence(IndexedSeq(1, 2, 3).map(x => IntRef(_.update(_ + x)))) >> IntRef(_.get)

    Ref[Task].of(0).flatMap(sequencer.run).runSyncUnsafe() shouldBe 6
  }

  it should "sequence optimized" in {
    val sequencer = IntRef.opt.sequence(IndexedSeq(1, 2, 3).map(x => IntRef(_.update(_ + x)))) >> IntRef(_.get)

    Ref[Task].of(0).flatMap(sequencer.run).runSyncUnsafe() shouldBe 6
  }

}
