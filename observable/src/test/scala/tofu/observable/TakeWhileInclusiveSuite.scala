package tofu.observable

import monix.catnap.MVar
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import monix.reactive.Observable
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.concurrent.duration._

class TakeWhileInclusiveSuite extends AnyFlatSpec with Matchers {

  private def writeElement[A](mvar: MVar[Task, Vector[A]])(a: A): Task[Unit]                        =
    mvar.take.flatMap(v => mvar.put(v :+ a))
  private def inclusiveElements[A](obs: Observable[A])(p: A => Boolean): Task[(Vector[A], List[A])] =
    for {
      mvar     <- MVar[Task].of(Vector.empty[A])
      produced <- obs.doOnNext(writeElement(mvar)).takeWhileInclusive(p).toListL
      written  <- mvar.read
    } yield (written, produced)

  "Observable.takeWhileInclusibe" should "produce and generate same elements" in {
    inclusiveElements(Observable.range(1, 100))(_ <= 10).runSyncUnsafe(Duration.Inf) shouldEqual
      ((Vector.range(1, 12), List.range(1, 12)))
  }
}
