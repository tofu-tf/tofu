package tofu.concurrent

import cats.effect.{ContextShift, IO, Timer}
import org.scalatest.{AsyncWordSpec, Matchers}
import tofu.syntax.monadic._

import scala.concurrent.ExecutionContext
import scala.concurrent.duration._

class AgentSuite extends AsyncWordSpec with Matchers {

  implicit override def executionContext: ExecutionContext = ExecutionContext.Implicits.global
  implicit val cs: ContextShift[IO]                        = IO.contextShift(executionContext)
  implicit val timer: Timer[IO]                            = IO.timer(executionContext)

  "Agent" should {
    "apply synchronous mutations" in {
      (for {
        counter <- newRef[IO].of(0)
        agent   <- newAgent[IO].of("")
        _       <- agent.updateM(s => counter.set(1).as(s + "I "))
        _       <- agent.updateM(s => counter.set(2).as(s + "am "))
        _       <- agent.updateM(s => counter.set(3).as(s + "agent!"))
        count   <- counter.get
        s       <- agent.get
      } yield assert(count === 3 && s === "I am agent!")).unsafeToFuture()
    }

    "apply asyncronous mutations" in {
      (for {
        agent <- newAgent[IO].of("")
        _     <- agent.fireUpdateM(s => IO.sleep(30.millis).as(s + "I "))
        _     <- agent.fireUpdateM(s => IO.sleep(20.millis).as(s + "am "))
        _     <- agent.fireUpdateM(s => IO.sleep(10.millis).as(s + "agent!"))
        s     <- agent.await
      } yield assert(s === "I am agent!")).unsafeToFuture()
    }

    "invoke disposable watches only once" in {
      (for {
        ref   <- newRef[IO].of("")
        agent <- newAgent[IO].watched("")
        _     <- agent.onNextUpdate((_, newS) => ref.set(newS))
        _     <- agent.updateM(s => (s + "I ").pure[IO])
        _     <- agent.updateM(s => (s + "am ").pure[IO])
        _     <- agent.updateM(s => (s + "agent!").pure[IO])
        s     <- ref.get
      } yield assert(s === "I ")).unsafeToFuture()
    }

    "invoke constant watches on each update" in {
      (for {
        ref1  <- newRef[IO].of("")
        ref2  <- newRef[IO].of("")
        agent <- newAgent[IO].watched("")
        _     <- agent.onNextUpdate((_, newS) => ref1.set(newS))
        _     <- agent.onEachUpdate((_, newS) => ref2.set(newS))
        _     <- agent.updateM(s => (s + "I ").pure[IO])
        _     <- agent.updateM(s => (s + "am ").pure[IO])
        _     <- agent.updateM(s => (s + "agent!").pure[IO])
        s1    <- ref1.get
        s2    <- ref2.get
      } yield assert(s1 === "I " && s2 === "I am agent!")).unsafeToFuture()
    }
  }
}
