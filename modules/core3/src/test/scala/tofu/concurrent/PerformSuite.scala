package tofu.concurrent

import tofu.PerformThrow
import cats.effect.IO
import tofu.internal.carriers.PerformCarrier3
import cats.effect.std.Dispatcher
import cats.data.ReaderT
import scala.annotation.unchecked.uncheckedVariance
import tofu.WithContext

case class MyContext(performer: Dispatcher[IO], traceId: Long)
object MyContext{
    type Reader[+A] = ReaderT[IO, MyContext, A @uncheckedVariance]
    implicit val withDispatcher: WithContext[Reader, Dispatcher[Reader]] = 
        WithContext.make(ReaderT.fromFunction(_.performer.))
}

class PerformSuite {
  type Eff[+A] = 
  PerformCarrier3.interop3IO[Eff]
  val p = PerformThrow[IO]
}
