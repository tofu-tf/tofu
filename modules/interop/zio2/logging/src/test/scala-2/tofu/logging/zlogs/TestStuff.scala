package tofu.logging.zlogs

import derevo.derive
import tofu.logging.derivation.loggable
import zio.{Clock, FiberRef, LogSpan, ZIO}

object TestStuff {

  // ZIO 2.0.4 LogSpan uses java.lang.System for currentTime, therefore it's untestable
  def addLogSpan[R, E, A](name: String)(zio: ZIO[R, E, A]): ZIO[R, E, A] = {
    Clock.instant.flatMap(instant =>
      FiberRef.currentLogSpan.locallyWith(list => LogSpan(name, instant.toEpochMilli) :: list)(zio)
    )
  }

  @derive(loggable)
  case class User(name: String)
}
