package tofu.logging
package zlogs

import org.slf4j.helpers.MessageFormatter
import zio.{Ref, UIO, URLayer, ZLayer}

class ZTestLog(svc: String, ref: Ref[Vector[String]]) extends Logging[UIO] {
  override def write(level: Logging.Level, message: String, values: LoggedValue*): UIO[Unit] = {
    val formatted = MessageFormatter.arrayFormat(message, values.toArray).getMessage()
    ref.update(_ :+ s"[$level] <$svc> $formatted")
  }
}

object ZTestLog {
  val test: URLayer[Ref[Vector[String]], TofuLogs] =
    ZLayer.fromFunction(ref => svc => new ZTestLog(svc, ref))
}
