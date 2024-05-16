package tofu.logging
package zlogs

import zio._
import org.slf4j.helpers.MessageFormatter

class ZTestLog(svc: String, val ref: Ref[Vector[String]]) extends Logging[UIO] {
  override def write(level: Logging.Level, message: String, values: LoggedValue*): UIO[Unit] = {
    val formatted = MessageFormatter.arrayFormat(message, values.toArray).getMessage()
    ref.update(_ :+ s"[$level] <$svc> $formatted")
  }
}

object ZTestLog {
  def make(): URIO[Ref[Vector[String]], Logging.Make[UIO]] =
    ZIO.serviceWith[Ref[Vector[String]]](ref => (svc: String) => new ZTestLog(svc, ref))

  val layer: URLayer[Ref[Vector[String]], ZLogging.Make] =
    ZLayer.fromZIO(make())
}
