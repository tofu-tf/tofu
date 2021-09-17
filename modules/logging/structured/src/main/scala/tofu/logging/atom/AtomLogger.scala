package tofu.logging.atom
import java.time.Instant
import java.util.concurrent.TimeUnit

import tofu.time.Clock
import cats.{Applicative, FlatMap}
import tofu.concurrent.Atom
import tofu.higherKind.Embed
import tofu.logging.{LoggedValue, Logging, Logs}
import tofu.syntax.monadic._

import scala.reflect.{ClassTag, classTag}

final case class LogLine(
    loggerName: String,
    level: Logging.Level,
    message: String,
    timestamp: Instant,
    values: Vector[LoggedValue],
)

class AtomLogging[F[_]: FlatMap: Clock](log: Atom[F, Vector[LogLine]], name: String) extends Logging[F] {
  override def write(level: Logging.Level, message: String, values: LoggedValue*): F[Unit] =
    Clock[F].realTime(TimeUnit.MILLISECONDS).flatMap { time =>
      log.update(
        _ :+ LogLine(
          loggerName = name,
          level = level,
          message = message,
          timestamp = Instant.ofEpochMilli(time),
          values = values.toVector
        )
      )
    }

}

final case class AtomLogs[I[_]: Applicative, F[_]: FlatMap: Clock](flog: F[Atom[F, Vector[LogLine]]])
    extends Logs[I, F] {
  override def forService[Svc: ClassTag]: I[Logging[F]] = byName(classTag[Svc].runtimeClass.getName)
  def byName(name: String): I[Logging[F]]               =
    Embed.of(flog.map[Logging[F]](new AtomLogging[F](_, name))).pure[I]
}
