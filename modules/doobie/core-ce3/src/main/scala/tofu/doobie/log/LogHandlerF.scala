package tofu.doobie.log

import derevo.derive
import doobie.util.log.LogEvent
import tofu.higherKind.derived.representableK

/** A pure analog of [[doobie.util.log.LogHandler]] that logs SQL execution events in an effect `F[_]`.
  */
@derive(representableK)
trait LogHandlerF[F[_]] {
  def run(event: LogEvent): F[Unit]
}

object LogHandlerF {
  def apply[F[_]](run: LogEvent => F[Unit]): LogHandlerF[F] = run(_)
}
