package tofu.logging.impl
import cats.{FlatMap, Id}
import tofu.lift.Lift
import tofu.logging.{Logging, Logs}

import scala.reflect.ClassTag

class UniversalEmbedLogs[I[_], F[_]: FlatMap](underlying: Logs[I, F])(implicit lift: Lift[I, F])
    extends Logs.Universal[F] {
  override def forService[Svc: ClassTag]: Logging[F] =
    Logging.loggingRepresentable.embed(lift.lift(underlying.forService[Svc]))
  def byName(name: String): Id[Logging[F]]           =
    Logging.loggingRepresentable.embed(lift.lift(underlying.byName(name)))
}
