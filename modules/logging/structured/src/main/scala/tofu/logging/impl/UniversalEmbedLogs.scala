package tofu.logging.impl
import scala.reflect.ClassTag

import cats.{FlatMap, Id}
import tofu.lift.Lift
import tofu.logging.{Logging, Logs}

class UniversalEmbedLogs[I[_], F[_]: FlatMap](underlying: Logs[I, F])(implicit lift: Lift[I, F])
    extends Logs.Universal[F] {
  override def forService[Svc: ClassTag]: Logging[F] =
    Logging.loggingRepresentable.embed(lift.lift(underlying.forService[Svc]))
  def byName(name: String): Id[Logging[F]]           =
    Logging.loggingRepresentable.embed(lift.lift(underlying.byName(name)))
}
