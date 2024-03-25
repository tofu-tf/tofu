package tofu.logging.internal

import tofu.higherKind
import tofu.higherKind.RepresentableK
import tofu.logging.Logs
import cats.Id

trait LogsRepresentableKInstances {
  private[this] val logs1RepresentableAny: RepresentableK[({ type L[x[_]] = Logs[x, Any] })#L] =
    higherKind.derived.genRepresentableK[({ type L[x[_]] = Logs[x, Any] })#L]

  implicit def logs1Representable[Y[_]]: RepresentableK[({ type L[x[_]] = Logs[x, Y] })#L] =
    logs1RepresentableAny.asInstanceOf[RepresentableK[({ type L[x[_]] = Logs[x, Y] })#L]]

  implicit val logs2UniversalRepresentable: RepresentableK[({ type L[x[_]] = Logs[Id, x] })#L] =
    higherKind.derived.genRepresentableK[({ type L[x[_]] = Logs[Id, x] })#L]

}
