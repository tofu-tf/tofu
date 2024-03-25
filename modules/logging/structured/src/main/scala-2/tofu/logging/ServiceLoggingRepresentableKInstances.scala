package tofu
package logging

import tofu.higherKind.RepresentableK

trait ServiceLoggingRepresentableKInstances {

  private[this] val representableAny: RepresentableK[({ type L[x[_]] = ServiceLogging[x, Any] })#L] =
    higherKind.derived.genRepresentableK[({ type L[x[_]] = ServiceLogging[x, Any] })#L]

  final implicit def serviceLoggingRepresentable[Svc]: RepresentableK[({ type L[x[_]] = ServiceLogging[x, Svc] })#L] =
    representableAny.asInstanceOf[RepresentableK[({ type L[x[_]] = ServiceLogging[x, Svc] })#L]]

}
