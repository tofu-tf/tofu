package tofu.logging
import tofu.higherKind.RepresentableK
import tofu.{ Init, higherKind}

import scala.reflect.ClassTag

/** Logging tagged with some arbitrary tag type.
  *
  * @note
  * there are no guarantees that `Service` correspond to the type parameter of `Logs.forService` method
  */
trait ServiceLogging[F[_], Service] extends Logging[F] {
  final def to[Svc2]: ServiceLogging[F, Svc2] = this.asService[Svc2]
}

object ServiceLogging {
  private[this] val representableAny: RepresentableK[ServiceLogging[*[_], Any]] =
    higherKind.derived.genRepresentableK[ServiceLogging[*[_], Any]]

  implicit def initByLogs[I[_], F[_], Svc: ClassTag](implicit logs: Logs[I, F]): Init[I, ServiceLogging[F, Svc]] =
    new Init[I, ServiceLogging[F, Svc]] {
      def init: I[ServiceLogging[F, Svc]] = logs.service[Svc]
    }

  final implicit def serviceLoggingRepresentable[Svc]: RepresentableK[ServiceLogging[*[_], Svc]] =
    representableAny.asInstanceOf[RepresentableK[ServiceLogging[*[_], Svc]]]

  final implicit def byUniversal[F[_], Svc: ClassTag](implicit unilogs: Logging.Make[F]): ServiceLogging[F, Svc] =
    unilogs.service[Svc]
}