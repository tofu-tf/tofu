package tofu.logging

import cats.~>
import tofu.higherKind.{RepresentableK, RepK}
import tofu.logging.Logging.Level

trait LoggingRepresentableKInstances {

  // TODO: use higherKind.derived macro when it is ready
  given loggingRepresentable: RepresentableK[Logging] = new RepresentableK[Logging] {
    def tabulate[F[_]](hom: RepK[Logging, _] ~> F): Logging[F] = new Logging[F] {
      def write(level: Level, message: String, values: LoggedValue*): F[Unit] =
        hom(RepK[Logging](_.write(level, message, values: _*)))
    }
  }
}
