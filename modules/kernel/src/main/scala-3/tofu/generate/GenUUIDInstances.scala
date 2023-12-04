package tofu.generate

import cats.~>
import tofu.higherKind.{RepresentableK, RepK}
import java.util.UUID

trait GenUUIDInstances {
  // TODO: use higherKind.derived macro when it is ready for scala 3
  given genUUIDRepresentableK: RepresentableK[GenUUID] = new RepresentableK[GenUUID] {
    def tabulate[F[_]](hom: RepK[GenUUID, _] ~> F): GenUUID[F] = new GenUUID[F] {
      def randomUUID: F[UUID] = hom(RepK[GenUUID](_.randomUUID))
    }
  }
}
