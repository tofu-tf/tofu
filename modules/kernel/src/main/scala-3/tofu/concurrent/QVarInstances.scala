package tofu.concurrent

import cats.~>
import tofu.higherKind.{RepresentableK, RepK}

trait QVarInstances {
  // TODO: use higherKind.derived macro when it is ready for scala 3
  given representableK[A]: RepresentableK[({ type L[x[_]] = QVar[x, A] })#L] =
    new RepresentableK[({ type L[x[_]] = QVar[x, A] })#L] {
      def tabulate[F[_]](hom: RepK[({ type L[x[_]] = QVar[x, A] })#L, _] ~> F): QVar[F, A] = new QVar[F, A] {
        def isEmpty: F[Boolean] = hom(RepK[({ type L[x[_]] = QVar[x, A] })#L](_.isEmpty))
        def put(a: A): F[Unit]  = hom(RepK[({ type L[x[_]] = QVar[x, A] })#L](_.put(a)))
        def take: F[A]          = hom(RepK[({ type L[x[_]] = QVar[x, A] })#L](_.take))
        def read: F[A]          = hom(RepK[({ type L[x[_]] = QVar[x, A] })#L](_.read))
      }
    }
}
