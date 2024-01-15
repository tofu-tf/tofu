package tofu.concurrent

import cats.~>
import tofu.higherKind.{RepresentableK, RepK}

trait AtomInstances {
  // TODO: use higherKind.derived macro when it is ready for scala 3
  given representableKInstance[A]: RepresentableK[({ type L[x[_]] = Atom[x, A] })#L] =
    new RepresentableK[({ type L[x[_]] = Atom[x, A] })#L] {
      def tabulate[F[_]](hom: RepK[({ type L[x[_]] = Atom[x, A] })#L, _] ~> F): Atom[F, A] = new Atom[F, A] {
        def get: F[A]                       = hom(RepK[({ type L[x[_]] = Atom[x, A] })#L](_.get))
        def set(a: A): F[Unit]              = hom(RepK[({ type L[x[_]] = Atom[x, A] })#L](_.set(a)))
        def getAndSet(a: A): F[A]           = hom(RepK[({ type L[x[_]] = Atom[x, A] })#L](_.getAndSet(a)))
        def update(f: A => A): F[Unit]      = hom(RepK[({ type L[x[_]] = Atom[x, A] })#L](_.update(f)))
        def modify[B](f: A => (A, B)): F[B] = hom(RepK[({ type L[x[_]] = Atom[x, A] })#L](_.modify(f)))
      }
    }
}
