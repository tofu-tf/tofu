package tofu.generate

import cats.~>
import tofu.higherKind.{RepresentableK, RepK}

trait GetRandomInstances {
  // TODO: use higherKind.derived macro when it is ready for scala 3
  given genRandomRepresentableK: RepresentableK[GenRandom] = new RepresentableK[GenRandom] {
    def tabulate[F[_]](hom: RepK[GenRandom, _] ~> F): GenRandom[F] = new GenRandom[F] {
      def nextLong: F[Long]       = hom(RepK[GenRandom](_.nextLong))
      def nextInt(n: Int): F[Int] = hom(RepK[GenRandom](_.nextInt(n)))
    }
  }

}
