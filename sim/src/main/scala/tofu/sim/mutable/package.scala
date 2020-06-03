package tofu.sim

import cats.data.EitherT
import cats.free.Free
import cats.Monad
import scala.annotation.unchecked.uncheckedVariance

package object mutable {
  type SimTF[A]    = Journal => A
  type SimTFree[A] = Free[SimTF, A]
  type SimT[+E, A]  = EitherT[SimTFree, Option[E @uncheckedVariance], A]

  type SimF[A]     = (Runtime, Long) => Exit[A]
  type SimFree[A]  = Free[SimF, A]
  type SimIO[E, A] = EitherT[SimFree, E, A]

  type SimProc = Free[Exit, Unit]

  implicit val freeMonad: Monad[Free[SimF, *]] = Free.catsFreeMonadForFree[SimF]

}
