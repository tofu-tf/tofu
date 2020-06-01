package tofu.sim

import cats.data.{EitherT, OptionT}
import cats.free.Free

package object mutable {
  type SimTF[A] = Journal => A
  type SimT[A]  = OptionT[Free[SimTF, *], A]

  type SimF[A]     = (Runtime, Long) => Exit[A]
  type SimIO[E, A] = EitherT[Free[SimF, *], E, A]

  type SimProc = Free[Exit, Unit]
}
