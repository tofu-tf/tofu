package tofu
import cats.Id
import cats.data.{EitherT, OptionT}
import cats.free.Free
import tofu.sim.Sim.Exit

import scala.annotation.unchecked.{uncheckedVariance => uc}

package object sim {
  type SimTF[A] = Journal => A
  type SimT[A]  = OptionT[Free[SimTF, *], A]

  type SimF[A] = (Runtime, Long) => Exit[A]
  type Sim[E, A] = EitherT[Free[SimF, *], E, A]

  type SimFiber = Free[Exit, Unit]
}
