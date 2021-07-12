package tofu

import _root_.doobie.ConnectionIO
import cats.data.ReaderT
import cats.~>
import tofu.lift.Lift

package object doobie {

  /** A contextual database effect with environment of type R. */
  type ConnectionRIO[R, A] = ReaderT[ConnectionIO, R, A]

  /** A continuational form of `ConnectionIO` equivalent to `[x] => ConnectionIO ~> F => F[x]`. */
  type ConnectionCIO[F[_], A] = ReaderT[F, ConnectionIO ~> F, A]

  /** A typeclass for lifting `ConnectionIO` into extended database effects such as `ConnectionRIO`. */
  type LiftConnectionIO[DB[_]] = Lift[ConnectionIO, DB]
}
