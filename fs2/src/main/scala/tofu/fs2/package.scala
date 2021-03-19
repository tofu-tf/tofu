package tofu

import _root_.fs2.Stream
import tofu.lift.Lift

package object fs2 {

  type LiftStream[S[_], F[_]] = Lift[Stream[F, *], S]

  object LiftStream {
    def apply[S[_], F[_]](implicit ev: LiftStream[S, F]): LiftStream[S, F] = ev
  }
}
