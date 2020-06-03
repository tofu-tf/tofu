package tofu.sim
package impl

import SIM._
import Transact.{cancel => _, _}
import cats.effect.Fiber
import tofu.sim.impl.FiberRes._
import tofu.syntax.monadic._

sealed trait FiberRes[+E, +A]
object FiberRes {
  case object Working extends FiberRes[Nothing, Nothing]

  sealed trait FiberExit[+E, +A] extends FiberRes[E, A]

  final case class Succeed[+A](a: A) extends FiberExit[Nothing, A]
  final case class Failed[+E](e: E)  extends FiberExit[E, Nothing]
  case object Canceled               extends FiberExit[Nothing, Nothing]

  def fromEither[E, A](e: Either[E, A]): FiberExit[E, A] = e.fold(Failed(_), Succeed(_))
}

case class SimFiber[F[+_, _]: IOMonad[*[_, _], E] : VoidMonad: STMVMonad: Transact, E, A](
    tvar: F[TVAR, FiberRes[E, A]],
    fiberId: FiberId
) extends Fiber[F[RUN[E], *], A] {
  def cancel: F[RUN[E], Unit] =
    tvar.read.flatMap {
      case Working => tvar.write(Canceled) as true
      case _       => false.pureSTM[F, Nothing]
    }.atomically >>= Transact.cancel[F, Nothing](fiberId).whenA

  def join: F[RUN[E], A] =
    (tvar.read
      .flatMap[FiberExit[E, A]] {
        case Working               => fail
        case exit: FiberExit[E, A] => exit.pure[F[STM[Nothing], *]]
      }
      .atomically : F[RUN[E], FiberExit[E, A]])
      .flatMap {
        case Succeed(a) => a.pure[F[RUN[E], *]]
        case Failed(e)  => error(e)
        case Canceled   => panic[F, E, A]("joining canceled fiber")
      }

}
