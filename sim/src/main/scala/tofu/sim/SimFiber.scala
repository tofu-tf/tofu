package tofu.sim
import cats.Monad
import cats.effect.Fiber
import tofu.Raise
import tofu.sim.FiberRes._
import tofu.syntax.monadic._
import tofu.syntax.raise._

sealed trait FiberRes[+E, +A]
object FiberRes {
  case object Working extends FiberRes[Nothing, Nothing]

  sealed trait FiberExit[+E, +A] extends FiberRes[E, A]

  final case class Succeed[+A](a: A) extends FiberExit[Nothing, A]
  final case class Failed[+E](e: E)  extends FiberExit[E, Nothing]
  case object Canceled               extends FiberExit[Nothing, Nothing]

  def fromEither[E, A](e: Either[E, A]): FiberExit[E, A] = e.fold(Failed(_), Succeed(_))
}

object SimFiber {
  def apply[F[_]: Monad: Raise[*[_], E], E](
      implicit sim: Simulated[F],
      at: Atomic[F]
  ): Applied[F, E, at.TVar, sim.FiberId] =
    new Applied[F, E, at.TVar, sim.FiberId](at, sim)

  class Applied[F[_]: Monad: Raise[*[_], E], E, TV[_], FId](
      atomic: Atomic[F] { type TVar[a] = TV[a] },
      sim: Simulated[F] { type FiberId = FId }
  ) {
    def of[A](tvar: atomic.TVar[FiberRes[E, A]], fiberId: sim.FiberId): Fiber[F, A] = new Fiber[F, A] {
      import atomic._
      def cancel: F[Unit] = atomic.atomically {
        transact.readTVar(tvar).flatMap {
          case Working => transact.writeTVar(tvar, Canceled)
          case _       => tmonad.unit
        }
      } *> sim.cancel(fiberId)

      def join: F[A] = atomically {
        transact.readTVar(tvar).flatMap[FiberExit[E, A]] {
          case Working               => transact.fail
          case exit: FiberExit[E, A] => exit.pure[T]
        }
      }.flatMap {
        case Succeed(a) => a.pure[F]
        case Failed(e)  => e.raise[F, A]
        case Canceled   => sim.panic[A]("joining canceled fiber")
      }
    }
  }

}
