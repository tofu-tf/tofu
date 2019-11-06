package tofu.sim
import cats.Monad
import cats.effect.Fiber
import tofu.Raise
import tofu.sim.FiberRes._
import SIM.{IO, IOMonad, IORaise, STM, STMMonad, TVAR, stmMonad}
import tofu.syntax.monadic._
import tofu.syntax.raise._
import Transact.{atomically, cancel, fail, newTVar, panic}

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
  def apply[F[_, _]: IOMonad: STMMonad: Transact, E: IORaise[F, *], A](
      tvar: F[TVAR, FiberRes[E, A]],
      fiberId: FiberId
  ) =
    new Fiber[F[IO, *], A] {
      def cancel: F[IO, Unit] = atomically {
        tvar.read.flatMap {
          case Working => tvar.write(Canceled)
          case _       => stmMonad[F].unit
        }
      } *> Transact.cancel[F](fiberId)

      def join: F[IO, A] = atomically {
        tvar.read.flatMap[FiberExit[E, A]] {
          case Working               => fail
          case exit: FiberExit[E, A] => exit.pure[F[STM, *]]
        }
      }.flatMap {
        case Succeed(a) => a.pure[F[IO, *]]
        case Failed(e)  => e.raise[F[IO, *], A]
        case Canceled   => panic[F, A]("joining canceled fiber")
      }
    }

}
