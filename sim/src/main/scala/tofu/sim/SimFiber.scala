package tofu.sim
import SIM.{IO, IOMonad, IORaise, STM, STMMonad, TVAR, stmMonad}
import Transact.{fail, panic}
import cats.effect.Fiber
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

case class SimFiber[F[_, _]: IOMonad: STMMonad: Transact, E: IORaise[F, *], A](
    tvar: F[TVAR, FiberRes[E, A]],
    fiberId: FiberId
) extends Fiber[F[IO, *], A] {
  def cancel: F[IO, Unit] =
    tvar.read.flatMap {
      case Working => tvar.write(Canceled)
      case _       => stmMonad[F].unit
    }.atomically *> Transact.cancel[F](fiberId)

  def join: F[IO, A] =
    tvar.read
      .flatMap[FiberExit[E, A]] {
        case Working               => fail
        case exit: FiberExit[E, A] => exit.pure[F[STM, *]]
      }
      .atomically
      .flatMap {
        case Succeed(a) => a.pure[F[IO, *]]
        case Failed(e)  => e.raise[F[IO, *], A]
        case Canceled   => panic[F, A]("joining canceled fiber")
      }

}
