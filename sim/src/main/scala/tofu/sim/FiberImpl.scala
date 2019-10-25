package tofu.sim
import cats.effect.Fiber
import tofu.sim.Sim._
import tofu.sim.SimT.{readTVar, writeTVar}
import tofu.syntax.monadic._
import FiberRes._

final class FiberImpl[E, A](tvar: SimTVar[FiberRes[E, A]], fiberId: Long) extends Fiber[Sim[E, *], A] {
  def cancel: Sim[E, Unit] = atomically {
    readTVar(tvar).flatMap {
      case Working => writeTVar(tvar)(Canceled)
      case _       => SimT.unit
    }
  } *> Sim.cancel(fiberId)

  def join: Sim[E, A] = atomically {
    readTVar(tvar).flatMap[FiberExit[E, A]] {
      case Working               => SimT.fail
      case exit: FiberExit[E, A] => SimT.pure(exit)
    }
  }.flatMap {
    case Succeed(a) => Sim.pure(a)
    case Failed(e)  => Sim.raise(e)
    case Canceled   => Sim.panic("joining canceled fiber")
  }
}

private[tofu] sealed trait FiberRes[+E, +A]
private[tofu] object FiberRes {
  case object Working extends FiberRes[Nothing, Nothing]

  sealed trait FiberExit[+E, +A] extends FiberRes[E, A]

  final case class Succeed[+A](a: A) extends FiberExit[Nothing, A]
  final case class Failed[+E](e: E)  extends FiberExit[E, Nothing]
  case object Canceled               extends FiberExit[Nothing, Nothing]
}
