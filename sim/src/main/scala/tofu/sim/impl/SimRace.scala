package tofu.sim
package impl
import RaceState.{Completed, Error, First, Second, Working}
import tofu.sim.SIM._
import tofu.syntax.monadic._
import Transact._
import cats.effect.Fiber

sealed trait RaceState[+E, +A, +B]

object RaceState {
  case object Working                extends RaceState[Nothing, Nothing, Nothing]
  sealed trait Completed[+E, +A, +B] extends RaceState[E, A, B]
  case class Error[E](e: E)          extends Completed[E, Nothing, Nothing]
  case class First[A](a: A)          extends Completed[Nothing, A, Nothing]
  case class Second[B](b: B)         extends Completed[Nothing, Nothing, B]
}

case class SimRace[F[+_, _]: Transact: STMVMonad, E: IOMonad[F, *], A, B](
    tvar: F[TVAR, RaceState[E, A, B]]
) {
  def putFirst(e: Either[E, A]): F[STM[Nothing], Unit]  =
    tvar.read.flatMap {
      case Working => tvar.write(e.fold(Error(_), First(_)))
      case _       => stmvMonad.unit
    }
  def putSecond(e: Either[E, B]): F[STM[Nothing], Unit] =
    tvar.read.flatMap {
      case Working => tvar.write(e.fold(Error(_), Second(_)))
      case _       => stmvMonad.unit
    }

  def get(
      fibA: Fiber[F[RUN[E], *], A],
      fibB: Fiber[F[RUN[E], *], B]
  ): F[RUN[E], Either[(A, Fiber[F[RUN[E], *], B]), (Fiber[F[RUN[E], *], A], B)]] =
    (tvar.read.flatMap {
      case Working                       => fail[F, Completed[E, A, B]]
      case completed: Completed[E, A, B] => completed.pureSTM[F, Nothing]
    }.atomically : F[RUN[E], Completed[E, A, B]]).flatMap {
      case Error(e)  => error(e)
      case First(a)  => ioMonad[F, E].pure(Left(a -> fibB))
      case Second(b) => ioMonad[F, E].pure(Right(fibA -> b))
    }
}
