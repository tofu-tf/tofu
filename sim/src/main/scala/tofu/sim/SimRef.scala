package tofu.sim
import cats.data.State
import cats.effect.concurrent.Ref
import cats.instances.tuple._
import tofu.sim.SIM._
import tofu.syntax.monadic._

case class SimRef[F[_, _]: IOMonad[*[_, _], E]: STMMonad: Transact, E, A](tvar: F[TVAR, (Long, A)]) extends Ref[F[RUN[E], *], A] {
  def get: F[RUN[E], A] = tvar.read.map(_._2).atomically
  def set(a: A): F[RUN[E], Unit] =
    tvar.read.flatMap { case (i, a) => tvar.write((i + 1, a)) }.atomically
  def getAndSet(a: A): F[RUN[E], A] =
    tvar.read.flatTap { case (i, _) => tvar.write((i + 1, a)) }.map(_._2).atomically
  def access: F[RUN[E], (A, A => F[RUN[E], Boolean])] =
    tvar.read.atomically[E].map {
      _.swap.map { i => a1 =>
        tvar.read.flatMap {
          case (`i`, _) => tvar.write((i + 1, a1)) as true
          case _        => false.pure[F[STM, *]]
        }.atomically[E]
      }
    }
  def tryUpdate(f: A => A): F[RUN[E], Boolean]           = update(f) as true
  def tryModify[B](f: A => (A, B)): F[RUN[E], Option[B]] = modify(f) map (Some(_))
  def update(f: A => A): F[RUN[E], Unit] =
    tvar.read.flatMap { case (i, a) => tvar.write((i + 1, f(a))) }.atomically
  def modify[B](f: A => (A, B)): F[RUN[E], B] =
    tvar.read.flatMap {
      case (i, a) =>
        val (a1, b) = f(a)
        tvar.write((i + 1, a1)) as b
    }.atomically
  def tryModifyState[B](state: State[A, B]): F[RUN[E], Option[B]] = tryModify(state.run(_).value)
  def modifyState[B](state: State[A, B]): F[RUN[E], B]            = modify(state.run(_).value)
}
