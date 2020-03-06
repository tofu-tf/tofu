package tofu.concurrent.impl
import cats.Functor
import cats.data.State
import cats.effect.concurrent.Ref
import tofu.optics.Contains
import cats.syntax.functor._

/** Having A Contains B relation, make a ref that would change only marked part of state */
final case class FocusedRef[F[_]: Functor, A, B](ref: Ref[F, A], focus: Contains[A, B]) extends Ref[F, B] {
  private def focusedMod[X](f: B => (B, X))(a: A): (A, X) = {
    val (next, res) = f(focus.extract(a))
    focus.set(a, next) -> res
  }

  def get: F[B]          = ref.get.map(focus.extract)
  def set(b: B): F[Unit] = ref.update(a => focus.set(a, b))

  def update(f: B => B): F[Unit]               = ref.update(focus.update(_, f))
  def modify[X](f: B => (B, X)): F[X]          = ref.modify(focusedMod(f))
  def modifyState[X](state: State[B, X]): F[X] = ref.modifyState(focus.focusState(state))

  def tryUpdate(f: B => B): F[Boolean]                    = ref.tryUpdate(focus.update(_, f))
  def tryModify[X](f: B => (B, X)): F[Option[X]]          = ref.tryModify(focusedMod(f))
  def tryModifyState[X](state: State[B, X]): F[Option[X]] = ref.tryModifyState(focus.focusState(state))

  def access: F[(B, B => F[Boolean])] = ref.access.map {
    case (a, update) => (focus.extract(a), b => update(focus.set(a, b)))
  }
}
