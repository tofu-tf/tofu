package tofu
package concurrent

import cats.Functor
import cats.effect.Bracket
import cats.effect.concurrent.{MVar, Ref}
import cats.syntax.functor._
import tofu.concurrent.Mut.FocusedMut
import tofu.optics.Contains
import tofu.syntax.bracket._

/** simplified form of synchonized mutable variable, that could be satisfied both by MVar and Ref */
trait Mut[F[_], A] {
  def get: F[A]
  def update(f: A => A): F[Unit]
  def set(a: A): F[Unit] = update(_ => a)

  def focused[B](implicit focus: A Contains B, F: Functor[F]): Mut[F, B] = new FocusedMut(this, focus)
}

object Mut {
  def ref[F[_], A](ref: Ref[F, A]): Mut[F, A]                                        = new RefMut(ref)
  def mvar[F[_], E, A](mvar: MVar[F, A])(implicit bracket: Bracket[F, E]): Mut[F, A] = new MVarMut(mvar)

  private class RefMut[F[_], A](ref: Ref[F, A]) extends Mut[F, A] {
    def get: F[A]                   = ref.get
    override def set(a: A): F[Unit] = ref.set(a)
    def update(f: A => A): F[Unit]  = ref.update(f)
  }

  private class MVarMut[F[_]: Bracket[*[_], E], E, A](mvar: MVar[F, A]) extends Mut[F, A] {
    def get: F[A]                  = mvar.read
    def update(f: A => A): F[Unit] = mvar.take.bracketIncomplete(f andThen mvar.put)(mvar.put)
  }

  private[Mut] class FocusedMut[F[_], A, B](v: Mut[F, A], focus: Contains[A, B])(implicit F: Functor[F])
      extends Mut[F, B] {
    def get: F[B]                   = v.get.map(focus.extract)
    def update(f: B => B): F[Unit]  = v.update(focus.update(_, f))
    override def set(b: B): F[Unit] = v.update(focus.set(_, b))

    override def focused[C](implicit next: B Contains C, F: Functor[F]): Mut[F, C] =
      new FocusedMut[F, A, C](v, focus >> next)
  }
}
