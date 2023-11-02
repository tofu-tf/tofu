package tofu.interop

import cats.effect.Ref
import cats.effect.kernel.MonadCancelThrow
import cats.effect.std.Semaphore
import cats.{Functor, Monad}
import tofu.Fire
import tofu.concurrent.{Agent, SerialAgent}
import cats.syntax.all._
import tofu.lift.Lift
import tofu.syntax.fire._
import tofu.syntax.liftKernel._

/** Default implementation of [[tofu.concurrent.Agent]] that consists of [[cats.effect.Ref]] and
  * [[cats.effect.std.Semaphore]]
  */
final case class SemRef[F[_]: MonadCancelThrow: Fire, A](ref: Ref[F, A], sem: Semaphore[F]) extends Agent[F, A] {
  def get: F[A]                                                          = ref.get
  def updateM(f: A => F[A]): F[A]                                        = sem.permit.use(_ => ref.get >>= (f(_) flatTap ref.set))
  def fireUpdateM(f: A => F[A]): F[Unit]                                 = updateM(f).fireAndForget
  def modifyM[B](f: A => F[(B, A)]): F[B]                                =
    sem.permit.use(_ => ref.get >>= (f(_).flatMap { case (b, a) => ref.set(a) as b }))
  def updateSomeM(f: PartialFunction[A, F[A]]): F[A]                     =
    updateM(a => if (f.isDefinedAt(a)) f(a) else a.pure[F])
  def modifySomeM[B](default: B)(f: PartialFunction[A, F[(B, A)]]): F[B] =
    modifyM(a => if (f.isDefinedAt(a)) f(a) else (default, a).pure[F])
}

/** Default implementation of [[tofu.concurrent.SerialAgent]] that consists of [[cats.effect.Ref]] and
  * [[cats.effect.std.Semaphore]]
  */
final case class SerialSemRef[F[_]: MonadCancelThrow, A](ref: Ref[F, A], sem: Semaphore[F]) extends SerialAgent[F, A] {
  def get: F[A]                                                          = ref.get
  def updateM(f: A => F[A]): F[A]                                        = sem.permit.use(_ => ref.get >>= (f(_) flatTap ref.set))
  def modifyM[B](f: A => F[(B, A)]): F[B]                                =
    sem.permit.use(_ => ref.get >>= (f(_).flatMap { case (b, a) => ref.set(a) as b }))
  def updateSomeM(f: PartialFunction[A, F[A]]): F[A]                     =
    updateM(a => if (f.isDefinedAt(a)) f(a) else a.pure[F])
  def modifySomeM[B](default: B)(f: PartialFunction[A, F[(B, A)]]): F[B] =
    modifyM(a => if (f.isDefinedAt(a)) f(a) else (default, a).pure[F])
}

/** If instances of [[cats.effect.Ref]] and [[cats.effect.std.Semaphore]] can not be created for some `G[_]`, but can be
  * created for some `F[_]`, for which an instance of [[tofu.lift.Lift]] `Lift[F, G]` is present, this implementation
  * can be used
  */
final case class UnderlyingSemRef[F[_]: Functor, G[_]: Monad: ({ type L[x[_]] = Lift[F, x] })#L, A](ref: Ref[F, A], sem: Semaphore[F])
    extends SerialAgent[G, A] {
  override def get: G[A] = ref.get.lift[G]

  override def updateM(f: A => G[A]): G[A] =
    for {
      _        <- sem.acquire.lift
      oldValue <- get
      newValue <- f(oldValue)
      _        <- ref.set(oldValue).lift
      _        <- sem.release.lift
    } yield newValue

  override def modifyM[B](f: A => G[(B, A)]): G[B] =
    for {
      _        <- sem.acquire.lift
      oldValue <- get
      newValue <- f(oldValue)
      result   <- ref.set(newValue._2).as(newValue._1).lift
      _        <- sem.release.lift
    } yield result

  override def updateSomeM(f: PartialFunction[A, G[A]]): G[A] =
    updateM(a => if (f.isDefinedAt(a)) f(a) else a.pure[G])

  override def modifySomeM[B](default: B)(f: PartialFunction[A, G[(B, A)]]): G[B] =
    modifyM(a => if (f.isDefinedAt(a)) f(a) else (default, a).pure[G])
}
