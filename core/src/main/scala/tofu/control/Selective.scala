package tofu.control

import cats.data.{EitherT, OptionT, ReaderT, WriterT}
import cats.instances.option._
import cats.syntax.coflatMap._
import cats.syntax.option._
import cats.{Applicative, Monad, Monoid}
import tofu.control.impl._

import tofu.internal.EffectComp

trait Selective[F[_]] extends Applicative[F] {
  def selectAp[A, B](fe: F[Either[A, B]])(ff: => F[A => B]): F[B]

  def select[A](fo: F[Option[A]])(fa: => F[A]): F[A] =
    selectAp[Unit, A](map(fo)(_.toRight(())))(map(fa)(a => (_: Unit) => a))

  def selectRight[A](fb: F[A], fo: F[Option[A]]): F[A] = select(fo)(fb)

  def orElses[A](fx: F[Option[A]])(fy: => F[Option[A]]): F[Option[A]] = select(map(fx)(_.coflatten))(fy)

  def whens[A](fb: F[Boolean])(fa: => F[A]): F[Option[A]] =
    select(map(fb)(x => if (x) None else Some(none[A])))(map(fa)(_.some))

  def unlesss[A](fb: F[Boolean])(fa: => F[A]): F[Option[A]] =
    select(map(fb)(x => if (x) Some(none[A]) else None))(map(fa)(_.some))

  def whens_[A](fb: F[Boolean])(fa: => F[A]): F[Unit] =
    select(map(fb)(x => if (x) None else Some(())))(void(fa))

  def unlesss_[A](fb: F[Boolean])(fa: => F[A]): F[Unit] =
    select(map(fb)(x => if (x) Some(()) else None))(void(fa))

  def optionMonoid[A]: Monoid[F[Option[A]]] = new Monoid[F[Option[A]]] {
    def empty: F[Option[A]]                                     = pure(None)
    def combine(x: F[Option[A]], y: F[Option[A]]): F[Option[A]] = orElses(x)(y)
  }
}

object Selective extends SelectiveInstances with EffectComp[Selective]

trait SelectiveInstances extends SelectiveInstances2 {
  final implicit def selectiveOverMonad[F[_]: Monad]: SelectiveOverMonad[F] = new SelectiveOverMonad[F]
}
trait SelectiveInstances2 {
  final implicit def selectiveOptionT[F[_]: Selective]: Selective[OptionT[F, *]]               = new SelectiveOptionT[F]
  final implicit def selectiveEitherT[F[_]: Selective, E]: Selective[EitherT[F, E, *]]         = new SelectiveEitherT[F, E]
  final implicit def selectiveReaderT[F[_]: Selective, R]: Selective[ReaderT[F, R, *]]         = new SelectiveReaderT[F, R]
  final implicit def selectiveWriterT[F[_]: Selective, W: Monoid]: Selective[WriterT[F, W, *]] =
    new SelectiveWriterT[F, W]
}
