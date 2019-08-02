package tofu

import cats.syntax.either._
import cats.{Applicative, ApplicativeError, Functor}
import errorInstances._
import internal.{CachedMatcher, DataEffectComp}
import tofu.optics.{Downcast, Subset, Upcast}

import scala.reflect.ClassTag

trait Raise[F[_], E] extends Raise.ContravariantRaise[F, E] {
  def raise[A](err: E): F[A]
}

object Raise extends RaiseInstances with DataEffectComp[Raise] {
  trait ContravariantRaise[F[_], -E] {
    def raise[A](err: E): F[A]
  }

  final implicit def raiseByCatsError[F[_]: ApplicativeError[*[_], E], E, E1: * <:< E]: Raise[F, E1] =
    new FromAppErr[F, E, E1] with RaiseAppApErr[F, E, E1]
}

sealed class RaiseInstances {
  final implicit def raiseUpcast[F[_], E, E1](implicit r: Raise[F, E], prism: Upcast[E, E1]): Raise[F, E1] =
    new FromPrism[F, E, E1, Raise, Upcast] with RaisePrism[F, E, E1]
}

trait RestoreTo[F[_], G[_]] {
  def restore[A](fa: F[A]): G[Option[A]]
}

trait Restore[F[_]] extends RestoreTo[F, F] {
  def restoreWith[A](fa: F[A])(ra: => F[A]): F[A]
}

trait HandleTo[F[_], G[_], E] extends RestoreTo[F, G] {
  def handleWith[A](fa: F[A])(f: E => G[A]): G[A]
  def handle[A](fa: F[A])(f: E => A)(implicit G: Applicative[G]): G[A] =
    handleWith(fa)(e => G.pure(f(e)))

  def attempt[A](fa: F[A])(implicit F: Functor[F], G: Applicative[G]): G[Either[E, A]] =
    handle(F.map(fa)(_.asRight[E]))(_.asLeft)
}

trait Handle[F[_], E] extends HandleTo[F, F, E] with Restore[F] {

  def tryHandleWith[A](fa: F[A])(f: E => Option[F[A]]): F[A]

  def tryHandle[A](fa: F[A])(f: E => Option[A])(implicit F: Applicative[F]): F[A] =
    tryHandleWith(fa)(e => f(e).map(F.pure))

  def handleWith[A](fa: F[A])(f: E => F[A]): F[A] =
    tryHandleWith(fa)(e => Some(f(e)))

  def recoverWith[A](fa: F[A])(pf: PartialFunction[E, F[A]]): F[A] =
    tryHandleWith(fa)(pf.lift)

  def recover[A](fa: F[A])(pf: PartialFunction[E, A])(implicit F: Applicative[F]): F[A] =
    tryHandle(fa)(pf.lift)


  def restoreWith[A](fa: F[A])(ra: => F[A]): F[A] = handleWith(fa)(_ => ra)
}

object Handle extends HandleInstances with DataEffectComp[Handle] {
  final implicit def handleByCatsError[F[_]: ApplicativeError[*[_], E], E, E1: ClassTag: * <:< E]: Handle[F, E1] =
    new HandleApErr

  trait ByRecover[F[_], E] extends Handle[F, E] {
    def recWith[A](fa: F[A])(pf: PartialFunction[E, F[A]]): F[A]

    def tryHandleWith[A](fa: F[A])(f: E => Option[F[A]]): F[A] =
      recWith(fa)(CachedMatcher(f))
    override def recoverWith[A](fa: F[A])(pf: PartialFunction[E, F[A]]): F[A] = recWith(fa)(pf)
    override def recover[A](fa: F[A])(pf: PartialFunction[E, A])(implicit F: Applicative[F]): F[A] =
      recWith(fa)(pf andThen F.pure _)
  }

}

sealed class HandleInstances {
  final implicit def handleDowncast[F[_], E, E1](implicit h: Handle[F, E], prism: Downcast[E, E1]): Handle[F, E1] =
    new FromPrism[F, E, E1, Handle, Downcast] with HandlePrism[F, E, E1]
}

trait Errors[F[_], E] extends Raise[F, E] with Handle[F, E]

object Errors extends ErrorInstances with DataEffectComp[Errors] {
  final implicit def errorByCatsError[F[_]: ApplicativeError[*[_], E], E, E1: ClassTag: * <:< E]: Errors[F, E1] =
    new HandleApErr[F, E, E1] with RaiseAppApErr[F, E, E1] with Errors[F, E1]
}

trait ErrorInstances {
  final implicit def errorPrismatic[F[_], E, E1](implicit e: Errors[F, E], prism: Subset[E, E1]): Errors[F, E1] =
    new FromPrism[F, E, E1, Errors, Subset] with RaisePrism[F, E, E1] with HandlePrism[F, E, E1] with Errors[F, E1]
}
