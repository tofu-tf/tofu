package tofu

import cats.data.{EitherT, OptionT, ReaderT}
import cats.syntax.either._
import cats.{Applicative, ApplicativeError, Functor, Id, Monad}
import tofu.errorInstances._
import tofu.internal.{CachedMatcher, DataEffectComp}
import tofu.lift.Lift
import tofu.optics.{Downcast, Subset, Upcast}

import scala.reflect.ClassTag

trait Raise[F[_], E] extends Raise.ContravariantRaise[F, E] {
  def raise[A](err: E): F[A]
}

object Raise extends RaiseInstances with DataEffectComp[Raise] with ErrorsInstanceChain[Raise] {

  trait ContravariantRaise[F[_], -E] {
    def raise[A](err: E): F[A]
  }

}

sealed class RaiseInstances {
  final implicit def raiseUpcast[F[_], E, E1](implicit r: Raise[F, E], prism: Upcast[E, E1]): Raise[F, E1] =
    new FromPrism[F, E, E1, Raise, Upcast] with RaisePrism[F, E, E1]
}

trait RestoreTo[F[_], G[_]] extends Lift[G, F] {
  def restore[A](fa: F[A]): G[Option[A]]
}

object RestoreTo extends ErrorsToInstanceChain[λ[(f[_], g[_], e) => RestoreTo[f, g]]]

trait Restore[F[_]] extends RestoreTo[F, F] {
  def restoreWith[A](fa: F[A])(ra: => F[A]): F[A]
}

trait HandleTo[F[_], G[_], E] extends RestoreTo[F, G] {
  def handleWith[A](fa: F[A])(f: E => G[A]): G[A]

  def attempt[A](fa: F[A])(implicit F: Functor[F], G: Applicative[G]): G[Either[E, A]] =
    handle(F.map(fa)(_.asRight[E]))(_.asLeft)

  def handle[A](fa: F[A])(f: E => A)(implicit G: Applicative[G]): G[A] =
    handleWith(fa)(e => G.pure(f(e)))
}

object HandleTo extends ErrorsToInstanceChain[HandleTo]

trait Handle[F[_], E] extends HandleTo[F, F, E] with Restore[F] {

  def tryHandleWith[A](fa: F[A])(f: E => Option[F[A]]): F[A]

  def recover[A](fa: F[A])(pf: PartialFunction[E, A])(implicit F: Applicative[F]): F[A] =
    tryHandle(fa)(pf.lift)

  def tryHandle[A](fa: F[A])(f: E => Option[A])(implicit F: Applicative[F]): F[A] =
    tryHandleWith(fa)(e => f(e).map(F.pure))

  def recoverWith[A](fa: F[A])(pf: PartialFunction[E, F[A]]): F[A] =
    tryHandleWith(fa)(pf.lift)

  def restoreWith[A](fa: F[A])(ra: => F[A]): F[A] = handleWith(fa)(_ => ra)

  def handleWith[A](fa: F[A])(f: E => F[A]): F[A] =
    tryHandleWith(fa)(e => Some(f(e)))
}

object Handle extends HandleInstances with DataEffectComp[Handle] with ErrorsInstanceChain[Handle] {

  trait ByRecover[F[_], E] extends Handle[F, E] {
    def recWith[A](fa: F[A])(pf: PartialFunction[E, F[A]]): F[A]

    def tryHandleWith[A](fa: F[A])(f: E => Option[F[A]]): F[A] =
      recWith(fa)(CachedMatcher(f))

    override def recoverWith[A](fa: F[A])(pf: PartialFunction[E, F[A]]): F[A] =
      recWith(fa)(pf)

    override def recover[A](fa: F[A])(pf: PartialFunction[E, A])(implicit F: Applicative[F]): F[A] =
      recWith(fa)(pf andThen F.pure _)
  }
}

sealed class HandleInstances {
  final implicit def handleDowncast[F[_], E, E1](implicit h: Handle[F, E], prism: Downcast[E, E1]): Handle[F, E1] =
    new FromPrism[F, E, E1, Handle, Downcast] with HandlePrism[F, E, E1]
}

trait ErrorsTo[F[_], G[_], E] extends Raise[F, E] with HandleTo[F, G, E]

object ErrorsTo extends ErrorsToInstanceChain[ErrorsTo]

trait ErrorsToInstanceChain[TC[f[_], g[_], e] >: ErrorsTo[f, g, e]]
    extends ErrorsInstanceChain[Lambda[(f[_], e) => TC[f, f, e]]] {
  final implicit def eitherTIntance[F[_], E](implicit F: Monad[F]): TC[EitherT[F, E, *], F, E] =
    new EitherTErrorsTo[F, E]

  final implicit def optionTIntance[F[_]](implicit F: Monad[F]): TC[OptionT[F, *], F, Unit] = new OptionTErrorsTo[F]

  final implicit def eitherIntance[E]: TC[Either[E, *], Id, E] =
    new EitherErrorsTo[E]
  final implicit val optionTIntance: TC[Option, Id, Unit] = OptionErrorsTo
}

trait Errors[F[_], E] extends Raise[F, E] with Handle[F, E] with ErrorsTo[F, F, E]

object Errors extends ErrorInstances with DataEffectComp[Errors] with ErrorsInstanceChain[Errors]

trait ErrorsInstanceChain[TC[f[_], e] >: Errors[f, e]] {
  final implicit def errorByCatsError[F[_]: ApplicativeError[*[_], E], E, E1: ClassTag: * <:< E]: TC[F, E1] =
    new HandleApErr[F, E, E1] with RaiseAppApErr[F, E, E1] with Errors[F, E1]
}

trait ErrorInstances {
  final implicit def errorPrismatic[F[_], E, E1](implicit e: Errors[F, E], prism: Subset[E, E1]): Errors[F, E1] =
    new FromPrism[F, E, E1, Errors, Subset] with RaisePrism[F, E, E1] with HandlePrism[F, E, E1] with Errors[F, E1]

  final implicit def readerTErrors[F[_], R, E](implicit F: Errors[F, E]): Errors[ReaderT[F, R, *], E] =
    new Errors[ReaderT[F, R, *], E] {
      def raise[A](err: E): ReaderT[F, R, A] =
        ReaderT.liftF(F.raise(err))

      def tryHandleWith[A](fa: ReaderT[F, R, A])(f: E => Option[ReaderT[F, R, A]]): ReaderT[F, R, A] =
        ReaderT(r => F.tryHandleWith(fa.run(r))(e => f(e).map(_.run(r))))

      def restore[A](fa: ReaderT[F, R, A]): ReaderT[F, R, Option[A]] =
        ReaderT(r => F.restore(fa.run(r)))

      def lift[A](fa: ReaderT[F, R, A]): ReaderT[F, R, A] = fa
    }
}
