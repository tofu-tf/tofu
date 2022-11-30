package tofu

import cats.data.{EitherT, OptionT, ReaderT}
import cats.syntax.either._
import cats.{Applicative, ApplicativeError, FlatMap, Functor, Id, Monad}
import tofu.errorInstances._
import tofu.internal.{CachedMatcher, DataEffectComp}
import tofu.lift.Lift
import glass.PUpcast.GenericSubtypeImpl
import glass.{Downcast, Subset, Upcast}

import scala.annotation.implicitNotFound

/** Allows to raise `E` inside type `F`.
  */
@implicitNotFound("""can't understand how to raise ${E} inside ${F} 
provide an instance of Raise[${F}, ${E}], cats.ApplicativeError[${F}, ${E}] or Upcast[..., ${E}]""")
trait Raise[F[_], E] extends ErrorBase with Raise.ContravariantRaise[F, E] {
  def raise[A](err: E): F[A]
}

object Raise extends DataEffectComp[Raise] {

  trait ContravariantRaise[F[_], -E] extends ErrorBase {
    def raise[A](err: E): F[A]

    def reRaise[A, E1 <: E](fa: F[Either[E1, A]])(implicit F: FlatMap[F], A: Applicative[F]): F[A] =
      F.flatMap(fa)(_.fold(raise[A], A.pure))
  }
}

/** Allows to recover after some error in a ${F} transiting to a ${G} as a result. A `G` can either be the same as a `F`
  * or some "subconstructor" having less errors semantically.
  */
@implicitNotFound("""can't understand how to restore from the type ${F} to the subtype ${G} 
provide an instance of RestoreTo[${F}, ${G}], cats.ApplicativeError[${F}, ...]""")
trait RestoreTo[F[_], G[_]] extends Lift[G, F] with ErrorBase {
  def restore[A](fa: F[A]): G[Option[A]]
}

/** Allows to recover after some error in a ${F}.
  */
@implicitNotFound("""can't understand how to restore in the type ${F}
provide an instance of Restore[${F}], cats.ApplicativeError[${F}, ...]""")
trait Restore[F[_]] extends RestoreTo[F, F] {
  def restoreWith[A](fa: F[A])(ra: => F[A]): F[A]
}

/** Allows to recover after an error of type ${E} in a ${F} transiting to a ${G} as a result. A `G` can either be the
  * same as a `F` or some "subconstructor" having less errors semantically.
  */
@implicitNotFound("""can't understand how to recover from ${E} in the type ${F} to the subtype ${G} 
provide an instance of HandleTo[${F}, ${G}, ${E}], cats.ApplicativeError[${F}, ${E}]""")
trait HandleTo[F[_], G[_], E] extends RestoreTo[F, G] {
  def handleWith[A](fa: F[A])(f: E => G[A]): G[A]

  def attempt[A](fa: F[A])(implicit F: Functor[F], G: Applicative[G]): G[Either[E, A]] =
    handle(F.map(fa)(_.asRight[E]))(_.asLeft)

  def handle[A](fa: F[A])(f: E => A)(implicit G: Applicative[G]): G[A] =
    handleWith(fa)(e => G.pure(f(e)))
}

/** Allows to recover after an error of type ${E} in a ${F}.
  */
@implicitNotFound("""can't understand how to recover from ${E} in the type ${F}
provide an instance of Handle[${F}, ${E}], cats.ApplicativeError[${F}, ${E}] or Downcast[..., ${E}]""")
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

object Handle extends DataEffectComp[Handle] {

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

/** Allows to throw and handle errors of type ${E} in a ${F} transiting to a ${G} when recovering. A `G` can either be
  * the same as `F` or some "subconstructor" having less errors semantically.
  */
@implicitNotFound("""can't understand how to deal with errors ${E} in the type ${F} with the subtype ${G}
provide an instance of ErrorsTo[${F}, ${G}, ${E}], cats.ApplicativeError[${F}, ${E}] or Contains[..., ${E}]""")
trait ErrorsTo[F[_], G[_], E] extends Raise[F, E] with HandleTo[F, G, E]

/** Allows to throw and handle errors of type ${E} in a ${F}.
  */
@implicitNotFound("""can't understand how to deal with errors ${E} in the type ${F}
provide an instance of Errors[${F}, ${E}], cats.ApplicativeError[${F}, ${E}] or Contains[..., ${E}]""")
trait Errors[F[_], E] extends Raise[F, E] with Handle[F, E] with ErrorsTo[F, F, E] {
  def adaptError[A](fa: F[A])(pf: PartialFunction[E, E]): F[A] =
    recoverWith(fa)(pf.andThen(raise[A] _))
}

object Errors extends DataEffectComp[Errors] {

  trait Companion[E] {
    type Raise[F[_]]  = tofu.Raise[F, E]
    type Handle[F[_]] = tofu.Handle[F, E]
    type Errors[F[_]] = tofu.Errors[F, E]
  }
}

/** Base trait for instance search
  */
trait ErrorBase
object ErrorBase extends ErrorsBaseInstances {

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
class ErrorsBaseInstances extends ErrorsBaseInstances1 {

  final implicit def errorByCatsError[F[_], E](implicit F: ApplicativeError[F, E]): Errors[F, E] =
    new HandleApErr[F, E] with RaiseAppApErr[F, E] with Errors[F, E]
}

class ErrorsBaseInstances1 extends ErrorsBaseInstances2 {

  final implicit def errorPrismatic[F[_], E, E1](implicit
      e: Errors[F, E],
      prism: Subset[E, E1]
  ): Errors[F, E1] =
    new FromPrism[F, E, E1, Errors, Subset] with RaisePrism[F, E, E1] with HandlePrism[F, E, E1] with Errors[F, E1]
}

class ErrorsBaseInstances2 extends ErrorsBaseInstances3 {

  final implicit def handleDowncast[F[_], E, E1](implicit h: Handle[F, E], prism: Downcast[E, E1]): Handle[F, E1] =
    new FromPrism[F, E, E1, Handle, Downcast] with HandlePrism[F, E, E1]

  final implicit def raiseUpcast[F[_], E, E1](implicit r: Raise[F, E], prism: Upcast[E, E1]): Raise[F, E1] =
    prism match {
      case GenericSubtypeImpl =>
        r.asInstanceOf[Raise[F, E1]]
      case _                  =>
        new Raise[F, E1] {
          def raise[A](err: E1): F[A] = r.raise(prism.upcast(err))
        }
    }
}

class ErrorsBaseInstances3 {

  final implicit def eitherTIntance[F[_], E](implicit F: Monad[F]): ErrorsTo[EitherT[F, E, *], F, E] =
    new EitherTErrorsTo[F, E]

  final implicit def optionTIntance[F[_]](implicit F: Monad[F]): ErrorsTo[OptionT[F, *], F, Unit] =
    new OptionTErrorsTo[F]

  final implicit def eitherIntance[E]: ErrorsTo[Either[E, *], Id, E] = new EitherErrorsTo[E]
  final implicit val optionTIntance: ErrorsTo[Option, Id, Unit]      = OptionErrorsTo
}
