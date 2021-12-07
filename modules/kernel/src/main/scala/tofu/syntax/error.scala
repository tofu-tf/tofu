package tofu
package syntax

import cats.{Applicative, Functor, Monad}
import cats.ApplicativeError
import tofu.Raise.ContravariantRaise
object raise {

  // ** special alias for Raise[F, E] for use in situations when F[_] can be unknown from the start */
  type FindRaise[E] = FindRaise.Search[E]

  object FindRaise extends FindRaiseInstances {
    trait Find[E] extends Any
    type Search[E]    = AnyRef with Find[E] { type Eff[_] }
    type Aux[E, F[_]] = FindRaise[E] { type Eff[a] = F[a] }

    def wrap[F[_], E](r: ContravariantRaise[F, E]): Aux[E, F]   = r.asInstanceOf[Aux[E, F]]
    def unwrap[F[_], E](r: Aux[E, F]): ContravariantRaise[F, E] = r.asInstanceOf[ContravariantRaise[F, E]]

    implicit final def findByApplicativeError[F[_], E1, E](implicit
        app: ApplicativeError[F, E],
        evE: E1 <:< E
    ): Aux[E1, F] = wrap(implicitly)
  }

  trait FindRaiseInstances extends FindRaiseInstances1 {
    implicit final def findByContravariantRaise[F[_], E](implicit
        raise: ContravariantRaise[F, E]
    ): FindRaise.Aux[E, F] = FindRaise.wrap(implicitly)
  }

  trait FindRaiseInstances1 {
    implicit final def findByRaise[F[_], E1, E](implicit
        raise: Raise[F, E],
        evE: E1 <:< E
    ): FindRaise.Aux[E1, F] = FindRaise.wrap(implicitly)
  }

  implicit final class RaiseOps[E](private val err: E) extends AnyVal {
    def raise[F[_], A](implicit raise: FindRaise.Aux[E, F]): F[A] = FindRaise.unwrap(raise).raise(err)
  }

  implicit final class RaiseMonadOps[F[_], A](private val fa: F[A]) extends AnyVal {
    def verified[E](p: A => Boolean)(err: E)(implicit raise: FindRaise.Aux[E, F], F: Monad[F]): F[A] =
      F.flatMap(fa)(a => if (p(a)) F.pure(a) else FindRaise.unwrap(raise).raise(err))
  }

  implicit final class RaiseOptionOps[A](private val opt: Option[A]) extends AnyVal {
    // name changed from `liftTo` to avoid conflicts with cats.syntax.option
    def orRaise[F[_]] = new RaiseOptionApplied[F, A](opt)
  }

  implicit final class RaiseEitherOps[E, A](private val either: Either[E, A]) extends AnyVal {
    def toRaise[F[_]](implicit
        app: Applicative[F],
        raise: FindRaise.Aux[E, F]
    ): F[A] =
      either match {
        case Left(err)    => FindRaise.unwrap(raise).raise(err)
        case Right(value) => app.pure(value)
      }
  }

  class RaiseOptionApplied[F[_], A](val opt: Option[A]) extends AnyVal {
    def apply[E](err: => E)(implicit raise: FindRaise.Aux[E, F], app: Applicative[F]): F[A] =
      opt match {
        case None    => FindRaise.unwrap(raise).raise(err)
        case Some(a) => app.pure(a)
      }
  }
}

object handle {
  implicit final class HandleOps[F[_], A](private val fa: F[A]) extends AnyVal {
    def restore(implicit FE: RestoreTo[F, F]): F[Option[A]]                                             = FE.restore(fa)
    def restoreTo[G[_]](implicit FE: RestoreTo[F, G]): G[Option[A]]                                     = FE.restore(fa)
    def restoreWith(ra: => F[A])(implicit FE: Restore[F]): F[A]                                         = FE.restoreWith(fa)(ra)
    def retry(count: Int)(implicit FE: Restore[F]): F[A]                                                = if (count <= 1) fa else restoreWith(retry(count - 1))
    def retryOnly[E](count: Int)(implicit FE: Handle[F, E]): F[A]                                       =
      if (count <= 1) fa else handleWith[E](_ => retryOnly[E](count - 1))
    def handleWith[E](f: E => F[A])(implicit FE: Handle[F, E]): F[A]                                    = FE.handleWith(fa)(f)
    def handleToWith[G[_], E](f: E => G[A])(implicit FE: HandleTo[F, G, E]): G[A]                       = FE.handleWith(fa)(f)
    def tryHandleWith[E](f: E => Option[F[A]])(implicit FE: Handle[F, E]): F[A]                         = FE.tryHandleWith(fa)(f)
    def tryHandle[E](f: E => Option[A])(implicit F: Applicative[F], FE: Handle[F, E]): F[A]             = FE.tryHandle(fa)(f)
    def handle[E](f: E => A)(implicit FE: Handle[F, E], F: Applicative[F]): F[A]                        = FE.handle(fa)(f)
    def handleTo[G[_]: Applicative, E](f: E => A)(implicit FE: HandleTo[F, G, E]): G[A]                 = FE.handle(fa)(f)
    def recoverWith[E](pf: PartialFunction[E, F[A]])(implicit FE: Handle[F, E]): F[A]                   = FE.recoverWith(fa)(pf)
    def recover[E](pf: PartialFunction[E, A])(implicit F: Applicative[F], FE: Handle[F, E]): F[A]       = FE.recover(fa)(pf)
    def attempt[E](implicit F: Applicative[F], FE: Handle[F, E]): F[Either[E, A]]                       = FE.attempt(fa)
    def attemptTo[G[_]: Applicative, E](implicit F: Functor[F], FE: HandleTo[F, G, E]): G[Either[E, A]] = FE.attempt(fa)
    def onError[B, E](f: E => F[B])(implicit FE: Errors[F, E], F: Applicative[F]): F[A]                 =
      FE.handleWith(fa)(e => F.productR(f(e))(FE.raise(e)))
  }
}

object error {
  implicit final class ErrorOps[F[_], A](private val fa: F[A]) extends AnyVal {
    def adaptError[E](pf: PartialFunction[E, E])(implicit FE: Errors[F, E]): F[A] = FE.adaptError(fa)(pf)
  }
}
