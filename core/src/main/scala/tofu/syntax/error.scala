package tofu
package syntax

import cats.Applicative
import tofu.Raise.ContravariantRaise

object raise {
  final implicit class RaiseOps[E](val err: E) extends AnyVal {
    def raise[F[_], A](implicit raise: Raise[F, E]): F[A] = raise.raise(err)
  }

  final implicit class RaiseOptionOps[A](val opt: Option[A]) extends AnyVal {
    // name changed from `liftTo` to avoid conflicts with cats.syntax.option
    def orRaise[F[_]] = new RaiseOptionApplied[F, A](opt)
  }

  final implicit class RaiseEitherOps[E, A](val either: Either[E, A]) extends AnyVal {
    def toRaise[F[_]](implicit
                      app: Applicative[F],
                      raise: Raise[F, E]): F[A] =
      either match {
        case Left(err)    => raise.raise(err)
        case Right(value) => app.pure(value)
      }
  }

  class RaiseOptionApplied[F[_], A](val opt: Option[A]) extends AnyVal {
    def apply[E](err: => E)(implicit raise: ContravariantRaise[F, E], app: Applicative[F]): F[A] =
      opt match {
        case None    => raise.raise(err)
        case Some(a) => app.pure(a)
      }
  }
}

object handle {
  final implicit class RestoreToOps[F[_], G[_], A, E](val fa: F[A]) extends AnyVal {
    def restore(implicit FE: RestoreTo[F, G]): G[Option[A]] = FE.restore(fa)
  }

  implicit class RestoreOps[F[_], A, E](val fa: F[A]) extends AnyVal {
    def restoreWith(ra: => F[A])(implicit FE: Restore[F]): F[A] = FE.restoreWith(fa)(ra)
    def retry(count: Int)(implicit FE: Handle[F, E]): F[A]      = if (count <= 1) fa else restoreWith(retry(count - 1))
  }

  final implicit class HandleOps[F[_], A, E](val fa: F[A]) extends AnyVal {
    def tryHandleWith(f: E => Option[F[A]])(implicit FE: Handle[F, E]): F[A]                   = FE.tryHandleWith(fa)(f)
    def tryHandle(f: E => Option[A])(implicit F: Applicative[F], FE: Handle[F, E]): F[A]       = FE.tryHandle(fa)(f)
    def handleWith(f: E => F[A])(implicit FE: Handle[F, E]): F[A]                              = FE.handleWith(fa)(f)
    def handle(f: E => A)(implicit F: Applicative[F], FE: Handle[F, E]): F[A]                  = FE.handle(fa)(f)
    def recoverWith(pf: PartialFunction[E, F[A]])(implicit FE: Handle[F, E]): F[A]             = FE.recoverWith(fa)(pf)
    def recover(pf: PartialFunction[E, A])(implicit F: Applicative[F], FE: Handle[F, E]): F[A] = FE.recover(fa)(pf)
    def attempt(implicit F: Applicative[F], FE: Handle[F, E]): F[Either[E, A]]                 = FE.attempt(fa)
  }
}
