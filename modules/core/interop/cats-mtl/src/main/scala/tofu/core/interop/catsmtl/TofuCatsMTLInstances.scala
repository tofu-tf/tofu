package tofu.core.interop.catsmtl

import cats.Functor
import cats.mtl.{Ask, Local, Handle => MHandle, Raise => MRaise}
import tofu.{Errors, Raise, WithContext, WithLocal}

private[catsmtl] object TofuCatsMTLInstances {

  private[catsmtl] final class TofuWithLocalInstance[F[_], C](L: Local[F, C])
      extends TofuWithContextInstance[F, C](L) with WithLocal[F, C] {
    def local[A](fa: F[A])(project: C => C): F[A] = L.local(fa)(project)
  }

  private[catsmtl] class TofuWithContextInstance[F[_], C](A: Ask[F, C]) extends WithContext[F, C] {
    def functor: Functor[F] = A.applicative
    def context: F[C]       = A.ask[C]
  }

  private[catsmtl] class TofuRaiseInstance[F[_], E](R: MRaise[F, E]) extends Raise[F, E] {
    def raise[A](err: E): F[A] = R.raise[E, A](err)
  }

  private[catsmtl] class TofuErrorsInstance[F[_], E](H: MHandle[F, E])
      extends TofuRaiseInstance[F, E](H) with Errors[F, E] {
    def tryHandleWith[A](fa: F[A])(f: E => Option[F[A]]): F[A] = H.handleWith(fa)(e => f(e).getOrElse(H.raise[E, A](e)))
    def restore[A](fa: F[A]): F[Option[A]]                     = H.applicative.map(H.attempt(fa))(_.toOption)
    def lift[A](fa: F[A]): F[A]                                = fa
  }

}
