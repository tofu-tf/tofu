package tofu.core.interop.catsmtl

import cats.{Applicative, Functor}
import cats.mtl.{Ask, Local, Handle => MHandle, Raise => MRaise}
import tofu.{Errors, Raise, WithContext, WithLocal}

private[catsmtl] object CatsMTLTofuInstances {
  private[catsmtl] class CatsMTLAskInstance[F[_], C](L: WithContext[F, C], A: Applicative[F]) extends Ask[F, C] {
    def applicative: Applicative[F] = A
    def ask[E2 >: C]: F[E2]         = A.widen[C, E2](L.context)
  }

  private[catsmtl] class CatsMTLLocalInstance[F[_], C](L: WithLocal[F, C], A: Applicative[F])
      extends CatsMTLAskInstance[F, C](L, A) with Local[F, C] {
    def local[A](fa: F[A])(f: C => C): F[A] = L.local(fa)(f)
  }

  private[catsmtl] class CatsMTLRaiseInstance[F[_], E](R: Raise[F, E], F: Functor[F]) extends MRaise[F, E] {
    def functor: Functor[F]            = F
    def raise[E2 <: E, A](e: E2): F[A] = R.raise[A](e)
  }

  private[catsmtl] class CatsMTLHandleInstance[F[_], E](E: Errors[F, E], A: Applicative[F])
      extends CatsMTLRaiseInstance[F, E](E, A) with MHandle[F, E] {
    def applicative: Applicative[F]                 = A
    def handleWith[A](fa: F[A])(f: E => F[A]): F[A] = E.handleWith(fa)(f)
  }
}
