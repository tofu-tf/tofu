package tofu.core.interop.catsmtl

import cats.mtl.{Ask, Local, Handle => MHandle, Raise => MRaise}
import cats.{Applicative, Functor}
import tofu.{Errors, Raise, WithContext, WithLocal}

object mtlimplicits extends CatsMtlInstances1

private[catsmtl] trait CatsMtlInstances1 extends CatsMtlInstances2 {
  @inline final implicit def deriveLocal[F[_], C](implicit L: WithLocal[F, C], A: Applicative[F]): Local[F, C] =
    new CatsMTLTofuInstances.CatsMTLLocalInstance(L, A)

  @inline final implicit def deriveMtlHandle[F[_], E](implicit E: Errors[F, E], A: Applicative[F]): MHandle[F, E] =
    new CatsMTLTofuInstances.CatsMTLHandleInstance(E, A)
}

private[catsmtl] trait CatsMtlInstances2 {
  @inline final implicit def deriveAsk[F[_], C](implicit C: WithContext[F, C], A: Applicative[F]): Ask[F, C] =
    new CatsMTLTofuInstances.CatsMTLAskInstance(C, A)

  @inline final implicit def deriveMtlRaise[F[_], E](implicit R: Raise[F, E], F: Functor[F]): MRaise[F, E] =
    new CatsMTLTofuInstances.CatsMTLRaiseInstance(R, F)
}
