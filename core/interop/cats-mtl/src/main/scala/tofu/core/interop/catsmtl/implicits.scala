package tofu.core.interop.catsmtl

import cats.{Applicative, Functor}
import cats.mtl.{Ask, Local, Handle => MHandle, Raise => MRaise}
import tofu.{Errors, Raise, WithContext, WithLocal}

object implicits extends CatsMtlInstances1

private[catsmtl] trait CatsMtlInstances1 extends CatsMtlInstances2 {

  @inline final implicit def deriveWithLocal[F[_], C](implicit L: Local[F, C]): WithLocal[F, C] =
    new TofuCatsMTLInstances.TofuWithLocalInstance(L)

  @inline final implicit def deriveTofuErrors[F[_], E](implicit H: MHandle[F, E], F: Functor[F]): Errors[F, E] =
    new TofuCatsMTLInstances.TofuErrorsInstance(H, F)

  @inline final implicit def deriveLocal[F[_], C](implicit L: WithLocal[F, C], A: Applicative[F]): Local[F, C] =
    new CatsMTLTofuInstances.CatsMTLLocalInstance(L, A)

  @inline final implicit def deriveMtlHandle[F[_], E](implicit E: Errors[F, E], A: Applicative[F]): MHandle[F, E] =
    new CatsMTLTofuInstances.CatsMTLHandleInstance(E, A)
}

private[catsmtl] trait CatsMtlInstances2 {

  @inline final implicit def deriveWithContext[F[_], C](implicit A: Ask[F, C]): WithContext[F, C] =
    new TofuCatsMTLInstances.TofuWithContextInstance(A)

  @inline final implicit def deriveTofuRaise[F[_], E](implicit R: MRaise[F, E]): Raise[F, E] =
    new TofuCatsMTLInstances.TofuRaiseInstance(R)

  @inline final implicit def deriveAsk[F[_], C](implicit C: WithContext[F, C], A: Applicative[F]): Ask[F, C] =
    new CatsMTLTofuInstances.CatsMTLAskInstance(C, A)

  @inline final implicit def deriveMtlRaise[F[_], E](implicit R: Raise[F, E], F: Functor[F]): MRaise[F, E] =
    new CatsMTLTofuInstances.CatsMTLRaiseInstance(R, F)
}
