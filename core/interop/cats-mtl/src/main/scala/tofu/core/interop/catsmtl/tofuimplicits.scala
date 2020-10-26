package tofu.core.interop.catsmtl

import cats.mtl.{Ask, Local, Handle => MHandle, Raise => MRaise}
import tofu.{Errors, Raise, WithContext, WithLocal}

object tofuimplicits extends TofuInstances1

private[catsmtl] trait TofuInstances1 extends TofuInstances2 {

  @inline final implicit def deriveWithLocal[F[_], C](implicit L: Local[F, C]): WithLocal[F, C] =
    new TofuCatsMTLInstances.TofuWithLocalInstance(L)

  @inline final implicit def deriveTofuErrors[F[_], E](implicit H: MHandle[F, E]): Errors[F, E] =
    new TofuCatsMTLInstances.TofuErrorsInstance(H)
}

private[catsmtl] trait TofuInstances2 {

  @inline final implicit def deriveWithContext[F[_], C](implicit A: Ask[F, C]): WithContext[F, C] =
    new TofuCatsMTLInstances.TofuWithContextInstance(A)

  @inline final implicit def deriveTofuRaise[F[_], E](implicit R: MRaise[F, E]): Raise[F, E] =
    new TofuCatsMTLInstances.TofuRaiseInstance(R)
}
