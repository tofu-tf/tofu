package tofu.core.interop.catsmtl

import cats.{Applicative, Functor}
import cats.mtl.{Ask, Local, Handle => MHandle, Raise => MRaise}
import tofu.{Errors, Raise, WithContext, WithLocal}

object instances {

  def makeTofuWithContextFromMtl[F[_], C](ask: Ask[F, C]): WithContext[F, C] =
    new TofuCatsMTLInstances.TofuWithContextInstance(ask)

  def makeTofuWithLocalFromMtl[F[_], C](local: Local[F, C]): WithLocal[F, C] =
    new TofuCatsMTLInstances.TofuWithLocalInstance(local)

  def makeTofuErrorsFromMtl[F[_], E](handle: MHandle[F, E]): Errors[F, E] =
    new TofuCatsMTLInstances.TofuErrorsInstance(handle)

  def makeTofuRaiseFromMtl[F[_], E](raise: MRaise[F, E]): Raise[F, E] =
    new TofuCatsMTLInstances.TofuRaiseInstance(raise)

  def makeMtlLocalFromTofu[F[_], C](local: WithLocal[F, C])(implicit A: Applicative[F]): Local[F, C] =
    new CatsMTLTofuInstances.CatsMTLLocalInstance(local, A)

  def makeMtlHandleFromTofu[F[_], E](errors: Errors[F, E])(implicit A: Applicative[F]): MHandle[F, E] =
    new CatsMTLTofuInstances.CatsMTLHandleInstance(errors, A)

  def makeMtlAskFromTofu[F[_], C](context: WithContext[F, C])(implicit A: Applicative[F]): Ask[F, C] =
    new CatsMTLTofuInstances.CatsMTLAskInstance(context, A)

  def makeMtlRaiseFromTofu[F[_], E](raise: Raise[F, E])(implicit F: Functor[F]): MRaise[F, E] =
    new CatsMTLTofuInstances.CatsMTLRaiseInstance(raise, F)
}
