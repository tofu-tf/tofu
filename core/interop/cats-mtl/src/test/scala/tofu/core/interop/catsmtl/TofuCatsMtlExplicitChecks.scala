package tofu.core.interop.catsmtl

import cats.mtl.{Ask, Local, Handle => MHandle, Raise => MRaise}
import cats.{Applicative, Functor}
import tofu.core.interop.catsmtl.instances._
import tofu.{Errors, Raise, WithContext, WithLocal}

object TofuCatsMtlExplicitChecks {

  def checkForMtlContext[F[_]: Applicative, A](C: WithContext[F, A]) = {
    makeMtlAskFromTofu[F, A](C)
  }

  def checkForMtlLocal[F[_]: Applicative, A](L: WithLocal[F, A]) = {
    makeMtlLocalFromTofu[F, A](L)
  }

  def checkForTofuWithContext[F[_], A](A: Ask[F, A]) = {
    makeTofuWithContextFromMtl[F, A](A)
  }

  def checkForTofuWithLocal[F[_], A](L: Local[F, A]) = {
    makeTofuWithLocalFromMtl[F, A](L)
  }

  def checkForMtlRaise[F[_]: Functor, E](R: Raise[F, E]) = {
    makeMtlRaiseFromTofu[F, E](R)
  }

  def checkForMtlHandle[F[_]: Applicative, E](E: Errors[F, E]) = {
    makeMtlHandleFromTofu[F, E](E)
  }

  def checkForTofuRaise[F[_], E](R: MRaise[F, E]) = {
    makeTofuRaiseFromMtl[F, E](R)
  }

  def checkForTofuErrorsAndHandle[F[_]: Functor, E](H: MHandle[F, E]) = {
    makeTofuErrorsFromMtl[F, E](H)
  }
}
