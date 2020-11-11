package tofu.core.interop.catsmtl

import cats.mtl.{Ask, Local, Handle => MHandle, Raise => MRaise}
import cats.{Applicative, Functor}
import tofu.core.interop.catsmtl.mtlimplicits._
import tofu.{Errors, Raise, WithContext, WithLocal}

object CatsMtlFromTofuSummonChecks {

  def checkForMtlContext[F[_]: Applicative, A: WithContext[F, *]] = {
    Ask[F, A]
  }

  def checkForMtlLocal[F[_]: Applicative, A: WithLocal[F, *]] = {
    Local[F, A]
  }

  def checkForMtlRaise[F[_]: Functor, E: Raise[F, *]] = {
    MRaise[F, E]
  }

  def checkForMtlHandle[F[_]: Applicative, E: Errors[F, *]] = {
    MHandle[F, E]
  }
}
