package tofu.kernel.interop.catsmtl

import cats.mtl.{Ask, Handle => MHandle, Local, Raise => MRaise}
import cats.{Applicative, Functor}
import tofu.{Errors, Raise, WithContext, WithLocal}

import mtlimplicits._

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
