package tofu.core.interop.catsmtl

import cats.mtl.{Ask, Local, Handle => MHandle, Raise => MRaise}
import tofu.core.interop.catsmtl.tofuimplicits._
import tofu._

object TofuFromCatsMtlSummonChecks {

  def checkForTofuWithContext[F[_], A: Ask[F, *]] = {
    WithContext[F, A]
  }

  def checkForTofuWithLocal[F[_], A: Local[F, *]] = {
    WithLocal[F, A]
  }

  def checkForTofuRaise[F[_], E: MRaise[F, *]] = {
    Raise[F, E]
  }

  def checkForTofuErrorsAndHandle[F[_], E: MHandle[F, *]] = {
    Errors[F, E]
    Handle[F, E]
  }
}
