import cats.{Applicative, Functor}
import cats.mtl.{Ask, Local, Handle => MHandle, Raise => MRaise}
import tofu.{Errors, Handle, Raise, WithContext, WithLocal}
import tofu.core.interop.catsmtl.implicits._

object TofuCatsMtlSummonChecks {

  def checkForMtlContext[F[_]: Applicative, A: WithContext[F, *]] = {
    Ask[F, A]
  }

  def checkForMtlContextNonAmbiguous[F[_]: Applicative, A: WithLocal[F, *]] = {
    Local[F, A]
    Ask[F, A]
  }

  def checkForTofuContext[F[_], A: Ask[F, *]] = {
    WithContext[F, A]
  }

  def checkForTofuContextNonAmbiguous[F[_], A: Local[F, *]] = {
    WithLocal[F, A]
    WithContext[F, A]
  }

  def checkForMtlRaise[F[_]: Functor, E: Raise[F, *]] = {
    MRaise[F, E]
  }

  def checkForTofuRaise[F[_], E: MRaise[F, *]] = {
    Raise[F, E]
  }

  def checkForTofuErrorsAndHandle[F[_]: Functor, E: MHandle[F, *]] = {
    Handle[F, E]
    Errors[F, E]
  }

  def checkForMtlHandle[F[_]: Applicative, E: Errors[F, *]] = {
    MHandle[F, E]
  }

  def checkForHandleAndRaiseNonAmbiguous[F[_]: Applicative, E: Errors[F, *]] = {
    MHandle[F, E]
    MRaise[F, E]
  }
}
