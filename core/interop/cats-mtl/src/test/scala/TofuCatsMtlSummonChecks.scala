import cats.{Applicative, Functor}
import cats.mtl.{Ask, Local, Handle => MHandle, Raise => MRaise}
import tofu.{Errors, Handle, Raise, WithContext, WithLocal}
import tofu.core.interop.catsmtl.implicits._

object TofuCatsMtlSummonChecks {

  def checkForMtlContext[F[_]: Applicative, A: WithContext[F, *]] = {
    Ask[F, A]
  }

  def checkForMtlLocal[F[_]: Applicative, A: WithLocal[F, *]] = {
    Local[F, A]
  }

  def checkForTofuWithContext[F[_], A: Ask[F, *]] = {
    WithContext[F, A]
  }

  def checkForTofuWithLocal[F[_], A: Local[F, *]] = {
    WithLocal[F, A]
  }

  def checkForMtlRaise[F[_]: Functor, E: Raise[F, *]] = {
    MRaise[F, E]
  }

  def checkForMtlHandle[F[_]: Applicative, E: Errors[F, *]] = {
    MHandle[F, E]
  }

  def checkForTofuRaise[F[_], E: MRaise[F, *]] = {
    Raise[F, E]
  }

  def checkForTofuErrorsAndHandle[F[_]: Functor, E: MHandle[F, *]] = {
    Errors[F, E]
    Handle[F, E]
  }
}
