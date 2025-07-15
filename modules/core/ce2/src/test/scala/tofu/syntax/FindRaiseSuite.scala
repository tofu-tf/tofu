package tofu.syntax

import cats.Monad
import tofu.{Raise, Timeout}
import tofu.syntax.foption._
import tofu.syntax.raise._
import tofu.syntax.feither._
import tofu.syntax.timeout._

import scala.concurrent.duration.DurationInt
import scala.language.postfixOps
import scala.annotation.nowarn

@nowarn
object FindRaiseSuite {
  trait CommonError
  trait AnotherCommonError

  case object ConcreteError extends CommonError

  def fOptionFindRaise[
      F[_]: Monad: ({ type L[x[_]] = Raise[x, CommonError] })#L: ({ type L[x[_]] = Raise[x, AnotherCommonError] })#L
  ]: F[Unit] = {
    Monad[F].pure(Option.empty[Unit]).orThrow(ConcreteError)
  }

  def verifiedFindRaise[F[
      _
  ]: Monad: ({ type L[x[_]] = Raise[x, CommonError] })#L: ({ type L[x[_]] = Raise[x, AnotherCommonError] })#L]
      : F[Unit] = {
    Monad[F].unit.verified(_ => true)(ConcreteError)
  }

  def fEitherFindRaise[F[
      _
  ]: Monad: ({ type L[x[_]] = Raise[x, CommonError] })#L: ({ type L[x[_]] = Raise[x, AnotherCommonError] })#L]
      : F[Unit] = {
    val x = ConcreteError.asLeftF[F, Unit]
    x.reRaise
    x.absolve
  }

  def timeoutFindRaise[F[
      _
  ]: Monad: ({ type L[x[_]] = Raise[x, CommonError] })#L: ({ type L[x[_]] = Raise[x, AnotherCommonError] })#L: Timeout]
      : F[Unit] = {
    Monad[F].unit.timeoutRaise(5 seconds, ConcreteError)
  }
}
