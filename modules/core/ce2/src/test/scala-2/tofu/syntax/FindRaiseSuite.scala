package tofu.syntax

import cats.Monad
import tofu.compat.unused
import tofu.{Raise, Timeout}
import tofu.syntax.foption._
import tofu.syntax.raise._
import tofu.syntax.feither._
import tofu.syntax.timeout._

import scala.concurrent.duration.DurationInt
import scala.language.postfixOps

@unused
object FindRaiseSuite {
  trait CommonError
  trait AnotherCommonError

  case object ConcreteError extends CommonError

  def fOptionFindRaise[F[_]: Monad: Raise[*[_], CommonError]: Raise[*[_], AnotherCommonError]]: F[Unit] = {
    Monad[F].pure(Option.empty[Unit]).orThrow(ConcreteError)
  }

  def verifiedFindRaise[F[_]: Monad: Raise[*[_], CommonError]: Raise[*[_], AnotherCommonError]]: F[Unit] = {
    Monad[F].unit.verified(_ => true)(ConcreteError)
  }

  def fEitherFindRaise[F[_]: Monad: Raise[*[_], CommonError]: Raise[*[_], AnotherCommonError]]: F[Unit] = {
    val x = ConcreteError.asLeftF[F, Unit]
    x.reRaise
    x.absolve
  }

  def timeoutFindRaise[F[_]: Monad: Raise[*[_], CommonError]: Raise[*[_], AnotherCommonError]: Timeout]: F[Unit] = {
    Monad[F].unit.timeoutRaise(5 seconds, ConcreteError)
  }
}
