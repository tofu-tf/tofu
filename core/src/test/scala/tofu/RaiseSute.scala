package tofu

import cats.MonadError
import cats.instances.either._
import Raise.ContravariantRaise

object RaiseSute {
  trait CommonError
  final case class ConcreteError() extends CommonError

  type F[+A] = Either[CommonError, A]
  implicitly[MonadError[F, CommonError]]
  implicitly[Raise[F, CommonError]]
  implicitly[ContravariantRaise[F, CommonError]]
  implicitly[Raise[F, ConcreteError]]
  implicitly[ContravariantRaise[F, ConcreteError]]
}
