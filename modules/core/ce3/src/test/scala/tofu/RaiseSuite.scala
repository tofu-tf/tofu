package tofu

import cats.MonadError
import Raise.ContravariantRaise
import tofu.syntax.raise._
import cats.Applicative
import org.scalatest.funsuite.AnyFunSuite
import tofu.compat.unused212

class RaiseSuite extends AnyFunSuite {
  import tofu.RaiseSuite._
  val rooster1 = new Rooster1[Either[Throwable, _]]
  val rooster2 = new Rooster2[Either[Throwable, _]]

  test("error should be raised") {
    assert(rooster1.crow === Left(CrowErr))
    assert(rooster1.crow2 === Left(CrowErr))
    assert(rooster1.crow3 === Left(CrowErr))
    assert(rooster1.crow4 === Left(CrowErr))

    assert(rooster2.crow === Left(CrowErr))
    assert(rooster2.crow2 === Left(CrowErr))
    assert(rooster2.crow3 === Left(CrowErr))
    assert(rooster2.crow4 === Left(CrowErr))
  }
}

object RaiseSuite {
  trait CommonError
  final case class ConcreteError()        extends CommonError
  final case class AnotherConcreteError() extends CommonError

  object CrowErr               extends Exception("Oh no!")
  case class LalErr(s: String) extends Exception(s)

  def foo0[F[
      _
  ]: ({ type L[x[_]] = Raise[x, ConcreteError] })#L: ({ type L[x[_]] = Raise[x, AnotherConcreteError] })#L]
      : F[Unit] = {
    ConcreteError().raise[F, Unit]
    AnotherConcreteError().raise[F, Unit]
  }

  def foo1[F[_]: ({ type L[x[_]] = Raise[x, CommonError] })#L]: F[Unit] =
    ConcreteError().raise[F, Unit]

  def foo2[F[_]: ({ type L[x[_]] = ContravariantRaise[x, CommonError] })#L]: F[Unit] =
    ConcreteError().raise[F, Unit]

  def foo3[F[_]: ({ type L[x[_]] = ContravariantRaise[x, ConcreteError] })#L]: F[Unit] =
    ConcreteError().raise[F, Unit]

  @unused212
  def foo4[
    F[_]: ({ type L[x[_]] = ContravariantRaise[x, ConcreteError] })#L: ({ type L[x[_]] = ContravariantRaise[x, AnotherConcreteError] })#L
  ]: F[Unit] =
    ConcreteError().raise[F, Unit]

  {
    type F[+A] = Either[CommonError, A]
    implicitly[MonadError[F, CommonError]]
    implicitly[Raise[F, CommonError]]
    implicitly[ContravariantRaise[F, CommonError]]
    implicitly[Raise[F, ConcreteError]]
    implicitly[ContravariantRaise[F, ConcreteError]]
  }

  val e: Either[CrowErr.type, Nothing] = Left(CrowErr)

  class Rooster1[G[_]: MonadThrow] {
    def crow: G[Unit]  = {
      CrowErr.raise
    }
    def crow2: G[Unit] = Option.empty[Unit].orRaise(CrowErr)
    def crow3: G[Unit] = {
      val err: Throwable = CrowErr
      err.raise
    }
    def crow4: G[Unit] = e.toRaise
  }

  class Rooster2[G[_]: Throws: Applicative] {
    def crow: G[Unit]  = CrowErr.raise
    def crow2: G[Unit] = Option.empty[Unit].orRaise(CrowErr)
    def crow3: G[Unit] = {
      val err: Throwable = CrowErr
      err.raise
    }

    def crow4: G[Unit] = e.toRaise
  }
}
