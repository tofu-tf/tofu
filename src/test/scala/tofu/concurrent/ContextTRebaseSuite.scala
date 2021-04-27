package tofu.concurrent

import cats.instances.list._
import cats.{FlatMap, Monad}
import derevo.derive
import derevo.tagless.invariantK
import org.scalatest.funsuite.AnyFunSuite
import simulacrum.typeclass
import tofu.Raise
import tofu.concurrent.ContextTRebaseSuite.{History, Name, Out, State, outer}
import tofu.data.calc.CalcM
import tofu.data.derived.ContextEmbed
import tofu.data.{ICalcM, Nothing2T}
import tofu.higherKind.derived.{embed, representableK}
import tofu.lift.{Rebase, Unlift}
import tofu.optics.macros.{ClassyOptics, GenContains, promote}
import tofu.syntax.lift._
import tofu.syntax.monadic._
import tofu.syntax.raise._
import tofu.syntax.selective._

import scala.annotation.nowarn

class ContextTRebaseSuite extends AnyFunSuite {
  val count = 100

  val joe       = "joe"
  val jane      = "jane"
  val oleg      = "oleg"
  val inkognito = "inkognito"
  val name      = "name"

  val initAuth = (List(oleg, joe, jane), List(inkognito, name)).tupled.toSet

  def setName[F[_]: Name: History: Monad](name: String) =
    History[F].inkognito(Name[F].getName).flatTap(History[F].putHistory) *> Name[F].setName(name)

  def program[F[_]: Name: History: Monad] =
    (setName[F](joe) *> setName[F](jane)).replicateM_(count) *> History[F].readHistory

  test("big program should accumulate input") {
    assert(
      program[Out].run(outer).runUnit(State(auth = initAuth, name = oleg))._2 == Right(
        List.fill(count - 1)(List(joe, jane)).flatten ++ List(joe, oleg)
      )
    )
  }
}

@nowarn("cat=unused-imports")
object ContextTRebaseSuite {
  @typeclass
  @derive(representableK)
  trait Name[F[_]] {
    def getName: F[String]
    def setName(name: String): F[Unit]
  }
  object Name extends ContextEmbed[Name]

  @typeclass
  @derive(representableK)
  trait Auth[F[_]] {
    def hasAuth(key: String): F[Boolean]
    def checkAuth(key: String): F[Unit]
  }
  object Auth extends ContextEmbed[Auth]

  @typeclass
  @derive(embed, invariantK)
  trait History[F[_]] {
    def readHistory: F[List[String]]
    def putHistory(s: String): F[Unit]
    def inkognito[A](actions: F[A]): F[A]
  }
  object History extends ContextEmbed[History]

  case class NameImpl[F[_]: Auth: Monad](atom: Atom[F, String]) extends Name[F] {
    def getName: F[String] = atom.get

    def setName(name: String): F[Unit] =
      Auth[F].checkAuth("name") *> atom.set(name)
  }

  type RaiseString[F[_]] = Raise[F, String]

  case class AuthImpl[F[_]: Monad: Name: RaiseString](values: F[Set[(String, String)]]) extends Auth[F] {
    def hasAuth(key: String): F[Boolean] = for {
      name      <- Name[F].getName
      authItems <- values
    } yield authItems((name, key))
    def checkAuth(key: String): F[Unit]  =
      hasAuth(key).unlesss_(Name[F].getName.flatTap(name => s"$name hasn't auth for: $key".raise))
  }

  case class HistoryImpl[F[_]: Monad: Auth](state: Atom[F, List[String]]) extends History[F] {
    def readHistory: F[List[String]] = state.get

    def putHistory(s: String): F[Unit] = state.update(s :: _)

    def inkognito[A](actions: F[A]): F[A] = for {
      _   <- Auth[F].checkAuth("inkognito")
      old <- state.get
      _   <- state.set(Nil)
      a   <- actions
      _   <- state.set(old)
    } yield a
  }

  @ClassyOptics
  case class Inner[F[_]](
      name: Name[F],
      auth: Auth[F],
  )

  implicit object rebase extends Rebase[Inner] {
    def rebase[F[_], G[_]: FlatMap](uf: Inner[F])(implicit FG: Unlift[F, G]): Inner[G] = Inner[G](
      Rebase[Name].rebase(uf.name),
      Rebase[Auth].rebase(uf.auth)
    )
  }

  @ClassyOptics
  case class Outer[F[_]](
      @promote inner: Inner[F],
      history: History[F],
  )

  case class State(
      auth: Set[(String, String)] = Set(),
      name: String = "",
      history: List[String] = Nil,
  )

  type Eff[+A] = ICalcM[Nothing2T, Any, State, String, A]

  type In[+A]  = ContextT[Eff, Inner, A]
  type Out[+A] = ContextT[Eff, Outer, A]

  val stateIn: Atom[In, State]   = Atom.calcMAtom[Nothing, Any, State, String].lift1[In]
  val stateOut: Atom[Out, State] = Atom.calcMAtom[Nothing, Any, State, String].lift1[Out]

  val inner = Inner[In](
    NameImpl[In](stateIn.focused(GenContains[State](_.name))),
    AuthImpl[In](_ => CalcM.get.map(_.auth))
  )

  val outer = Outer[Out](
    ContextT.transfer(inner),
    HistoryImpl[Out](stateOut.focused(GenContains[State](_.history)))
  )
}
