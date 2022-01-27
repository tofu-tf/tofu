package tofu.higherKind

import scala.reflect.{ClassTag, classTag}

import cats.Show
import cats.syntax.show._
import org.scalatest.funsuite.AnyFunSuite
import tofu.higherKind.derived.HigherKindedMacros

class FactorizeSuite extends AnyFunSuite {
  import FactorizeSuite._

  type MapF[A] = MyMap

  def testFactorization(fooMap: Foo[MapF]) = {
    assert(
      fooMap.building[Int](100)(List(1, 2, 3)) ===
        Map(
          ""         -> (classOf[Foo[Any]]  -> ""),
          "building" -> (classOf[List[Any]] -> ""),
          "weight"   -> (classOf[Double]    -> (100: Double).show),
          "elems"    -> (classOf[List[Int]] -> List(1, 2, 3).show),
        )
    )

    assert(
      fooMap.person("Oli", 26) ===
        Map(
          ""       -> (classOf[Foo[Any]] -> ""),
          "person" -> (classOf[Unit]     -> ""),
          "name"   -> (classOf[String]   -> "Oli"),
          "age"    -> (classOf[Int]      -> "26"),
        )
    )
  }

  test("simple factorization")(testFactorization(derived.factorize[Builder.type, MapF, Foo](Builder)))

  import FactorizeSuite.{Builder => `strange and fancy name`}
  import `strange and fancy name`.{conjure => `🤫`}

  test("derive factorization")(testFactorization(`🤫`[Foo, MapF]))

  type MapB[E, A] = Map[String, (Class[_], String)]

  def testBiFactorization(barMap: Bar[MapB]) = {
    assert(
      barMap.building[Int](100)(List(1, 2, 3)) ===
        Map(
          ""             -> (classOf[Bar[Any]]  -> ""),
          "building-res" -> (classOf[List[Any]] -> ""),
          "building-err" -> (classOf[String]    -> ""),
          "weight"       -> (classOf[Double]    -> (100: Double).show),
          "elems"        -> (classOf[List[Int]] -> List(1, 2, 3).show),
        )
    )

    assert(
      barMap.person("Oli", 26) ===
        Map(
          ""           -> (classOf[Bar[Any]] -> ""),
          "person-err" -> (classOf[Nothing]  -> ""),
          "person-res" -> (classOf[Unit]     -> ""),
          "name"       -> (classOf[String]   -> "Oli"),
          "age"        -> (classOf[Int]      -> "26"),
        )
    )
  }

  test("simple bifactorization")(testBiFactorization(derived.bifactorize[BiBuilder.type, MapB, Bar](BiBuilder)))

  import FactorizeSuite.{BiBuilder => `frightening and inappropriate name`}
  import `frightening and inappropriate name`.{conjure => `😨`}
  test("derive bifactorization")(testBiFactorization(`😨`[Bar, MapB]))

}

object FactorizeSuite {
  type MyMap = Map[String, (Class[_], String)]

  trait Foo[F[_]] {
    def person(name: String, age: Int): F[Unit]
    def building[A: Show](weight: Double)(elems: List[A]): F[List[Double]]
  }

  trait Bar[F[_, _]] {
    def person(name: String, age: Int): F[Nothing, Unit]
    def building[A: Show](weight: Double)(elems: List[A]): F[String, List[Double]]
  }

  object Builder {
    type Result[A] = MyMap

    def prepare[Alg[_[_]]](implicit Alg: ClassTag[Alg[Any]]) = new Builder(Alg.runtimeClass)

    def conjure[U[f[_]], F[_]]: U[F] = macro HigherKindedMacros.factorizeThis[U]
  }

  class Builder(algCls: Class[_]) {
    def start[Res: ClassTag](name: String): Building =
      Building(Map(name -> (classTag[Res].runtimeClass -> ""), "" -> (algCls -> "")))
  }

  object BiBuilder {
    type Result[E, A] = MyMap

    def prepare[Alg[_[_, _]]](implicit Alg: ClassTag[Alg[Any]]) = new BiBuilder(Alg.runtimeClass)

    def conjure[U[f[_, _]], F[_, _]]: U[F] = macro HigherKindedMacros.bifactorizeThis[U]
  }

  class BiBuilder(algCls: Class[_]) {
    def start[Err: ClassTag, Res: ClassTag](name: String): Building =
      Building(
        Map(
          s"$name-err" -> (classTag[Err].runtimeClass -> ""),
          s"$name-res" -> (classTag[Res].runtimeClass -> ""),
          ""           -> (algCls                     -> "")
        )
      )
  }

  case class Building(result: Map[String, (Class[_], String)] = Map()) {
    def arg[V: ClassTag: Show](name: String, a: V): Building =
      copy(result = result + (name -> (classTag[V].runtimeClass -> a.show)))
  }
}
