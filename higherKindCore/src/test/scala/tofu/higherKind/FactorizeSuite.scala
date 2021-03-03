package tofu.higherKind

import scala.reflect.ClassTag
import scala.reflect.classTag

import cats.Show
import cats.syntax.show._
import org.scalatest.funsuite.AnyFunSuite
import tofu.higherKind.derived.HigherKindedMacros

class FactorizeSuite extends AnyFunSuite {
  import FactorizeSuite._

  type MapF[A] = Map[String, (Class[_], String)]

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
  test("derive factorization")(`🤫`[Foo, MapF])

}

object FactorizeSuite {
  trait Foo[F[_]] {
    def person(name: String, age: Int): F[Unit]
    def building[A: Show](weight: Double)(elems: List[A]): F[List[Double]]
  }

  object Builder {
    def prepare[Alg[_[_]]](implicit Alg: ClassTag[Alg[Any]]) = new Builder(Alg.runtimeClass)

    def conjure[U[f[_]], F[_]]: U[F] = macro HigherKindedMacros.factorizeThis[F, U]
  }

  class Builder(algCls: Class[_]) {
    def start[Res: ClassTag](name: String): Building =
      Building(algCls, classTag[Res].runtimeClass, name)
  }

  case class Building(
      algClass: Class[_],
      resClass: Class[_],
      methodName: String,
      params: Vector[(String, (Class[_], String))] = Vector()
  ) {

    def arg[V: ClassTag: Show](name: String, a: V): Building =
      copy(params = params :+ (name -> (classTag[V].runtimeClass -> a.show)))

    def result: Map[String, (Class[_], String)]              =
      params.toMap + (methodName -> (resClass -> "")) + ("" -> (algClass -> ""))
  }

}
