package tofu.higherKind

import scala.reflect.ClassTag
import scala.reflect.classTag

import cats.Show
import cats.syntax.show._
import org.scalatest.funsuite.AnyFunSuite

class FactorizeSuite extends AnyFunSuite {
  type MapF[A] = Map[String, (Class[_], String)]

  test("simple factorization") {
    val fooMap = derived.factorize[Builder.type, MapF, Foo](Builder)
    assert(
      fooMap.building[Int](100)(List(1, 2, 3)) ===
        Map(
          "weight"   -> (classOf[Double]    -> (100: Double).show),
          "elems"    -> (classOf[List[Int]] -> List(1, 2, 3).show),
          "building" -> (classOf[List[Any]] -> "")
        )
    )

    assert(
      fooMap.person("Oli", 26) ===
        Map(
          "name"   -> (classOf[String] -> "Oli"),
          "age"    -> (classOf[Int]    -> "26"),
          "person" -> (classOf[Unit]   -> "")
        )
    )
  }

}

trait Foo[F[_]] {
  def person(name: String, age: Int): F[Unit]
  def building[A: Show](weight: Double)(elems: List[A]): F[List[Double]]
}

object Builder {
  def start[Res: ClassTag](name: String): Building = Building(classTag[Res].runtimeClass, name)
}

case class Building(
    resClass: Class[_],
    methodName: String,
    params: Vector[(String, (Class[_], String))] = Vector()
) {
  def arg[V: ClassTag: Show](name: String, a: V): Building =
    copy(params = params :+ (name -> (classTag[V].runtimeClass -> a.show)))

  def result: Map[String, (Class[_], String)]              = params.toMap + (methodName -> (resClass -> ""))
}
