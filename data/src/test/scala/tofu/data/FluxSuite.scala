package tofu.data
import cats.Id
import cats.instances.int._
import cats.instances.list._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class FluxSuite extends AnyFlatSpec with Matchers {
  val flux: Flux.Stream[Id, Int] = Flux.Stream.range[Id, Int](1, 11)

  "id flux" should "sum all elements" in {
    flux.foldMap(x => x) shouldBe 55
  }

  it should "convert all to List" in {
    flux.foldMapK(x => List(x)) shouldBe List.range(1, 11)
  }
}
