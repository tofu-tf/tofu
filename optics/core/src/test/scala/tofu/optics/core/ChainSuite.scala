package tofu.optics
package core
import org.scalatest.{FlatSpec, Matchers}
import tofu.optics.tags.index

class ChainSuite extends FlatSpec with Matchers {
  def onion(x: Int, y: Int, z: Int) =
    Map("hello" -> Vector(List(Map(true -> x))), "world" -> Vector(), "!!!" -> Vector(List(Map(true -> y, false -> z))))

  "chains" should "do long composition using index" in {
    (chain(onion(4, 1, 2)) > index >@ "hello" > index >@ 0 > index >@ 0 > index >@ true update (3 +)) shouldBe
      onion(7, 1, 2)
  }
  import tofu.optics.tags.{every => _every}

  it should "do long compositions using each" in {
    ((chain(onion(4, 1, 2)) > _every > _every > _every > _every end) update (2 +)) shouldBe
      onion(6, 3, 4)
  }

  it should "do long compositions using each and index" in {
    (chain(onion(4, 1, 2)) > _every > _every > _every > index >@ false update (5 +)) shouldBe
      onion(4, 1, 7)
  }
}
