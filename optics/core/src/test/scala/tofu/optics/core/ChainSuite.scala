package tofu.optics
package core
import org.scalatest.FlatSpec
import org.scalatest.Matchers.convertToAnyShouldWrapper
import tofu.optics.tags.{every, index}
import language.postfixOps

class ChainSuite extends FlatSpec {
  def onion(x: Int, y: Int, z: Int) =
    Map("hello" -> Vector(List(Map(true -> x))), "world" -> Vector(), "!!!" -> Vector(List(Map(true -> y, false -> z))))

  "chains" should "do long composition using index" in {
    (chain(onion(4, 1, 2)) > index >@ "hello" > index >@ 0 > index >@ 0 > index >@ true update (3 +)) shouldBe
      onion(7, 1, 2)
  }

  it should "do long compositions using each" in {
    (chain(onion(4, 1, 2)) > every > every > every > every >@ {} put 1000) shouldBe
      onion(1000, 1000, 1000)
  }

  it should "do long compositions using each and index" in {
    (chain(onion(4, 1, 2)) > every > every > every > index >@ false getAll) shouldBe List(2)
  }
}
