package tofu.optics
package core

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.shouldBe
import tofu.optics.tags.{every, index}

import language.postfixOps

class ChainSuite extends AnyFlatSpec {
  type Onion[A] = Map[String, Vector[List[Map[Boolean, A]]]]

  def onion[A](x: A, y: A, z: A): Onion[A] =
    Map("hello" -> Vector(List(Map(true -> x))), "world" -> Vector(), "!!!" -> Vector(List(Map(true -> y, false -> z))))

  case class OnionSoup[A](onion: Onion[A])
  def soup[A](x: A, y: A, z: A): OnionSoup[A] = OnionSoup(onion(x, y, z))

  def _onion[A] = Equivalent[OnionSoup[A]](_.onion)(OnionSoup(_))

  "chains" should "do long composition using index" in {
    (chain(onion(4, 1, 2)) > index >@ "hello" > index >@ 0 > index >@ 0 > index >@ true update (3 +)) shouldBe
      onion(7, 1, 2)

    (
      soup(4, 1, 2) ->: _onion to index app "hello" to index app 0 to index app 0 to index app true update (3 +)
    ) shouldBe soup(7, 1, 2)

    (
      _onion[Int] > index >@ "hello" > index >@ 0 > index >@ 0 > index >@ true update (soup(4, 1, 2), 3 +)
    ) shouldBe soup(7, 1, 2)
  }

  it should "do long compositions using each" in {
    (chain(onion(4, 1, 2)) > every > every > every > every >@ {} put 1000) shouldBe
      onion(1000, 1000, 1000)

    (soup(4, 1, 2) ->: _onion to every to every to every to every app {} put 1000) shouldBe
      soup(1000, 1000, 1000)

    (_onion[Int] > every > every > every > every >@ {} put (soup(4, 1, 2), 1000)) shouldBe soup(1000, 1000, 1000)
  }

  it should "do long compositions using each and index" in {
    (chain(onion(4, 1, 2)) > every > every > every > index >@ false getAll) shouldBe List(2)

    (soup(4, 1, 2) ->: _onion to every to every to every to index app false getAll) shouldBe List(2)

    (_onion[Int] > every > every > every > index >@ false getAll soup(4, 1, 2)) shouldBe List(2)
  }
}
