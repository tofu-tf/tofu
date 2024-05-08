package tofu.common.derived

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import tofu.common.Display
import tofu.common.Display._
import tofu.common.derived.DisplayData._

class DisplaySpec extends AnyFunSpec with Matchers {
  describe("derivation") {

    describe("simple cases") {
      val bar = Bar(
        3,
        "abc"
      )

      val expectedBar =
        """Bar {
          |	value = 3,
          |	another = "abc"
          |}""".stripMargin

      val expectedBarNoLabels =
        """Bar {
          |	3,
          |	"abc"
          |}""".stripMargin

      val expectedBarBuild: Vector[String] =
        Vector("Bar {\n", "\tvalue = 3,\n", "\tanother = \"abc\"\n", "}")

      it("should display case classes string") {
        bar.display() shouldBe expectedBar
      }
      it("should display case classes build") {
        val build = Display[Bar].displayBuild(Display.Config.default, bar).value
        build shouldBe expectedBarBuild
      }

      it("should display case classes string without labels") {
        bar.display(config = Display.Config.default.copy(showFieldLabels = false)) shouldBe expectedBarNoLabels
      }

      it("should display sealed traits") {
        val adt: FooBar = FooBar.Barn(3)
        adt.display() shouldBe "Barn {\n\ti = 3\n}"
      }

      it("should display empty display as empty string") {
        Bjarn(Emptiness()).display() shouldBe "Bjarn {\n\ti = \n}"
      }

    }

    describe("nested case classes") {
      val foo =
        Foo(
          bar = Bar(
            3,
            "abc"
          ),
          field = 3.4,
          xs = List(1, 2, 3)
        )

      val expectedFoo                      =
        """Foo {
          |	bar = Bar {
          |		value = 3,
          |		another = "abc"
          |	},
          |	field = 3.4,
          |	xs = List(1, 2, 3)
          |}""".stripMargin
      val expectedFooBuild: Vector[String] =
        Vector(
          "Foo {\n",
          "\tbar = Bar {\n",
          "\t\tvalue = 3,\n",
          "\t\tanother = \"abc\"\n",
          "\t},\n",
          "\tfield = 3.4,\n",
          "\txs = List(1, 2, 3)\n",
          "}"
        )

      it("should display complex case classes string") {
        foo.display() shouldBe expectedFoo
      }
      it("should display complex case classes build") {
        val build = Display[Foo].displayBuild(Display.Config.default, foo).value
        build shouldBe expectedFooBuild
      }

      it("should display nested non newlined case classes without indents") {
        val cont: Cont           = Cont(4, Nested(5, 6, 7))
        val expectedCont: String =
          """Cont {
            |	a = 4,
            |	b = Nested {a = 5, b = 6, c = 7}
            |}""".stripMargin
        cont.display() shouldBe expectedCont
      }
    }
  }
}
