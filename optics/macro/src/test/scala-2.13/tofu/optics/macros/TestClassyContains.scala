package tofu.optics.macros
import tofu.optics.{Contains, Label, PContains, Update}

object TestClassyContains {
  // compile test for searching classy optics
  implicitly[Contains[FooBar2, Int] with Label["i"]]
  implicitly[Contains[FooBar2, String] with Label["j"]]
  implicitly[Contains[FooBar4[Double], Int] with Label["i"]]
  implicitly[Contains[FooBar4[Double], String] with Label["j"]]
  implicitly[Contains[FooBar4[Double], String]]
  implicitly[Update[FooBar4[Double], String]]
  implicitly[PContains[FooBar4[Double], FooBar4[Long], Double, Long] with Label["x"]]
  implicitly[Contains[FooBar5[String], List[String]] with Label["x"]]
}
