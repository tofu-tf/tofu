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

  //promote tests
  implicitly[Contains[FooInner, FooField1]]
  implicitly[Contains[FooInner, FooField2]]

  implicitly[Contains[FooBar2, FooInner] with Label["inner"]]
  implicitly[Contains[FooBar2, FooField1]]
  implicitly[Contains[FooBar2, FooField2]]

  implicitly[Contains[FooBar5[Int], FooInner] with Label["inner"]]
  implicitly[Contains[FooBar5[Double], FooField1]]
  implicitly[Contains[FooBar5[String], FooField2]]
}
