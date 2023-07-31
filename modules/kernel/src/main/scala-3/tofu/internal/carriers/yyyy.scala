package tofu.internal.carriers

import scala.concurrent.Future

import tofu.internal.carriers.Kek

object xxx {

  implicit def instance[F[_]]: Kek[F] = {
    TestMacro.delegate0[F, Kek]("tofu.internal.carriers.abc.testPath")
  }
}