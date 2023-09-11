package tofu.internal.carriers

import scala.concurrent.Future

import tofu.internal.carriers.Kek
import tofu.internal.Interop

object xxx {

  implicit def instance1[F[_]]: Kek[F] =
    Interop.delegate1[F, Kek[F]]("tofu.internal.carriers.abc.testPath")

  implicit def instance1_0[F[_], E]: Kek1[F, E] =
    Interop.delegate1_0[F, E, Kek1[F, E]]("tofu.internal.carriers.abc.testPath1")

  implicit def instance2[F[_], I[_]]: Kek2[F, I] =
    Interop.delegate2[I, F, Kek2[F, I]]("tofu.internal.carriers.abc.testPath2")
}