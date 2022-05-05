package tofu.fs2Syntax

import cats.tagless.ApplyK
import fs2.Stream
import tofu.higherKind.Pre.T
import tofu.syntax.funk.funK

object pre {
  implicit final class TofuPreStreamSyntax[F[_], U[f[_]]](private val self: U[T[F, *]]) extends AnyVal {
    def attachStream(alg: U[Stream[F, *]])(implicit U: ApplyK[U]): U[Stream[F, *]] =
      U.map2K(self, alg)(funK(t2k => Stream.eval_(t2k.first.value) ++ t2k.second))
  }
}
