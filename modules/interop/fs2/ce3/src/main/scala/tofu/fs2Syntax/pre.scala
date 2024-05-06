package tofu.fs2Syntax

import cats.tagless.ApplyK
import cats.data.Tuple2K
import fs2.Stream
import tofu.higherKind.Pre.T
import tofu.syntax.funk.funK

object pre {
  implicit final class TofuPreStreamSyntax[F[_], U[f[_]]](private val self: U[T[F, _]]) extends AnyVal {
    def attachStream(alg: U[Stream[F, _]])(implicit U: ApplyK[U]): U[Stream[F, _]] =
      U.map2K(self, alg)(
        funK[Tuple2K[T[F, _], Stream[F, _], _], Stream[F, _]](t2k => Stream.exec(t2k.first.value) ++ t2k.second)
      )
  }
}
