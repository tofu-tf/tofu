package tofu.streams

import cats.Applicative
import cats.instances.list._
import tofu.syntax.monadic._
import tofu.syntax.streams.all._

object SyntaxTypeInferenceSuite {

  class EvalsInference[S[_]: ({ type L[x[_]] = Evals[x, F] })#L, F[_]: Applicative] {
    def foo: S[Unit] = eval(().pure[F])
    def bar: S[Unit] = evals(List((), (), ()).pure[F])
  }
}
