package tofu.data.calc
import tofu.compat.uv212
import tofu.control.Bind

sealed trait StepResult[+F[+_, +_], +S, +E, +A]

object StepResult {
  sealed trait Now[+S, +E, +A] extends StepResult[Nothing, S, E, A] {
    def state: S
    def result: Either[E, A] = this match {
      case Ok(_, a)    => Right(a)
      case Error(_, e) => Left(e)
    }
  }

  final case class Ok[+S, +A](state: S, value: A)    extends Now[S, Nothing, A]
  final case class Error[+S, +E](state: S, error: E) extends Now[S, E, Nothing]
  final case class Wrap[+F[+_, +_], R, S1, +S2, X, +E, M, +A](
      input: R,
      state: S1,
      inner: F[X, M],
      fail: X => CalcM[F, R, S1, S2, E, A],
      succ: M => CalcM[F, R, S1, S2, E, A],
  )                                                  extends StepResult[F, S2, E, A] {
    def stepFailure(x: X): StepResult[F, S2, E, A] = fail(x).step(input, state)
    def stepSuccess(m: M): StepResult[F, S2, E, A] = succ(m).step(input, state)

    def provided[F1[+x, +y] >: F[x, y] @uv212](implicit
        F: Bind[F1]
    ): F1[CalcM[F1, Any, Any, S2, E, A], CalcM[F1, Any, Any, S2, E, A]] =
      F.bimap(inner)(x => fail(x).provideSet(input, state), m => succ(m).provideSet(input, state))
  }
}
