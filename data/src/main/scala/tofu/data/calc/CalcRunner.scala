package tofu.data.calc

trait CalcRunner[-F[+_, +_]] {
  def apply[R, SI, SO, E, A](calc: CalcM[F, R, SI, SO, E, A])(r: R, init: SI): (SO, Either[E, A])
}

object CalcRunner {
  implicit val nothingRunner: CalcRunner[Nothing] =
    new CalcRunner[Nothing] {
      def apply[R, SI, SO, E, A](calc: CalcM[Nothing, R, SI, SO, E, A])(r: R, init: SI): (SO, Either[E, A]) =
        calc.step(r, init) match {
          case wrap: StepResult.Wrap[Nothing, _, _, SO, x, E, m, A] => wrap.inner: Nothing
          case StepResult.Error(s, err)                             => (s, Left(err))
          case StepResult.Ok(s, a)                                  => (s, Right(a))
        }
    }
}
