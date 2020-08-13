package tofu.data.calc

import tofu.data.Nothing2T

trait CalcRunner[-F[+_, +_]] {
  def apply[R, SI, SO, E, A](calc: CalcM[F, R, SI, SO, E, A])(r: R, init: SI): (SO, Either[E, A])
}

object CalcRunner extends LowPriorRunner {
  implicit val nothingRunner: CalcRunner[Nothing] = nothing2TRunner

}

trait LowPriorRunner { self: CalcRunner.type =>
  // scala 2.12 instance for use when Nothing breaks implicit search
  implicit val nothing2TRunner: CalcRunner[Nothing2T] = new CalcRunner[Nothing] {
    def apply[R, SI, SO, E, A](calc: CalcM[Nothing, R, SI, SO, E, A])(r: R, init: SI): (SO, Either[E, A]) =
      calc.step(r, init) match {
        case wrap: StepResult.Wrap[Nothing, _, _, SO, x, E, m, A] => wrap.inner: Nothing
        case StepResult.Error(s, err)                             => (s, Left(err))
        case StepResult.Ok(s, a)                                  => (s, Right(a))
      }
  }
}
