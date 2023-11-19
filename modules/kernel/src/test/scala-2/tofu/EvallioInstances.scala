package tofu

import cats.Defer
import cats.Applicative

trait EvallioInstances extends LowPrioEvallio {
  implicit def prioritized[F[_]: Delay]: Evallio[F] = Evallio.byDelay[F]
}

trait LowPrioEvallio {
  implicit def lowPrioEval[F[_]: Defer: Applicative]: Evallio[F] = Evallio.byDeferWithApplicative[F]
}
