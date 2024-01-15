package tofu

import scala.compiletime.{summonFrom, summonInline}

import cats.{Applicative, Defer}

trait EvallioInstances:
  inline given [F[_]]: Evallio[F] = summonFrom {
    case delay: Delay[F] => Evallio.byDelay[F](delay)
    case _               => Evallio.byDeferWithApplicative[F](summonInline[Defer[F]], summonInline[Applicative[F]])
  }
