package tofu

import cats.effect.ExitCase

object GuaranteeSuite {

  def summonInstancesForBracket[F[_]: BracketThrow](): Unit = {
    implicitly[Guarantee[F]]
    implicitly[Finally[F, TConst[ExitCase[Throwable], *]]](Finally.fromBracket)
    ()
  }

}
