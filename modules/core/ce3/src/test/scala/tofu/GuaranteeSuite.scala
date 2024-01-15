package tofu

import scala.annotation.nowarn
import cats.effect.kernel.MonadCancelThrow
import tofu.internal.carriers.FinallyCarrier3
import cats.effect.kernel.Outcome

object GuaranteeSuite {

  def summonFinally[F[_]] = new SummonSomeFinally[F]

  class SummonSomeFinally[F[_]] {
    def some[Exit[_]](implicit fin: Finally[F, Exit]): Finally[F, Exit]                                 = fin
    def carrier(implicit cc: FinallyCarrier3[F, Throwable]): FinallyCarrier3.Aux[F, Throwable, cc.Exit] = cc
  }

  def summonInstancesForBracket[F[_]](implicit F: MonadCancelThrow[F]): Unit = {
    implicitly[Guarantee[F]]

    implicitly[FinallyCarrier3.Aux[F, Throwable, Outcome[F, Throwable, _]]]

    implicitly[Finally[F, Outcome[F, Throwable, _]]]

    val xx                                               = summonFinally[F].some[Outcome[F, Throwable, _]]
    @nowarn val yy: Finally[F, Outcome[F, Throwable, _]] = xx

    ()
  }

}
