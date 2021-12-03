package tofu

import cats.effect.kernel.{MonadCancelThrow, Outcome}
import tofu.internal.carriers.FinallyCarrier3

import scala.annotation.nowarn

object GuaranteeSuite {

  def summonFinally[F[_]] = new SummonSomeFinally[F]

  class SummonSomeFinally[F[_]] {
    def some[Exit[_]](implicit fin: Finally[F, Exit]): Finally[F, Exit]                                 = fin
    def carrier(implicit cc: FinallyCarrier3[F, Throwable]): FinallyCarrier3.Aux[F, Throwable, cc.Exit] = cc
  }

  def summonInstancesForBracket[F[_]](implicit F: MonadCancelThrow[F]): Unit = {
    implicitly[Guarantee[F]]

    implicitly[FinallyCarrier3.Aux[F, Throwable, Outcome[F, Throwable, *]]]

    implicitly[Finally[F, Outcome[F, Throwable, *]]]

    val xx                                               = summonFinally[F].some
    @nowarn val yy: Finally[F, Outcome[F, Throwable, *]] = xx

    ()
  }

}
