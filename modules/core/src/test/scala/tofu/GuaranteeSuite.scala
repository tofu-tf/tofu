package tofu

import scala.annotation.nowarn
import tofu.interop.CE2Kernel.CEExit
import tofu.internal.carriers.FinallyCarrier2

object GuaranteeSuite {

  def summonFinally[F[_]] = new SummonSomeFinally[F]

  class SummonSomeFinally[F[_]] {
    def some[Exit[_]](implicit fin: Finally[F, Exit]): Finally[F, Exit]                                 = fin
    def carrier(implicit cc: FinallyCarrier2[F, Throwable]): FinallyCarrier2.Aux[F, Throwable, cc.Exit] = cc
  }

  def summonInstancesForBracket[F[_]: BracketThrow](): Unit = {
    implicitly[Guarantee[F]]

    implicitly[FinallyCarrier2.Aux[F, Throwable, CEExit[Throwable, *]]]

    implicitly[Finally[F, CEExit[Throwable, *]]]

    val xx                                           = summonFinally[F].some
    @nowarn val yy: Finally[F, CEExit[Throwable, *]] = xx

    ()
  }

}
