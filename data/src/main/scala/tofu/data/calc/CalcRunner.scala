package tofu.data.calc

import tofu.data.Nothing2T

import scala.annotation.tailrec

trait CalcRunner[-F[+_, +_]] {
  def apply[R, SI, SO, E, A, X](calc: CalcM[F, R, SI, SO, E, A])(r: R, init: SI, cont: Continue[A, E, SO, X]): X

  def runPair[R, SI, SO, E, A](calc: CalcM[F, R, SI, SO, E, A])(r: R, init: SI): (SO, Either[E, A]) = {
    val cont = new Continue[A, E, SO, (SO, Either[E, A])] {
      def success(s: SO, a: A) = (s, Right(a))
      def error(s: SO, e: E)   = (s, Left(e))
    }
    apply(calc)(r, init, cont)
  }
}

object CalcRunner extends LowPriorRunner {
  implicit val nothingRunner: CalcRunner[Nothing] = nothing2TRunner
}

trait LowPriorRunner { self: CalcRunner.type =>
  // scala 2.12 instance for use when Nothing breaks implicit search
  implicit val nothing2TRunner: CalcRunner[Nothing2T] = new CalcRunner[Nothing] {

    @tailrec
    override final def apply[R, SI, SO, E, A, X](
        calc: CalcM[Nothing, R, SI, SO, E, A]
    )(r: R, init: SI, cont: Continue[A, E, SO, X]): X =
      calc match {
        case res: CalcM.CalcMRes[R, SI, SO, E, A]                  => res.submit(r, init, cont)
        case d: CalcM.Defer[Nothing, R, _, _, _, _]                => apply(d.runStep())(r, init, cont)
        case m: CalcM.ProvideM[Nothing, R, SI, SO, E, A]           => apply(m.inner)(m.r, init, cont)
        case sub: CalcM.Sub[Nothing, SI, SO, E, A]                 => sub.fa
        case b1: CalcM.Bound[Nothing, R, SI, sm, SO, em, E, am, A] =>
          b1.src match {
            case res: CalcM.CalcMRes[_, _, _, _, _]                   =>
              val (st, next) = res.submit(r, init, b1.continue.withState[sm])
              apply(next)(r, st, cont)
            case d: CalcM.Defer[Nothing, R, _, _, _, _]               => apply(d.runStep().bind(b1.continue))(r, init, cont)
            case m: CalcM.ProvideM[Nothing, R, SI, _, _, _]           =>
              type Cont[r] = Continue[am, em, sm, CalcM[Nothing, r, sm, SO, E, A]]
              val kcont = m.any.substitute[Cont](b1.continue)
              apply(m.inner.bind(kcont))(m.r, init, cont)
            case sub: CalcM.Sub[Nothing, _, _, _, _]                  => sub.fa
            case b2: CalcM.Bound[Nothing, R, SI, sp, _, ep, _, ap, _] =>
              apply(b2.src.bind(Continue.compose(b2.continue, b1.continue)))(r, init, cont)
          }
      }

  }
}
