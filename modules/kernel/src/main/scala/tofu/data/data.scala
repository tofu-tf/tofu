package tofu

import cats.Functor
import cats.arrow.FunctionK
import scala.annotation.unchecked.{uncheckedVariance => uv}

package object data {
  type PArray[+A]                   = PArray.Type[A]
  type ICalc[-R, S, +E, +A]         = Calc[R, S, S, E, A]
  type Embedded[+F[+_], +G[+_], +A] = Embedded.T[F, G, A]
  type ExceptT[+F[+_], +E, +A]      = Embedded[F, Either[E, +*], A]

  type ∘[+F[+_], +G[+_], +A]  = Embedded.T[F, G, A]
  type Flux[+F[_], +G[_], +A] = Flux.FluxRepr[F, G, A]
  type Identity[+A]           = A
  type Nothing1 <: Nothing
  type NothingT[+A]           = Nothing
  type Nothing2T[+A, +B]      = Nothing

  val CalcM: tofu.data.calc.CalcM.type = tofu.data.calc.CalcM
  val CalcT: tofu.data.calc.CalcT.type = tofu.data.calc.CalcT

  type CalcM[+F[+_, +_], -R, -SI, +SO, +E, +A] = tofu.data.calc.CalcM[F, R, SI, SO, E, A]
  type ICalcM[+F[+_, +_], -R, S, +E, +A]       = CalcM[F, R, S, S, E, A]

  type UnaryM[+F[+_], +E, +A]              = F[A]
  type CalcT[+F[+_], -R, -SI, +SO, +E, +A] = CalcM[λ[(`+x`, `+y`) => F[y]], R, SI, SO, E, A]
  type ICalcT[+F[+_], -R, S, +E, +A]       = CalcT[F, R, S, S, E, A]

  implicit val nothingFunctor: Functor[Nothing] = new Functor[Nothing] {
    def map[A, B](fa: Nothing)(f: A => B): Nothing = fa
  }

  type FunK[-F[_], +G[_]] = FunctionK[F @uv, G @uv]
}
