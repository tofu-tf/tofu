package tofu

import cats.Functor
import cats.arrow.FunctionK
import scala.annotation.unchecked.{uncheckedVariance => uv}

package object data {
  type PArray[+A]                   = PArray.Type[A]
  type ICalc[-R, S, +E, +A]         = Calc[R, S, S, E, A]
  type Embedded[+F[+_], +G[+_], +A] = Embedded.T[F, G, A]
  type ∘[+F[+_], +G[+_], +A]        = Embedded.T[F, G, A]
  type Flux[+F[_], +G[_], +A]       = Flux.FluxRepr[F, G, A]
  type Identity[+A]                 = A
  type Nothing1 <: Nothing
  type NothingT[+A]                 = Nothing


  implicit val nothingFunctor: Functor[Nothing]  = new Functor[Nothing] {
    def map[A, B](fa: Nothing)(f: A => B): Nothing = fa
  }

  type FunK[-F[_], +G[_]] = FunctionK[F @uv, G @uv]

}
