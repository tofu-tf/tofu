package tofu

package object data {
  type PArray[+A]                   = PArray.Type[A]
  type ICalc[-R, S, +E, +A]         = Calc[R, S, S, E, A]
  type Embedded[+F[+_], +G[+_], +A] = Embedded.T[F, G, A]
  type ∘[+F[+_], +G[+_], +A]        = Embedded.T[F, G, A]
  type Flux[+F[_], +G[_], +A]       = Flux.FluxRepr[F, G, A]
  type Identity[+A]                 = A
}
