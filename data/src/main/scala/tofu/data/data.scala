package tofu

package object data {
  type PArray[+A]                = PArray.Type[A]
  type ICalc[-R, S, +E, +A]      = Calc[R, S, S, E, A]
  type Embed[+F[+_], +G[+_], +A] = Embed.T[F, G, A]
  type Identity[+A]                 = A
}
