package tofu.optics

package object data extends DataInstancesLv0 {
  type Identity[+A]             = A
  type CoKleisli[F[+_], -A, +B] = F[A] => B
  type Proxy[+A]                = Unit
  type Tagged[-A, +B]           = Unit => B
  type Constant[+A, +B]         = A

}
