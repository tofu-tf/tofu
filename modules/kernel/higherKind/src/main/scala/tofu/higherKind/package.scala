package tofu

package object higherKind {
  type Pre[F[_], A] = Pre.T[F, A]

  type UnitK[A] = Unit

  type HKAny[_] = Any
}
