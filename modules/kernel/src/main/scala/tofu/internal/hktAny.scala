package tofu.internal

object hktAny {
  type AnyK[A] = Any

  type AnyKK[+A, +B] = Any
}
