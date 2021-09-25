package tofu.internal.carriers
import tofu.Fibers
import tofu.internal.WBInterop

abstract class FibersCarrier2[F[_]] {
  type Fib[_]
  type Exit[_]
  def content: Fibers[F, Exit, Fib]
}

object FibersCarrier2                  {
  type Aux[F[_], Ex[_], Fb[_]] = FibersCarrier2[F] { type Exit[a] = Ex[a]; type Fib[a] = Fb[a]; }

  trait Impl[F[_], Ex[_], Fb[_]] extends FibersCarrier2[F] with Fibers[F, Ex, Fb] {
    type Exit[a] = Ex[a]
    type Fib[a]  = Fb[a]
    def content = this
  }

  final implicit def startFromConcurrent[F[_], Exit[_], Fiber[_]]: Aux[F, Exit, Fiber] =
    macro WBInterop.delegate0[F, { val `tofu.interop.CE2Kernel.startFromConcurrent`: Unit }]
}

abstract class FibersCarrier3[F[_], E] {
  type Fib[_]
  type Exit[_]
  def content: Fibers[F, Exit, Fib]
}

object FibersCarrier3 {
  type Aux[F[_], E, Ex[_], Fb[_]] = FibersCarrier3[F, E] { type Exit[a] = Ex[a]; type Fib[a] = Fb[a]; }

  trait Impl[F[_], E, Ex[_], Fb[_]] extends FibersCarrier3[F, E] with Fibers[F, Ex, Fb] {
    type Exit[a] = Ex[a]
    type Fib[a]  = Fb[a]
    def content = this
  }

  final implicit def startFromConcurrent[F[_], E, Exit[_], Fiber[_]]: Aux[F, E, Exit, Fiber] =
    macro WBInterop.delegate1[F, E, { val `tofu.interop.CE3Kernel.startFromConcurrent`: Unit }]
}
