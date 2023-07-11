package tofu.internal.carriers
import tofu.Fibers

abstract class FibersCarrier2[F[_]] {
  type Fib[_]
  type Exit[_]
  def content: Fibers[F, Exit, Fib]
}

object FibersCarrier2 extends FibersCarrier2Macro {
  type Aux[F[_], Ex[_], Fb[_]] = FibersCarrier2[F] { type Exit[a] = Ex[a]; type Fib[a] = Fb[a]; }

  trait Impl[F[_], Ex[_], Fb[_]] extends FibersCarrier2[F] with Fibers[F, Ex, Fb] {
    type Exit[a] = Ex[a]
    type Fib[a]  = Fb[a]
    def content = this
  }

}

abstract class FibersCarrier3[F[_], E] {
  type Fib[_]
  type Exit[_]
  def content: Fibers[F, Exit, Fib]
}

object FibersCarrier3 extends FibersCarrier3Macro {
  type Aux[F[_], E, Ex[_], Fb[_]] = FibersCarrier3[F, E] { type Exit[a] = Ex[a]; type Fib[a] = Fb[a]; }

  trait Impl[F[_], E, Ex[_], Fb[_]] extends FibersCarrier3[F, E] with Fibers[F, Ex, Fb] {
    type Exit[a] = Ex[a]
    type Fib[a]  = Fb[a]
    def content = this
  }

}
