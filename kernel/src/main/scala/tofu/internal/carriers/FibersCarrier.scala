package tofu.internal.carriers
import tofu.Fibers
import tofu.internal.WBInterop

abstract class FibersCarrier[F[_]] {
  type Fib[_]
  type Exit[_]
  val content: Fibers[F, Exit, Fib]
}

object FibersCarrier {
  type Aux[F[_], Ex[_], Fb[_]] = FibersCarrier[F] { type Exit[a] = Ex[a]; type Fib[a] = Fb[a]; }
  def apply[F[_], Ex[_], Fb[_]](fin: Fibers[F, Ex, Fb]) = new FibersCarrier[F] {
    type Fib[a]  = Fb[a]
    type Exit[a] = Ex[a]
    val content = fin
  }

  final implicit def startFromConcurrent[F[_], Exit[_], Fiber[_]]: Aux[F, Exit, Fiber] =
    macro WBInterop.delegate0[F, { val `tofu.interop.CE2Kernel.startFromConcurrent`: Unit }]
}
