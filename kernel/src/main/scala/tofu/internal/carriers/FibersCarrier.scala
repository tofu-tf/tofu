package tofu.internal.carriers
import tofu.Fibers
import tofu.internal.WBInterop

abstract class FibersCarrier[F[_]] {
  type Fib[_]

  val content: Fibers[F, Fib]
}

object FibersCarrier {
  type Aux[F[_], Fb[_]] = FibersCarrier[F] { type Fib[a] = Fb[a] }
  def apply[F[_], Fb[_]](fin: Fibers[F, Fb]) = new FibersCarrier[F] {
    type Fib[a] = Fb[a]
    val content = fin
  }

  final implicit def startFromConcurrent[F[_], Exit[_]]: Aux[F, Exit] =
    macro WBInterop.delegate0[F, { val `tofu.interop.CE2Kernel.startFromConcurrent`: Unit }]
}
