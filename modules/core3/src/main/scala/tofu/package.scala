import cats.data.Ior
import cats.effect.Fiber
import cats.effect.kernel.Outcome
import tofu.kernel.KernelTypes

package object tofu extends KernelTypes {

  type IorC[C[_], E, A] = Ior[C[E], C[A]]

  type GenStart[F[_], E] = Fibers[F, Outcome[F, E, *], Fiber[F, E, *]]
  type Start[F[_]]       = GenStart[F, Throwable]

  type Timeout[F[_]] = tofu.time.Timeout[F]

  val Timeout: tofu.time.Timeout.type = tofu.time.Timeout
}
