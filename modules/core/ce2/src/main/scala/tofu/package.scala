import cats.effect.Bracket
import cats.effect.Fiber
import cats.data.Ior
import cats.Id

package object tofu extends tofu.kernel.KernelTypes {

  type BracketThrow[F[_]] = Bracket[F, Throwable]

  type IorC[C[_], E, A] = Ior[C[E], C[A]]

  type Start[F[_]] = Fibers[F, Id, Fiber[F, _]]

  type Timeout[F[_]] = tofu.time.Timeout[F]

  val Timeout: tofu.time.Timeout.type = tofu.time.Timeout
}
