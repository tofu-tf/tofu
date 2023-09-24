package tofu

import scala.concurrent.ExecutionContext

import tofu.internal.Interop
import tofu.kernel.types._

trait ScopedInstancesMacro:
  /** make ScopedExecute instance with given tag using cats-effect2 hidden implicits are: Async[F], ContextShift[F]
    */
  implicit inline def makeExecuteCE2[Tag, F[_]](p1: ExecutionContext): ScopedExecute[Tag, F] =
    Interop.delegate0_1_p[Tag, F, ExecutionContext, ScopedExecute[Tag, F]]("tofu.interop.CE2Kernel.makeExecute", p1)

  /** make ScopedExecute instance with given tag using cats-effect3 hidden implicit is Async[F]
    */
  implicit inline def makeExecuteCE3[Tag, F[_]](p1: ExecutionContext): ScopedExecute[Tag, F] =
    Interop.delegate0_1_p[Tag, F, ExecutionContext, ScopedExecute[Tag, F]]("tofu.interop.CE3Kernel.makeExecute", p1)
