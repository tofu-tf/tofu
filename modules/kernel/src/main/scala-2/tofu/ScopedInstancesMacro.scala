package tofu

import scala.concurrent.ExecutionContext
import tofu.internal.Interop
import tofu.kernel.types._

trait ScopedInstancesMacro {

  /** make ScopedExecute instance with given tag using cats-effect2 hidden implicits are: Async[F], ContextShift[F]
    */
  final def makeExecuteCE2[Tag, F[_]](p1: ExecutionContext): ScopedExecute[Tag, F] =
    macro Interop.delegate1p1[Execute[F], Tag, F, { val `tofu.interop.CE2Kernel.makeExecute`: Unit }]

  /** make ScopedExecute instance with given tag using cats-effect3 hidden implicit is Async[F]
    */
  final def makeExecuteCE3[Tag, F[_]](p1: ExecutionContext): ScopedExecute[Tag, F] =
    macro Interop.delegate1p1[Execute[F], Tag, F, { val `tofu.interop.CE3Kernel.makeExecute`: Unit }]

}
